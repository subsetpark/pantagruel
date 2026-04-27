/**
 * TS AST → L1 statements (mutating-body recognizers).
 *
 * Recognizes the mutating-body shapes ts2pant handles and produces
 * canonical L1 forms:
 *
 * - `buildL1IfMutation` — `if (g) { … }` / `if (g) { … } else { … }`
 *   where each branch is a sequence of property assignments,
 *   compound assigns (desugared to `obj.p = obj.p OP v`), Map/Set
 *   effect calls, or nested ifs (recursed). Builds an L1 `cond-stmt`.
 * - `buildL1ForOfMutation` / `buildL1ForEachCall` — `for (const x of arr)
 *   { … }` and `arr.forEach(x => { … })`. Body statements are classified
 *   into Shape A (uniform iterator writes — `x.p = e`) and Shape B
 *   (accumulator folds — `a.p OP= f(x)`); the latter become
 *   `IR1FoldLeaf` entries on the foreach. A build-time subState lets
 *   Shape B `rhs`/`guard` translations observe in-iter Shape A writes.
 *
 * Sub-expressions (conditions, receivers, values) translate via the
 * existing `translateBodyExpr` and wrap as `ir1FromL2`. The lower pass
 * unwraps via `lowerL1Expr` (passes the OpaqueExpr through verbatim).
 *
 * Property names are qualified at build time via `qualifyFieldAccess`
 * so the L1 form carries the Pantagruel rule symbol, not the raw TS
 * field name.
 */

import ts from "typescript";
import type { IRBinop } from "./ir.js";
import { irWrap } from "./ir.js";
import {
  type IR1Expr,
  type IR1FoldLeaf,
  type IR1ForeachBody,
  type IR1Stmt,
  ir1Assign,
  ir1Block,
  ir1CondStmt,
  ir1Foreach,
  ir1FromL2,
  ir1MapDelete,
  ir1MapSet,
  ir1Member,
  ir1SetAddOrDelete,
  ir1SetClear,
} from "./ir1.js";
import type { OpaqueExpr } from "./pant-ast.js";
import {
  addWrittenKey,
  ambiguousFieldMsg,
  bodyExpr,
  cloneSymbolicState,
  expressionHasSideEffects,
  expressionReferencesNames,
  freshHygienicBinder,
  getRootIdentifier,
  isBodyEffect,
  isBodyUnsupported,
  isGuardStatement,
  putWrite,
  qualifyFieldAccess,
  rejectEffect,
  type SymbolicState,
  symbolicKey,
  translateBodyExpr,
  translateCallExpr,
  type UniqueSupply,
  unwrapExpression,
} from "./translate-body.js";
import type { NumericStrategy } from "./translate-types.js";

export interface BuildBodyCtx {
  checker: ts.TypeChecker;
  strategy: NumericStrategy;
  paramNames: ReadonlyMap<string, string>;
  state: SymbolicState;
  supply: UniqueSupply;
  applyConst: (e: OpaqueExpr) => OpaqueExpr;
}

export type BuildResult<T> = T | { unsupported: string };

export function isUnsupported<T>(
  x: BuildResult<T>,
): x is { unsupported: string } {
  return (
    typeof x === "object" &&
    x !== null &&
    "unsupported" in (x as Record<string, unknown>)
  );
}

/**
 * Validate that a built `IR1Stmt` is admissible as a `foreach` body:
 * only `assign` (with `member` target), `cond-stmt`, and `block`s of
 * those. The cast to `IR1ForeachBody` is type-level only — `cond-stmt`
 * arms are `IR1Stmt` in the static type, so we recurse into them at
 * runtime to keep the M3 Shape A contract honest. Map/Set effects
 * inside an `if` branch in the foreach body are rejected here rather
 * than later in `lowerForeach`'s sub-state walk.
 */
function ensureForeachBodyShape(stmt: IR1Stmt): BuildResult<IR1ForeachBody> {
  if (stmt.kind === "assign") {
    return stmt;
  }
  if (stmt.kind === "block") {
    for (const child of stmt.stmts) {
      const r = ensureForeachBodyShape(child);
      if (isUnsupported(r)) {
        return r;
      }
    }
    return stmt as IR1ForeachBody;
  }
  if (stmt.kind === "cond-stmt") {
    for (const [, arm] of stmt.arms) {
      const r = ensureForeachBodyShape(arm);
      if (isUnsupported(r)) {
        return r;
      }
    }
    if (stmt.otherwise !== null) {
      const r = ensureForeachBodyShape(stmt.otherwise);
      if (isUnsupported(r)) {
        return r;
      }
    }
    return stmt as IR1ForeachBody;
  }
  return {
    unsupported: `foreach body cannot contain a ${stmt.kind} statement (Shape A is assign-only)`,
  };
}

/**
 * Recognize a TS `for (const x of arr) { body }` and build the
 * canonical L1 `foreach` form. Body statements are classified into
 * Shape A (uniform iterator write — `x.p = e`) and Shape B
 * (accumulator fold — `a.p OP= f(x)`) via `buildL1ForeachBody`. Shape A
 * statements become L1 stmts in the foreach body; Shape B statements
 * become `IR1FoldLeaf` entries.
 *
 * The TS iter name is bound to a fresh hygienic binder (`$N`) in a
 * copy of `paramNames`, so reads of `x.p` inside the body resolve to
 * `prop $N` — the IR binder cannot collide with parameter names or
 * accessor rules and obeys the document-wide UniqueSupply invariant.
 */
export function buildL1ForOfMutation(
  stmt: ts.ForOfStatement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  const initList = stmt.initializer;
  if (
    !ts.isVariableDeclarationList(initList) ||
    !(initList.flags & ts.NodeFlags.Const) ||
    initList.declarations.length !== 1
  ) {
    return {
      unsupported: "for-of initializer must be a single const binding",
    };
  }
  const decl = initList.declarations[0]!;
  if (!ts.isIdentifier(decl.name)) {
    return { unsupported: "for-of destructuring pattern is not supported" };
  }
  const iterName = decl.name.text;
  const sourceR = rejectEffect(
    translateBodyExpr(
      stmt.expression,
      ctx.checker,
      ctx.strategy,
      ctx.paramNames,
      ctx.state,
      ctx.supply,
    ),
  );
  if (isBodyUnsupported(sourceR)) {
    return { unsupported: sourceR.unsupported };
  }
  const source = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(sourceR))));
  return finishForeach(iterName, source, stmt.statement, ctx);
}

/**
 * Recognize a TS `arr.forEach(x => body)` call-statement and build
 * the canonical L1 `foreach` form. Same structure as `buildL1ForOf`,
 * just with the iter binder coming from the arrow callback.
 */
export function buildL1ForEachCall(
  call: ts.CallExpression,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  if (
    !ts.isPropertyAccessExpression(call.expression) ||
    call.expression.name.text !== "forEach" ||
    call.arguments.length !== 1
  ) {
    return { unsupported: "not a forEach call" };
  }
  const arg = call.arguments[0]!;
  if (!ts.isArrowFunction(arg)) {
    return { unsupported: "forEach callback must be an arrow function" };
  }
  if (
    arg.parameters.length !== 1 ||
    !ts.isIdentifier(arg.parameters[0]!.name)
  ) {
    return {
      unsupported: "forEach callback must take a single identifier parameter",
    };
  }
  const iterName = arg.parameters[0]!.name.text;
  const sourceR = rejectEffect(
    translateBodyExpr(
      call.expression.expression,
      ctx.checker,
      ctx.strategy,
      ctx.paramNames,
      ctx.state,
      ctx.supply,
    ),
  );
  if (isBodyUnsupported(sourceR)) {
    return { unsupported: sourceR.unsupported };
  }
  const source = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(sourceR))));
  const bodyStmt = ts.isBlock(arg.body)
    ? arg.body
    : ts.factory.createBlock([ts.factory.createExpressionStatement(arg.body)]);
  return finishForeach(iterName, source, bodyStmt, ctx);
}

function finishForeach(
  iterName: string,
  source: IR1Expr,
  bodyStmt: ts.Statement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  const binder = freshHygienicBinder(ctx.supply);
  const r = buildL1ForeachBody(bodyStmt, iterName, binder, ctx);
  if (isUnsupported(r)) {
    return r;
  }
  const { bodyStmts, foldLeaves } = r;
  // bodyStmts comes from `classifyForeachStmt` which only produces
  // `assign` / `cond-stmt` / `map-effect` / `set-effect` (Shape A
  // iterator writes) — all `IR1ForeachBody` shapes. The block here
  // is also Shape-A-only, satisfying `IR1ForeachBody.block`'s
  // self-recursive constraint.
  const body: IR1ForeachBody | null =
    bodyStmts.length === 0
      ? null
      : bodyStmts.length === 1
        ? bodyStmts[0]!
        : ({
            kind: "block",
            stmts: [bodyStmts[0]!, ...bodyStmts.slice(1)],
          } as IR1ForeachBody);
  if (body === null && foldLeaves.length === 0) {
    return { unsupported: "empty foreach body" };
  }
  return ir1Foreach(binder, source, body, foldLeaves);
}

interface ForeachBodyResult {
  bodyStmts: IR1ForeachBody[];
  foldLeaves: IR1FoldLeaf[];
}

interface ShapeBLeafTS {
  /** Accumulator base expression (must not depend on iterator). */
  target: ts.Expression;
  /** Qualified rule name for the property. */
  prop: string;
  /** Inner combiner (add/mul/and/or). */
  combiner: "add" | "mul" | "and" | "or";
  /** Outer binary op joining prior state to the comprehension. */
  outerOp: IRBinop;
  /** Per-iter contribution (may reference iterator). */
  rhs: ts.Expression;
  /** Optional guard from a wrapping `if (g(x)) a.p OP= rhs`. */
  guard: ts.Expression | null;
}

type ForeachStmtClass =
  | { kind: "shapeA"; built: IR1ForeachBody }
  | { kind: "shapeB"; leaves: ShapeBLeafTS[] };

/**
 * TS compound-assign operator → fold-info: inner combiner + outer op.
 *
 * `+=` / `-=` use `+` as combiner (additive identity 0). `-=` accumulates
 * with `+` inside, then subtracts the aggregate outside (non-commutative
 * outer `-` paired with commutative combiner `+` is sound).
 *
 * `*=` / `/=` similarly pair `*` combiner with `*`/`/` outer.
 */
const COMPOUND_ASSIGN_TO_FOLD: Map<
  ts.SyntaxKind,
  { combiner: "add" | "mul"; outerOp: IRBinop }
> = new Map([
  [ts.SyntaxKind.PlusEqualsToken, { combiner: "add", outerOp: "add" }],
  [ts.SyntaxKind.MinusEqualsToken, { combiner: "add", outerOp: "sub" }],
  [ts.SyntaxKind.AsteriskEqualsToken, { combiner: "mul", outerOp: "mul" }],
  [ts.SyntaxKind.SlashEqualsToken, { combiner: "mul", outerOp: "div" }],
]);

/**
 * Classify and build the body of a foreach loop. Each statement is
 * either Shape A (per-iter property write on the iter binder) or Shape
 * B (accumulator-fold contribution `a.p OP= f(x)`). Shape A items are
 * built into IR1 stmts using a build-time subState that tracks
 * accumulated Shape A property writes — so subsequent Shape B reads of
 * the iter binder's properties resolve through the in-iter writes.
 *
 * Returns `{ bodyStmts, foldLeaves }`. The caller composes these into
 * an `ir1Foreach`.
 */
function buildL1ForeachBody(
  bodyStmt: ts.Statement,
  iterName: string,
  binder: string,
  ctx: BuildBodyCtx,
): BuildResult<ForeachBodyResult> {
  // Strip guard statements (`assert(...)`, `if (g) throw …`) before
  // classification, mirroring what `symbolicExecute` does for top-level
  // bodies. Without this, a guarded loop body like
  // `for (const x of xs) { assert(x.amount > 0); a.total += x.amount; }`
  // would fail classification on the guard call.
  const rawStmts = ts.isBlock(bodyStmt)
    ? Array.from(bodyStmt.statements)
    : [bodyStmt];
  const stmts = rawStmts.filter((s) => !isGuardStatement(s, ctx.checker));
  // Build-time subState: forks the outer state so prior writes are
  // visible during in-iter sub-expression translation, then accumulates
  // Shape A property writes so Shape B rhs/guard sub-expressions observe
  // them. The fork prevents Shape A writes from leaking back into the
  // outer state. The lower pass runs its own subState over the body
  // statements; this one is purely local to the build pass.
  const subState = cloneSymbolicState(ctx.state);
  const subParams = new Map(ctx.paramNames);
  subParams.set(iterName, binder);
  const subCtx: BuildBodyCtx = {
    ...ctx,
    state: subState,
    paramNames: subParams,
  };

  const bodyStmts: IR1ForeachBody[] = [];
  const foldLeaves: IR1FoldLeaf[] = [];
  // Track whether any Shape A statement is a non-bare-assign (i.e., a
  // cond-stmt). `simulateShapeA` only models bare assigns into the
  // subState, so a guarded Shape A write leaves the iter binder's
  // properties at their pre-iter value during Shape B `rhs`/`guard`
  // translation. Combining the two would silently produce a fold-RHS
  // built against the stale iter property — reject conservatively.
  let hasGuardedShapeA = false;

  for (const s of stmts) {
    const cls = classifyForeachStmt(s, iterName, subCtx, ctx, null);
    if (isUnsupported(cls)) {
      return cls;
    }
    if (cls.kind === "shapeA") {
      bodyStmts.push(cls.built);
      if (cls.built.kind !== "assign") {
        hasGuardedShapeA = true;
      }
      // Simulate the property write into subState. We re-translate the
      // assign so subsequent Shape B reads observe the value computed
      // against the in-iter scope.
      const sim = simulateShapeA(s, subCtx);
      if (isUnsupported(sim)) {
        return sim;
      }
    } else {
      if (hasGuardedShapeA) {
        return {
          unsupported:
            "guarded Shape A iterator write is not supported alongside a Shape B fold leaf — the fold would read the iter binder's pre-iter value",
        };
      }
      for (const leaf of cls.leaves) {
        const built = buildShapeBLeaf(leaf, ctx, subCtx);
        if (isUnsupported(built)) {
          return built;
        }
        foldLeaves.push(built);
      }
    }
  }

  return { bodyStmts, foldLeaves };
}

/**
 * Decide whether a TS statement is Shape A or Shape B at the AST level.
 * Drives both build-pass output (Shape A → L1 stmt; Shape B → fold leaf)
 * and the compatibility check on nested ifs.
 *
 * `parentGuard` propagates from a wrapping `if (g(x)) a.p OP= …` —
 * when present, the inner Shape B leaf folds the guard into its
 * comprehension. Limited to a single level (per legacy
 * classifyLoopStmt).
 */
function classifyForeachStmt(
  stmt: ts.Statement,
  iterName: string,
  subCtx: BuildBodyCtx,
  outerCtx: BuildBodyCtx,
  parentGuard: ts.Expression | null,
): BuildResult<ForeachStmtClass> {
  if (
    ts.isExpressionStatement(stmt) &&
    ts.isBinaryExpression(unwrapExpression(stmt.expression))
  ) {
    const bin = unwrapExpression(stmt.expression) as ts.BinaryExpression;
    if (!ts.isPropertyAccessExpression(bin.left)) {
      return {
        unsupported: "loop body assignment target must be a property access",
      };
    }
    const rootName = getRootIdentifier(bin.left.expression);
    const isSimpleAssign = bin.operatorToken.kind === ts.SyntaxKind.EqualsToken;
    const compoundFold = COMPOUND_ASSIGN_TO_FOLD.get(bin.operatorToken.kind);

    if (rootName === iterName) {
      if (!isSimpleAssign) {
        // Compound-assign on an iter property is allowed (Slice 3 desugar).
        // Build via the standard assign builder against subCtx.
        const built = buildL1AssignStmt(stmt as ts.ExpressionStatement, subCtx);
        if (isUnsupported(built)) {
          return built;
        }
        if (parentGuard !== null) {
          return {
            unsupported:
              "Shape A iterator write under an if-guard is not supported in foreach",
          };
        }
        // `buildL1AssignStmt` only emits `ir1Assign(...)` (kind "assign"),
        // a member of `IR1ForeachBody`.
        return { kind: "shapeA", built: built as IR1ForeachBody };
      }
      const built = buildL1AssignStmt(stmt as ts.ExpressionStatement, subCtx);
      if (isUnsupported(built)) {
        return built;
      }
      if (parentGuard !== null) {
        return {
          unsupported:
            "Shape A iterator write under an if-guard is not supported in foreach",
        };
      }
      return { kind: "shapeA", built: built as IR1ForeachBody };
    }
    if (compoundFold === undefined) {
      return {
        unsupported:
          "loop accumulator write must use a compound assignment (+=, -=, *=, /=)",
      };
    }
    if (expressionReferencesNames(bin.left.expression, new Set([iterName]))) {
      return { unsupported: "loop accumulator target depends on iterator" };
    }
    const receiverType = outerCtx.checker.getTypeAtLocation(
      bin.left.expression,
    );
    const rawProp = bin.left.name.text;
    const propName = qualifyFieldAccess(
      receiverType,
      rawProp,
      outerCtx.checker,
      outerCtx.strategy,
      outerCtx.supply.synthCell,
    );
    if (propName === null) {
      return { unsupported: ambiguousFieldMsg(rawProp) };
    }
    const leaf: ShapeBLeafTS = {
      target: bin.left.expression,
      prop: propName,
      combiner: compoundFold.combiner,
      outerOp: compoundFold.outerOp,
      rhs: bin.right,
      guard: parentGuard,
    };
    return { kind: "shapeB", leaves: [leaf] };
  }

  if (ts.isIfStatement(stmt)) {
    if (expressionHasSideEffects(stmt.expression, outerCtx.checker)) {
      return { unsupported: "impure if-condition in loop body" };
    }
    // Shape B singleton: `if (g(x)) { a.p OP= f(x) }` — single leaf,
    // no else, no parent guard. Probe via classification on the inner
    // body; if it's a Shape B leaf, fold the if-condition into its
    // guard. Otherwise fall through to Shape A.
    if (stmt.elseStatement === undefined && parentGuard === null) {
      const thenStmts = ts.isBlock(stmt.thenStatement)
        ? Array.from(stmt.thenStatement.statements)
        : [stmt.thenStatement];
      if (thenStmts.length === 1) {
        const inner = classifyForeachStmt(
          thenStmts[0]!,
          iterName,
          subCtx,
          outerCtx,
          stmt.expression,
        );
        if (!isUnsupported(inner) && inner.kind === "shapeB") {
          return inner;
        }
        // Fall through — try Shape A.
      }
    }
    if (parentGuard !== null) {
      return {
        unsupported:
          "nested if inside a Shape B guard is not supported in foreach",
      };
    }
    // Shape A: build the cond-stmt via the standard if-mutation builder
    // against subCtx so the iter binder is in scope. `buildL1IfMutation`
    // can emit `map-effect` / `set-effect` from inner `m.set/.add` calls;
    // foreach bodies are assign-only at the M3 contract level, so reject
    // here rather than letting the lower pass surface an "out of scope"
    // diagnostic.
    const built = buildL1IfMutation(stmt, subCtx);
    if (isUnsupported(built)) {
      return built;
    }
    const shapeCheck = ensureForeachBodyShape(built);
    if (isUnsupported(shapeCheck)) {
      return shapeCheck;
    }
    return { kind: "shapeA", built: shapeCheck };
  }

  return {
    unsupported: `loop body statement: ${ts.SyntaxKind[stmt.kind]}`,
  };
}

/**
 * Build a Shape B fold leaf from its TS-level descriptor. `target`
 * translates against the OUTER ctx (no iter dependence); `rhs` and
 * `guard` translate against the SUB ctx (so they observe Shape A
 * in-iter writes).
 */
function buildShapeBLeaf(
  leaf: ShapeBLeafTS,
  outerCtx: BuildBodyCtx,
  subCtx: BuildBodyCtx,
): BuildResult<IR1FoldLeaf> {
  const targetR = rejectEffect(
    translateBodyExpr(
      leaf.target,
      outerCtx.checker,
      outerCtx.strategy,
      outerCtx.paramNames,
      outerCtx.state,
      outerCtx.supply,
    ),
  );
  if (isBodyUnsupported(targetR)) {
    return { unsupported: targetR.unsupported };
  }
  const target = ir1FromL2(irWrap(outerCtx.applyConst(bodyExpr(targetR))));

  const rhsR = rejectEffect(
    translateBodyExpr(
      leaf.rhs,
      subCtx.checker,
      subCtx.strategy,
      subCtx.paramNames,
      subCtx.state,
      subCtx.supply,
    ),
  );
  if (isBodyUnsupported(rhsR)) {
    return { unsupported: rhsR.unsupported };
  }
  const rhs = ir1FromL2(irWrap(subCtx.applyConst(bodyExpr(rhsR))));

  let guard: IR1Expr | null = null;
  if (leaf.guard !== null) {
    const gR = rejectEffect(
      translateBodyExpr(
        leaf.guard,
        subCtx.checker,
        subCtx.strategy,
        subCtx.paramNames,
        subCtx.state,
        subCtx.supply,
      ),
    );
    if (isBodyUnsupported(gR)) {
      return { unsupported: gR.unsupported };
    }
    guard = ir1FromL2(irWrap(subCtx.applyConst(bodyExpr(gR))));
  }

  return {
    target,
    prop: leaf.prop,
    combiner: leaf.combiner,
    outerOp: leaf.outerOp,
    rhs,
    guard,
  };
}

/**
 * Apply a Shape A property write to the build-time subState so
 * subsequent Shape B leaves read the in-iter value. Re-translates the
 * receiver / value sub-expressions against subCtx.state, mirroring
 * what the lower pass would do under its own subState. The simulation
 * is one-way — the L1 stmt has already been built and pushed by the
 * caller; this just keeps the subState in sync.
 */
function simulateShapeA(
  stmt: ts.Statement,
  subCtx: BuildBodyCtx,
): BuildResult<true> {
  if (!ts.isExpressionStatement(stmt)) {
    return true;
  }
  const expr = unwrapExpression(stmt.expression);
  if (!ts.isBinaryExpression(expr)) {
    return true;
  }
  const compoundOp = COMPOUND_ASSIGN_TO_BINOP.get(expr.operatorToken.kind);
  const isSimple = expr.operatorToken.kind === ts.SyntaxKind.EqualsToken;
  if (!isSimple && compoundOp === undefined) {
    return true;
  }
  if (!ts.isPropertyAccessExpression(expr.left)) {
    return true;
  }
  const rawProp = expr.left.name.text;
  const receiverType = subCtx.checker.getTypeAtLocation(expr.left.expression);
  const prop = qualifyFieldAccess(
    receiverType,
    rawProp,
    subCtx.checker,
    subCtx.strategy,
    subCtx.supply.synthCell,
  );
  if (prop === null) {
    return { unsupported: ambiguousFieldMsg(rawProp) };
  }
  // `rejectEffect` turns a Map/Set mutation result into `unsupported`
  // so the simulation never feeds an `{ effect }` shape into
  // `bodyExpr` (which throws). Mirrors the guard in
  // `buildL1AssignStmt`.
  const objR = rejectEffect(
    translateBodyExpr(
      expr.left.expression,
      subCtx.checker,
      subCtx.strategy,
      subCtx.paramNames,
      subCtx.state,
      subCtx.supply,
    ),
  );
  if (isBodyUnsupported(objR)) {
    return { unsupported: objR.unsupported };
  }
  const rhsNode =
    compoundOp !== undefined
      ? ts.factory.createBinaryExpression(expr.left, compoundOp, expr.right)
      : expr.right;
  const valR = rejectEffect(
    translateBodyExpr(
      rhsNode,
      subCtx.checker,
      subCtx.strategy,
      subCtx.paramNames,
      subCtx.state,
      subCtx.supply,
    ),
  );
  if (isBodyUnsupported(valR)) {
    return { unsupported: valR.unsupported };
  }
  const objExpr = subCtx.applyConst(bodyExpr(objR));
  const value = subCtx.applyConst(bodyExpr(valR));
  const key = symbolicKey(prop, objExpr);
  subCtx.state.writes = putWrite(subCtx.state.writes, key, {
    kind: "property",
    prop,
    objExpr,
    value,
  });
  subCtx.state.writtenKeys = addWrittenKey(subCtx.state.writtenKeys, key);
  return true;
}

/**
 * Recognize a TS `if`-statement with property-assignment branches and
 * build the canonical L1 `cond-stmt` form.
 */
export function buildL1IfMutation(
  stmt: ts.IfStatement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  if (expressionHasSideEffects(stmt.expression, ctx.checker)) {
    return { unsupported: "impure if-condition in mutating body" };
  }
  const gResult = translateBodyExpr(
    stmt.expression,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(gResult)) {
    return { unsupported: gResult.unsupported };
  }
  const guard: IR1Expr = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(gResult))));

  const thenBody = buildL1MutationBody(stmt.thenStatement, ctx);
  if (isUnsupported(thenBody)) {
    return thenBody;
  }

  let elseBody: IR1Stmt | null = null;
  if (stmt.elseStatement) {
    const e = buildL1MutationBody(stmt.elseStatement, ctx);
    if (isUnsupported(e)) {
      return e;
    }
    elseBody = e;
  }

  return ir1CondStmt([[guard, thenBody]], elseBody);
}

/**
 * Build the L1 statement form for a single branch body. Accepts a
 * block of simple property-assignment statements or a single such
 * statement; rejects everything else.
 */
function buildL1MutationBody(
  stmt: ts.Statement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  if (ts.isBlock(stmt)) {
    const stmts: IR1Stmt[] = [];
    // Strip guard statements (`assert(...)`, `if (g) throw …`) so
    // branch-local preconditions don't surface as unsupported branch
    // bodies — same discipline as `symbolicExecute` and the foreach
    // builder above.
    const children = stmt.statements.filter(
      (s) => !isGuardStatement(s, ctx.checker),
    );
    for (const child of children) {
      const built = buildL1MutationBody(child, ctx);
      if (isUnsupported(built)) {
        return built;
      }
      stmts.push(built);
    }
    if (stmts.length === 0) {
      return { unsupported: "empty branch body" };
    }
    if (stmts.length === 1) {
      return stmts[0]!;
    }
    const [head, ...rest] = stmts;
    return ir1Block([head!, ...rest]);
  }
  if (ts.isIfStatement(stmt)) {
    // Nested if inside a branch body — recurse via the same builder.
    // Handles `else if` chains (TS parses these as nested IfStatement)
    // and inner ifs in the then-branch.
    return buildL1IfMutation(stmt, ctx);
  }
  if (ts.isExpressionStatement(stmt)) {
    if (ts.isCallExpression(stmt.expression)) {
      return buildL1EffectCall(stmt.expression, ctx);
    }
    return buildL1AssignStmt(stmt, ctx);
  }
  return {
    unsupported:
      "branch body must be a property assignment, Map/Set effect, or nested if",
  };
}

/**
 * Build an L1 `map-effect` or `set-effect` from a Map/Set mutation
 * call inside a branch body. Reuses the existing `translateCallExpr`
 * recognizer which returns a `{ effect: CollectionMutation }` for
 * recognized `.set/.delete/.add/.delete/.clear` calls on Map/Set
 * receivers.
 */
function buildL1EffectCall(
  call: ts.CallExpression,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  const result = translateCallExpr(
    call,
    ctx.checker,
    ctx.strategy,
    ctx.paramNames,
    ctx.state,
    ctx.supply,
  );
  if (isBodyUnsupported(result)) {
    return { unsupported: result.unsupported };
  }
  if (!isBodyEffect(result)) {
    return { unsupported: "branch call is not a recognized Map/Set effect" };
  }
  const effect = result.effect;
  if (effect.op === "set" || (effect.op === "delete" && "keyExpr" in effect)) {
    // Map mutation
    const m = effect as Extract<typeof effect, { op: "set" | "delete" }> & {
      keyExpr: OpaqueExpr;
    };
    const objExpr = ir1FromL2(irWrap(ctx.applyConst(m.objExpr)));
    const keyExpr = ir1FromL2(irWrap(ctx.applyConst(m.keyExpr)));
    if (m.op === "set") {
      // Upstream MapMutation still types valueExpr as nullable; for op
      // "set" it's invariably non-null (translateCallExpr guarantees it),
      // so assert here at the boundary into the discriminated IR1 form.
      return ir1MapSet(
        m.ruleName,
        m.keyPredName,
        m.ownerType,
        m.keyType,
        objExpr,
        keyExpr,
        ir1FromL2(irWrap(ctx.applyConst(m.valueExpr!))),
      );
    }
    return ir1MapDelete(
      m.ruleName,
      m.keyPredName,
      m.ownerType,
      m.keyType,
      objExpr,
      keyExpr,
    );
  }
  // Set mutation: op ∈ {add, delete, clear}, no keyExpr field
  const s = effect as Extract<
    typeof effect,
    { op: "add" | "delete" | "clear" }
  > & { elemExpr: OpaqueExpr | null };
  const objExpr = ir1FromL2(irWrap(ctx.applyConst(s.objExpr)));
  if (s.op === "clear") {
    return ir1SetClear(s.ruleName, s.ownerType, s.elemType, objExpr);
  }
  // add / delete — elemExpr is non-null per translateCallExpr.
  return ir1SetAddOrDelete(
    s.op,
    s.ruleName,
    s.ownerType,
    s.elemType,
    objExpr,
    ir1FromL2(irWrap(ctx.applyConst(s.elemExpr!))),
  );
}

/**
 * Build an L1 `assign` statement from a TS property-assignment
 * expression statement. Slice 1 supports simple `=` only; compound
 * assigns (`+=` etc.) return `{unsupported}` and are deferred.
 */
/**
 * Map TS compound-assign operator kinds to their plain binary
 * counterparts. `+=` desugars to `lhs = lhs + rhs`, `-=` to `lhs =
 * lhs - rhs`, and so on.
 */
const COMPOUND_ASSIGN_TO_BINOP: Map<ts.SyntaxKind, ts.BinaryOperator> = new Map(
  [
    [ts.SyntaxKind.PlusEqualsToken, ts.SyntaxKind.PlusToken],
    [ts.SyntaxKind.MinusEqualsToken, ts.SyntaxKind.MinusToken],
    [ts.SyntaxKind.AsteriskEqualsToken, ts.SyntaxKind.AsteriskToken],
    [ts.SyntaxKind.SlashEqualsToken, ts.SyntaxKind.SlashToken],
  ],
);

function buildL1AssignStmt(
  stmt: ts.ExpressionStatement,
  ctx: BuildBodyCtx,
): BuildResult<IR1Stmt> {
  const expr = stmt.expression;
  if (!ts.isBinaryExpression(expr)) {
    return { unsupported: "branch statement must be an assignment" };
  }
  const isSimpleAssign = expr.operatorToken.kind === ts.SyntaxKind.EqualsToken;
  const compoundOp = COMPOUND_ASSIGN_TO_BINOP.get(expr.operatorToken.kind);
  if (!isSimpleAssign && compoundOp === undefined) {
    return { unsupported: "unsupported assignment operator" };
  }
  if (!ts.isPropertyAccessExpression(expr.left)) {
    return { unsupported: "assign target must be a property access" };
  }
  const rawProp = expr.left.name.text;
  const receiverType = ctx.checker.getTypeAtLocation(expr.left.expression);
  const prop = qualifyFieldAccess(
    receiverType,
    rawProp,
    ctx.checker,
    ctx.strategy,
    ctx.supply.synthCell,
  );
  if (prop === null) {
    return { unsupported: ambiguousFieldMsg(rawProp) };
  }
  const objR = rejectEffect(
    translateBodyExpr(
      expr.left.expression,
      ctx.checker,
      ctx.strategy,
      ctx.paramNames,
      ctx.state,
      ctx.supply,
    ),
  );
  if (isBodyUnsupported(objR)) {
    return { unsupported: objR.unsupported };
  }
  // For compound `a.p OP= v`, desugar the rhs to `a.p OP v`. The new
  // `a.p` read goes through translateBodyExpr, which consults the
  // symbolic state and returns the prior-write value or the pre-state
  // identity.
  const rhsNode =
    compoundOp !== undefined
      ? ts.factory.createBinaryExpression(expr.left, compoundOp, expr.right)
      : expr.right;
  // `rejectEffect` turns a Map/Set mutation result into `unsupported`
  // — without this, a value-position `m.set(k, v)` (or `s.add(x)`)
  // would slip past `isBodyUnsupported` and `bodyExpr` would throw
  // when called on the `{ effect }` shape downstream.
  const valR = rejectEffect(
    translateBodyExpr(
      rhsNode,
      ctx.checker,
      ctx.strategy,
      ctx.paramNames,
      ctx.state,
      ctx.supply,
    ),
  );
  if (isBodyUnsupported(valR)) {
    return { unsupported: valR.unsupported };
  }
  const obj = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(objR))));
  const val = ir1FromL2(irWrap(ctx.applyConst(bodyExpr(valR))));
  return ir1Assign(ir1Member(obj, prop), val);
}
