import type { SourceFile } from "ts-morph";
import ts from "typescript";
import type {
  OpaqueCombiner,
  OpaqueExpr,
  OpaqueGuard,
  OpaqueParam,
} from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import { isKnownPureCall } from "./purity.js";
import {
  classifyFunction,
  findFunction,
  isAssertionCall,
  isFollowableGuardCall,
  isPureExpression,
  shortParamName,
  translateExpr,
  translateOperator,
} from "./translate-signature.js";
import {
  isMapType,
  isSetType,
  mapTsType,
  type NumericStrategy,
} from "./translate-types.js";
import type { PantDeclaration, PropResult } from "./types.js";

// --- Const-binding inlining infrastructure (let-elimination) ---

interface UniqueSupply {
  next: () => number;
}
function makeUniqueSupply(): UniqueSupply {
  let counter = 0;
  return { next: () => counter++ };
}

function freshHygienicBinder(supply: UniqueSupply): string {
  return `$${supply.next()}`;
}

interface ConstBinding {
  tsName: string;
  initializer: ts.Expression;
}

/**
 * Result of translating a body expression. Either an opaque expression
 * (possibly with a deferred list-comprehension structure for chain fusion),
 * or a failure.
 *
 * When `pendingComprehension` is set, `expr` holds the *projection body*
 * (e.g., `name u`) and the field carries the binder, root array, and
 * accumulated filter predicates. Materialization (calling `bodyExpr`) emits
 * the flat `each([], [gIn(binder, arrExpr), ...guards], expr)`.
 *
 * Deforestation (Wadler, TCS 1990) — chained `.filter`/`.map`/`.reduce` fuse
 * into a single traversal by deferring materialization until a consumer
 * outside the chain demands an opaque expression.
 */
interface PendingComprehension {
  binder: string;
  arrExpr: OpaqueExpr;
  guards: OpaqueGuard[];
}

type BodyResult =
  | { unsupported: string }
  | { expr: OpaqueExpr; pendingComprehension?: PendingComprehension };

/** Type guard for unsupported BodyResult. */
function isBodyUnsupported(r: BodyResult): r is { unsupported: string } {
  return "unsupported" in r;
}

/** Extract the OpaqueExpr from a successful BodyResult, materializing any
 * deferred comprehension chain into a flat `each` at the boundary. */
function bodyExpr(r: BodyResult): OpaqueExpr {
  if ("unsupported" in r) {
    throw new Error(`bodyExpr called on unsupported: ${r.unsupported}`);
  }
  if (r.pendingComprehension) {
    const ast = getAst();
    const { binder, arrExpr, guards } = r.pendingComprehension;
    return ast.each([], [ast.gIn(binder, arrExpr), ...guards], r.expr);
  }
  return r.expr;
}

// --- Symbolic last-write state (Dijkstra's guarded commands, 1975) ---
//
// Forward symbolic execution with path merging. Each property-assignment
// statement updates `writes[prop::objRepr]`. If statements clone the state
// for each branch and merge via `cond` at the join. Later reads of the same
// property access see the accumulated value. See CLAUDE.md § Guarded Commands.

interface WriteEntry {
  prop: string;
  objExpr: OpaqueExpr;
  value: OpaqueExpr;
}

interface SymbolicState {
  writes: Map<string, WriteEntry>;
  // Keys *written during the current branch* (reset on clone). Used by the
  // if-merge algorithm to determine which locations are "touched."
  writtenKeys: Set<string>;
  // Names of rules that have been modified by *any* write in this execution,
  // including Shape A loop writes whose per-element equation is emitted
  // directly (bypassing `writes`). Consumed by the frame-condition generator
  // so loop-modified rules don't get a spurious identity frame.
  modifiedProps: Set<string>;
  // Canonicalizer — applies the ambient const-binding substitution to an
  // expression before it is used as a state key. Writes store keys under the
  // post-substitution form so `const x = a; x.balance = 1` and a later
  // `x.balance` read resolve to the same key. Updated by `symbolicExecute`
  // whenever a new const binding is inlined so the in-flight `applyConst`
  // stays in sync with the state.
  canonicalize: (e: OpaqueExpr) => OpaqueExpr;
}

function makeSymbolicState(
  canonicalize: (e: OpaqueExpr) => OpaqueExpr = (e) => e,
): SymbolicState {
  return {
    writes: new Map(),
    writtenKeys: new Set(),
    modifiedProps: new Set(),
    canonicalize,
  };
}

function cloneSymbolicState(s: SymbolicState): SymbolicState {
  return {
    writes: new Map(s.writes),
    writtenKeys: new Set(),
    modifiedProps: s.modifiedProps,
    canonicalize: s.canonicalize,
  };
}

function symbolicKey(prop: string, objExpr: OpaqueExpr): string {
  return `${prop}::${getAst().strExpr(objExpr)}`;
}

function isBareReturn(stmt: ts.Statement): boolean {
  if (ts.isReturnStatement(stmt) && !stmt.expression) {
    return true;
  }
  if (ts.isBlock(stmt) && stmt.statements.length === 1) {
    const s = stmt.statements[0]!;
    return ts.isReturnStatement(s) && !s.expression;
  }
  return false;
}

function flattenStmt(stmt: ts.Statement): ts.Statement[] {
  return ts.isBlock(stmt) ? Array.from(stmt.statements) : [stmt];
}

/**
 * Early-exit if-conversion (Allen et al., POPL 1983, extended to early
 * exits). Recognizes three patterns with a bare `return;` on one side:
 *
 *   if (c) { return; }              → early-exit when c; continuation = post-if
 *   if (c) { return; } else { X }   → early-exit when c; continuation = X ++ post-if
 *   if (c) { X } else { return; }   → early-exit when !c; continuation = X ++ post-if
 *
 * The non-returning branch's statements are lifted into the continuation
 * (to be executed together with the statements following the `if`). The
 * flag `earlyExitWhenTrue` indicates whether the if-condition directly
 * represents the early-exit path or needs to be negated at the merge.
 */
interface EarlyExitDetection {
  condition: ts.Expression;
  /** If false, early exit is taken when !condition. */
  earlyExitWhenTrue: boolean;
  continuationPrefix: ts.Statement[];
}

function detectEarlyExit(stmt: ts.Statement): EarlyExitDetection | null {
  if (!ts.isIfStatement(stmt)) {
    return null;
  }
  const thenExits = isBareReturn(stmt.thenStatement);
  const elseExits =
    stmt.elseStatement !== undefined && isBareReturn(stmt.elseStatement);

  if (thenExits && !stmt.elseStatement) {
    return {
      condition: stmt.expression,
      earlyExitWhenTrue: true,
      continuationPrefix: [],
    };
  }
  if (thenExits && stmt.elseStatement && !elseExits) {
    return {
      condition: stmt.expression,
      earlyExitWhenTrue: true,
      continuationPrefix: flattenStmt(stmt.elseStatement),
    };
  }
  if (!thenExits && elseExits) {
    return {
      condition: stmt.expression,
      earlyExitWhenTrue: false,
      continuationPrefix: flattenStmt(stmt.thenStatement),
    };
  }
  return null;
}

/**
 * Map TypeScript compound-assignment tokens (`+=`, `-=`, ...) to their
 * binary operator counterparts. Used to desugar `a.prop += v` into
 * `a.prop = a.prop + v` during translation of mutating bodies.
 *
 * Restricted to the four arithmetic operators Pantagruel's AST supports
 * (`+`, `-`, `*`, `/`). `%=` and `**=` are intentionally excluded because
 * the underlying `%` and `**` operators have no Pantagruel counterpart —
 * desugaring them would produce an unsupported binary expression anyway.
 */
const COMPOUND_ASSIGN_TO_BINOP: Map<ts.SyntaxKind, ts.BinaryOperator> = new Map(
  [
    [ts.SyntaxKind.PlusEqualsToken, ts.SyntaxKind.PlusToken],
    [ts.SyntaxKind.MinusEqualsToken, ts.SyntaxKind.MinusToken],
    [ts.SyntaxKind.AsteriskEqualsToken, ts.SyntaxKind.AsteriskToken],
    [ts.SyntaxKind.SlashEqualsToken, ts.SyntaxKind.SlashToken],
  ],
);

/**
 * Map each compound assignment operator to the pair it induces for
 * loop-fold translation: the *inside* combiner (for the comprehension)
 * and the *outside* binary operator (joining prior state to the aggregate).
 *
 *   a.p += f(x)  iterated  =>  p' a = p a + (+ over each x in arr | f x)
 *   a.p -= f(x)  iterated  =>  p' a = p a - (+ over each x in arr | f x)
 *   a.p *= f(x)  iterated  =>  p' a = p a * (* over each x in arr | f x)
 *   a.p /= f(x)  iterated  =>  p' a = p a / (* over each x in arr | f x)
 *
 * Non-commutative outer ops (`-`, `/`) pair with the commutative combiner
 * of their identity group (`+`/`*`), since e.g. `p - f(x1) - f(x2)`
 * equals `p - (f(x1) + f(x2))`.
 */
type CombinerKind = "add" | "mul" | "and" | "or";

interface FoldOps {
  combiner: CombinerKind;
  outer: ts.BinaryOperator;
}
const COMPOUND_ASSIGN_TO_FOLD: Map<ts.SyntaxKind, FoldOps> = new Map([
  [
    ts.SyntaxKind.PlusEqualsToken,
    { combiner: "add", outer: ts.SyntaxKind.PlusToken },
  ],
  [
    ts.SyntaxKind.MinusEqualsToken,
    { combiner: "add", outer: ts.SyntaxKind.MinusToken },
  ],
  [
    ts.SyntaxKind.AsteriskEqualsToken,
    { combiner: "mul", outer: ts.SyntaxKind.AsteriskToken },
  ],
  [
    ts.SyntaxKind.SlashEqualsToken,
    { combiner: "mul", outer: ts.SyntaxKind.SlashToken },
  ],
]);

interface ReduceOpInfo {
  combiner: CombinerKind;
  outer: ts.BinaryOperator;
  /** Source text of the init value that permits eliding init (combiner identity). */
  identityText: string | null;
  /** Whether `acc` can appear on either side — true iff the outer op is commutative. */
  commutative: boolean;
}

/** Map a TypeScript binary operator (as used in `.reduce` callback body) to fold info. */
function binopToReduceInfo(kind: ts.SyntaxKind): ReduceOpInfo | null {
  switch (kind) {
    case ts.SyntaxKind.PlusToken:
      return {
        combiner: "add",
        outer: kind,
        identityText: "0",
        commutative: true,
      };
    case ts.SyntaxKind.MinusToken:
      return {
        combiner: "add",
        outer: kind,
        identityText: null,
        commutative: false,
      };
    case ts.SyntaxKind.AsteriskToken:
      return {
        combiner: "mul",
        outer: kind,
        identityText: "1",
        commutative: true,
      };
    case ts.SyntaxKind.SlashToken:
      return {
        combiner: "mul",
        outer: kind,
        identityText: null,
        commutative: false,
      };
    case ts.SyntaxKind.AmpersandAmpersandToken:
      return {
        combiner: "and",
        outer: kind,
        identityText: "true",
        commutative: true,
      };
    case ts.SyntaxKind.BarBarToken:
      return {
        combiner: "or",
        outer: kind,
        identityText: "false",
        commutative: true,
      };
    default:
      return null;
  }
}

/**
 * Decide whether an init expression evaluates to the combiner's identity element.
 * Normalizes parenthesized/cast wrappers and numeric-literal variants so that
 * `0`, `(0)`, `0.0`, `+0`, `-0` all match identity 0 for `+`, etc. Avoids
 * relying on raw source text, which fails on whitespace or syntactically
 * distinct but semantically equivalent forms.
 */
function isIdentityInit(node: ts.Expression, identityText: string): boolean {
  const inner = unwrapExpression(node);
  if (identityText === "true") {
    return inner.kind === ts.SyntaxKind.TrueKeyword;
  }
  if (identityText === "false") {
    return inner.kind === ts.SyntaxKind.FalseKeyword;
  }
  const n = evaluateNumericLiteral(inner);
  if (n === null) {
    return false;
  }
  const target = Number(identityText);
  return Number.isFinite(target) && n === target;
}

function evaluateNumericLiteral(node: ts.Expression): number | null {
  if (ts.isNumericLiteral(node)) {
    const n = Number(node.text);
    return Number.isFinite(n) ? n : null;
  }
  if (
    ts.isPrefixUnaryExpression(node) &&
    (node.operator === ts.SyntaxKind.PlusToken ||
      node.operator === ts.SyntaxKind.MinusToken) &&
    ts.isNumericLiteral(node.operand)
  ) {
    const n = Number(node.operand.text);
    if (!Number.isFinite(n)) {
      return null;
    }
    return node.operator === ts.SyntaxKind.MinusToken ? -n : n;
  }
  return null;
}

function makeCombiner(kind: CombinerKind): OpaqueCombiner {
  const ast = getAst();
  switch (kind) {
    case "add":
      return ast.combAdd();
    case "mul":
      return ast.combMul();
    case "and":
      return ast.combAnd();
    case "or":
      return ast.combOr();
    default: {
      const _exhaustive: never = kind;
      throw new Error(`unknown combiner kind: ${_exhaustive as string}`);
    }
  }
}

export interface TranslateBodyOptions {
  sourceFile: SourceFile;
  functionName: string;
  strategy: NumericStrategy;
  /** Declarations in scope — used for frame condition generation. */
  declarations?: PantDeclaration[];
}

/**
 * Translate a TypeScript function body to Pantagruel propositions.
 *
 * Pure functions: return expression becomes `all params | f args = <expr>`.
 * Mutating functions: property assignments become primed propositions,
 * plus frame conditions for unmodified rules.
 */
export function translateBody(opts: TranslateBodyOptions): PropResult[] {
  const { sourceFile, functionName, strategy, declarations } = opts;
  const checker = sourceFile.getProject().getTypeChecker().compilerObject;
  const { node, className } = findFunction(sourceFile, functionName);
  // Strip class qualifier for use in Pantagruel identifiers
  const baseName = functionName.includes(".")
    ? functionName.split(".", 2)[1]!
    : functionName;
  const classification = classifyFunction(node, checker);

  // Build param name map (same logic as translateSignature)
  const paramNames = new Map<string, string>();
  const paramList: Array<{ name: string; type: string }> = [];

  const sig = checker.getSignatureFromDeclaration(node);

  if (className) {
    const existingParamNames = new Set(
      sig ? sig.getParameters().map((p) => p.name) : [],
    );
    const pName = shortParamName(className, existingParamNames);
    paramNames.set("this", pName);
    paramList.push({ name: pName, type: className });
  }

  if (sig) {
    for (const param of sig.getParameters()) {
      const paramType = checker.getTypeOfSymbol(param);
      const typeName = mapTsType(paramType, checker, strategy);
      paramNames.set(param.name, param.name);
      paramList.push({ name: param.name, type: typeName });
    }
  }

  if (!node.body) {
    return [];
  }

  if (classification === "pure") {
    return translatePureBody(
      node,
      baseName,
      paramList,
      checker,
      strategy,
      paramNames,
    );
  } else {
    return translateMutatingBody(
      node,
      checker,
      strategy,
      paramNames,
      declarations ?? [],
    );
  }
}

function translatePureBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  functionName: string,
  params: Array<{ name: string; type: string }>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PropResult[] {
  const ast = getAst();

  if (!node.body) {
    return [];
  }

  const extracted = extractReturnExpression(node.body, checker);
  if (!extracted) {
    const reason = describeRejectedBody(node.body, checker);
    return [{ kind: "unsupported", reason: `${functionName} — ${reason}` }];
  }

  const supply = makeUniqueSupply();
  const inlined = inlineConstBindings(
    extracted.bindings.map((b) => ({
      tsName: b.name,
      initializer: b.initializer,
    })),
    checker,
    strategy,
    paramNames,
    supply,
  );
  if ("error" in inlined) {
    return [
      { kind: "unsupported", reason: `${functionName} — ${inlined.error}` },
    ];
  }

  const body = translateBodyExpr(
    extracted.returnExpr,
    checker,
    strategy,
    inlined.scopedParams,
    undefined,
    supply,
  );

  if (isBodyUnsupported(body)) {
    return [{ kind: "unsupported", reason: body.unsupported }];
  }

  const rhs = inlined.applyTo(bodyExpr(body));

  const argExprs = params.map((p) => ast.var(p.name));
  const lhs = ast.app(ast.var(functionName), argExprs);
  return [
    {
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs,
      rhs,
    },
  ];
}

interface ExtractedBody {
  bindings: Array<{ name: string; initializer: ts.Expression }>;
  returnExpr: ts.Expression | ts.IfStatement;
}

/**
 * Extract the return expression from a function body, collecting any leading
 * const bindings with pure initializers for inline substitution.
 * Handles:
 *   - Single return statement
 *   - Leading const bindings + return statement
 *   - if/else with returns in both branches (produces a synthetic conditional)
 * Returns null if the body contains let/var bindings or effectful const initializers.
 */
function extractReturnExpression(
  body: ts.Block,
  checker: ts.TypeChecker,
): ExtractedBody | null {
  // Skip guard statements (if-throw patterns and assertion calls)
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));

  if (stmts.length === 0) {
    return null;
  }

  const bindings: Array<{ name: string; initializer: ts.Expression }> = [];
  let i = 0;

  // Collect leading const bindings
  for (; i < stmts.length - 1; i++) {
    const stmt = stmts[i]!;
    if (!ts.isVariableStatement(stmt)) {
      break;
    }

    const declList = stmt.declarationList;
    // Reject let/var
    if (!(declList.flags & ts.NodeFlags.Const)) {
      return null;
    }

    for (const decl of declList.declarations) {
      // Must have a simple identifier name and an initializer
      if (!ts.isIdentifier(decl.name) || !decl.initializer) {
        return null;
      }
      // Reject effectful initializers
      if (expressionHasSideEffects(decl.initializer, checker)) {
        return null;
      }
      bindings.push({ name: decl.name.text, initializer: decl.initializer });
    }
  }

  // The last statement must be a return or if/else-with-returns
  const last = stmts[i]!;
  // If we didn't consume all preceding statements as const bindings, reject
  if (i < stmts.length - 1) {
    return null;
  }

  if (ts.isReturnStatement(last) && last.expression) {
    return { bindings, returnExpr: last.expression };
  }
  if (ts.isIfStatement(last) && last.elseStatement) {
    return { bindings, returnExpr: last };
  }

  return null;
}

function describeRejectedBody(body: ts.Block, checker: ts.TypeChecker): string {
  const stmts = body.statements.filter((s) => !isGuardStatement(s, checker));
  if (stmts.length === 0) {
    return "empty body";
  }
  if (stmts.length > 1) {
    // Check for specific rejection reasons in leading statements
    for (const stmt of stmts) {
      if (ts.isVariableStatement(stmt)) {
        const declList = stmt.declarationList;
        if (!(declList.flags & ts.NodeFlags.Const)) {
          return "let/var bindings not supported";
        }
        for (const decl of declList.declarations) {
          if (
            decl.initializer &&
            expressionHasSideEffects(decl.initializer, checker)
          ) {
            return "const binding with side-effectful initializer";
          }
        }
      }
    }
    return "local bindings or multiple statements before return";
  }
  const stmt = stmts[0]!;
  if (ts.isReturnStatement(stmt) && !stmt.expression) {
    return "return without expression";
  }
  return "non-translatable control flow";
}

/**
 * Shared const-binding inlining (let-elimination) for both pure and mutating paths.
 *
 * Three phases:
 *   1. TDZ validation on TS AST (rejects forward/self references)
 *   2. Translate initializers forward, building scopedParams incrementally
 *   3. Return a right-fold substitution closure
 *
 * The right-fold means substitutions are applied inside-out: the last binding
 * is substituted first, so each step naturally resolves references to earlier
 * bindings that are already embedded in the result.
 */
function inlineConstBindings(
  bindings: ConstBinding[],
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  baseParams: Map<string, string>,
  supply: UniqueSupply,
  state?: SymbolicState,
):
  | {
      applyTo: (expr: OpaqueExpr) => OpaqueExpr;
      scopedParams: Map<string, string>;
    }
  | { error: string } {
  const ast = getAst();

  // Phase 1: TDZ validation — reject forward/self references on TS AST
  for (const [idx, binding] of bindings.entries()) {
    const blockedNames = new Set(bindings.slice(idx).map((b) => b.tsName));
    if (expressionReferencesNames(binding.initializer, blockedNames)) {
      return { error: "const initializer references a later binding" };
    }
  }

  // Phase 2: translate initializers, building scopedParams incrementally
  const scopedParams = new Map(baseParams);
  const translatedBindings: Array<{
    hygienicName: string;
    initExpr: OpaqueExpr;
  }> = [];

  for (const binding of bindings) {
    const hygienicName = `$${supply.next()}`;
    const initResult = translateBodyExpr(
      binding.initializer,
      checker,
      strategy,
      scopedParams,
      state,
      supply,
    );
    if (isBodyUnsupported(initResult)) {
      return { error: initResult.unsupported };
    }
    translatedBindings.push({ hygienicName, initExpr: bodyExpr(initResult) });
    scopedParams.set(binding.tsName, hygienicName);
  }

  // Phase 3: right-fold substitution closure
  const reversed = translatedBindings.slice().reverse();
  const applyTo = (expr: OpaqueExpr): OpaqueExpr => {
    for (const { hygienicName, initExpr } of reversed) {
      expr = ast.substituteBinder(expr, hygienicName, initExpr);
    }
    return expr;
  };

  return { applyTo, scopedParams };
}

function isGuardStatement(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
): boolean {
  // Assertion call or followable guard call — must match the same purity
  // checks used by scanBodyForGuards in translate-signature.ts
  if (ts.isExpressionStatement(stmt) && ts.isCallExpression(stmt.expression)) {
    const call = stmt.expression;
    if (!isPureExpression(call.expression)) {
      return false;
    }
    if (!call.arguments.every(isPureExpression)) {
      return false;
    }
    if (isAssertionCall(checker, call) !== null) {
      return true;
    }
    if (isFollowableGuardCall(call, checker)) {
      return true;
    }
  }

  if (!ts.isIfStatement(stmt)) {
    return false;
  }
  // Condition must be pure (aligned with classifyGuardIf's isPureExpression check)
  if (!isPureExpression(stmt.expression)) {
    return false;
  }
  // if (...) { throw } without else
  if (!stmt.elseStatement && blockThrows(stmt.thenStatement, checker)) {
    return true;
  }
  // if (...) { ... } else { throw }
  if (stmt.elseStatement && blockThrows(stmt.elseStatement, checker)) {
    // Only a guard if the then-block has no side effects and doesn't return.
    // A mutating then-branch like `if (ok) { a.balance = 1; } else { throw e; }`
    // must NOT be classified as a guard — collectAssignments() needs to see it.
    return (
      blockHasNoSideEffects(stmt.thenStatement, checker) &&
      !blockReturns(stmt.thenStatement)
    );
  }
  return false;
}

function variableStatementHasNoSideEffects(
  stmt: ts.VariableStatement,
  checker: ts.TypeChecker,
): boolean {
  return stmt.declarationList.declarations.every(
    (decl) =>
      !decl.initializer || !expressionHasSideEffects(decl.initializer, checker),
  );
}

function blockThrows(node: ts.Statement, checker: ts.TypeChecker): boolean {
  if (ts.isThrowStatement(node)) {
    return true;
  }
  if (ts.isBlock(node)) {
    const stmts = node.statements;
    if (stmts.length === 0) {
      return false;
    }
    // Last statement must be a throw; all preceding must be side-effect-free
    // (variable declarations for building the error message, etc.)
    if (!ts.isThrowStatement(stmts[stmts.length - 1]!)) {
      return false;
    }
    return stmts
      .slice(0, -1)
      .every(
        (s) =>
          ts.isVariableStatement(s) &&
          variableStatementHasNoSideEffects(s, checker),
      );
  }
  return false;
}

function blockReturns(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.some((s) => ts.isReturnStatement(s));
  }
  return ts.isReturnStatement(node);
}

/** Check that a statement/block contains no assignments or property writes. */
function blockHasNoSideEffects(
  node: ts.Statement,
  checker: ts.TypeChecker,
): boolean {
  if (ts.isBlock(node)) {
    return node.statements.every((s) => blockHasNoSideEffects(s, checker));
  }
  if (ts.isExpressionStatement(node)) {
    return !expressionHasSideEffects(node.expression, checker);
  }
  // Variable declarations are fine only if initializers have no side effects
  if (ts.isVariableStatement(node)) {
    return variableStatementHasNoSideEffects(node, checker);
  }
  // Return statements, throw statements are fine
  if (ts.isReturnStatement(node) || ts.isThrowStatement(node)) {
    return true;
  }
  // if/for/while/switch may contain mutations — treat as side-effectful
  return false;
}

/** Check whether a TS expression references any variable name from the given set.
 *  Only checks identifier *uses* (variable references), not syntactic name
 *  positions like property names in `a.balance` or method names in `a.foo()`. */
function expressionReferencesNames(
  expr: ts.Expression,
  names: Set<string>,
): boolean {
  expr = unwrapExpression(expr);
  if (ts.isIdentifier(expr)) {
    return names.has(expr.text);
  }
  // For property access, only recurse into the object expression —
  // the .name identifier is a syntactic token, not a variable reference.
  if (ts.isPropertyAccessExpression(expr)) {
    return expressionReferencesNames(expr.expression, names);
  }
  return (
    ts.forEachChild(expr, (child) =>
      ts.isExpression(child) ? expressionReferencesNames(child, names) : false,
    ) ?? false
  );
}

/** Unwrap parentheses, type assertions, and non-null assertions to get the inner expression. */
function unwrapExpression(expr: ts.Expression): ts.Expression {
  while (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isSatisfiesExpression(expr) ||
    ts.isNonNullExpression(expr)
  ) {
    expr = expr.expression;
  }
  return expr;
}

function expressionHasSideEffects(
  expr: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  expr = unwrapExpression(expr);

  if (ts.isDeleteExpression(expr)) {
    return true;
  }

  if (ts.isBinaryExpression(expr)) {
    // Any assignment operator
    return (
      (expr.operatorToken.kind >= ts.SyntaxKind.EqualsToken &&
        expr.operatorToken.kind <= ts.SyntaxKind.CaretEqualsToken) ||
      expressionHasSideEffects(expr.left, checker) ||
      expressionHasSideEffects(expr.right, checker)
    );
  }
  if (ts.isCallExpression(expr)) {
    if (isKnownPureCall(expr, checker)) {
      return expr.arguments.some((a) => expressionHasSideEffects(a, checker));
    }
    return true;
  }
  if (ts.isNewExpression(expr) || ts.isAwaitExpression(expr)) {
    return true;
  }
  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    const op = expr.operator;
    return (
      op === ts.SyntaxKind.PlusPlusToken ||
      op === ts.SyntaxKind.MinusMinusToken ||
      expressionHasSideEffects(expr.operand, checker)
    );
  }
  return (
    ts.forEachChild(expr, (child) =>
      ts.isExpression(child) ? expressionHasSideEffects(child, checker) : false,
    ) ?? false
  );
}

/**
 * Translate a TS expression to an opaque Pantagruel AST node, extending the
 * base translateExpr with support for ternary, array ops, and if/else as cond.
 *
 * When a `state` is provided (mutating-body context), property-access reads
 * first consult the symbolic state so that `a.balance` after an assignment
 * `a.balance = v` evaluates to `v`.
 */
export function translateBodyExpr(
  expr: ts.Expression | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult {
  const ast = getAst();

  if (ts.isExpression(expr)) {
    expr = unwrapExpression(expr);
  }

  // if/else statement -> cond
  if (ts.isIfStatement(expr)) {
    return translateIfStatement(
      expr,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
  }

  // Ternary: a ? b : c -> cond([[a, b], [true, c]])
  if (ts.isConditionalExpression(expr)) {
    const cond = translateBodyExpr(
      expr.condition,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(cond)) {
      return cond;
    }
    const whenTrue = translateBodyExpr(
      expr.whenTrue,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(whenTrue)) {
      return whenTrue;
    }
    const whenFalse = translateBodyExpr(
      expr.whenFalse,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(whenFalse)) {
      return whenFalse;
    }
    return {
      expr: ast.cond([
        [bodyExpr(cond), bodyExpr(whenTrue)],
        [ast.litBool(true), bodyExpr(whenFalse)],
      ]),
    };
  }

  // Property access with special array operations
  if (ts.isPropertyAccessExpression(expr)) {
    const prop = expr.name.text;
    const obj = translateBodyExpr(
      expr.expression,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(obj)) {
      return obj;
    }
    // .length (array) / .size (Set) -> #obj
    if (prop === "length" || prop === "size") {
      const receiverType = checker.getTypeAtLocation(expr.expression);
      const isArray = prop === "length" && checker.isArrayType(receiverType);
      const isSet = prop === "size" && isSetType(receiverType);
      if (isArray || isSet) {
        return { expr: ast.unop(ast.opCard(), bodyExpr(obj)) };
      }
    }
    // Consult symbolic state for a prior write at this location. Apply the
    // same canonicalization as the write site (see SymbolicState) so that
    // reads through const aliases hit the prior write — e.g.,
    // `const x = a; x.balance = 1; x.balance += 2` must see the `= 1` write
    // under a key that matches both the read and the write.
    if (state !== undefined) {
      const key = symbolicKey(prop, state.canonicalize(bodyExpr(obj)));
      const entry = state.writes.get(key);
      if (entry !== undefined) {
        return { expr: entry.value };
      }
    }
    // Regular property access: a.balance -> app(var("balance"), [obj])
    return { expr: ast.app(ast.var(prop), [bodyExpr(obj)]) };
  }

  // Call expression: handle .includes(), .filter().map(), etc.
  if (ts.isCallExpression(expr)) {
    return translateCallExpr(
      expr,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
  }

  // Prefix unary: !x -> unop(opNot(), x), -x -> unop(opNeg(), x)
  if (ts.isPrefixUnaryExpression(expr)) {
    const operand = translateBodyExpr(
      expr.operand,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(operand)) {
      return operand;
    }
    if (expr.operator === ts.SyntaxKind.ExclamationToken) {
      return { expr: ast.unop(ast.opNot(), bodyExpr(operand)) };
    }
    if (expr.operator === ts.SyntaxKind.MinusToken) {
      return { expr: ast.unop(ast.opNeg(), bodyExpr(operand)) };
    }
  }

  // Binary expression
  if (ts.isBinaryExpression(expr)) {
    const op = translateOperator(expr.operatorToken.kind);
    if (op === null) {
      return {
        unsupported: `operator ${ts.SyntaxKind[expr.operatorToken.kind]}`,
      };
    }
    const left = translateBodyExpr(
      expr.left,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(left)) {
      return left;
    }
    const right = translateBodyExpr(
      expr.right,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(right)) {
      return right;
    }
    return { expr: ast.binop(op, bodyExpr(left), bodyExpr(right)) };
  }

  // Fall through to base translateExpr for identifiers, literals, this, etc.
  if (ts.isExpression(expr)) {
    return { expr: translateExpr(expr, checker, strategy, paramNames) };
  }

  return { unsupported: "non-expression statement" };
}

function translateIfStatement(
  stmt: ts.IfStatement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult {
  const ast = getAst();

  const cond = translateBodyExpr(
    stmt.expression,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(cond)) {
    return cond;
  }
  const thenExpr = extractReturnFromBranch(stmt.thenStatement, checker);
  const elseExpr = stmt.elseStatement
    ? extractReturnFromBranch(stmt.elseStatement, checker)
    : null;

  if (thenExpr && elseExpr) {
    const thenVal = translateBodyExpr(
      thenExpr,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(thenVal)) {
      return thenVal;
    }
    const elseVal = translateBodyExpr(
      elseExpr,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(elseVal)) {
      return elseVal;
    }
    return {
      expr: ast.cond([
        [bodyExpr(cond), bodyExpr(thenVal)],
        [ast.litBool(true), bodyExpr(elseVal)],
      ]),
    };
  }

  return { unsupported: "if statement without return in both branches" };
}

function extractReturnFromBranch(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
): ts.Expression | null {
  if (ts.isReturnStatement(stmt) && stmt.expression) {
    return stmt.expression;
  }
  if (ts.isBlock(stmt)) {
    // Apply the same rule as extractReturnExpression: only allow a single
    // return (after filtering guards). Blocks with local declarations or
    // multiple non-guard statements are rejected so we don't leak
    // branch-scoped bindings into the generated proposition.
    const nonGuard = stmt.statements.filter(
      (s) => !isGuardStatement(s, checker),
    );
    if (nonGuard.length === 1) {
      const s = nonGuard[0]!;
      if (ts.isReturnStatement(s) && s.expression) {
        return s.expression;
      }
    }
  }
  return null;
}

/** Get element type name for an array-typed TS expression, or null. */
function getArrayElementType(
  tsExpr: ts.Expression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): string | null {
  const sourceType = checker.getTypeAtLocation(tsExpr);
  if (!checker.isArrayType(sourceType)) {
    return null;
  }
  const typeArgs = checker.getTypeArguments(sourceType as ts.TypeReference);
  return typeArgs.length === 1
    ? mapTsType(typeArgs[0]!, checker, strategy)
    : "?";
}

/**
 * Translate `.filter()` / `.map()` on an array. Returns a BodyResult whose
 * `pendingComprehension` encodes the deferred chain (Wadler, TCS 1990).
 * Materialization into `each([], [gIn(binder, arrExpr), ...guards], body)`
 * happens at the chain boundary via `bodyExpr`.
 *
 * `.filter(p)` extends the guard list; `.map(f)` rewrites the projection.
 * Composition preserves the original root array in `arrExpr`.
 */
function translateArrayMethod(
  methodName: "filter" | "map",
  tsReceiver: ts.Expression,
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult | null {
  const ast = getAst();

  if (!getArrayElementType(tsReceiver, checker, strategy)) {
    return null;
  }

  const receiver = translateBodyExpr(
    tsReceiver,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(receiver)) {
    return receiver;
  }

  const pending = receiver.pendingComprehension;
  const isComposing = pending !== undefined;
  // Hygienic `$N` binders (Barendregt convention): they cannot clash with
  // user-visible identifiers or with other fresh binders from the same
  // translation session.
  const sourceBinder = isComposing
    ? pending.binder
    : freshHygienicBinder(supply);
  const callbackBinder = isComposing
    ? freshHygienicBinder(supply)
    : sourceBinder;
  const extendedParams = new Map(paramNames);
  extendedParams.set(callbackBinder, callbackBinder);

  const rawBody = extractArrowBody(
    expr.arguments[0]!,
    callbackBinder,
    extendedParams,
    checker,
    strategy,
    supply,
  );
  if (!rawBody) {
    return { unsupported: expr.getText() };
  }
  if (isBodyUnsupported(rawBody)) {
    return rawBody;
  }

  // Substitute the callback binder with the prior chain's *projection* (not
  // `var(sourceBinder)` — for a prior `.map(u => score u)`, the element in
  // scope is `score u`, not `u`). Initial step has no prior projection, so
  // `callbackBinder === sourceBinder` and this is a no-op.
  const bodyE = isComposing
    ? ast.substituteBinder(bodyExpr(rawBody), callbackBinder, receiver.expr)
    : bodyExpr(rawBody);

  if (methodName === "filter") {
    if (isComposing) {
      return {
        expr: receiver.expr,
        pendingComprehension: {
          binder: pending.binder,
          arrExpr: pending.arrExpr,
          guards: [...pending.guards, ast.gExpr(bodyE)],
        },
      };
    }
    return {
      expr: ast.var(sourceBinder),
      pendingComprehension: {
        binder: sourceBinder,
        arrExpr: receiver.expr,
        guards: [ast.gExpr(bodyE)],
      },
    };
  }
  // map
  if (isComposing) {
    return {
      expr: bodyE,
      pendingComprehension: pending,
    };
  }
  return {
    expr: bodyE,
    pendingComprehension: {
      binder: sourceBinder,
      arrExpr: receiver.expr,
      guards: [],
    },
  };
}

/**
 * Translate `arr.reduce((acc, x) => acc OP f(x), init)` to a comprehension fold.
 * Emits `init OP (combOP over each x in arr | f(x))`, eliding `init` when it
 * equals the combiner's identity element.
 *
 * Fuses with an upstream `.filter`/`.map` pending comprehension (Wadler,
 * TCS 1990) into a single `eachComb` with accumulated guards and the
 * composed projection.
 */
function translateReduceCall(
  methodName: "reduce" | "reduceRight",
  tsReceiver: ts.Expression,
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult | null {
  const ast = getAst();

  if (expr.arguments.length !== 2) {
    return { unsupported: `.${methodName} requires an explicit initial value` };
  }

  if (!getArrayElementType(tsReceiver, checker, strategy)) {
    return null;
  }

  const receiver = translateBodyExpr(
    tsReceiver,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(receiver)) {
    return receiver;
  }

  const cb = expr.arguments[0]!;
  if (!ts.isArrowFunction(cb)) {
    return { unsupported: `.${methodName} callback must be an arrow function` };
  }
  if (cb.parameters.length !== 2) {
    return {
      unsupported: `.${methodName} callback must take exactly (acc, x)`,
    };
  }
  const accParamNode = cb.parameters[0]!;
  const xParamNode = cb.parameters[1]!;
  if (
    !ts.isIdentifier(accParamNode.name) ||
    !ts.isIdentifier(xParamNode.name)
  ) {
    return {
      unsupported: `.${methodName} callback parameters must be identifiers`,
    };
  }
  const accName = (accParamNode.name as ts.Identifier).text;
  const xName = (xParamNode.name as ts.Identifier).text;

  let body: ts.Expression;
  if (ts.isBlock(cb.body)) {
    const stmts = cb.body.statements;
    if (
      stmts.length !== 1 ||
      !ts.isReturnStatement(stmts[0]!) ||
      !stmts[0]!.expression
    ) {
      return {
        unsupported: `.${methodName} callback block body must be a single return`,
      };
    }
    body = stmts[0]!.expression;
  } else {
    body = cb.body;
  }
  body = unwrapExpression(body);

  if (!ts.isBinaryExpression(body)) {
    return {
      unsupported: `.${methodName} callback body must be 'acc OP f(x)'`,
    };
  }
  const info = binopToReduceInfo(body.operatorToken.kind);
  if (info === null) {
    return {
      unsupported: `.${methodName} operator ${ts.SyntaxKind[body.operatorToken.kind]} has no combiner`,
    };
  }

  // `reduceRight` visits elements right-to-left; safe only for commutative combiners.
  if (methodName === "reduceRight" && !info.commutative) {
    return {
      unsupported: `.reduceRight with non-commutative operator`,
    };
  }

  const leftRoot = getRootIdentifier(body.left);
  const rightRoot = getRootIdentifier(body.right);
  let innerExpr: ts.Expression;
  if (leftRoot === accName && rightRoot !== accName) {
    innerExpr = body.right;
  } else if (rightRoot === accName && leftRoot !== accName) {
    if (!info.commutative) {
      return {
        unsupported: `.${methodName} with acc on the right of a non-commutative operator`,
      };
    }
    innerExpr = body.left;
  } else {
    return {
      unsupported: `.${methodName} callback must reference acc exactly once`,
    };
  }

  if (expressionReferencesNames(innerExpr, new Set([accName]))) {
    return {
      unsupported: `.${methodName} inner expression must not reference acc`,
    };
  }

  const pending = receiver.pendingComprehension;
  // Hygienic `$N` binder for the callback's `x`; in the composing case we'll
  // substitute it away with the prior projection so the outer guard binds
  // `pending.binder`.
  const xBinder = freshHygienicBinder(supply);
  const extendedParams = new Map(paramNames);
  extendedParams.set(xName, xBinder);

  const innerResult = translateBodyExpr(
    innerExpr,
    checker,
    strategy,
    extendedParams,
    state,
    supply,
  );
  if (isBodyUnsupported(innerResult)) {
    return innerResult;
  }

  const comb = makeCombiner(info.combiner);
  let folded: OpaqueExpr;
  if (pending) {
    const projectedInner = ast.substituteBinder(
      bodyExpr(innerResult),
      xBinder,
      receiver.expr,
    );
    folded = ast.eachComb(
      [],
      [ast.gIn(pending.binder, pending.arrExpr), ...pending.guards],
      comb,
      projectedInner,
    );
  } else {
    folded = ast.eachComb(
      [],
      [ast.gIn(xBinder, receiver.expr)],
      comb,
      bodyExpr(innerResult),
    );
  }

  const initNode = expr.arguments[1]!;
  if (
    info.identityText !== null &&
    isIdentityInit(initNode, info.identityText)
  ) {
    return { expr: folded };
  }

  const initResult = translateBodyExpr(
    initNode,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(initResult)) {
    return initResult;
  }

  const outerOp = translateOperator(info.outer);
  if (outerOp === null) {
    return { unsupported: `.${methodName} outer operator translation` };
  }
  return {
    expr: ast.binop(outerOp, bodyExpr(initResult), folded),
  };
}

// --- Structured iteration (for-of / forEach / reduce) ---
//
// Catamorphisms over arrays (Meijer et al., "Functional Programming with
// Bananas, Lenses, Envelopes and Barbed Wire", FPCA 1991). Three shapes:
//
//   A. `for (const x of arr) { x.p = e }`   → `all x in arr | p' x = e`
//   B. `for (const x of arr) { a.p OP= f }` → `p' a = p a OP (combOP over each x in arr | f)`
//   C. `arr.reduce((acc, x) => acc OP f, i)`→ `i OP (combOP over each x in arr | f)`
//
// See CLAUDE.md § Structured Iteration.

interface ShapeBLeaf {
  /** `a` in `a.total += x.v` — the accumulator base expression (must not depend on iterator). */
  target: ts.Expression;
  /** `total` in `a.total += x.v`. */
  prop: string;
  /** Fold op pair (inner combiner + outer binary operator). */
  ops: FoldOps;
  /** RHS expression `f(x)` — may reference the iterator. */
  rhs: ts.Expression;
  /** Optional guard from a wrapping `if (g(x)) { a.p OP= f(x) }`. */
  guard?: ts.Expression;
}

type LoopStmtClass =
  | { kind: "shapeA" }
  | { kind: "shapeB"; leaves: ShapeBLeaf[] }
  | { unsupported: string };

/** Extract the root identifier of a chained property access (`a.b.c` → `a`). */
function getRootIdentifier(expr: ts.Expression): string | null {
  expr = unwrapExpression(expr);
  if (ts.isIdentifier(expr)) {
    return expr.text;
  }
  if (ts.isPropertyAccessExpression(expr)) {
    return getRootIdentifier(expr.expression);
  }
  return null;
}

/**
 * Classify one loop-body statement as Shape A (iterator-targeted write),
 * Shape B (accumulator fold), or unsupported. Handles nested if-stmts:
 *   - if all branches are Shape A, the whole is Shape A
 *   - if the stmt is `if (g(x)) { a.p OP= f }` with no else, it becomes a
 *     single Shape B leaf with `guard = g(x)`.
 */
function classifyLoopStmt(
  stmt: ts.Statement,
  iterName: string,
  checker: ts.TypeChecker,
  parentGuard?: ts.Expression,
): LoopStmtClass {
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
        return {
          unsupported: "compound assignment on loop iterator property",
        };
      }
      return { kind: "shapeA" };
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
    const leaf: ShapeBLeaf = {
      target: bin.left.expression,
      prop: bin.left.name.text,
      ops: compoundFold,
      rhs: bin.right,
    };
    if (parentGuard) {
      leaf.guard = parentGuard;
    }
    return { kind: "shapeB", leaves: [leaf] };
  }

  if (ts.isIfStatement(stmt)) {
    if (expressionHasSideEffects(stmt.expression, checker)) {
      return { unsupported: "impure if-condition in loop body" };
    }
    const thenStmts = flattenStmt(stmt.thenStatement);
    const elseStmts = stmt.elseStatement ? flattenStmt(stmt.elseStatement) : [];

    // Shape A: every branch-leaf is an iterator-write
    const allA = [...thenStmts, ...elseStmts].every((s) => {
      const c = classifyLoopStmt(s, iterName, checker);
      return "kind" in c && c.kind === "shapeA";
    });
    if (allA) {
      return { kind: "shapeA" };
    }

    // Shape B: `if (g) { a.p OP= f }` — single leaf, no else, no parent guard
    if (
      elseStmts.length === 0 &&
      thenStmts.length === 1 &&
      parentGuard === undefined
    ) {
      const inner = classifyLoopStmt(
        thenStmts[0]!,
        iterName,
        checker,
        stmt.expression,
      );
      if ("kind" in inner && inner.kind === "shapeB") {
        return inner;
      }
    }
    return {
      unsupported:
        "loop body if-statement mixes shapes or has an unsupported structure",
    };
  }

  if (ts.isBlock(stmt)) {
    const stmts = Array.from(stmt.statements);
    const classes = stmts.map((s) =>
      classifyLoopStmt(s, iterName, checker, parentGuard),
    );
    const firstBad = classes.find((c) => "unsupported" in c);
    if (firstBad) {
      return firstBad;
    }
    const allA = classes.every((c) => "kind" in c && c.kind === "shapeA");
    if (allA) {
      return { kind: "shapeA" };
    }
    const allB = classes.every((c) => "kind" in c && c.kind === "shapeB");
    if (allB) {
      const leaves = classes.flatMap(
        (c) => (c as { kind: "shapeB"; leaves: ShapeBLeaf[] }).leaves,
      );
      return { kind: "shapeB", leaves };
    }
    return {
      unsupported: "loop body block mixes iterator and accumulator writes",
    };
  }

  return { unsupported: `loop body statement: ${ts.SyntaxKind[stmt.kind]}` };
}

/**
 * Emit Shape A / Shape B propositions for a loop body.
 *
 * Shape A uses a sub-`symbolicExecute` to reuse the full symbolic-state
 * machinery (path merging via `cond` for conditional writes). Each emitted
 * write is wrapped in `all x in arr | p' x = v`.
 *
 * Shape B writes are merged into the caller's `state`: the outer state's
 * prior value (or the pre-state identity) is combined with the comprehension
 * `(combOP over each x in arr[, g(x)] | f(x))` via the outer binary operator.
 */
function translateForOfLoopBody(
  iterName: string,
  arrExpr: OpaqueExpr,
  bodyStmts: ts.Statement[],
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
  supply: UniqueSupply,
): boolean {
  const ast = getAst();

  const shapeAStmts: ts.Statement[] = [];
  const shapeBLeaves: ShapeBLeaf[] = [];
  for (const stmt of bodyStmts) {
    const cls = classifyLoopStmt(stmt, iterName, checker);
    if ("unsupported" in cls) {
      propositions.push({ kind: "unsupported", reason: cls.unsupported });
      return false;
    }
    if (cls.kind === "shapeA") {
      shapeAStmts.push(stmt);
    } else {
      shapeBLeaves.push(...cls.leaves);
    }
  }

  // `subState` captures Shape A's per-iteration writes so that Shape B reads
  // of the iterator's properties resolve to the updated expression. E.g.
  // `x.value = x.value + 1; a.total += x.value` must fold as
  // `total a + (+ over each x in xs | value x + 1)`, not `value x`. Property
  // reads against non-iterator objects pass through unchanged (they don't
  // appear as keys in `subState.writes`).
  const subState = makeSymbolicState(applyConst);
  const subParams = new Map(paramNames);
  subParams.set(iterName, iterName);

  if (shapeAStmts.length > 0) {
    const subProps: PropResult[] = [];
    const subBlock = ts.factory.createBlock(shapeAStmts, true);
    const okA = symbolicExecute(
      subBlock,
      checker,
      strategy,
      subParams,
      subState,
      subProps,
      applyConst,
      supply,
      false,
    );
    if (!okA) {
      for (const p of subProps) {
        propositions.push(p);
      }
      return false;
    }
    for (const [, entry] of subState.writes) {
      propositions.push({
        kind: "equation",
        quantifiers: [] as OpaqueParam[],
        guards: [ast.gIn(iterName, arrExpr)],
        lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
        rhs: entry.value,
      });
      state.modifiedProps.add(entry.prop);
    }
  }

  for (const leaf of shapeBLeaves) {
    const accResult = translateBodyExpr(
      leaf.target,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(accResult)) {
      propositions.push({ kind: "unsupported", reason: accResult.unsupported });
      return false;
    }
    const accExpr = applyConst(bodyExpr(accResult));

    const rhsResult = translateBodyExpr(
      leaf.rhs,
      checker,
      strategy,
      subParams,
      subState,
      supply,
    );
    if (isBodyUnsupported(rhsResult)) {
      propositions.push({ kind: "unsupported", reason: rhsResult.unsupported });
      return false;
    }
    const rhsExpr = applyConst(bodyExpr(rhsResult));

    const guards: OpaqueGuard[] = [ast.gIn(iterName, arrExpr)];
    if (leaf.guard) {
      const gResult = translateBodyExpr(
        leaf.guard,
        checker,
        strategy,
        subParams,
        subState,
        supply,
      );
      if (isBodyUnsupported(gResult)) {
        propositions.push({ kind: "unsupported", reason: gResult.unsupported });
        return false;
      }
      guards.push(ast.gExpr(applyConst(bodyExpr(gResult))));
    }

    const comb = makeCombiner(leaf.ops.combiner);
    const folded = ast.eachComb([], guards, comb, rhsExpr);

    const outerOp = translateOperator(leaf.ops.outer);
    if (outerOp === null) {
      propositions.push({
        kind: "unsupported",
        reason: "loop-fold outer operator",
      });
      return false;
    }
    const key = symbolicKey(leaf.prop, accExpr);
    const priorEntry = state.writes.get(key);
    const priorVal =
      priorEntry?.value ?? ast.app(ast.var(leaf.prop), [accExpr]);
    const newVal = ast.binop(outerOp, priorVal, folded);

    state.writes.set(key, { prop: leaf.prop, objExpr: accExpr, value: newVal });
    state.writtenKeys.add(key);
  }

  return true;
}

/** Translate a `for (const x of arr) { ... }` statement. */
function translateForOfLoop(
  stmt: ts.ForOfStatement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
  supply: UniqueSupply,
): boolean {
  const initList = stmt.initializer;
  if (
    !ts.isVariableDeclarationList(initList) ||
    !(initList.flags & ts.NodeFlags.Const) ||
    initList.declarations.length !== 1
  ) {
    propositions.push({
      kind: "unsupported",
      reason: "for-of initializer must be a single const binding",
    });
    return false;
  }
  const decl = initList.declarations[0]!;
  if (!ts.isIdentifier(decl.name)) {
    propositions.push({
      kind: "unsupported",
      reason: "for-of destructuring pattern is not supported",
    });
    return false;
  }
  const iterName = decl.name.text;

  const arrResult = translateBodyExpr(
    stmt.expression,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(arrResult)) {
    propositions.push({ kind: "unsupported", reason: arrResult.unsupported });
    return false;
  }
  const arrExpr = applyConst(bodyExpr(arrResult));
  const bodyStmts = flattenStmt(stmt.statement);

  return translateForOfLoopBody(
    iterName,
    arrExpr,
    bodyStmts,
    checker,
    strategy,
    paramNames,
    state,
    propositions,
    applyConst,
    supply,
  );
}

/**
 * Translate a `arr.forEach(x => { ... })` call-statement. Returns null if
 * the call is not a forEach (caller falls back to normal handling).
 */
function translateForEachStmt(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
  supply: UniqueSupply,
): boolean | null {
  if (
    !ts.isPropertyAccessExpression(call.expression) ||
    call.expression.name.text !== "forEach" ||
    call.arguments.length !== 1
  ) {
    return null;
  }
  const receiver = call.expression.expression;
  const arg = call.arguments[0]!;
  if (!ts.isArrowFunction(arg)) {
    propositions.push({
      kind: "unsupported",
      reason: "forEach callback must be an arrow function",
    });
    return false;
  }
  if (
    arg.parameters.length !== 1 ||
    !ts.isIdentifier(arg.parameters[0]!.name)
  ) {
    propositions.push({
      kind: "unsupported",
      reason: "forEach callback must take a single identifier parameter",
    });
    return false;
  }
  const iterName = (arg.parameters[0]!.name as ts.Identifier).text;

  const arrResult = translateBodyExpr(
    receiver,
    checker,
    strategy,
    paramNames,
    state,
    supply,
  );
  if (isBodyUnsupported(arrResult)) {
    propositions.push({ kind: "unsupported", reason: arrResult.unsupported });
    return false;
  }
  const arrExpr = applyConst(bodyExpr(arrResult));

  const bodyStmts = ts.isBlock(arg.body)
    ? Array.from(arg.body.statements)
    : [ts.factory.createExpressionStatement(arg.body)];

  return translateForOfLoopBody(
    iterName,
    arrExpr,
    bodyStmts,
    checker,
    strategy,
    paramNames,
    state,
    propositions,
    applyConst,
    supply,
  );
}

function translateCallExpr(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState | undefined,
  supply: UniqueSupply,
): BodyResult {
  const ast = getAst();

  // Method calls: obj.method(args)
  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const tsReceiver = expr.expression.expression;

    // .get(k) / .has(k) on a Map<K,V> field -> 2-arity rule application.
    // Map fields translate to a pair of rules: `<name>Key c k => Bool` and
    // `<name> c k, <name>Key c k => V`. See translate-types.ts.
    if (
      (methodName === "get" || methodName === "has") &&
      expr.arguments.length === 1 &&
      ts.isPropertyAccessExpression(tsReceiver) &&
      isMapType(checker.getTypeAtLocation(tsReceiver))
    ) {
      const fieldName = tsReceiver.name.text;
      const innerObj = tsReceiver.expression;
      const kExpr = translateBodyExpr(
        expr.arguments[0]!,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(kExpr)) {
        return kExpr;
      }
      const objExpr = translateBodyExpr(
        innerObj,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      const ruleName = methodName === "has" ? `${fieldName}Key` : fieldName;
      return {
        expr: ast.app(ast.var(ruleName), [bodyExpr(objExpr), bodyExpr(kExpr)]),
      };
    }

    // .includes(x) on Array / .has(x) on Set -> x in obj
    if (
      (methodName === "includes" || methodName === "has") &&
      expr.arguments.length === 1
    ) {
      const receiverType = checker.getTypeAtLocation(tsReceiver);
      const isArray =
        methodName === "includes" && checker.isArrayType(receiverType);
      const isSet = methodName === "has" && isSetType(receiverType);
      if (!isArray && !isSet) {
        return {
          unsupported:
            methodName === "includes"
              ? "non-array .includes()"
              : "non-Set .has()",
        };
      }
      const arg = translateBodyExpr(
        expr.arguments[0]!,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(arg)) {
        return arg;
      }
      const objExpr = translateBodyExpr(
        tsReceiver,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(objExpr)) {
        return objExpr;
      }
      return { expr: ast.binop(ast.opIn(), bodyExpr(arg), bodyExpr(objExpr)) };
    }

    // .filter(pred) / .map(fn) — each independently produces or refines a comprehension
    if (
      (methodName === "filter" || methodName === "map") &&
      expr.arguments.length === 1
    ) {
      const result = translateArrayMethod(
        methodName,
        tsReceiver,
        expr,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (result) {
        return result;
      }
    }

    // .reduce(cb, init) / .reduceRight(cb, init) — fold into `over each` aggregate
    if (methodName === "reduce" || methodName === "reduceRight") {
      const result = translateReduceCall(
        methodName,
        tsReceiver,
        expr,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (result) {
        return result;
      }
    }

    // General method call: obj.method(args) → method obj args (EUF encoding)
    // Ref: Kroening & Strichman, Decision Procedures, Ch. 4
    if (expr.arguments.some(ts.isSpreadElement)) {
      return { unsupported: expr.getText() };
    }
    const receiver = translateBodyExpr(
      tsReceiver,
      checker,
      strategy,
      paramNames,
      state,
      supply,
    );
    if (isBodyUnsupported(receiver)) {
      return receiver;
    }
    const methodArgs: OpaqueExpr[] = [bodyExpr(receiver)];
    for (const arg of expr.arguments) {
      const a = translateBodyExpr(
        arg,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(a)) {
        return a;
      }
      methodArgs.push(bodyExpr(a));
    }
    return { expr: ast.app(ast.var(methodName), methodArgs) };
  }

  // Free function calls: fn(args) → fn args (EUF encoding)
  if (ts.isIdentifier(expr.expression)) {
    const fnName = expr.expression.text;

    if (expr.arguments.some(ts.isSpreadElement)) {
      return { unsupported: expr.getText() };
    }

    // Zero-arity call → variable reference (EUF constant)
    if (expr.arguments.length === 0) {
      return { expr: ast.var(paramNames.get(fnName) ?? fnName) };
    }

    const fnArgs: OpaqueExpr[] = [];
    for (const arg of expr.arguments) {
      const a = translateBodyExpr(
        arg,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(a)) {
        return a;
      }
      fnArgs.push(bodyExpr(a));
    }
    return { expr: ast.app(ast.var(paramNames.get(fnName) ?? fnName), fnArgs) };
  }

  // Unsupported call (computed calls, tagged templates, optional calls, etc.)
  return { unsupported: expr.getText() };
}

function extractArrowBody(
  expr: ts.Expression,
  binderName: string,
  paramNames: Map<string, string>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  supply: UniqueSupply,
): BodyResult | null {
  if (!ts.isArrowFunction(expr)) {
    return null;
  }
  if (
    expr.parameters.length !== 1 ||
    !ts.isIdentifier(expr.parameters[0]!.name)
  ) {
    return {
      unsupported:
        "filter/map callback must have exactly one identifier parameter",
    };
  }

  // Map arrow param to the fresh binder
  const param = expr.parameters[0]!;
  const arrowParams = new Map(paramNames);
  arrowParams.set((param.name as ts.Identifier).text, binderName);

  if (ts.isBlock(expr.body)) {
    // Only allow a single return (after filtering guards), same rule as
    // extractReturnExpression — blocks with locals or multiple statements
    // would introduce free variables in the generated comprehension.
    const nonGuard = expr.body.statements.filter(
      (s) => !isGuardStatement(s, checker),
    );
    if (nonGuard.length === 1) {
      const s = nonGuard[0]!;
      if (ts.isReturnStatement(s) && s.expression) {
        return translateBodyExpr(
          s.expression,
          checker,
          strategy,
          arrowParams,
          undefined,
          supply,
        );
      }
    }
    return null;
  }

  // Expression body
  return translateBodyExpr(
    expr.body,
    checker,
    strategy,
    arrowParams,
    undefined,
    supply,
  );
}

// --- Mutating function body translation ---

function translateMutatingBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  declarations: PantDeclaration[],
): PropResult[] {
  if (!node.body) {
    return [];
  }

  const ast = getAst();
  const propositions: PropResult[] = [];
  const state = makeSymbolicState();

  const ok = symbolicExecute(
    node.body,
    checker,
    strategy,
    paramNames,
    state,
    propositions,
  );

  // Only emit state equations + frames when the whole body was translatable;
  // partial emission would be unsound (frames would mask unhandled writes).
  if (!ok) {
    return propositions;
  }

  for (const [, entry] of state.writes) {
    propositions.push({
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs: ast.app(ast.primed(entry.prop), [entry.objExpr]),
      rhs: entry.value,
    });
    state.modifiedProps.add(entry.prop);
  }

  const frames = generateFrameConditions(state.modifiedProps, declarations);
  propositions.push(...frames);

  return propositions;
}

/**
 * Forward symbolic execution with path merging (Dijkstra CACM 1975;
 * Allen POPL 1983 if-conversion). Updates `state.writes` for each property
 * assignment; merges `if`/`else` via `cond` at the join point. Returns
 * `false` when an unsupported construct was encountered; unsupported
 * markers are pushed into `propositions` for the caller to inspect.
 */
function symbolicExecute(
  body: ts.Block | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  state: SymbolicState,
  propositions: PropResult[],
  outerApply: (e: OpaqueExpr) => OpaqueExpr = (e) => e,
  supply: UniqueSupply = makeUniqueSupply(),
  insideBranch: boolean = false,
): boolean {
  const ast = getAst();
  let ok = true;
  let applyConst = outerApply;
  // Keep the state's canonicalize in sync with the frame's applyConst so
  // symbolic-state reads see the same normalization the write site uses.
  state.canonicalize = applyConst;
  const stmts = ts.isBlock(body) ? Array.from(body.statements) : [body];

  for (let i = 0; i < stmts.length; i++) {
    const stmt = stmts[i]!;
    // Skip guard statements (if-throw patterns and assertion calls)
    if (isGuardStatement(stmt, checker)) {
      continue;
    }

    // Early-exit if-conversion (Allen et al., POPL 1983, extended to
    // early exits). Any `if` with a bare-return branch lifts the remaining
    // statements — plus the other branch's statements when present — into
    // a single continuation conditioned on the non-early-exit path.
    const exit = !insideBranch ? detectEarlyExit(stmt) : null;
    if (exit !== null) {
      if (expressionHasSideEffects(exit.condition, checker)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: "impure if-condition in mutating body",
        });
        break;
      }
      const gResult = translateBodyExpr(
        exit.condition,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(gResult)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: gResult.unsupported,
        });
        break;
      }
      const gExpr = applyConst(bodyExpr(gResult));

      // Continuation = (other-branch's stmts if any) ++ post-if stmts.
      // The continuation is the fall-through path at the same logical scope
      // as the current statement list, so preserve the caller's `insideBranch`
      // flag rather than forcing it. This lets a chain of top-level guards
      // like `if (g) return; if (h) return; a.balance = 1` flatten into
      // nested conds via successive if-conversion passes (Allen et al.,
      // POPL 1983); forcing `true` would instead reject the second guard as
      // `return in mutating branch`.
      const continuation = [...exit.continuationPrefix, ...stmts.slice(i + 1)];
      const sR = cloneSymbolicState(state);
      const remainingProps: PropResult[] = [];
      const continuationBlock = ts.factory.createBlock(continuation, true);
      const okR = symbolicExecute(
        continuationBlock,
        checker,
        strategy,
        new Map(paramNames),
        sR,
        remainingProps,
        applyConst,
        supply,
        insideBranch,
      );
      if (!okR) {
        ok = false;
        propositions.push(...remainingProps);
        break;
      }

      // Shape A loop equations emit directly into `propositions` rather than
      // flowing through `state.writes`, so they'd be silently dropped by the
      // merge-only path below. Reject when the continuation produced such
      // equations — we'd need to thread `gExpr` into each rhs (and reconcile
      // `state.modifiedProps`) to preserve the early-exit semantics, which
      // isn't yet implemented.
      const directEquations = remainingProps.filter(
        (p) => p.kind === "equation",
      );
      if (directEquations.length > 0) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason:
            "loop with per-iteration writes cannot appear after an early-exit guard",
        });
        break;
      }

      // Merge: for each key touched by the continuation, emit a cond
      // selecting the pre-state value when we take the early exit and
      // the continuation's value otherwise. `earlyExitWhenTrue` picks
      // which arm of the cond the condition guards.
      for (const key of sR.writtenKeys) {
        const entryR = sR.writes.get(key)!;
        const prior = state.writes.get(key);
        const identity = ast.app(ast.var(entryR.prop), [entryR.objExpr]);
        const vEarlyReturn = prior?.value ?? identity;
        const vContinuation = entryR.value;
        const merged = exit.earlyExitWhenTrue
          ? ast.cond([
              [gExpr, vEarlyReturn],
              [ast.litBool(true), vContinuation],
            ])
          : ast.cond([
              [gExpr, vContinuation],
              [ast.litBool(true), vEarlyReturn],
            ]);
        state.writes.set(key, {
          prop: entryR.prop,
          objExpr: entryR.objExpr,
          value: merged,
        });
        state.writtenKeys.add(key);
      }
      // Remaining stmts have been consumed by the continuation.
      break;
    }

    // Handle const bindings via shared inlineConstBindings
    if (ts.isVariableStatement(stmt)) {
      const declList = stmt.declarationList;
      if (declList.flags & ts.NodeFlags.Const) {
        const bindings: ConstBinding[] = [];
        let allPure = true;
        for (const decl of declList.declarations) {
          if (
            !ts.isIdentifier(decl.name) ||
            !decl.initializer ||
            expressionHasSideEffects(decl.initializer, checker)
          ) {
            allPure = false;
            break;
          }
          bindings.push({
            tsName: decl.name.text,
            initializer: decl.initializer,
          });
        }
        if (allPure) {
          const inlined = inlineConstBindings(
            bindings,
            checker,
            strategy,
            paramNames,
            supply,
            state,
          );
          if ("error" in inlined) {
            ok = false;
            propositions.push({
              kind: "unsupported",
              reason: inlined.error,
            });
          } else {
            for (const [key, value] of inlined.scopedParams) {
              paramNames.set(key, value);
            }
            const prevApply = applyConst;
            applyConst = (e) => prevApply(inlined.applyTo(e));
            state.canonicalize = applyConst;
          }
          continue;
        }
      }
      ok = false;
      propositions.push({
        kind: "unsupported",
        reason: "local variable declaration (let/var or effectful const)",
      });
      continue;
    }

    // Property assignment: obj.prop = rhs
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isBinaryExpression(unwrapExpression(stmt.expression))
    ) {
      const bin = unwrapExpression(stmt.expression) as ts.BinaryExpression;
      const compoundOp = COMPOUND_ASSIGN_TO_BINOP.get(bin.operatorToken.kind);
      const isSimpleAssign =
        bin.operatorToken.kind === ts.SyntaxKind.EqualsToken;
      if (
        (isSimpleAssign || compoundOp !== undefined) &&
        ts.isPropertyAccessExpression(bin.left)
      ) {
        const prop = bin.left.name.text;
        const obj = translateBodyExpr(
          bin.left.expression,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        if (isBodyUnsupported(obj)) {
          ok = false;
          propositions.push({ kind: "unsupported", reason: obj.unsupported });
          continue;
        }
        // For compound assignment `a.p OP= v`, desugar rhs to `a.p OP v`.
        // The rhs's `a.p` read goes through translateBodyExpr, which
        // consults the symbolic state and returns the prior-write value
        // or the pre-state identity.
        const rhsNode =
          compoundOp !== undefined
            ? ts.factory.createBinaryExpression(bin.left, compoundOp, bin.right)
            : bin.right;
        const val = translateBodyExpr(
          rhsNode,
          checker,
          strategy,
          paramNames,
          state,
          supply,
        );
        if (isBodyUnsupported(val)) {
          ok = false;
          propositions.push({ kind: "unsupported", reason: val.unsupported });
          continue;
        }
        const objExpr = applyConst(bodyExpr(obj));
        const valExpr = applyConst(bodyExpr(val));
        const key = symbolicKey(prop, objExpr);
        state.writes.set(key, { prop, objExpr, value: valExpr });
        state.writtenKeys.add(key);
        continue;
      }
    }

    // `arr.forEach(x => { ... })` — structurally equivalent to `for-of` when
    // used as a statement. Dispatch to the same loop-body translator.
    if (
      ts.isExpressionStatement(stmt) &&
      ts.isCallExpression(unwrapExpression(stmt.expression)) &&
      !insideBranch
    ) {
      const call = unwrapExpression(stmt.expression) as ts.CallExpression;
      const okF = translateForEachStmt(
        call,
        checker,
        strategy,
        paramNames,
        state,
        propositions,
        applyConst,
        supply,
      );
      if (okF !== null) {
        if (!okF) {
          ok = false;
        }
        continue;
      }
    }

    if (
      ts.isExpressionStatement(stmt) &&
      expressionHasSideEffects(stmt.expression, checker)
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful expression",
      });
      ok = false;
      continue;
    }

    if (
      ts.isVariableStatement(stmt) &&
      stmt.declarationList.declarations.some(
        (d) =>
          d.initializer && expressionHasSideEffects(d.initializer, checker),
      )
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful variable initializer",
      });
      ok = false;
      continue;
    }

    if (
      (ts.isReturnStatement(stmt) || ts.isThrowStatement(stmt)) &&
      stmt.expression &&
      expressionHasSideEffects(stmt.expression, checker)
    ) {
      propositions.push({
        kind: "unsupported",
        reason: "side-effectful control-flow expression",
      });
      ok = false;
      continue;
    }

    // Bare `return;` at top level is a no-op (void function). Inside a
    // branch it's unsound — symbolic execution assumes each branch reaches
    // the join point with a well-defined state. Early exit would leave
    // later writes conditionally unreachable, which the merge cannot encode.
    if (ts.isReturnStatement(stmt) && !stmt.expression) {
      if (insideBranch) {
        propositions.push({
          kind: "unsupported",
          reason: "return in mutating branch",
        });
        ok = false;
      }
      continue;
    }

    // `return expr;` or `throw;` always break the path-merging model.
    if (ts.isReturnStatement(stmt) || ts.isThrowStatement(stmt)) {
      propositions.push({
        kind: "unsupported",
        reason: "return/throw in mutating body",
      });
      ok = false;
      continue;
    }

    // Nested block — flow state through sequentially
    if (ts.isBlock(stmt)) {
      const inner = symbolicExecute(
        stmt,
        checker,
        strategy,
        paramNames,
        state,
        propositions,
        applyConst,
        supply,
        insideBranch,
      );
      if (!inner) {
        ok = false;
      }
      continue;
    }

    // Conditional mutation: path merging via cond
    if (ts.isIfStatement(stmt)) {
      if (expressionHasSideEffects(stmt.expression, checker)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: "impure if-condition in mutating body",
        });
        continue;
      }
      const gResult = translateBodyExpr(
        stmt.expression,
        checker,
        strategy,
        paramNames,
        state,
        supply,
      );
      if (isBodyUnsupported(gResult)) {
        ok = false;
        propositions.push({
          kind: "unsupported",
          reason: gResult.unsupported,
        });
        continue;
      }
      const gExpr = applyConst(bodyExpr(gResult));

      const sT = cloneSymbolicState(state);
      const thenProps: PropResult[] = [];
      const okT = symbolicExecute(
        stmt.thenStatement,
        checker,
        strategy,
        new Map(paramNames),
        sT,
        thenProps,
        applyConst,
        supply,
        true,
      );
      if (!okT) {
        ok = false;
        propositions.push(...thenProps);
        continue;
      }

      const sE = cloneSymbolicState(state);
      const elseProps: PropResult[] = [];
      if (stmt.elseStatement) {
        const okE = symbolicExecute(
          stmt.elseStatement,
          checker,
          strategy,
          new Map(paramNames),
          sE,
          elseProps,
          applyConst,
          supply,
          true,
        );
        if (!okE) {
          ok = false;
          propositions.push(...elseProps);
          continue;
        }
      }

      // Merge touched keys via cond(g => vT, true => vE)
      const touched = new Set<string>([...sT.writtenKeys, ...sE.writtenKeys]);
      for (const key of touched) {
        const entryT = sT.writes.get(key);
        const entryE = sE.writes.get(key);
        // At least one branch wrote the key, so at least one entry exists.
        const objExpr = (entryT ?? entryE)!.objExpr;
        const prop = (entryT ?? entryE)!.prop;
        const identity = ast.app(ast.var(prop), [objExpr]);
        const vT = entryT?.value ?? identity;
        const vE = entryE?.value ?? identity;
        const merged = ast.cond([
          [gExpr, vT],
          [ast.litBool(true), vE],
        ]);
        state.writes.set(key, { prop, objExpr, value: merged });
        state.writtenKeys.add(key);
      }
      continue;
    }

    if (ts.isForOfStatement(stmt) && !insideBranch) {
      const okL = translateForOfLoop(
        stmt,
        checker,
        strategy,
        paramNames,
        state,
        propositions,
        applyConst,
        supply,
      );
      if (!okL) {
        ok = false;
      }
      continue;
    }

    if (
      ts.isForStatement(stmt) ||
      ts.isForOfStatement(stmt) ||
      ts.isForInStatement(stmt) ||
      ts.isWhileStatement(stmt) ||
      ts.isDoStatement(stmt)
    ) {
      propositions.push({ kind: "unsupported", reason: "loop assignment" });
      ok = false;
      continue;
    }

    if (ts.isTryStatement(stmt)) {
      if (stmt.catchClause) {
        propositions.push({
          kind: "unsupported",
          reason: "try/catch assignment",
        });
        ok = false;
      } else {
        const inner = symbolicExecute(
          stmt.tryBlock,
          checker,
          strategy,
          paramNames,
          state,
          propositions,
          applyConst,
          supply,
          insideBranch,
        );
        if (!inner) {
          ok = false;
        }
      }
      if (stmt.finallyBlock) {
        const inner = symbolicExecute(
          stmt.finallyBlock,
          checker,
          strategy,
          paramNames,
          state,
          propositions,
          applyConst,
          supply,
          insideBranch,
        );
        if (!inner) {
          ok = false;
        }
      }
      continue;
    }

    if (ts.isSwitchStatement(stmt)) {
      propositions.push({ kind: "unsupported", reason: "switch assignment" });
      ok = false;
    }
  }

  return ok;
}

/**
 * Generate frame conditions: for each rule in declarations not explicitly
 * modified, emit `rule' x = rule x`. Variables are already in scope from
 * the rule declarations in the chapter head.
 */
function generateFrameConditions(
  modifiedRules: Set<string>,
  declarations: PantDeclaration[],
): PropResult[] {
  const ast = getAst();
  const frames: PropResult[] = [];

  for (const decl of declarations) {
    if (decl.kind !== "rule") {
      continue;
    }
    if (modifiedRules.has(decl.name)) {
      continue;
    }

    const paramArgs = decl.params.map((p) => ast.var(p.name));
    const lhs = ast.app(ast.primed(decl.name), paramArgs);
    const rhs = ast.app(ast.var(decl.name), paramArgs);
    frames.push({
      kind: "equation",
      quantifiers: [] as OpaqueParam[],
      lhs,
      rhs,
    });
  }

  return frames;
}
