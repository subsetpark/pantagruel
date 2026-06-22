// @archlint.module core
// @archlint.domain ts2pant.purity

/**
 * Conservative purity oracle for TypeScript call expressions.
 *
 * Standard name: Allowlist-based must-analysis over a {pure, effectful}
 * abstract domain (trivial two-point lattice).
 *
 * Framework: Abstract interpretation (Cousot & Cousot, POPL 1977).
 *   - Domain: {pure, effectful} with pure ⊑ effectful.
 *   - Default: effectful (⊤). Only positive evidence yields pure (⊥).
 *   - This is a must-analysis: returns "pure" only when provably so.
 *
 * Allowlist approach: Equivalent to the stub annotation technique used by
 * the Checker Framework (@Pure/@SideEffectFree, Dietl et al. ICSE 2011)
 * and Closure Compiler externs. The allowlist encodes the effect signature
 * of known library functions without requiring whole-program inference.
 *
 * Higher-order functions: Call-site callback specialization per
 * Lucassen & Gifford (POPL 1988). The effect of arr.map(f) depends on
 * the effect of f at each call site, not on a fixed effect for map.
 *
 * Pragmatic assumptions (shared with Webpack, Rollup, Closure Compiler):
 *   - Property access does not trigger effectful getters.
 *   - Implicit toString()/valueOf() coercions in template literals are pure.
 *   - Combinator names (pipe, flow, identity) match by identifier text,
 *     not by import source. Argument purity checking provides a safety net.
 *
 * Tiers:
 *   1a: Known-pure builtin allowlist (Math methods, String/Array methods)
 *   1b: Effect-TS combinator allowlist (pipe, flow, identity)
 *   1c: Conservative default (unknown = effectful)
 *
 * Ref: Cousot & Cousot, "Abstract Interpretation", POPL 1977.
 * Ref: Lucassen & Gifford, "Polymorphic Effect Systems", POPL 1988.
 * Ref: Dietl et al., "Building and Using Pluggable Type-Checkers", ICSE 2011.
 * Ref: Talpin & Jouvelot, "The Type and Effect Discipline", I&C 1994.
 */
import ts from "typescript";
import { isSetType } from "./translate-types.js";

// ---------------------------------------------------------------------------
// Tier 1a — Known-pure builtin allowlists
// ---------------------------------------------------------------------------

/**
 * Pure Math methods (enumerated, not blanket namespace).
 * Math.random() is excluded — it is non-deterministic.
 */
const PURE_MATH_METHODS: ReadonlySet<string> = new Set([
  "abs",
  "acos",
  "acosh",
  "asin",
  "asinh",
  "atan",
  "atan2",
  "atanh",
  "cbrt",
  "ceil",
  "clz32",
  "cos",
  "cosh",
  "exp",
  "expm1",
  "floor",
  "fround",
  "hypot",
  "imul",
  "log",
  "log10",
  "log1p",
  "log2",
  "max",
  "min",
  "pow",
  "round",
  "sign",
  "sin",
  "sinh",
  "sqrt",
  "tan",
  "tanh",
  "trunc",
]);

/**
 * Pure methods by primitive receiver type.
 *
 * String methods that accept RegExp or replacement callbacks are excluded
 * (split, replace, replaceAll, match, search) — these can execute user code
 * via RegExp Symbol.replace/Symbol.match hooks.
 */
const PURE_METHODS_BY_TYPE: ReadonlyMap<string, ReadonlySet<string>> = new Map([
  [
    "string",
    new Set([
      "indexOf",
      "slice",
      "substring",
      "includes",
      "startsWith",
      "endsWith",
      "trim",
      "toLowerCase",
      "toUpperCase",
      "charAt",
      "charCodeAt",
      "repeat",
      "padStart",
      "padEnd",
      "concat",
      "normalize",
    ]),
  ],
  [
    "number",
    new Set([
      "toFixed",
      "toPrecision",
      "toString",
      "toExponential",
      "toLocaleString",
    ]),
  ],
]);

/** Non-mutating `Map<K, V>` methods — read-only lookups. */
const PURE_MAP_METHODS: ReadonlySet<string> = new Set(["get", "has"]);

/** Non-mutating array methods that take no callback. */
const PURE_ARRAY_METHODS: ReadonlySet<string> = new Set([
  "at",
  "concat",
  "flat",
  "includes",
  "indexOf",
  "join",
  "keys",
  "lastIndexOf",
  "slice",
  "toString",
  "values",
]);

/** Array methods that are pure if their callback argument is side-effect-free. */
const HO_PURE_ARRAY_METHODS: ReadonlySet<string> = new Set([
  "every",
  "filter",
  "find",
  "findIndex",
  "flatMap",
  "map",
  "reduce",
  "reduceRight",
  "some",
]);

// ---------------------------------------------------------------------------
// Tier 1b — Effect-TS awareness
//
// Standard name: Callee symbol resolution (Dietl et al., ICSE 2011).
// The effect library's type declarations serve as implicit @Pure annotations.
// We verify each call by tracing the callee symbol back to its declaration
// file. Only exports explicitly listed in the pure allowlist are accepted.
//
// Approach: Explicit allowlist (not denylist). Only Effect-TS exports with
// known purity guarantees are listed. Unknown exports default to impure,
// consistent with the conservative must-analysis principle.
//
// Fallback: bare-name matching for pipe/flow/identity with argument purity
// checking, for test environments where the `effect` package is not installed.
// ---------------------------------------------------------------------------

/** Known-pure Effect-TS combinators (from effect/Function) — name-based fallback. */
const EFFECT_PURE_COMBINATORS: ReadonlySet<string> = new Set([
  "pipe",
  "flow",
  "identity",
]);

/**
 * Explicit allowlist of known-pure Effect-TS exports, keyed by module.
 *
 * Only exports listed here are classified as pure. Unknown exports default
 * to impure (conservative). This is an allowlist, not a denylist — adding
 * a new Effect-TS version's exports requires updating this list.
 *
 * Source: Effect-TS API docs, @category annotations in Effect.d.ts.
 * All constructors and combinators that return Effect<A,E,R> are pure
 * (referential transparency by design). Runners and mutable allocators
 * are excluded.
 */
const EFFECT_PURE_EXPORTS: ReadonlyMap<string, ReadonlySet<string>> = new Map([
  [
    "Effect",
    new Set([
      // Constructors (@category Creating Effects)
      "succeed",
      "fail",
      "sync",
      "promise",
      "tryPromise",
      "failSync",
      "failCause",
      "failCauseSync",
      "die",
      "dieSync",
      "dieMessage",
      "gen",
      "suspend",
      "never",
      "void",
      "succeedNone",
      "succeedSome",
      "yieldNow",
      // Mapping
      "map",
      "mapBoth",
      "mapError",
      "mapErrorCause",
      "as",
      "asSome",
      "asSomeError",
      "asVoid",
      "flip",
      "negate",
      "merge",
      // Sequencing
      "flatMap",
      "flatten",
      "andThen",
      "tap",
      "tapBoth",
      "tapDefect",
      "tapError",
      "tapErrorCause",
      "tapErrorTag",
      // Error handling
      "catchAll",
      "catchAllCause",
      "catchAllDefect",
      "catchIf",
      "catchSome",
      "catchSomeCause",
      "catchSomeDefect",
      "catchTag",
      "catchTags",
      "cause",
      "eventually",
      "ignore",
      "ignoreLogged",
      "retry",
      "retryOrElse",
      "sandbox",
      "unsandbox",
      "orDie",
      "orDieWith",
      // Fallback
      "orElse",
      "orElseFail",
      "orElseSucceed",
      "firstSuccessOf",
      // Zipping
      "zip",
      "zipLeft",
      "zipRight",
      "zipWith",
      // Matching
      "match",
      "matchCause",
      "matchCauseEffect",
      "matchEffect",
      // Filtering
      "filter",
      "filterMap",
      "filterOrDie",
      "filterOrDieMessage",
      "filterOrElse",
      "filterOrFail",
      // Conditional
      "when",
      "whenEffect",
      "unless",
      "unlessEffect",
      "if",
      // Collecting
      "all",
      "allSuccesses",
      "allWith",
      "forEach",
      "reduce",
      "reduceEffect",
      "reduceRight",
      // Outcome
      "either",
      "exit",
      "option",
      // Optional
      "fromNullable",
      "optionFromOptional",
      // Condition checking
      "every",
      "exists",
      "isFailure",
      "isSuccess",
      "liftPredicate",
      // Do notation
      "Do",
      "bind",
      "bindAll",
      "bindTo",
      // Context/provide
      "provide",
      "provideService",
      "provideServiceEffect",
      "contextWith",
      "contextWithEffect",
      "mapInputContext",
      "updateService",
      // Timing
      "delay",
      "sleep",
      "timed",
      "timeout",
      "timeoutFail",
      "timeoutFailCause",
      "timeoutTo",
      // Repetition
      "repeat",
      "repeatN",
      "repeatOrElse",
      "forever",
      "iterate",
      "loop",
      "schedule",
      // Racing
      "race",
      "raceAll",
      "raceFirst",
      "raceWith",
      // Validation
      "validate",
      "validateAll",
      "validateFirst",
      "validateWith",
      // Scoping
      "acquireRelease",
      "acquireUseRelease",
      "addFinalizer",
      "ensuring",
      "onError",
      "onExit",
      "scope",
      "scoped",
      // Logging (return Effect<void>)
      "log",
      "logDebug",
      "logError",
      "logFatal",
      "logInfo",
      "logTrace",
      "logWarning",
      // Tracing
      "annotateCurrentSpan",
      "currentSpan",
      "withSpan",
      // Interruption
      "allowInterrupt",
      "interrupt",
      "interruptible",
      "uninterruptible",
      // Forking (returns Effect<Fiber>)
      "fork",
      "forkDaemon",
      "forkScoped",
      "forkAll",
      // Caching
      "cached",
      "cachedWithTTL",
      "once",
      // Summarized
      "summarized",
      // Miscellaneous
      "fn",
      "partition",
    ]),
  ],
  [
    "Function",
    new Set([
      "pipe",
      "flow",
      "identity",
      "dual",
      "constTrue",
      "constFalse",
      "constVoid",
      "constUndefined",
      "absurd",
      "hole",
    ]),
  ],
  ["Pipeable", new Set(["pipe"])],
]);

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export interface EffectOracleOptions {
  admitForeignBoolPredicates?: boolean;
}

export function isDeclarationFileCallableSymbol(
  symbol: ts.Symbol | undefined,
  checker: ts.TypeChecker,
): boolean {
  if (symbol === undefined) {
    return false;
  }
  const resolved =
    symbol.flags & ts.SymbolFlags.Alias
      ? checker.getAliasedSymbol(symbol)
      : symbol;
  return (resolved.getDeclarations() ?? []).some((decl) => {
    const sf = decl.getSourceFile();
    return (
      sf.isDeclarationFile &&
      (ts.isFunctionDeclaration(decl) || ts.isMethodSignature(decl))
    );
  });
}

export function isBoolReturningDeclarationFileCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  if (!ts.isPropertyAccessExpression(expr.expression)) {
    return false;
  }
  if (
    !isDeclarationFileCallableSymbol(
      checker.getSymbolAtLocation(expr.expression.name),
      checker,
    )
  ) {
    return false;
  }
  const returnType = checker.getResolvedSignature(expr)?.getReturnType();
  if (
    returnType === undefined ||
    (returnType.flags &
      (ts.TypeFlags.Boolean | ts.TypeFlags.BooleanLiteral)) ===
      0
  ) {
    return false;
  }
  return isPredicateLikeDeclarationFileBoolCallName(expr.expression.name.text);
}

function isPredicateLikeDeclarationFileBoolCallName(name: string): boolean {
  return /^(?:is|has)(?:[A-Z_]|$)/.test(name);
}

/**
 * Determine whether a call expression is known to be pure (no side effects).
 *
 * Returns `true` only when there is positive evidence of purity.
 * Returns `false` (conservative) for any unknown call.
 */
export function isKnownPureCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  try {
    return (
      isKnownBuiltinOrLibraryPureCall(expr, checker) ||
      isPureUserCall(expr, checker)
    );
  } catch {
    // TypeChecker can throw on malformed/incomplete ASTs.
    // Conservative fallback: treat as effectful.
    return false;
  }
}

function isKnownBuiltinOrLibraryPureCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  // --- Tier 1a: builtin allowlist ---

  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const receiver = expr.expression.expression;

    // Pure Math methods: Math.max(...), Math.abs(...), etc.
    // Math.random() is excluded (non-deterministic).
    if (
      ts.isIdentifier(receiver) &&
      receiver.text === "Math" &&
      PURE_MATH_METHODS.has(methodName)
    ) {
      return true;
    }

    // Method on a typed receiver (getTypeAtLocation may throw)
    const receiverType = checker.getTypeAtLocation(receiver);

    // String methods
    if (receiverType.flags & ts.TypeFlags.StringLike) {
      const pureMethods = PURE_METHODS_BY_TYPE.get("string");
      if (pureMethods?.has(methodName)) {
        return true;
      }
    }

    // Number methods
    if (receiverType.flags & ts.TypeFlags.NumberLike) {
      const pureMethods = PURE_METHODS_BY_TYPE.get("number");
      if (pureMethods?.has(methodName)) {
        return true;
      }
    }

    // Map methods — .get(k) and .has(k) are pure lookups. Pantagruel's
    // read encoding (guarded rule application) has no effects, so these
    // are safe inside if-conditions and rhs expressions of mutating bodies.
    // The receiver expression itself is evaluated eagerly, so it must also
    // be pure (e.g., `effectfulFactory().get(k)` is not pure).
    if (receiverType.getSymbol()?.getName() === "Map") {
      if (PURE_MAP_METHODS.has(methodName)) {
        return (
          expressionIsPure(receiver, checker) &&
          expr.arguments.every((arg) => expressionIsPure(arg, checker))
        );
      }
    }

    // Set / ReadonlySet — .has(x) is a pure membership test. Same
    // eager-evaluation rule as Map.has: the receiver and args must also
    // be pure.
    if (isSetType(receiverType) && methodName === "has") {
      return (
        expressionIsPure(receiver, checker) &&
        expr.arguments.every((arg) => expressionIsPure(arg, checker))
      );
    }

    // Array methods
    if (checker.isArrayType(receiverType)) {
      if (PURE_ARRAY_METHODS.has(methodName)) {
        return true;
      }

      // Higher-order array methods: pure if callback AND all eagerly-evaluated
      // args (thisArg, initialValue) are side-effect-free.
      if (HO_PURE_ARRAY_METHODS.has(methodName) && expr.arguments.length >= 1) {
        const [callback, ...restArgs] = expr.arguments;
        return (
          callback !== undefined &&
          isArrowPure(callback, checker) &&
          restArgs.every((arg) => expressionIsPure(arg, checker))
        );
      }
    }
  }

  // --- Tier 1b: Effect-TS symbol resolution ---
  // Trace the callee symbol to its declaration file. Only exports
  // explicitly listed in EFFECT_PURE_EXPORTS are accepted as pure.
  // Unknown effect exports default to impure (conservative).
  const effectExport = resolveEffectLibraryExport(expr.expression, checker);
  if (effectExport !== null) {
    const moduleAllowlist = EFFECT_PURE_EXPORTS.get(effectExport.module);
    if (moduleAllowlist?.has(effectExport.name)) {
      // Known-pure effect library export. Arguments are still eagerly
      // evaluated, so check their purity.
      return expr.arguments.every((arg) => expressionIsPure(arg, checker));
    }
    // Effect library export not in allowlist → conservative (impure)
    return false;
  }

  // Tier 1b fallback: bare-name combinator matching for environments where
  // the `effect` package is not installed (e.g. tests with `declare function`).
  // Arguments must be checked: identity(sideEffect()) is impure.
  if (ts.isIdentifier(expr.expression)) {
    if (EFFECT_PURE_COMBINATORS.has(expr.expression.text)) {
      return expr.arguments.every((arg) => expressionIsPure(arg, checker));
    }
  }

  // --- Tier 1c: conservative default ---
  return false;
}

/**
 * Conservatively classify a direct user-function call as pure.
 *
 * This feeds the canonical checker-aware oracle. Calls stay effectful unless
 * the callee resolves to a user function whose body is conservatively pure.
 */
export function isPureUserCall(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  try {
    return isPureUserCallWithVisited(call, checker, new Set());
  } catch {
    return false;
  }
}

function isPureUserCallWithVisited(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
  visited: Set<ts.Node>,
): boolean {
  if (call.arguments.some(ts.isSpreadElement)) {
    return false;
  }
  if (
    !call.arguments.every((arg) => userExpressionIsPure(arg, checker, visited))
  ) {
    return false;
  }
  const decl = resolveUserCallTarget(call, checker);
  return decl !== null && isPureUserFunction(decl, checker, visited);
}

/**
 * Conservative whole-body, interprocedural purity classifier for user code.
 *
 * Unknown syntax, dynamic dispatch, unresolved callees, node_modules targets,
 * recursion, mutation, throw, await, IO, and higher-order calls all bail to
 * effectful. Returning true is therefore positive evidence only.
 */
export function isPureUserFunction(
  decl: ts.FunctionLikeDeclaration,
  checker: ts.TypeChecker,
  visited: Set<ts.Node> = new Set(),
): boolean {
  try {
    if (declarationIsFromNodeModules(decl)) {
      return false;
    }
    if (
      !parameterInitializersArePureForUserAnalysis(decl.parameters, checker)
    ) {
      return false;
    }

    const body = getBlockBody(decl);
    if (!body) {
      return false;
    }
    if (visited.has(body)) {
      return false;
    }

    visited.add(body);
    const result = blockIsPureForUserAnalysis(body, checker, visited);
    visited.delete(body);
    return result;
  } catch {
    return false;
  }
}

/**
 * Canonical checker-aware effect oracle for TypeScript expressions.
 *
 * Returns true only when evaluating `expr` is known not to produce observable
 * side effects. Unknown calls and unknown syntax stay effectful by default.
 */
export function isEffectFree(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  options: EffectOracleOptions = {},
): boolean {
  return !expressionHasSideEffects(expr, checker, options);
}

/**
 * Compatibility spelling for existing body-lowering callers. New purity
 * decisions should prefer `isEffectFree` so the positive predicate is shared
 * across signature and body stages.
 */
export function expressionHasSideEffects(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  options: EffectOracleOptions = {},
): boolean {
  expr = unwrapEffectExpression(expr);

  if (ts.isDeleteExpression(expr)) {
    return true;
  }

  if (ts.isBinaryExpression(expr)) {
    return (
      isAssignmentOperator(expr.operatorToken.kind) ||
      expressionHasSideEffects(expr.left, checker, options) ||
      expressionHasSideEffects(expr.right, checker, options)
    );
  }

  if (ts.isCallExpression(expr)) {
    if (
      options.admitForeignBoolPredicates === true &&
      canAdmitForeignBoolPredicate(expr, checker)
    ) {
      return (
        expressionHasSideEffects(expr.expression, checker, options) ||
        expr.arguments.some((arg) =>
          expressionHasSideEffects(arg, checker, options),
        )
      );
    }
    if (isKnownPureCall(expr, checker)) {
      // Known-pure callees still require pure receiver/callee and arguments:
      // e.g. `makeString().trim()` matches the String method allowlist, but
      // the receiver call itself has unknown effects.
      return (
        expressionHasSideEffects(expr.expression, checker, options) ||
        expr.arguments.some((arg) =>
          expressionHasSideEffects(arg, checker, options),
        )
      );
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
      expressionHasSideEffects(expr.operand, checker, options)
    );
  }

  return (
    ts.forEachChild(expr, (child) =>
      ts.isExpression(child)
        ? expressionHasSideEffects(child, checker, options)
        : false,
    ) ?? false
  );
}

function canAdmitForeignBoolPredicate(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): boolean {
  try {
    return isBoolReturningDeclarationFileCall(expr, checker);
  } catch {
    return false;
  }
}

function unwrapEffectExpression(expr: ts.Expression): ts.Expression {
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

function isAssignmentOperator(kind: ts.SyntaxKind): boolean {
  return ASSIGNMENT_OPERATORS.has(kind);
}

const ASSIGNMENT_OPERATORS: ReadonlySet<ts.SyntaxKind> = new Set([
  ts.SyntaxKind.EqualsToken,
  ts.SyntaxKind.PlusEqualsToken,
  ts.SyntaxKind.MinusEqualsToken,
  ts.SyntaxKind.AsteriskEqualsToken,
  ts.SyntaxKind.SlashEqualsToken,
  ts.SyntaxKind.PercentEqualsToken,
  ts.SyntaxKind.AsteriskAsteriskEqualsToken,
  ts.SyntaxKind.LessThanLessThanEqualsToken,
  ts.SyntaxKind.GreaterThanGreaterThanEqualsToken,
  ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken,
  ts.SyntaxKind.AmpersandEqualsToken,
  ts.SyntaxKind.BarEqualsToken,
  ts.SyntaxKind.CaretEqualsToken,
  ts.SyntaxKind.BarBarEqualsToken,
  ts.SyntaxKind.AmpersandAmpersandEqualsToken,
  ts.SyntaxKind.QuestionQuestionEqualsToken,
]);

type AnalyzableFunctionLike =
  | ts.FunctionDeclaration
  | ts.MethodDeclaration
  | ts.ConstructorDeclaration
  | ts.FunctionExpression
  | ts.ArrowFunction;

function resolveUserCallTarget(
  call: ts.CallExpression,
  checker: ts.TypeChecker,
): AnalyzableFunctionLike | null {
  // Match guard-following discipline: direct calls only. Property access is
  // dynamic dispatch and may need `this`, so it bails.
  if (!ts.isIdentifier(call.expression)) {
    return null;
  }

  const calleeSymbol = checker.getSymbolAtLocation(call.expression);
  const calleeDecl = calleeSymbol?.valueDeclaration;
  if (calleeDecl && ts.isParameter(calleeDecl)) {
    return null;
  }

  let decl = asAnalyzableFunctionLike(
    checker.getResolvedSignature(call)?.declaration,
  );
  if (!decl || !getBlockBody(decl)) {
    const resolved = resolveFunctionLikeFromSymbol(calleeSymbol, checker);
    if (resolved) {
      decl = resolved;
    }
  }

  if (!decl) {
    return null;
  }
  if (declarationIsFromNodeModules(decl)) {
    return null;
  }
  if (!getBlockBody(decl)) {
    return null;
  }

  return decl;
}

function resolveFunctionLikeFromSymbol(
  symbol: ts.Symbol | undefined,
  checker: ts.TypeChecker,
): AnalyzableFunctionLike | null {
  if (!symbol) {
    return null;
  }

  let resolved = symbol;
  if (resolved.flags & ts.SymbolFlags.Alias) {
    resolved = checker.getAliasedSymbol(resolved);
  }

  const declarations = resolved.getDeclarations() ?? [];
  for (const decl of declarations) {
    const functionDecl = asAnalyzableFunctionLike(decl);
    if (functionDecl && getBlockBody(functionDecl)) {
      return functionDecl;
    }
    if (ts.isVariableDeclaration(decl) && decl.initializer) {
      const init = decl.initializer;
      if (
        (ts.isFunctionExpression(init) || ts.isArrowFunction(init)) &&
        getBlockBody(init)
      ) {
        return init;
      }
    }
  }

  return null;
}

function asAnalyzableFunctionLike(
  node: ts.Node | undefined,
): AnalyzableFunctionLike | null {
  if (!node) {
    return null;
  }
  if (
    ts.isFunctionDeclaration(node) ||
    ts.isMethodDeclaration(node) ||
    ts.isConstructorDeclaration(node) ||
    ts.isFunctionExpression(node) ||
    ts.isArrowFunction(node)
  ) {
    return node;
  }
  return null;
}

function getBlockBody(decl: ts.FunctionLikeDeclaration): ts.Block | undefined {
  if (
    ts.isFunctionDeclaration(decl) ||
    ts.isMethodDeclaration(decl) ||
    ts.isFunctionExpression(decl) ||
    ts.isConstructorDeclaration(decl)
  ) {
    return decl.body;
  }
  if (ts.isArrowFunction(decl)) {
    return ts.isBlock(decl.body) ? decl.body : undefined;
  }
  return undefined;
}

function declarationIsFromNodeModules(decl: ts.Node): boolean {
  return /(?:^|[/\\])node_modules(?:[/\\]|$)/u.test(
    decl.getSourceFile().fileName,
  );
}

function blockIsPureForUserAnalysis(
  block: ts.Block,
  checker: ts.TypeChecker,
  visited: Set<ts.Node>,
): boolean {
  return block.statements.every((stmt) =>
    statementIsPureForUserAnalysis(stmt, checker, visited),
  );
}

function statementIsPureForUserAnalysis(
  stmt: ts.Statement,
  checker: ts.TypeChecker,
  visited: Set<ts.Node>,
): boolean {
  if (ts.isBlock(stmt)) {
    return blockIsPureForUserAnalysis(stmt, checker, visited);
  }
  if (ts.isEmptyStatement(stmt) || ts.isFunctionDeclaration(stmt)) {
    return true;
  }
  if (ts.isReturnStatement(stmt)) {
    return (
      !stmt.expression ||
      userExpressionIsPure(stmt.expression, checker, visited)
    );
  }
  if (ts.isExpressionStatement(stmt)) {
    return userExpressionIsPure(stmt.expression, checker, visited);
  }
  if (ts.isVariableStatement(stmt)) {
    return stmt.declarationList.declarations.every((decl) => {
      if (!bindingNameIsSimpleLocal(decl.name)) {
        return false;
      }
      return (
        decl.initializer === undefined ||
        userExpressionIsPure(decl.initializer, checker, visited)
      );
    });
  }
  if (ts.isIfStatement(stmt)) {
    return (
      userExpressionIsPure(stmt.expression, checker, visited) &&
      statementIsPureForUserAnalysis(stmt.thenStatement, checker, visited) &&
      (stmt.elseStatement === undefined ||
        statementIsPureForUserAnalysis(stmt.elseStatement, checker, visited))
    );
  }

  // Loops, switch, try/catch/finally, throw, debugger, imports/exports, and
  // unknown statements are effectful until proven otherwise by a later patch.
  return false;
}

function bindingNameIsSimpleLocal(name: ts.BindingName): boolean {
  if (ts.isIdentifier(name)) {
    return true;
  }
  return name.elements.every((element) => {
    if (ts.isOmittedExpression(element)) {
      return true;
    }
    return bindingNameIsSimpleLocal(element.name);
  });
}

function parameterInitializersArePureForUserAnalysis(
  params: ts.NodeArray<ts.ParameterDeclaration>,
  checker: ts.TypeChecker,
): boolean {
  for (const param of params) {
    if (param.dotDotDotToken) {
      return false;
    }
    if (
      param.initializer &&
      !userExpressionIsPure(param.initializer, checker)
    ) {
      return false;
    }
    if (
      (ts.isObjectBindingPattern(param.name) ||
        ts.isArrayBindingPattern(param.name)) &&
      !bindingPatternInitializersArePureForUserAnalysis(param.name, checker)
    ) {
      return false;
    }
  }
  return true;
}

function bindingPatternInitializersArePureForUserAnalysis(
  pattern: ts.BindingPattern,
  checker: ts.TypeChecker,
): boolean {
  for (const element of pattern.elements) {
    if (ts.isOmittedExpression(element)) {
      continue;
    }
    if (
      element.initializer &&
      !userExpressionIsPure(element.initializer, checker)
    ) {
      return false;
    }
    if (
      (ts.isObjectBindingPattern(element.name) ||
        ts.isArrayBindingPattern(element.name)) &&
      !bindingPatternInitializersArePureForUserAnalysis(element.name, checker)
    ) {
      return false;
    }
  }
  return true;
}

function userExpressionIsPure(
  expr: ts.Expression,
  checker: ts.TypeChecker,
  visited: Set<ts.Node> = new Set(),
): boolean {
  expr = unwrapEffectExpression(expr);

  if (
    ts.isIdentifier(expr) ||
    ts.isNumericLiteral(expr) ||
    ts.isStringLiteral(expr) ||
    ts.isNoSubstitutionTemplateLiteral(expr) ||
    expr.kind === ts.SyntaxKind.TrueKeyword ||
    expr.kind === ts.SyntaxKind.FalseKeyword ||
    expr.kind === ts.SyntaxKind.NullKeyword ||
    expr.kind === ts.SyntaxKind.UndefinedKeyword ||
    expr.kind === ts.SyntaxKind.ThisKeyword
  ) {
    return true;
  }

  if (ts.isPropertyAccessExpression(expr)) {
    return userExpressionIsPure(expr.expression, checker, visited);
  }
  if (ts.isElementAccessExpression(expr)) {
    return (
      expr.argumentExpression !== undefined &&
      userExpressionIsPure(expr.expression, checker, visited) &&
      userExpressionIsPure(expr.argumentExpression, checker, visited)
    );
  }
  if (ts.isArrayLiteralExpression(expr)) {
    return expr.elements.every((element) => {
      if (ts.isSpreadElement(element)) {
        return false;
      }
      return userExpressionIsPure(element, checker, visited);
    });
  }
  if (ts.isObjectLiteralExpression(expr)) {
    return expr.properties.every((prop) => {
      if (ts.isPropertyAssignment(prop)) {
        return (
          (!ts.isComputedPropertyName(prop.name) ||
            userExpressionIsPure(prop.name.expression, checker, visited)) &&
          userExpressionIsPure(prop.initializer, checker, visited)
        );
      }
      if (ts.isShorthandPropertyAssignment(prop)) {
        return true;
      }
      if (ts.isSpreadAssignment(prop)) {
        return false;
      }
      return false;
    });
  }
  if (ts.isTemplateExpression(expr)) {
    return expr.templateSpans.every((span) =>
      userExpressionIsPure(span.expression, checker, visited),
    );
  }
  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    if (
      expr.operator === ts.SyntaxKind.PlusPlusToken ||
      expr.operator === ts.SyntaxKind.MinusMinusToken
    ) {
      return false;
    }
    return userExpressionIsPure(expr.operand, checker, visited);
  }
  if (ts.isBinaryExpression(expr)) {
    return (
      !isAssignmentOperator(expr.operatorToken.kind) &&
      userExpressionIsPure(expr.left, checker, visited) &&
      userExpressionIsPure(expr.right, checker, visited)
    );
  }
  if (ts.isConditionalExpression(expr)) {
    return (
      userExpressionIsPure(expr.condition, checker, visited) &&
      userExpressionIsPure(expr.whenTrue, checker, visited) &&
      userExpressionIsPure(expr.whenFalse, checker, visited)
    );
  }
  if (ts.isCallExpression(expr)) {
    if (isKnownBuiltinOrLibraryPureCall(expr, checker)) {
      return (
        userExpressionIsPure(expr.expression, checker, visited) &&
        expr.arguments.every((arg) =>
          userExpressionIsPure(arg, checker, visited),
        )
      );
    }
    return isPureUserCallWithVisited(expr, checker, visited);
  }

  return false;
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/**
 * Resolve a callee expression to an Effect-TS library export.
 *
 * Uses TypeChecker symbol resolution (getSymbolAtLocation + getAliasedSymbol)
 * to trace the callee through import aliases back to its original declaration.
 * Returns { module, name } if the declaration originates from the `effect`
 * package, or null if it doesn't (user code, other packages, unresolvable).
 *
 * This is the sound alternative to return-type detection. A user function
 * that returns Effect<A,E,R> will resolve to the user's source file, not
 * to the effect package — so it correctly falls through to the conservative
 * default.
 *
 * Handles: Effect.succeed(x), E.map(fn) (aliased import), pipe(x, ...) (bare).
 */
export function resolveEffectLibraryExport(
  callee: ts.Expression,
  checker: ts.TypeChecker,
): { module: string; name: string } | null {
  try {
    let symbol: ts.Symbol | undefined;

    if (ts.isPropertyAccessExpression(callee)) {
      // Effect.succeed, Effect.map, etc.
      symbol = checker.getSymbolAtLocation(callee.name);
    } else if (ts.isIdentifier(callee)) {
      // pipe, flow, identity (bare imports)
      symbol = checker.getSymbolAtLocation(callee);
    }

    if (!symbol) {
      return null;
    }

    // Follow import aliases to the original declaration
    let resolved = symbol;
    while (resolved.flags & ts.SymbolFlags.Alias) {
      resolved = checker.getAliasedSymbol(resolved);
    }

    const decls = resolved.getDeclarations();
    if (!decls || decls.length === 0) {
      return null;
    }

    const fileName = decls[0]!.getSourceFile().fileName;

    // Match effect package declaration files.
    // Patterns: node_modules/effect/dist/dts/Effect.d.ts
    //           node_modules/effect/src/Effect.ts
    //           node_modules/.pnpm/effect@.../node_modules/effect/dist/dts/Effect.d.ts
    const match = fileName.match(
      /node_modules\/effect\/(?:dist\/dts|src)\/(\w+)\.(?:d\.ts|ts)$/u,
    );
    if (!match) {
      return null;
    }

    return { module: match[1]!, name: resolved.getName() };
  } catch {
    // TypeChecker can throw on malformed AST — conservative fallback
    return null;
  }
}

/**
 * Check whether an arrow function callback is side-effect-free.
 *
 * For expression-bodied arrows: check the expression.
 * For block-bodied arrows with a single return: check the return expression.
 * All other shapes are conservatively treated as impure.
 */
function isArrowPure(
  callback: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  if (!ts.isArrowFunction(callback)) {
    return false;
  }

  // Check parameter initializers: default values are eagerly evaluated
  // when the argument is undefined, e.g. (x = sideEffect()) => x.
  if (!parameterInitializersArePure(callback.parameters, checker)) {
    return false;
  }

  if (ts.isBlock(callback.body)) {
    // Block body: only pure if it's a single return statement
    const stmts = callback.body.statements;
    if (
      stmts.length === 1 &&
      ts.isReturnStatement(stmts[0]!) &&
      stmts[0]!.expression
    ) {
      return expressionIsPure(stmts[0]!.expression, checker);
    }
    return false;
  }

  // Expression body
  return expressionIsPure(callback.body, checker);
}

/**
 * Check that all parameter default initializers are side-effect-free.
 * Handles both simple defaults ((x = expr) => ...) and destructuring
 * defaults (({a = expr}) => ..., ([a = expr]) => ...).
 */
function parameterInitializersArePure(
  params: ts.NodeArray<ts.ParameterDeclaration>,
  checker: ts.TypeChecker,
): boolean {
  for (const param of params) {
    // Simple default: (x = expr)
    if (param.initializer && !expressionIsPure(param.initializer, checker)) {
      return false;
    }
    // Destructuring: check binding element initializers recursively
    if (
      ts.isObjectBindingPattern(param.name) ||
      ts.isArrayBindingPattern(param.name)
    ) {
      if (!bindingPatternInitializersArePure(param.name, checker)) {
        return false;
      }
    }
  }
  return true;
}

/**
 * Recursively check initializers in destructuring binding patterns.
 */
function bindingPatternInitializersArePure(
  pattern: ts.BindingPattern,
  checker: ts.TypeChecker,
): boolean {
  for (const element of pattern.elements) {
    if (ts.isOmittedExpression(element)) {
      continue;
    }
    if (
      element.initializer &&
      !expressionIsPure(element.initializer, checker)
    ) {
      return false;
    }
    // Nested destructuring: ({a: {b = expr}}) => ...
    if (
      ts.isObjectBindingPattern(element.name) ||
      ts.isArrayBindingPattern(element.name)
    ) {
      if (!bindingPatternInitializersArePure(element.name, checker)) {
        return false;
      }
    }
  }
  return true;
}

/**
 * Compositional purity analysis over the expression AST.
 *
 * Standard name: Structural induction over expression forms, classifying
 * each into the {pure, effectful} lattice. This is the standard recursive
 * descent approach described in Nielson, Nielson & Hankin, "Principles of
 * Program Analysis" (Springer 1999), Ch. 2 — instantiated for a trivial
 * two-point effect domain rather than a full type-and-effect system.
 *
 * Soundness invariant: returns true only when the expression is provably
 * side-effect-free. Returns false (conservative) for any unknown form.
 *
 * Pragmatic assumption: property access is assumed pure (no effectful
 * getters). This matches the universal assumption in JavaScript bundler
 * tree-shaking (Webpack, Rollup, Closure Compiler). Getter-bearing types
 * in specification-relevant code are out of scope for ts2pant.
 */
function expressionIsPure(
  expr: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  // Unwrap parentheses, type assertions, non-null assertions
  while (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isSatisfiesExpression(expr) ||
    ts.isNonNullExpression(expr)
  ) {
    expr = expr.expression;
  }

  // Literals, identifiers, property access are pure
  if (
    ts.isIdentifier(expr) ||
    ts.isNumericLiteral(expr) ||
    ts.isStringLiteral(expr) ||
    ts.isNoSubstitutionTemplateLiteral(expr) ||
    expr.kind === ts.SyntaxKind.TrueKeyword ||
    expr.kind === ts.SyntaxKind.FalseKeyword ||
    expr.kind === ts.SyntaxKind.NullKeyword ||
    expr.kind === ts.SyntaxKind.UndefinedKeyword
  ) {
    return true;
  }

  // Property access: recurse into receiver. Assumes no effectful getters
  // (see module-level pragmatic assumptions documentation).
  if (ts.isPropertyAccessExpression(expr)) {
    return expressionIsPure(expr.expression, checker);
  }

  if (ts.isElementAccessExpression(expr)) {
    return (
      expressionIsPure(expr.expression, checker) &&
      expr.argumentExpression !== undefined &&
      expressionIsPure(expr.argumentExpression, checker)
    );
  }

  // Arrow functions and function expressions are pure value-creating
  // expressions — the function body is not executed at evaluation time.
  if (ts.isArrowFunction(expr) || ts.isFunctionExpression(expr)) {
    return true;
  }

  if (ts.isBinaryExpression(expr)) {
    // Assignment operators are side-effectful
    if (
      expr.operatorToken.kind >= ts.SyntaxKind.EqualsToken &&
      expr.operatorToken.kind <= ts.SyntaxKind.CaretEqualsToken
    ) {
      return false;
    }
    return (
      expressionIsPure(expr.left, checker) &&
      expressionIsPure(expr.right, checker)
    );
  }

  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    if (
      expr.operator === ts.SyntaxKind.PlusPlusToken ||
      expr.operator === ts.SyntaxKind.MinusMinusToken
    ) {
      return false;
    }
    return expressionIsPure(expr.operand, checker);
  }

  if (ts.isConditionalExpression(expr)) {
    return (
      expressionIsPure(expr.condition, checker) &&
      expressionIsPure(expr.whenTrue, checker) &&
      expressionIsPure(expr.whenFalse, checker)
    );
  }

  if (ts.isCallExpression(expr)) {
    return isKnownPureCall(expr, checker);
  }

  if (ts.isArrayLiteralExpression(expr)) {
    return expr.elements.every((el) => {
      // Spread elements: [...arr] is pure only for built-in arrays/tuples.
      // Custom iterables invoke Symbol.iterator/next() which can execute
      // arbitrary code. Per Talpin & Jouvelot 1994, allocation effects
      // are maskable — the fresh array is local.
      if (ts.isSpreadElement(el)) {
        const spreadType = checker.getTypeAtLocation(el.expression);
        if (
          !checker.isArrayType(spreadType) &&
          !checker.isTupleType(spreadType)
        ) {
          return false;
        }
        return expressionIsPure(el.expression, checker);
      }
      return expressionIsPure(el, checker);
    });
  }

  // Object literals: { a: expr } is pure if all property values are pure.
  // Allocation is a maskable effect (Talpin & Jouvelot, I&C 1994) — the
  // fresh object is local and does not escape to observable state.
  if (ts.isObjectLiteralExpression(expr)) {
    return expr.properties.every((prop) => {
      if (ts.isPropertyAssignment(prop)) {
        // Computed property names: { [expr]: value } evaluates expr eagerly
        if (
          ts.isComputedPropertyName(prop.name) &&
          !expressionIsPure(prop.name.expression, checker)
        ) {
          return false;
        }
        return expressionIsPure(prop.initializer, checker);
      }
      if (ts.isShorthandPropertyAssignment(prop)) {
        return true; // { x } just reads a variable
      }
      if (ts.isSpreadAssignment(prop)) {
        return expressionIsPure(prop.expression, checker);
      }
      // Method declarations, accessors → conservative
      return false;
    });
  }

  if (ts.isTemplateExpression(expr)) {
    // Assumes implicit toString() on interpolated values is pure
    // (see module-level pragmatic assumptions documentation).
    return expr.templateSpans.every((span) =>
      expressionIsPure(span.expression, checker),
    );
  }

  // Side-effectful by default
  if (
    ts.isDeleteExpression(expr) ||
    ts.isNewExpression(expr) ||
    ts.isAwaitExpression(expr)
  ) {
    return false;
  }

  // Unknown expression kind → conservative
  return false;
}

// ---------------------------------------------------------------------------
// Bool-type detection (M1 imperative-IR workstream — &&/|| normalization)
// ---------------------------------------------------------------------------

/**
 * True when every constituent of `expr`'s apparent type satisfies the
 * `BooleanLike` flag (`boolean`, `true`, `false`, the synthesized union
 * `true | false`). Returns false for `boolean | undefined`, `any`,
 * `unknown`, and any union that includes a non-Bool constituent.
 *
 * Used by the L1 conditional builder to gate `&&`/`||` normalization
 * (workstream M1 / `workstreams/ts2pant-imperative-ir.md`): TS short-
 * circuit on non-Bool operands has truthy/falsy semantics that diverge
 * from `Cond`'s Boolean-guard semantics, so non-Bool short-circuit is
 * rejected (conservative-refusal policy 3(b)).
 *
 * The check uses the apparent type at the location, walks union
 * constituents, and requires every constituent to be Bool. Optional
 * Bools (`boolean | undefined`) reject — the user must disambiguate via
 * `??` first. Bool literal types (`true`, `false`) and unions of them
 * accept.
 */
export function isStaticallyBoolTyped(
  expr: ts.Expression,
  checker: ts.TypeChecker,
): boolean {
  const t = checker.getTypeAtLocation(expr);
  return isAllBool(t);
}

function isAllBool(t: ts.Type): boolean {
  if (t.isUnion()) {
    return t.types.every(isAllBool);
  }
  if (t.isIntersection()) {
    // Intersection narrowed to bool is still bool; require every
    // constituent to be Bool. In practice, intersections like
    // `boolean & {}` are simplified by TypeScript's checker to
    // `boolean` before we see them; the constituent check is a
    // conservative guard for complex intersection shapes.
    return t.types.every(isAllBool);
  }
  return (t.flags & ts.TypeFlags.BooleanLike) !== 0;
}
