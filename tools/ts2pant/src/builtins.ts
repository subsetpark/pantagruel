/**
 * TS-stdlib dispatch index.
 *
 * Maps surface forms of JavaScript built-in calls (`Math.max`,
 * `s.toUpperCase()`, …) to the qualified Pantagruel rule references
 * exposed by the hand-written modules under `samples/js-stdlib/`.
 * This file carries only data + a thin loader; the .pant modules
 * themselves are the source of truth for the rule signatures and any
 * EUF axioms — they sit on the same CI typecheck path as the rest of
 * the samples corpus, which catches regressions in the curated axiom
 * sets without bespoke test infrastructure.
 *
 * Reference: Kroening & Strichman, *Decision Procedures* Ch. 4 (EUF
 * semantics — congruence is the only built-in axiom for opaque calls;
 * additional axioms in the .pant modules must be sound under JS
 * semantics).
 */

import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import ts from "typescript";

/** Module name of a hand-written js-stdlib dependency module. */
export type DepModuleName =
  | "JS_MATH"
  | "JS_STRING"
  | "TS_PRELUDE"
  | "JS_ARRAY"
  | "JS_MAP";

/**
 * Dispatch entry: `rule` is the qualified Pantagruel rule reference a
 * consumer module would use after `import JS_MATH.` (e.g.,
 * `JS_MATH::max-of`); `mod` is the dep module the rule lives in;
 * `arity` is the JS-side argument count the rule's signature is sound
 * for. The lookup gates on argument count because the Pantagruel
 * rules are fixed-arity (`max-of` is binary; `Math.max(a, b, c)` would
 * be a signature mismatch under EUF lowering — Kroening & Strichman
 * Ch. 4 § Congruence). Calls outside the supported arity fall through
 * to ts2pant's ordinary EUF path rather than mis-dispatching.
 */
export interface BuiltinSpec {
  rule: string;
  mod: DepModuleName;
  arity: number;
  receiver: "arg" | "none";
}

/**
 * Internal key for the dispatch table. The keys are TS surface forms
 * keyed by namespace (Math.* / Array.* use "<Namespace>.<name>";
 * prototype methods use "<Type>.prototype.<name>") so the lookup logic
 * can route by which symbol-kind it just resolved.
 */
export type BuiltinKey =
  | `Array.${string}`
  | `Map.prototype.${string}`
  | `Math.${string}`
  | `String.prototype.${string}`;

const RESERVED_RULE_NAMES: ReadonlySet<string> = new Set([
  "max",
  "min",
  "cond",
  "over",
]);

const BUILTINS: Map<BuiltinKey, { arity: number }> = new Map([
  ["Array.from", { arity: 1 }],
  ["Map.prototype.values", { arity: 0 }],
  ["Map.prototype.entries", { arity: 0 }],
  ["Map.prototype.keys", { arity: 0 }],
  ["Math.max", { arity: 2 }],
  ["Math.min", { arity: 2 }],
  ["Math.abs", { arity: 1 }],
  ["String.prototype.toUpperCase", { arity: 0 }],
  ["String.prototype.replace", { arity: 2 }],
  ["String.prototype.indexOf", { arity: 1 }],
  ["String.prototype.endsWith", { arity: 1 }],
  ["String.prototype.includes", { arity: 1 }],
  ["String.prototype.lastIndexOf", { arity: 1 }],
  ["String.prototype.startsWith", { arity: 1 }],
  ["String.prototype.toLowerCase", { arity: 0 }],
  ["String.prototype.trim", { arity: 0 }],
]);

export function deriveBuiltinSpec(key: BuiltinKey, arity: number): BuiltinSpec {
  const namespace = getBuiltinNamespace(key);
  const method = key.slice(`${namespace}.`.length);
  const mod = moduleForNamespace(namespace);
  const receiver = namespace.includes(".prototype") ? "arg" : "none";
  const kebab = method
    .replace(/([A-Z])/gu, "-$1")
    .toLowerCase()
    .replace(/^-/u, "");
  const ruleName = RESERVED_RULE_NAMES.has(kebab) ? `${kebab}-of` : kebab;
  return { rule: `${mod}::${ruleName}`, mod, arity, receiver };
}

type BuiltinNamespace = "Array" | "Map.prototype" | "Math" | "String.prototype";

function getBuiltinNamespace(key: BuiltinKey): BuiltinNamespace {
  if (key.startsWith("Array.")) {
    return "Array";
  }
  if (key.startsWith("Map.prototype.")) {
    return "Map.prototype";
  }
  if (key.startsWith("Math.")) {
    return "Math";
  }
  return "String.prototype";
}

function moduleForNamespace(namespace: BuiltinNamespace): DepModuleName {
  switch (namespace) {
    case "Array":
      return "JS_ARRAY";
    case "Map.prototype":
      return "JS_MAP";
    case "Math":
      return "JS_MATH";
    case "String.prototype":
      return "JS_STRING";
    default: {
      const _exhaustive: never = namespace;
      throw new Error(`Unhandled builtin namespace: ${_exhaustive}`);
    }
  }
}

/**
 * Resolve a `ts.CallExpression` to a {@link BuiltinSpec} when its
 * callee is a recognized JavaScript built-in, else `null`.
 *
 * Matching is symbol-based, not textual: a user-shadowed
 * `const Math = ...; Math.max(a, b)` resolves to a symbol declared in
 * the user's source file rather than a TS default-library file, so
 * the lookup fails and the call falls through to ts2pant's regular
 * EUF lowering. This is the same robustness posture the nullish
 * recognizer takes (see `nullish-recognizer.ts` and AGENTS.md M4
 * "structural-not-textual" rule).
 *
 * Lib detection uses `Program.isSourceFileDefaultLibrary`, the public
 * TS Compiler API for distinguishing `node_modules/typescript/lib/lib.*.d.ts`
 * from arbitrary user-supplied `.d.ts` (ambient `declare const Math`
 * shims, merged `interface String { … }` augmentations, etc.).
 * `isDeclarationFile` alone would match the latter and silently re-bind
 * a user augmentation to the dispatch table.
 *
 * For namespace calls (`Math.<name>(...)`, `Array.<name>(...)`) we
 * resolve the symbol of the *receiver* identifier and accept it iff
 * its declarations sit in a TS default-library file. For prototype
 * calls (`s.toUpperCase()`, `m.values()`) we resolve the symbol of
 * the *method* and accept it iff its parent declaration is the
 * corresponding lib interface.
 *
 * Once the namespace+name match, the call's argument count must equal
 * the dispatch entry's `arity` — see {@link BuiltinSpec}. Off-arity
 * calls (`Math.max(a, b, c)`, `Math.abs()`) fall through to null so
 * they reach the ordinary EUF lowering rather than mis-dispatching to
 * a fixed-arity Pant rule.
 */
export function lookupBuiltinByCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  program: ts.Program,
): BuiltinSpec | null {
  if (!ts.isPropertyAccessExpression(expr.expression)) {
    return null;
  }
  const propAccess = expr.expression;
  const memberName = propAccess.name.text;

  let key: BuiltinKey | undefined;
  if (
    ts.isIdentifier(propAccess.expression) &&
    propAccess.expression.text === "Array" &&
    isDefaultLibSymbol(
      checker.getSymbolAtLocation(propAccess.expression),
      program,
    )
  ) {
    key = `Array.${memberName}`;
  } else if (
    ts.isIdentifier(propAccess.expression) &&
    propAccess.expression.text === "Math" &&
    isDefaultLibSymbol(
      checker.getSymbolAtLocation(propAccess.expression),
      program,
    )
  ) {
    key = `Math.${memberName}`;
  } else {
    const methodSymbol = checker.getSymbolAtLocation(propAccess.name);
    if (methodSymbol && isStringPrototypeMember(methodSymbol, program)) {
      key = `String.prototype.${memberName}`;
    } else if (methodSymbol && isMapPrototypeMember(methodSymbol, program)) {
      key = `Map.prototype.${memberName}`;
    }
  }

  if (!key) {
    return null;
  }
  const entry = BUILTINS.get(key);
  if (!entry) {
    return null;
  }
  const fullSpec = deriveBuiltinSpec(key, entry.arity);
  if (
    key === "String.prototype.replace" &&
    !expr.arguments.every((arg) =>
      isStringLikeType(checker.getTypeAtLocation(arg)),
    )
  ) {
    return null;
  }
  return expr.arguments.length === fullSpec.arity ? fullSpec : null;
}

function isDefaultLibSymbol(
  symbol: ts.Symbol | undefined,
  program: ts.Program,
): boolean {
  const decls = symbol?.declarations;
  if (!decls || decls.length === 0) {
    return false;
  }
  return decls.some((d) =>
    program.isSourceFileDefaultLibrary(d.getSourceFile()),
  );
}

function isStringPrototypeMember(
  symbol: ts.Symbol,
  program: ts.Program,
): boolean {
  return isDefaultLibInterfaceMember(symbol, program, ["String"]);
}

function isMapPrototypeMember(symbol: ts.Symbol, program: ts.Program): boolean {
  return isDefaultLibInterfaceMember(symbol, program, ["Map", "ReadonlyMap"]);
}

function isDefaultLibInterfaceMember(
  symbol: ts.Symbol,
  program: ts.Program,
  interfaceNames: readonly string[],
): boolean {
  const decls = symbol.declarations;
  if (!decls) {
    return false;
  }
  for (const decl of decls) {
    if (!program.isSourceFileDefaultLibrary(decl.getSourceFile())) {
      continue;
    }
    const parent = decl.parent;
    if (
      parent &&
      ts.isInterfaceDeclaration(parent) &&
      interfaceNames.includes(parent.name.text)
    ) {
      return true;
    }
  }
  return false;
}

function isStringLikeType(type: ts.Type): boolean {
  if (type.isUnion()) {
    return type.types.every(isStringLikeType);
  }
  return (type.flags & ts.TypeFlags.StringLike) !== 0;
}

const PROJECT_ROOT = resolve(import.meta.dirname, "../../..");
const depModuleCache: Map<DepModuleName, string> = new Map();

/**
 * Load and cache the on-disk text of a hand-written js-stdlib dep
 * module. The path resolves relative to the workspace root so the
 * lookup is independent of the caller's cwd.
 */
export function loadBuiltinDepModule(name: DepModuleName): string {
  const cached = depModuleCache.get(name);
  if (cached !== undefined) {
    return cached;
  }
  const path = resolve(PROJECT_ROOT, "samples/js-stdlib", `${name}.pant`);
  const text = readFileSync(path, "utf-8");
  depModuleCache.set(name, text);
  return text;
}
