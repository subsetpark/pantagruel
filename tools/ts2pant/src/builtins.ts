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
export type DepModuleName = "JS_MATH" | "JS_STRING";

/**
 * Dispatch entry: `rule` is the qualified Pantagruel rule reference
 * a consumer module would use after `import JS_MATH.` (e.g.,
 * `JS_MATH::max-of`); `mod` is the dep module the rule lives in.
 */
export interface BuiltinSpec {
  rule: string;
  mod: DepModuleName;
}

/**
 * Internal key for the dispatch table. The keys are TS surface forms
 * keyed by namespace (Math.* uses "Math.<name>"; String.prototype.*
 * uses "String.prototype.<name>") so the lookup logic can route by
 * which symbol-kind it just resolved.
 */
type BuiltinKey = `Math.${string}` | `String.prototype.${string}`;

const BUILTINS: Map<BuiltinKey, BuiltinSpec> = new Map([
  ["Math.max", { rule: "JS_MATH::max-of", mod: "JS_MATH" }],
  ["Math.abs", { rule: "JS_MATH::abs", mod: "JS_MATH" }],
  [
    "String.prototype.toUpperCase",
    { rule: "JS_STRING::to-upper-case", mod: "JS_STRING" },
  ],
  [
    "String.prototype.indexOf",
    { rule: "JS_STRING::index-of", mod: "JS_STRING" },
  ],
]);

/**
 * Resolve a `ts.CallExpression` to a {@link BuiltinSpec} when its
 * callee is a recognized JavaScript built-in, else `null`.
 *
 * Matching is symbol-based, not textual: a user-shadowed
 * `const Math = ...; Math.max(a, b)` resolves to a symbol declared in
 * the user's source file rather than a TS lib declaration file, so
 * the lookup fails and the call falls through to ts2pant's regular
 * EUF lowering. This is the same robustness posture the nullish
 * recognizer takes (see `nullish-recognizer.ts` and CLAUDE.md M4
 * "structural-not-textual" rule).
 *
 * For `Math.<name>(...)` we resolve the symbol of the *receiver*
 * (the `Math` identifier) and accept it iff its declarations sit in
 * a TS lib `.d.ts` file. For `<receiver>.<name>(...)` where the
 * method is on `String.prototype` (e.g., `s.toUpperCase()`) we
 * resolve the symbol of the *method* and accept it iff its parent
 * declaration is the lib `String` interface.
 */
export function lookupBuiltinByCall(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
): BuiltinSpec | null {
  if (!ts.isPropertyAccessExpression(expr.expression)) {
    return null;
  }
  const propAccess = expr.expression;
  const memberName = propAccess.name.text;

  if (
    ts.isIdentifier(propAccess.expression) &&
    propAccess.expression.text === "Math" &&
    isLibSymbol(checker.getSymbolAtLocation(propAccess.expression))
  ) {
    return BUILTINS.get(`Math.${memberName}`) ?? null;
  }

  const methodSymbol = checker.getSymbolAtLocation(propAccess.name);
  if (methodSymbol && isStringPrototypeMember(methodSymbol)) {
    return BUILTINS.get(`String.prototype.${memberName}`) ?? null;
  }

  return null;
}

function isLibSymbol(symbol: ts.Symbol | undefined): boolean {
  const decls = symbol?.declarations;
  if (!decls || decls.length === 0) {
    return false;
  }
  return decls.some((d) => d.getSourceFile().isDeclarationFile);
}

function isStringPrototypeMember(symbol: ts.Symbol): boolean {
  const decls = symbol.declarations;
  if (!decls) {
    return false;
  }
  for (const decl of decls) {
    if (!decl.getSourceFile().isDeclarationFile) {
      continue;
    }
    const parent = decl.parent;
    if (
      parent &&
      ts.isInterfaceDeclaration(parent) &&
      parent.name.text === "String"
    ) {
      return true;
    }
  }
  return false;
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
