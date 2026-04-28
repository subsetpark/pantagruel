import type { OpaqueExpr, OpaqueGuard, OpaqueParam } from "./pant-ast.js";

/** Pantagruel numeric type strategy. */
export type NumericType = "Int" | "Real" | "Nat0";

/** A Pantagruel domain declaration (e.g. `User.`). */
export interface PantDomain {
  kind: "domain";
  name: string;
}

/** A Pantagruel type alias (e.g. `Point = Nat * Nat.`). */
export interface PantAlias {
  kind: "alias";
  name: string;
  type: string;
}

/** A named, typed parameter in a rule or action. */
export interface PantParam {
  name: string;
  type: string;
}

/** A Pantagruel rule declaration (e.g. `owner d: Document => User.`). */
export interface PantRule {
  kind: "rule";
  name: string;
  params: PantParam[];
  returnType: string;
  guard?: OpaqueExpr;
}

/** A Pantagruel action declaration (e.g. `~> Withdraw @ u: User.`). */
export interface PantAction {
  kind: "action";
  label: string;
  params: PantParam[];
  guard?: OpaqueExpr;
}

/** A declaration that could not be translated. Renders as
 *  `> UNSUPPORTED: <reason>` (a doc-comment line) at emit time so the
 *  document's surrounding Pant text stays parseable while still
 *  surfacing the cause to the user. Mirrors `PropResult`'s
 *  `unsupported` variant — the structural rejection path for
 *  type/declaration emit sites that would otherwise embed an internal
 *  sentinel string (e.g. `__unsupported_unknown__`) into generated
 *  Pant. */
export interface PantUnsupported {
  kind: "unsupported";
  reason: string;
}

/** A declaration in a Pantagruel document. */
export type PantDeclaration =
  | PantDomain
  | PantAlias
  | PantRule
  | PantAction
  | PantUnsupported;

/**
 * Proposition result: either a structured equation, an unsupported
 * translation, or a raw text proposition (from @pant annotations).
 */
export type PropResult =
  | {
      kind: "equation";
      quantifiers: OpaqueParam[];
      guards?: OpaqueGuard[];
      lhs: OpaqueExpr;
      rhs: OpaqueExpr;
    }
  | {
      kind: "assertion";
      quantifiers: OpaqueParam[];
      guards?: OpaqueGuard[];
      body: OpaqueExpr;
    }
  | { kind: "unsupported"; reason: string }
  | { kind: "raw"; text: string };

/** A Pantagruel `import <Name>.` line. */
export interface ImportSpec {
  name: string;
}

/** A complete Pantagruel document ready for emission. */
export interface PantDocument {
  moduleName: string;
  imports: ImportSpec[];
  declarations: PantDeclaration[];
  propositions: PropResult[];
  checks: { text: string }[];
  /**
   * In-memory dependency module sources keyed by module name. Populated by
   * later patches that synthesize / bundle dep modules; the wasm bridge's
   * cross-module typecheck path resolves `imports` against this map.
   */
  bundleModules?: Map<string, string>;
}

/** CLI options parsed from command-line arguments. */
export interface CliOptions {
  inputFile: string;
  functionName: string;
  check: boolean;
  noBody: boolean;
  numericType: NumericType;
}
