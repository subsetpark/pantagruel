import type { OpaqueExpr, PropResult } from "./pant-ast.js";

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

/** A declaration in a Pantagruel document. */
export type PantDeclaration = PantDomain | PantAlias | PantRule | PantAction;

/** A complete Pantagruel document ready for emission. */
export interface PantDocument {
  moduleName: string;
  declarations: PantDeclaration[];
  propositions: PropResult[];
  checks: { text: string }[];
}

/** CLI options parsed from command-line arguments. */
export interface CliOptions {
  inputFile: string;
  functionName: string;
  check: boolean;
  noBody: boolean;
  numericType: NumericType;
}
