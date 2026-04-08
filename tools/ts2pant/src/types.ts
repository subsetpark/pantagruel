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

/** A Pantagruel rule declaration (e.g. `owner d: Document => User.`). */
export interface PantRule {
  kind: "rule";
  name: string;
  params: Array<{ name: string; type: string }>;
  returnType: string;
}

/** A Pantagruel action declaration (e.g. `~> Withdraw @ u: User.`). */
export interface PantAction {
  kind: "action";
  label: string;
  params: Array<{ name: string; type: string }>;
}

/** A declaration in a Pantagruel document. */
export type PantDeclaration = PantDomain | PantAlias | PantRule | PantAction;

/** A Pantagruel proposition (a boolean expression in the body section). */
export interface PantProposition {
  text: string;
}

/** A complete Pantagruel document ready for emission. */
export interface PantDocument {
  moduleName: string;
  declarations: PantDeclaration[];
  propositions: PantProposition[];
}

/** CLI options parsed from command-line arguments. */
export interface CliOptions {
  inputFile: string;
  functionName: string;
  check: boolean;
  noBody: boolean;
  numericType: NumericType;
}
