/**
 * Opaque branded types for Pantagruel AST values constructed via the
 * wasm-embedded OCaml parser. These are handles to OCaml heap values —
 * they cannot be inspected from TypeScript, only passed to other
 * wasm module functions.
 */

export type OpaqueExpr = { readonly __brand: "PantExpr" };
export type OpaqueTypeExpr = { readonly __brand: "PantTypeExpr" };
export type OpaqueBinop = { readonly __brand: "PantBinop" };
export type OpaqueUnop = { readonly __brand: "PantUnop" };
export type OpaqueCombiner = { readonly __brand: "PantCombiner" };
export type OpaqueParam = { readonly __brand: "PantParam" };
export type OpaqueGuard = { readonly __brand: "PantGuard" };
export type OpaqueDecl = { readonly __brand: "PantDecl" };

/**
 * Interface for the pantAst wasm export.
 *
 * Zero-arg operator methods are functions (wasm_of_ocaml convention for
 * object%js methods with no arguments) — call as `ast.opAnd()`, not
 * `ast.opAnd`.
 */
export interface PantAstModule {
  // Expression constructors
  var: (name: string) => OpaqueExpr;
  domain: (name: string) => OpaqueExpr;
  app: (fn: OpaqueExpr, args: OpaqueExpr[]) => OpaqueExpr;
  primed: (name: string) => OpaqueExpr;
  binop: (op: OpaqueBinop, l: OpaqueExpr, r: OpaqueExpr) => OpaqueExpr;
  unop: (op: OpaqueUnop, e: OpaqueExpr) => OpaqueExpr;
  litNat: (n: number) => OpaqueExpr;
  litBool: (b: boolean) => OpaqueExpr;
  litString: (s: string) => OpaqueExpr;
  cond: (arms: [OpaqueExpr, OpaqueExpr][]) => OpaqueExpr;
  tuple: (es: OpaqueExpr[]) => OpaqueExpr;
  proj: (e: OpaqueExpr, n: number) => OpaqueExpr;
  override: (name: string, pairs: [OpaqueExpr, OpaqueExpr][]) => OpaqueExpr;
  initially: (e: OpaqueExpr) => OpaqueExpr;
  forall: (
    params: OpaqueParam[],
    guards: OpaqueGuard[],
    body: OpaqueExpr,
  ) => OpaqueExpr;
  each: (
    params: OpaqueParam[],
    guards: OpaqueGuard[],
    body: OpaqueExpr,
  ) => OpaqueExpr;
  eachComb: (
    params: OpaqueParam[],
    guards: OpaqueGuard[],
    comb: OpaqueCombiner,
    body: OpaqueExpr,
  ) => OpaqueExpr;
  exists: (
    params: OpaqueParam[],
    guards: OpaqueGuard[],
    body: OpaqueExpr,
  ) => OpaqueExpr;

  // Binary operators
  opAnd: () => OpaqueBinop;
  opOr: () => OpaqueBinop;
  opImpl: () => OpaqueBinop;
  opIff: () => OpaqueBinop;
  opEq: () => OpaqueBinop;
  opNeq: () => OpaqueBinop;
  opLt: () => OpaqueBinop;
  opGt: () => OpaqueBinop;
  opLe: () => OpaqueBinop;
  opGe: () => OpaqueBinop;
  opIn: () => OpaqueBinop;
  opSubset: () => OpaqueBinop;
  opAdd: () => OpaqueBinop;
  opSub: () => OpaqueBinop;
  opMul: () => OpaqueBinop;
  opDiv: () => OpaqueBinop;

  // Unary operators
  opNot: () => OpaqueUnop;
  opNeg: () => OpaqueUnop;
  opCard: () => OpaqueUnop;

  // Combiners (for over-each)
  combAdd: () => OpaqueCombiner;
  combMul: () => OpaqueCombiner;
  combAnd: () => OpaqueCombiner;
  combOr: () => OpaqueCombiner;
  combMin: () => OpaqueCombiner;
  combMax: () => OpaqueCombiner;

  // Param / guard / type constructors
  param: (name: string, typeExpr: OpaqueTypeExpr) => OpaqueParam;
  tName: (name: string) => OpaqueTypeExpr;
  tList: (t: OpaqueTypeExpr) => OpaqueTypeExpr;
  tProduct: (ts: OpaqueTypeExpr[]) => OpaqueTypeExpr;
  tSum: (ts: OpaqueTypeExpr[]) => OpaqueTypeExpr;
  gExpr: (e: OpaqueExpr) => OpaqueGuard;
  gIn: (name: string, e: OpaqueExpr) => OpaqueGuard;
  gParam: (p: OpaqueParam) => OpaqueGuard;

  // Declaration constructors
  declDomain: (name: string) => OpaqueDecl;
  declAlias: (name: string, typeExpr: OpaqueTypeExpr) => OpaqueDecl;
  declRule: (
    name: string,
    params: OpaqueParam[],
    guards: OpaqueGuard[],
    returnType: OpaqueTypeExpr,
  ) => OpaqueDecl;
  declAction: (
    label: string,
    params: OpaqueParam[],
    guards: OpaqueGuard[],
  ) => OpaqueDecl;

  // Rendering
  strExpr: (e: OpaqueExpr) => string;
  strTypeExpr: (te: OpaqueTypeExpr) => string;
  strDecl: (d: OpaqueDecl) => string;

  // AST traversal
  substituteBinder: (
    expr: OpaqueExpr,
    name: string,
    replacement: OpaqueExpr,
  ) => OpaqueExpr;
}
