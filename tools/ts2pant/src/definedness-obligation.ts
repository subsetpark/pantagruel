import type { InScopeDiscriminantFact } from "./assumption-env.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";

export interface DefinednessObligationInput {
  receiver: OpaqueExpr;
  discRule: string;
  requiredLiteral?: string;
  requiredLiterals?: readonly string[];
  inScope: readonly InScopeDiscriminantFact[];
}

export interface DefinednessObligation {
  text: string;
}

export function renderDefinednessObligation(
  input: DefinednessObligationInput,
): DefinednessObligation {
  const ast = getAst();
  const requiredLiterals =
    input.requiredLiterals ??
    (input.requiredLiteral ? [input.requiredLiteral] : []);
  const consequent = disjoin(
    requiredLiterals.map((literal) =>
      discriminantEquality(input.discRule, input.receiver, literal),
    ),
  );
  if (consequent === null) {
    throw new Error("definedness obligation requires at least one literal");
  }
  const antecedents = input.inScope.map((fact) => {
    const equality = discriminantEquality(
      input.discRule,
      input.receiver,
      fact.literal,
    );
    return fact.negated ? ast.unop(ast.opNot(), equality) : equality;
  });
  const antecedent = conjoin(antecedents);
  const result =
    antecedent === null
      ? consequent
      : ast.binop(ast.opImpl(), antecedent, consequent);
  return { text: ast.strExpr(result) };
}

function discriminantEquality(
  discRule: string,
  receiver: OpaqueExpr,
  literal: string,
): OpaqueExpr {
  const ast = getAst();
  return ast.binop(
    ast.opEq(),
    ast.app(ast.var(discRule), [receiver]),
    ast.litString(literal),
  );
}

function conjoin(exprs: readonly OpaqueExpr[]): OpaqueExpr | null {
  const ast = getAst();
  let out: OpaqueExpr | null = null;
  for (const expr of exprs) {
    out = out === null ? expr : ast.binop(ast.opAnd(), out, expr);
  }
  return out;
}

function disjoin(exprs: readonly OpaqueExpr[]): OpaqueExpr | null {
  const ast = getAst();
  let out: OpaqueExpr | null = null;
  for (const expr of exprs) {
    out = out === null ? expr : ast.binop(ast.opOr(), out, expr);
  }
  return out;
}
