import type {
  InScopeDiscriminantFact,
  InScopeNonNullFact,
} from "./assumption-env.js";
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

export interface NullishObligationInput {
  receiver: OpaqueExpr;
  inScope: readonly InScopeNonNullFact[];
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

export function renderNullishObligation(
  input: NullishObligationInput,
): DefinednessObligation {
  const ast = getAst();
  const receiverText = ast.strExpr(input.receiver);
  if (
    input.inScope.some(
      (fact) =>
        !fact.negated && ast.strExpr(ast.var(fact.receiver)) === receiverText,
    )
  ) {
    return { text: ast.strExpr(ast.litBool(true)) };
  }
  const consequent = nonNullExpr(input.receiver);
  const antecedents = input.inScope.map((fact) => {
    const equality = nonNullExpr(ast.var(fact.receiver));
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

function nonNullExpr(receiver: OpaqueExpr): OpaqueExpr {
  const ast = getAst();
  return ast.binop(ast.opGt(), ast.unop(ast.opCard(), receiver), ast.litNat(0));
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
