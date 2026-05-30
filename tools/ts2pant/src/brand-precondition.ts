import type ts from "typescript";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";

type PredicateBuilder = (paramName: string) => OpaqueExpr;

export const BRAND_PREDICATES: ReadonlyMap<string, PredicateBuilder> = new Map([
  ["positive", (name) => compare(name, ">", 0)],
  ["Positive", (name) => compare(name, ">", 0)],
  ["negative", (name) => compare(name, "<", 0)],
  ["Negative", (name) => compare(name, "<", 0)],
  ["nonNegative", (name) => compare(name, ">=", 0)],
  ["NonNegative", (name) => compare(name, ">=", 0)],
  ["nonPositive", (name) => compare(name, "<=", 0)],
  ["NonPositive", (name) => compare(name, "<=", 0)],
  [
    "int",
    (name) => getAst().app(getAst().var("integral"), [getAst().var(name)]),
  ],
  [
    "Int",
    (name) => getAst().app(getAst().var("integral"), [getAst().var(name)]),
  ],
  [
    "integer",
    (name) => getAst().app(getAst().var("integral"), [getAst().var(name)]),
  ],
  [
    "Integer",
    (name) => getAst().app(getAst().var("integral"), [getAst().var(name)]),
  ],
  ["nonEmpty", (name) => nonEmpty(name)],
  ["NonEmpty", (name) => nonEmpty(name)],
  ["NonEmptyString", (name) => nonEmpty(name)],
]);

/**
 * Recover a Pantagruel precondition from a recognized Effect Brand/Schema
 * refinement carried by a parameter type. Unknown brand intersections return
 * null instead of emitting a partial predicate.
 */
export function recognizeBrandedPrecondition(
  type: ts.Type,
  pantParamName: string,
): OpaqueExpr | null {
  const brands = collectBrandNames(type);
  if (brands === null || brands.length === 0) {
    return null;
  }

  const predicates: OpaqueExpr[] = [];
  for (const brand of brands) {
    const build = BRAND_PREDICATES.get(brand);
    if (!build) {
      return null;
    }
    predicates.push(build(pantParamName));
  }

  const ast = getAst();
  return predicates.reduce((acc, pred) => ast.binop(ast.opAnd(), acc, pred));
}

function collectBrandNames(type: ts.Type): string[] | null {
  const brands: string[] = [];
  let sawUnknownBrand = false;

  function visit(current: ts.Type): void {
    if (current.isUnion()) {
      sawUnknownBrand = true;
      return;
    }

    if (current.isIntersection()) {
      for (const part of current.types) {
        visit(part);
      }
      return;
    }

    const brandArg = brandTypeArgument(current);
    if (brandArg !== undefined) {
      if (brandArg === null) {
        sawUnknownBrand = true;
      } else {
        brands.push(brandArg);
      }
      return;
    }

    const schemaName = effectSchemaFilterName(current);
    if (schemaName !== null) {
      brands.push(schemaName);
    }
  }

  visit(type);
  return sawUnknownBrand ? null : [...new Set(brands)];
}

function brandTypeArgument(type: ts.Type): string | null | undefined {
  const ref = type as ts.TypeReference;
  const target = ref.target;
  const symbol =
    target?.aliasSymbol ?? target?.symbol ?? type.aliasSymbol ?? type.symbol;
  if (symbol?.getName() !== "Brand") {
    return undefined;
  }

  if (
    !symbol
      .getDeclarations()
      ?.some((decl) =>
        /node_modules\/effect\/(?:dist\/dts|src)\/Brand\.(?:d\.ts|ts)$/u.test(
          decl.getSourceFile().fileName,
        ),
      )
  ) {
    return undefined;
  }

  const arg = ref.typeArguments?.[0];
  if (!arg?.isStringLiteral()) {
    return null;
  }
  return arg.value;
}

function effectSchemaFilterName(type: ts.Type): string | null {
  const symbol = type.aliasSymbol ?? type.symbol;
  const name = symbol?.getName();
  if (!name || !BRAND_PREDICATES.has(name)) {
    return null;
  }
  const fromEffectSchema =
    symbol
      .getDeclarations()
      ?.some((decl) =>
        /node_modules\/effect\/(?:dist\/dts|src)\/Schema\.(?:d\.ts|ts)$/u.test(
          decl.getSourceFile().fileName,
        ),
      ) ?? false;
  return fromEffectSchema ? name : null;
}

function compare(
  paramName: string,
  op: ">" | ">=" | "<" | "<=",
  literal: number,
): OpaqueExpr {
  const ast = getAst();
  const left = ast.var(paramName);
  const right = ast.litNat(literal);
  switch (op) {
    case ">":
      return ast.binop(ast.opGt(), left, right);
    case ">=":
      return ast.binop(ast.opGe(), left, right);
    case "<":
      return ast.binop(ast.opLt(), left, right);
    case "<=":
      return ast.binop(ast.opLe(), left, right);
    default: {
      const _exhaustive: never = op;
      throw new Error(`Unhandled brand predicate operator: ${_exhaustive}`);
    }
  }
}

function nonEmpty(paramName: string): OpaqueExpr {
  const ast = getAst();
  return ast.binop(
    ast.opGt(),
    ast.unop(ast.opCard(), ast.var(paramName)),
    ast.litNat(0),
  );
}
