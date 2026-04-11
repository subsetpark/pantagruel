import ts from "typescript";
import type { ExtractedTypes } from "./extract.js";
import type { PantDeclaration } from "./types.js";

/** Strategy for mapping TS `number` to a Pantagruel numeric type. */
export interface NumericStrategy {
  mapNumber: (context?: string) => string;
}

export const IntStrategy: NumericStrategy = {
  mapNumber() {
    return "Int";
  },
};

export const RealStrategy: NumericStrategy = {
  mapNumber() {
    return "Real";
  },
};

/** Map a TypeScript type to a Pantagruel type string. */
export function mapTsType(
  type: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): string {
  const flags = type.flags;

  if (flags & ts.TypeFlags.String || flags & ts.TypeFlags.StringLiteral) {
    return "String";
  }
  if (flags & ts.TypeFlags.Number || flags & ts.TypeFlags.NumberLiteral) {
    return strategy.mapNumber();
  }
  if (flags & ts.TypeFlags.Boolean || flags & ts.TypeFlags.BooleanLiteral) {
    return "Bool";
  }
  if (
    flags & ts.TypeFlags.Null ||
    flags & ts.TypeFlags.Undefined ||
    flags & ts.TypeFlags.Void
  ) {
    return "Nothing";
  }

  // Tuple (check before array since tuples are also type references)
  if (checker.isTupleType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    return typeArgs.map((t) => mapTsType(t, checker, strategy)).join(" * ");
  }

  // Array
  if (checker.isArrayType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    if (typeArgs.length === 1) {
      return `[${mapTsType(typeArgs[0]!, checker, strategy)}]`;
    }
    return checker.typeToString(type);
  }

  // Union
  if (type.isUnion()) {
    // Boolean is represented as true | false union
    if (type.types.every((t) => t.flags & ts.TypeFlags.BooleanLiteral)) {
      return "Bool";
    }
    const parts = type.types.map((t) => mapTsType(t, checker, strategy));
    // Deduplicate (e.g. boolean literal collapse)
    const unique = parts.filter((v, i, a) => a.indexOf(v) === i);
    // Sort Nothing to the end for consistent output
    unique.sort((a, b) => (a === "Nothing" ? 1 : b === "Nothing" ? -1 : 0));
    return unique.join(" + ");
  }

  // Named type (interface, class, enum, type alias)
  const symbol = type.aliasSymbol ?? type.symbol;
  if (symbol) {
    return symbol.getName();
  }

  return checker.typeToString(type);
}

/** Derive a short parameter name from a type name (first letter, lowercased). */
function paramName(typeName: string): string {
  if (!typeName) {
    return "x";
  }
  return typeName[0]!.toLowerCase();
}

/**
 * Translate extracted TypeScript types into Pantagruel declarations.
 *
 * - Interfaces -> domain + one rule per property
 * - Type aliases -> alias declaration
 * - Enums -> domain declaration
 */
export function translateTypes(
  extracted: ExtractedTypes,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): PantDeclaration[] {
  const decls: PantDeclaration[] = [];

  for (const iface of extracted.interfaces) {
    decls.push({ kind: "domain", name: iface.name });
    const pName = paramName(iface.name);
    for (const prop of iface.properties) {
      decls.push({
        kind: "rule",
        name: prop.name,
        params: [{ name: pName, type: iface.name }],
        returnType: mapTsType(prop.type, checker, strategy),
      });
    }
  }

  for (const alias of extracted.aliases) {
    decls.push({
      kind: "alias",
      name: alias.name,
      type: mapTsType(alias.type, checker, strategy),
    });
  }

  for (const enumDecl of extracted.enums) {
    decls.push({ kind: "domain", name: enumDecl.name });
  }

  return decls;
}
