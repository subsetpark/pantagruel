import { Project, type SourceFile } from "ts-morph";
import ts from "typescript";

export type { SourceFile } from "ts-morph";

export interface ExtractedProperty {
  name: string;
  type: ts.Type;
}

export interface ExtractedInterface {
  name: string;
  properties: ExtractedProperty[];
}

export interface ExtractedAlias {
  name: string;
  type: ts.Type;
}

export interface ExtractedEnum {
  name: string;
  members: string[];
}

export interface ExtractedTypes {
  interfaces: ExtractedInterface[];
  aliases: ExtractedAlias[];
  enums: ExtractedEnum[];
}

const COMPILER_OPTIONS = {
  target: ts.ScriptTarget.ES2022,
  module: ts.ModuleKind.NodeNext,
  moduleResolution: ts.ModuleResolutionKind.NodeNext,
  strict: true,
  noUncheckedIndexedAccess: true,
  noPropertyAccessFromIndexSignature: true,
  exactOptionalPropertyTypes: true,
  verbatimModuleSyntax: true,
  isolatedModules: true,
} satisfies ts.CompilerOptions;

/** Create a ts-morph SourceFile from a file on disk. */
export function createSourceFile(fileName: string): SourceFile {
  const project = new Project({ compilerOptions: COMPILER_OPTIONS });
  project.addSourceFileAtPath(fileName);
  return project.getSourceFileOrThrow(fileName);
}

/** Create a ts-morph SourceFile from an in-memory source string (useful for tests). */
export function createSourceFileFromSource(
  source: string,
  fileName = "test.ts",
): SourceFile {
  const project = new Project({
    compilerOptions: COMPILER_OPTIONS,
    useInMemoryFileSystem: true,
  });
  return project.createSourceFile(fileName, source);
}

/** Get the raw TypeScript TypeChecker from a ts-morph SourceFile. */
export function getChecker(sourceFile: SourceFile): ts.TypeChecker {
  return sourceFile.getProject().getTypeChecker().compilerObject;
}

/** Extract all interfaces, type aliases, and enums from a source file. */
export function extractAllTypes(sourceFile: SourceFile): ExtractedTypes {
  const checker = getChecker(sourceFile);

  const interfaces = sourceFile
    .getInterfaces()
    .map((node) => extractInterface(node.compilerNode, checker));

  const aliases = sourceFile
    .getTypeAliases()
    .map((node) => extractAlias(node.compilerNode, checker));

  const enums = sourceFile
    .getEnums()
    .map((node) => extractEnum(node.compilerNode));

  return { interfaces, aliases, enums };
}

/**
 * Extract types referenced by a function's parameter and return types,
 * following named types recursively.
 */
export function extractReferencedTypes(
  sourceFile: SourceFile,
  functionName: string,
): ExtractedTypes {
  const checker = getChecker(sourceFile);

  // Find the function or class method
  let funcNode: ts.FunctionDeclaration | ts.MethodDeclaration | undefined;
  let className: string | undefined;

  const func = sourceFile
    .getFunctions()
    .find((f) => f.getName() === functionName);
  if (func) {
    funcNode = func.compilerNode;
  } else {
    for (const cls of sourceFile.getClasses()) {
      const method = cls.getMethods().find((m) => m.getName() === functionName);
      if (method) {
        funcNode = method.compilerNode;
        className = cls.getName();
        break;
      }
    }
  }

  if (!funcNode) {
    throw new Error(`Function not found: ${functionName}`);
  }

  const signature = checker.getSignatureFromDeclaration(funcNode);
  if (!signature) {
    throw new Error(`Cannot get signature for: ${functionName}`);
  }

  const visited = new Set<string>();

  // For class methods, collect the class type (implicit `this` parameter)
  if (className) {
    visited.add(className);
  }

  for (const param of signature.getParameters()) {
    collectNamedTypes(checker.getTypeOfSymbol(param), checker, visited);
  }
  collectNamedTypes(signature.getReturnType(), checker, visited);

  const allTypes = extractAllTypes(sourceFile);
  return {
    interfaces: allTypes.interfaces.filter((i) => visited.has(i.name)),
    aliases: allTypes.aliases.filter((a) => visited.has(a.name)),
    enums: allTypes.enums.filter((e) => visited.has(e.name)),
  };
}

function extractInterface(
  node: ts.InterfaceDeclaration,
  checker: ts.TypeChecker,
): ExtractedInterface {
  const name = node.name.text;
  const properties: ExtractedProperty[] = [];

  for (const member of node.members) {
    if (
      ts.isPropertySignature(member) &&
      member.name &&
      ts.isIdentifier(member.name)
    ) {
      const symbol = checker.getSymbolAtLocation(member.name);
      if (symbol) {
        properties.push({
          name: member.name.text,
          type: checker.getTypeOfSymbol(symbol),
        });
      }
    }
  }

  return { name, properties };
}

function extractAlias(
  node: ts.TypeAliasDeclaration,
  checker: ts.TypeChecker,
): ExtractedAlias {
  const name = node.name.text;
  const type = checker.getTypeFromTypeNode(node.type);
  return { name, type };
}

function extractEnum(node: ts.EnumDeclaration): ExtractedEnum {
  const name = node.name.text;
  const members: string[] = [];
  for (const member of node.members) {
    if (ts.isIdentifier(member.name)) {
      members.push(member.name.text);
    }
  }
  return { name, members };
}

const BUILTIN_NAMES = new Set([
  "Array",
  "ReadonlyArray",
  "Promise",
  "Map",
  "Set",
  "Date",
  "RegExp",
  "Error",
  "Object",
  "Function",
  "Symbol",
  "BigInt",
  "WeakMap",
  "WeakSet",
]);

function collectNamedTypes(
  type: ts.Type,
  checker: ts.TypeChecker,
  visited: Set<string>,
): void {
  const flags = type.flags;

  if (
    flags &
    (ts.TypeFlags.String |
      ts.TypeFlags.Number |
      ts.TypeFlags.Boolean |
      ts.TypeFlags.Null |
      ts.TypeFlags.Undefined |
      ts.TypeFlags.Void |
      ts.TypeFlags.StringLiteral |
      ts.TypeFlags.NumberLiteral |
      ts.TypeFlags.BooleanLiteral)
  ) {
    return;
  }

  if (type.isUnion()) {
    for (const t of type.types) {
      collectNamedTypes(t, checker, visited);
    }
    return;
  }

  if (type.isIntersection()) {
    for (const t of type.types) {
      collectNamedTypes(t, checker, visited);
    }
    return;
  }

  if (checker.isArrayType(type) || checker.isTupleType(type)) {
    const typeArgs = checker.getTypeArguments(type as ts.TypeReference);
    for (const arg of typeArgs) {
      collectNamedTypes(arg, checker, visited);
    }
    return;
  }

  const symbol = type.aliasSymbol ?? type.symbol;
  if (symbol && !BUILTIN_NAMES.has(symbol.name)) {
    const name = symbol.name;
    if (visited.has(name)) {
      return;
    }
    visited.add(name);

    for (const prop of type.getProperties()) {
      const propType = checker.getTypeOfSymbol(prop);
      collectNamedTypes(propType, checker, visited);
    }
  }
}
