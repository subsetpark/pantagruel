import ts from "typescript";

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

/** Create a TS Program from a source file on disk. */
export function createProgram(fileName: string): ts.Program {
  return ts.createProgram([fileName], {
    target: ts.ScriptTarget.ES2022,
    module: ts.ModuleKind.Node16,
    strict: true,
  });
}

/** Create a TS Program from an in-memory source string (useful for tests). */
export function createProgramFromSource(
  source: string,
  fileName = "test.ts",
): ts.Program {
  const sourceFile = ts.createSourceFile(
    fileName,
    source,
    ts.ScriptTarget.ES2022,
    true,
  );
  const defaultHost = ts.createCompilerHost({
    target: ts.ScriptTarget.ES2022,
    strict: true,
  });

  const host: ts.CompilerHost = {
    ...defaultHost,
    getSourceFile(name, languageVersion) {
      if (name === fileName) return sourceFile;
      return defaultHost.getSourceFile(name, languageVersion);
    },
    fileExists(name) {
      return name === fileName || defaultHost.fileExists(name);
    },
    readFile(name) {
      return name === fileName ? source : defaultHost.readFile(name);
    },
  };

  return ts.createProgram(
    [fileName],
    { target: ts.ScriptTarget.ES2022, strict: true },
    host,
  );
}

/** Extract all interfaces, type aliases, and enums from a source file. */
export function extractAllTypes(
  program: ts.Program,
  fileName: string,
): ExtractedTypes {
  const checker = program.getTypeChecker();
  const sourceFile = program.getSourceFile(fileName);
  if (!sourceFile) throw new Error(`Source file not found: ${fileName}`);

  const result: ExtractedTypes = { interfaces: [], aliases: [], enums: [] };

  ts.forEachChild(sourceFile, (node) => {
    if (ts.isInterfaceDeclaration(node)) {
      result.interfaces.push(extractInterface(node, checker));
    } else if (ts.isTypeAliasDeclaration(node)) {
      result.aliases.push(extractAlias(node, checker));
    } else if (ts.isEnumDeclaration(node)) {
      result.enums.push(extractEnum(node));
    }
  });

  return result;
}

/**
 * Extract types referenced by a function's parameter and return types,
 * following named types recursively.
 */
export function extractReferencedTypes(
  program: ts.Program,
  fileName: string,
  functionName: string,
): ExtractedTypes {
  const checker = program.getTypeChecker();
  const sourceFile = program.getSourceFile(fileName);
  if (!sourceFile) throw new Error(`Source file not found: ${fileName}`);

  let funcNode: ts.FunctionDeclaration | undefined;
  ts.forEachChild(sourceFile, (node) => {
    if (ts.isFunctionDeclaration(node) && node.name?.text === functionName) {
      funcNode = node;
    }
  });
  if (!funcNode) throw new Error(`Function not found: ${functionName}`);

  const signature = checker.getSignatureFromDeclaration(funcNode);
  if (!signature)
    throw new Error(`Cannot get signature for: ${functionName}`);

  const visited = new Set<string>();

  for (const param of signature.getParameters()) {
    collectNamedTypes(checker.getTypeOfSymbol(param), checker, visited);
  }
  collectNamedTypes(signature.getReturnType(), checker, visited);

  const allTypes = extractAllTypes(program, fileName);
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
    if (visited.has(name)) return;
    visited.add(name);

    for (const prop of type.getProperties()) {
      const propType = checker.getTypeOfSymbol(prop);
      collectNamedTypes(propType, checker, visited);
    }
  }
}
