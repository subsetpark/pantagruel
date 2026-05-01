import { Project, type SourceFile } from "ts-morph";
import ts from "typescript";

import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import { translateSignature } from "./translate-signature.js";
import {
  cellRegisterName,
  isMapType,
  isSetType,
  mapTsType,
  type NumericStrategy,
  newSynthCell,
  type SynthCell,
  toPantTermName,
} from "./translate-types.js";
import type { PantRule, PropResult } from "./types.js";

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

  // Support "ClassName.methodName" for disambiguating methods
  const [classHint, memberName] = functionName.includes(".")
    ? (functionName.split(".", 2) as [string, string])
    : [undefined, functionName];

  // Find the function or class method, preferring implementations with bodies
  let funcNode: ts.FunctionDeclaration | ts.MethodDeclaration | undefined;
  let className: string | undefined;

  if (!classHint) {
    const funcs = sourceFile
      .getFunctions()
      .filter((f) => f.getName() === memberName);
    const func = funcs.find((f) => f.hasBody()) ?? funcs[0];
    if (func) {
      funcNode = func.compilerNode;
    }
  }

  if (!funcNode) {
    let methodMatches = 0;
    for (const cls of sourceFile.getClasses()) {
      if (classHint && cls.getName() !== classHint) {
        continue;
      }
      const methods = cls
        .getMethods()
        .filter((m) => m.getName() === memberName);
      const method = methods.find((m) => m.hasBody()) ?? methods[0];
      if (method) {
        funcNode = method.compilerNode;
        className = cls.getName();
        methodMatches += 1;
        if (classHint) {
          break;
        }
      }
    }
    if (!classHint && methodMatches > 1) {
      throw new Error(
        `Ambiguous method name: ${memberName}. Use ClassName.methodName`,
      );
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

/**
 * One ambient (`declare function`) declaration extracted from a source
 * file. The declaration is the same `PantRule` shape `translateSignature`
 * would produce for an exported function with the same name, params, and
 * return type — only the body is omitted (declares have no body to
 * translate).
 */
export interface AmbientFunctionDecl {
  /** The function name as written in the TS source. */
  tsName: string;
  /** Pantagruel rule head (signature only, no body). */
  declaration: PantRule;
}

/**
 * Pure, deterministic Pantagruel module name for the ambient bundle of
 * a source file. `expressions-calls.ts` → `EXPRESSIONS_CALLS_AMBIENT`.
 * Every consumer translated from the same source file resolves to the
 * same name, so multiple consumers can import the same module without
 * collision (patch 11 plumbing relies on this property).
 *
 * The path is reduced to its basename — directory components are
 * irrelevant — then the trailing extension (`.ts`, `.tsx`, `.d.ts`) is
 * dropped, hyphens are mapped to underscores, the result is uppercased,
 * and `_AMBIENT` is appended.
 */
export function ambientModuleName(sourceFile: SourceFile): string {
  const baseName = sourceFile.getBaseName().replace(/\.[^.]+$/u, "");
  return `${baseName.replace(/-/gu, "_").toUpperCase()}_AMBIENT`;
}

/**
 * Extract the full set of `declare function` ambient declarations in a
 * source file. Each is routed through the same `translateSignature`
 * pipeline used for exported functions so the rule head matches what a
 * consumer translation would emit. Body translation is implicitly
 * skipped — declares have no body.
 *
 * Each ambient gets its own fresh `SynthCell` so per-ambient param
 * names stay independent; the per-file scoping (so multiple consumers
 * of the same source file see the same set) is implicit in the
 * `sourceFile` argument and the determinism of `ambientModuleName`.
 *
 * Ambients with `void` return type translate to actions rather than
 * rules and are filtered out — the ambient-module emission only
 * produces rule heads.
 *
 * Overloaded `declare function`s collapse to a single rule head: the
 * first overload's signature wins, later overloads with the same name
 * are skipped. Pantagruel's positional coherence rejects two rules
 * with the same `(name, arity)` and disagreeing types, so emitting one
 * head per overload would either duplicate (when the underlying
 * `translateSignature` resolves all overloads to the same first
 * declaration — its current behavior) or unsoundly emit conflicting
 * heads. The first-overload-wins choice matches the conventional TS
 * pattern of declaring the canonical signature first.
 */
export function extractAmbientFunctions(
  sourceFile: SourceFile,
  strategy: NumericStrategy,
): AmbientFunctionDecl[] {
  const result: AmbientFunctionDecl[] = [];
  const seen = new Set<string>();
  for (const fn of sourceFile.getFunctions()) {
    if (!fn.hasDeclareKeyword()) {
      continue;
    }
    const tsName = fn.getName();
    if (!tsName) {
      continue;
    }
    if (seen.has(tsName)) {
      continue;
    }
    seen.add(tsName);
    const sig = translateSignature(
      sourceFile,
      tsName,
      strategy,
      newSynthCell(),
    );
    if (sig.declaration.kind !== "rule") {
      continue;
    }
    result.push({ tsName, declaration: sig.declaration });
  }
  return result;
}

/**
 * One module-level `const NAME = <literal>` declaration, translated
 * into a 0-arity Pantagruel rule + an equation binding its value.
 *
 * Pantagruel has no top-level value-binding syntax — module constants
 * map onto rules with empty params and an equation in the body. A TS
 * `const X: T = lit` becomes:
 *
 *   {pant-name X} => {pant T}.
 *   ---
 *   {pant-name X} = {pant lit}.
 *
 * The kebab-cased Pant name is also threaded into the consumer
 * function's `paramNameMap` so identifier references inside bodies
 * resolve correctly (`UNSUPPORTED_UNKNOWN` → `unsupported-unknown`).
 *
 * Scope: only literal initializers (string, numeric, boolean, no-sub
 * template) translate. Compound expressions (object literals, calls,
 * arithmetic) are out of scope — they'd require running the full body
 * pipeline at module-load time, and most useful TS-side constants are
 * just literals anyway. Unsupported initializers are silently skipped
 * so a module's other constants still emit.
 */
export interface ExtractedConst {
  tsName: string;
  pantName: string;
  declaration: PantRule;
  equation: PropResult;
}

/**
 * Translate a TS expression to a Pant `OpaqueExpr` if it's a
 * recognized literal. Returns null for non-literal initializers, which
 * are silently filtered out of the module-const extraction.
 */
function translateConstInitializer(init: ts.Expression): OpaqueExpr | null {
  const ast = getAst();
  if (ts.isStringLiteral(init) || ts.isNoSubstitutionTemplateLiteral(init)) {
    return ast.litString(init.text);
  }
  if (init.kind === ts.SyntaxKind.TrueKeyword) {
    return ast.litBool(true);
  }
  if (init.kind === ts.SyntaxKind.FalseKeyword) {
    return ast.litBool(false);
  }
  if (ts.isNumericLiteral(init)) {
    const n = Number(init.text);
    if (Number.isFinite(n) && Number.isInteger(n) && n >= 0) {
      return ast.litNat(n);
    }
    return null;
  }
  return null;
}

/**
 * Walk a source file's top-level statements for `const NAME = <literal>`
 * declarations referenced from `functionName`'s body. Each match
 * becomes an {@link ExtractedConst} carrying the 0-arity rule head +
 * value equation. The pipeline runs this after `translateSignature` so
 * the registry's already claimed the function's param names —
 * collisions resolve via numeric suffixing.
 *
 * Filtering by reference (rather than emitting every top-level const)
 * keeps the output focused: a function that uses one constant doesn't
 * pull in the other dozen consts that happen to live in the same
 * source file.
 */
export function extractModuleConsts(
  sourceFile: SourceFile,
  functionName: string,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): ExtractedConst[] {
  const checker = getChecker(sourceFile);
  const ast = getAst();

  // Pre-pass: gather candidate top-level `const NAME = <literal>` decls
  // keyed by TS name. Skipped if the initializer is non-literal.
  const candidates = new Map<string, ts.VariableDeclaration>();
  for (const stmt of sourceFile.compilerNode.statements) {
    if (!ts.isVariableStatement(stmt)) {
      continue;
    }
    const flags = ts.getCombinedNodeFlags(stmt.declarationList);
    if ((flags & ts.NodeFlags.Const) === 0) {
      continue;
    }
    for (const decl of stmt.declarationList.declarations) {
      if (!ts.isIdentifier(decl.name) || !decl.initializer) {
        continue;
      }
      if (translateConstInitializer(decl.initializer) === null) {
        continue;
      }
      candidates.set(decl.name.text, decl);
    }
  }
  if (candidates.size === 0) {
    return [];
  }

  // Find the consumer function and walk its body for identifier
  // references that name a candidate const. Symbol-resolve each match
  // against the candidate's declaration so a shadowing local variable
  // (a parameter / `const x = ...` inside the body that happens to
  // share a top-level name) doesn't pull in the wrong const.
  const fnNode = findFunctionNode(sourceFile, functionName);
  if (!fnNode?.body) {
    return [];
  }
  const referenced = new Set<string>();
  function visit(n: ts.Node): void {
    if (ts.isIdentifier(n) && candidates.has(n.text)) {
      const candidate = candidates.get(n.text)!;
      const sym = checker.getSymbolAtLocation(n);
      if (sym?.valueDeclaration === candidate) {
        referenced.add(n.text);
      }
    }
    ts.forEachChild(n, visit);
  }
  visit(fnNode.body);

  const result: ExtractedConst[] = [];
  for (const tsName of referenced) {
    const decl = candidates.get(tsName)!;
    const valueExpr = translateConstInitializer(decl.initializer!);
    if (valueExpr === null) {
      continue;
    }
    const tsType = checker.getTypeAtLocation(decl);
    const pantType = mapTsType(tsType, checker, strategy, synthCell);
    const baseName = toPantTermName(tsName);
    const pantName = synthCell
      ? cellRegisterName(synthCell, baseName)
      : baseName;
    const declaration: PantRule = {
      kind: "rule",
      name: pantName,
      params: [],
      returnType: pantType,
    };
    const equation: PropResult = {
      kind: "equation",
      quantifiers: [],
      lhs: ast.var(pantName),
      rhs: valueExpr,
    };
    result.push({ tsName, pantName, declaration, equation });
  }
  return result;
}

/**
 * Resolve a function-or-method name to its TS AST node, mirroring the
 * lookup logic in `extractReferencedTypes`. Returns null when no match
 * is found rather than throwing — `extractModuleConsts` falls through
 * gracefully when invoked on a source file/function pair that doesn't
 * resolve.
 */
function findFunctionNode(
  sourceFile: SourceFile,
  functionName: string,
): ts.FunctionDeclaration | ts.MethodDeclaration | null {
  const [classHint, memberName] = functionName.includes(".")
    ? (functionName.split(".", 2) as [string, string])
    : [undefined, functionName];

  if (!classHint) {
    const funcs = sourceFile
      .getFunctions()
      .filter((f) => f.getName() === memberName);
    const func = funcs.find((f) => f.hasBody()) ?? funcs[0];
    if (func) {
      return func.compilerNode;
    }
  }
  for (const cls of sourceFile.getClasses()) {
    if (classHint && cls.getName() !== classHint) {
      continue;
    }
    const methods = cls.getMethods().filter((m) => m.getName() === memberName);
    const method = methods.find((m) => m.hasBody()) ?? methods[0];
    if (method) {
      return method.compilerNode;
    }
  }
  return null;
}

/**
 * Emit a standalone Pantagruel module containing one rule head per
 * ambient declaration. Module name comes from `ambientModuleName`. The
 * body is `true.` so the module is well-formed even with zero ambients;
 * this matches `emit.ts`'s convention for empty bodies.
 */
export function emitAmbientModule(
  sourceFile: SourceFile,
  decls: ReadonlyArray<AmbientFunctionDecl>,
): string {
  const moduleName = ambientModuleName(sourceFile);
  const lines: string[] = [`module ${moduleName}.`, ""];
  for (const { declaration: decl } of decls) {
    const params = decl.params.map((p) => `${p.name}: ${p.type}`).join(", ");
    if (decl.params.length === 0) {
      lines.push(`${decl.name} => ${decl.returnType}.`);
    } else {
      lines.push(`${decl.name} ${params} => ${decl.returnType}.`);
    }
  }
  lines.push("", "---", "", "true.", "");
  return lines.join("\n");
}

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

  if (
    checker.isArrayType(type) ||
    checker.isTupleType(type) ||
    isSetType(type) ||
    isMapType(type)
  ) {
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
