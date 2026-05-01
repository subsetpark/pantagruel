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
  /**
   * The TS symbol for this interface declaration. Optional because the
   * legacy single-file path doesn't need it; the cross-file aggregator
   * in `extractReferencedTypes` populates it so the visited filter can
   * compare by symbol identity instead of bare name (two files
   * exporting `interface Foo` resolve to distinct symbols).
   */
  symbol?: ts.Symbol;
}

export interface ExtractedAlias {
  name: string;
  type: ts.Type;
  symbol?: ts.Symbol;
}

export interface ExtractedEnum {
  name: string;
  members: string[];
  symbol?: ts.Symbol;
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
  const program = sourceFile.getProject().getProgram().compilerObject;

  const interfaces = sourceFile
    .getInterfaces()
    .map((node) => extractInterface(node.compilerNode, checker, program));

  const aliases = sourceFile
    .getTypeAliases()
    .map((node) => extractAlias(node.compilerNode, checker));

  const enums = sourceFile
    .getEnums()
    .map((node) => extractEnum(node.compilerNode, checker));

  return { interfaces, aliases, enums };
}

/**
 * Resolve `import { X }` / `export { X }` re-export chains to the
 * underlying declaration symbol. TypeScript's type checker normally
 * inlines these when computing `type.symbol` (so `import { Foo as Bar }`
 * still gives `Foo_symbol`), but we apply this defensively at every
 * symbol-keying site so a future TS upgrade or edge case can't sneak
 * an import-binding symbol into the visited set under a different
 * identity than the one `extractInterface`/`extractAlias`/`extractEnum`
 * stash via `getSymbolAtLocation`.
 *
 * `SymbolFlags.Alias` is the import/export kind — distinct from
 * `SymbolFlags.TypeAlias` (`type X = ...`), which `getAliasedSymbol`
 * does NOT follow and which already canonicalizes through `type.symbol`
 * resolution.
 */
function canonicalSymbol(sym: ts.Symbol, checker: ts.TypeChecker): ts.Symbol {
  return sym.flags & ts.SymbolFlags.Alias ? checker.getAliasedSymbol(sym) : sym;
}

/**
 * True when every declaration of `symbol` lives in a source file the
 * extractor filters out — TS stdlib (`lib.*.d.ts`) or a node_modules
 * package. Used to drop interface fields whose type resolves only to
 * a foreign declaration: emitting an accessor rule against such a
 * type produces a dangling reference (the type itself is excluded
 * from `extractReferencedTypes`'s aggregate, so its declaration
 * never lands in the consumer document).
 *
 * Concrete trigger: `SynthCell.sourceFile?: ts.SourceFile` — the
 * `ts.SourceFile` symbol declares only inside `typescript.d.ts`. The
 * TS compiler API's own types are pure infrastructure on the ts2pant
 * side, never appear in user spec semantics, and should be invisible
 * to the consumer document.
 */
function isSymbolFromFilteredSourceOnly(
  symbol: ts.Symbol,
  program: ts.Program,
): boolean {
  const decls = symbol.declarations;
  if (!decls || decls.length === 0) {
    return false;
  }
  return decls.every((d) => {
    const sf = d.getSourceFile();
    return (
      program.isSourceFileDefaultLibrary(sf) ||
      program.isSourceFileFromExternalLibrary(sf)
    );
  });
}

/**
 * Walk a TS type to determine whether its named-type leaves all
 * resolve to foreign declarations. Used to skip interface fields
 * whose Pant emission would dangle.
 *
 * Unions strip nullish members (`null`, `undefined`, `void`) before
 * checking — those are handled by `mapTsType`'s list-lift and don't
 * count toward foreignness. So `ts.SourceFile | undefined` correctly
 * registers as foreign-only because the only non-nullish member is
 * the foreign `ts.SourceFile`.
 *
 * Returns false for primitives, literal types, and types with no
 * symbol — none of those produce dangling references.
 */
function typeRefsOnlyForeign(
  type: ts.Type,
  program: ts.Program,
  checker: ts.TypeChecker,
): boolean {
  const PRIMITIVE_FLAGS =
    ts.TypeFlags.String |
    ts.TypeFlags.Number |
    ts.TypeFlags.Boolean |
    ts.TypeFlags.StringLiteral |
    ts.TypeFlags.NumberLiteral |
    ts.TypeFlags.BooleanLiteral;
  const NULLISH_FLAGS =
    ts.TypeFlags.Null | ts.TypeFlags.Undefined | ts.TypeFlags.Void;
  if (type.flags & PRIMITIVE_FLAGS) {
    return false;
  }
  if (type.flags & NULLISH_FLAGS) {
    return false;
  }
  if (type.isUnion() || type.isIntersection()) {
    const nonNullish = type.types.filter(
      (t) => (t.flags & NULLISH_FLAGS) === 0,
    );
    if (nonNullish.length === 0) {
      return false;
    }
    return nonNullish.every((t) => typeRefsOnlyForeign(t, program, checker));
  }
  // Built-in containers (Array, ReadonlyArray, Set, ReadonlySet, Map,
  // ReadonlyMap, tuples) are handled natively by `mapTsType`. Their
  // symbols declare in TS lib, but the lowering doesn't fall through
  // to the symbol-name path — it produces `[T]` / `T * U` / etc. So
  // these are NOT foreign-only even though the constructor symbol is
  // from `lib.*.d.ts`. Recurse into the type arguments to check the
  // inner element types instead — but only the args that can carry a
  // named-type leaf. Primitive / nullish args (`string` in
  // `Map<string, ts.SourceFile>`) introduce no Pant references and so
  // shouldn't sink the foreign-only check; if every named-leaf-bearing
  // arg is foreign, the container's emitted shape still dangles.
  if (
    checker.isTupleType(type) ||
    checker.isArrayType(type) ||
    isSetType(type) ||
    isMapType(type)
  ) {
    const args = checker.getTypeArguments(type as ts.TypeReference);
    if (args.length === 0) {
      return false;
    }
    const namedArgs = args.filter(
      (a) => (a.flags & (PRIMITIVE_FLAGS | NULLISH_FLAGS)) === 0,
    );
    if (namedArgs.length === 0) {
      return false;
    }
    return namedArgs.every((a) => typeRefsOnlyForeign(a, program, checker));
  }
  const rawSymbol = type.aliasSymbol ?? type.symbol;
  if (!rawSymbol) {
    return false;
  }
  const symbol = canonicalSymbol(rawSymbol, checker);
  return isSymbolFromFilteredSourceOnly(symbol, program);
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

  // Track visited types by *symbol identity* rather than bare name so
  // two files exporting the same type name (e.g. `interface Options` in
  // unrelated modules) don't collide in the cross-file aggregator
  // below — visiting one would otherwise pull both into the consumer
  // document and emit duplicate Pant declarations.
  const visited = new Set<ts.Symbol>();

  // For class methods, the implicit `this` parameter is the enclosing
  // class type — collect its symbol the same way `collectNamedTypes`
  // would for a referenced parameter type.
  if (funcNode && ts.isMethodDeclaration(funcNode)) {
    const classDecl = funcNode.parent;
    if (ts.isClassDeclaration(classDecl) && classDecl.name) {
      const classSymbol = checker.getSymbolAtLocation(classDecl.name);
      if (classSymbol) {
        visited.add(canonicalSymbol(classSymbol, checker));
      }
    }
  }

  for (const param of signature.getParameters()) {
    collectNamedTypes(checker.getTypeOfSymbol(param), checker, visited);
  }
  collectNamedTypes(signature.getReturnType(), checker, visited);

  // Aggregate types from every project source file the consumer's
  // import graph reaches.
  //
  // `createSourceFile` adds only the one file the caller named; the
  // TS Program builds source-file objects for transitive imports
  // during type checking, but ts-morph's `getSourceFiles()` returns
  // only files explicitly added or resolved into the Project wrapper.
  // `resolveSourceFileDependencies()` is the canonical knob for
  // pulling those in — idempotent, so calling it here is safe even if
  // the caller pre-resolved.
  //
  // `collectNamedTypes` already walks the TS type graph via symbols,
  // so cross-file type names are already in `visited` — the bug
  // before this change was purely on the lookup side: the previous
  // `extractAllTypes(sourceFile)` only saw the consumer's file, so
  // any visited name whose declaration lives elsewhere got dropped.
  //
  // Filter out TS stdlib (`lib.*.d.ts`) and node_modules using TS's
  // own predicates so we don't pull in `Array`, `Promise`, etc.
  // declarations from `lib.es2022.d.ts`. The pattern mirrors
  // `src/builtins.ts:130-141`'s use of `isSourceFileDefaultLibrary`.
  const project = sourceFile.getProject();
  project.resolveSourceFileDependencies();
  const program = project.getProgram().compilerObject;
  const aggregated: ExtractedTypes = {
    interfaces: [],
    aliases: [],
    enums: [],
  };
  for (const sf of project.getSourceFiles()) {
    const compilerSf = sf.compilerNode;
    if (program.isSourceFileDefaultLibrary(compilerSf)) {
      continue;
    }
    if (program.isSourceFileFromExternalLibrary(compilerSf)) {
      continue;
    }
    const types = extractAllTypes(sf);
    aggregated.interfaces.push(...types.interfaces);
    aggregated.aliases.push(...types.aliases);
    aggregated.enums.push(...types.enums);
  }
  return {
    interfaces: aggregated.interfaces.filter(
      (i) => i.symbol !== undefined && visited.has(i.symbol),
    ),
    aliases: aggregated.aliases.filter(
      (a) => a.symbol !== undefined && visited.has(a.symbol),
    ),
    enums: aggregated.enums.filter(
      (e) => e.symbol !== undefined && visited.has(e.symbol),
    ),
  };
}

function extractInterface(
  node: ts.InterfaceDeclaration,
  checker: ts.TypeChecker,
  program: ts.Program,
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
      if (!symbol) {
        continue;
      }
      const type = checker.getTypeOfSymbol(symbol);
      // Skip properties whose type resolves only to foreign declarations
      // (TS stdlib / node_modules). The accessor rule for such a property
      // would emit a return type with no declaration in the consumer
      // document. ts2pant has no encoding for opaque foreign types, and
      // most fields like this (`SynthCell.sourceFile?: ts.SourceFile`)
      // are translation-time infrastructure that shouldn't be visible
      // in the spec semantics anyway.
      if (typeRefsOnlyForeign(type, program, checker)) {
        continue;
      }
      properties.push({ name: member.name.text, type });
    }
  }

  const rawSymbol = checker.getSymbolAtLocation(node.name);
  const symbol = rawSymbol ? canonicalSymbol(rawSymbol, checker) : undefined;
  return symbol ? { name, properties, symbol } : { name, properties };
}

function extractAlias(
  node: ts.TypeAliasDeclaration,
  checker: ts.TypeChecker,
): ExtractedAlias {
  const name = node.name.text;
  const type = checker.getTypeFromTypeNode(node.type);
  const rawSymbol = checker.getSymbolAtLocation(node.name);
  const symbol = rawSymbol ? canonicalSymbol(rawSymbol, checker) : undefined;
  return symbol ? { name, type, symbol } : { name, type };
}

function extractEnum(
  node: ts.EnumDeclaration,
  checker: ts.TypeChecker,
): ExtractedEnum {
  const name = node.name.text;
  const members: string[] = [];
  for (const member of node.members) {
    if (ts.isIdentifier(member.name)) {
      members.push(member.name.text);
    }
  }
  const rawSymbol = checker.getSymbolAtLocation(node.name);
  const symbol = rawSymbol ? canonicalSymbol(rawSymbol, checker) : undefined;
  return symbol ? { name, members, symbol } : { name, members };
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
  /**
   * The call-site identifier text — what the consumer body wrote at
   * the use site. With `import { ERR, ERR as Err }`, a single
   * declaration can be referenced by multiple local names; each
   * entry carries one local name so `paramNameMap` can route every
   * spelling to the same kebab'd Pant rule.
   */
  tsName: string;
  /** Kebab'd Pant name — what the call site emits. */
  pantName: string;
  /**
   * Pantagruel rule head (signature only). Populated on the first
   * entry per underlying declaration; subsequent entries (additional
   * local-name aliases for the same declaration) leave this undefined
   * so the consumer pipeline emits one rule head, not one per alias.
   */
  declaration?: PantRule;
  /**
   * Value equation binding the const to its initializer. Populated on
   * the first entry per underlying declaration alongside `declaration`;
   * alias entries leave this undefined.
   */
  equation?: PropResult;
}

/**
 * Translate a TS expression to a Pant `OpaqueExpr` if it's a
 * recognized literal. Transparently unwraps surface wrappers that
 * don't change the runtime value — `(expr)`, `expr as T`,
 * `<T>expr`, `expr satisfies T` — so idiomatic spellings like
 * `("hello")` or `"a" as const` reach the same literal target as
 * the bare form. Returns null for any other shape; the caller
 * silently filters those out of the module-const extraction.
 */
function translateConstInitializer(init: ts.Expression): OpaqueExpr | null {
  const ast = getAst();
  while (
    ts.isParenthesizedExpression(init) ||
    ts.isAsExpression(init) ||
    ts.isTypeAssertionExpression(init) ||
    ts.isSatisfiesExpression(init)
  ) {
    init = init.expression;
  }
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
 * Walk top-level `const NAME = <literal>` declarations referenced from
 * `functionName`'s body — both *local* (same file) and *imported*
 * (another in-project file). Each match becomes an
 * {@link ExtractedConst} carrying the 0-arity rule head + value
 * equation. The pipeline runs this after `translateSignature` so the
 * registry's already claimed the function's param names — collisions
 * resolve via numeric suffixing.
 *
 * Cross-file resolution mirrors `extractReferencedFunctions`: we walk
 * the body for free-form `Identifier` references, follow alias chains
 * via `getAliasedSymbol`, and accept any resolved value declaration
 * that points at a top-level `const` with a translatable initializer.
 * Without this, an `import { ERR } from "./dep"; return ERR;` body
 * fell through to a raw `ERR` Pant identifier and dangled.
 *
 * Filtering by *reference* (rather than emitting every top-level const)
 * keeps the output focused. Foreign declarations (TS lib, node_modules)
 * are skipped — those are EUF uninterpreted at the SMT layer.
 */
export function extractModuleConsts(
  sourceFile: SourceFile,
  functionName: string,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): ExtractedConst[] {
  const project = sourceFile.getProject();
  project.resolveSourceFileDependencies();
  const checker = getChecker(sourceFile);
  const program = project.getProgram().compilerObject;
  const ast = getAst();

  const fnNode = findFunctionNode(sourceFile, functionName);
  if (!fnNode?.body) {
    return [];
  }

  // Walk the body for identifier references that resolve through the
  // TS symbol resolver to a top-level `const` with a translatable
  // initializer. Map keys are the resolved declarations so duplicate
  // call sites collapse into a single rule head; the value (a Set of
  // local identifiers) preserves every call-site spelling so each one
  // gets its own `paramNameMap` rename. Without the alias spellings,
  // `import { ERR, ERR as Err }; ...ERR...; ...Err...` would only wire
  // up the first encountered name and the second would fall through to
  // a raw Pant identifier — same pattern as `extractReferencedFunctions`.
  const referenced = new Map<ts.VariableDeclaration, Set<string>>();
  function visit(n: ts.Node): void {
    if (ts.isIdentifier(n)) {
      const localCalleeName = n.text;
      let sym = checker.getSymbolAtLocation(n);
      // Follow `import { X }` / `export { X }` chains to the
      // underlying declaration's symbol — same pattern as
      // `extractReferencedFunctions`.
      if (sym && sym.flags & ts.SymbolFlags.Alias) {
        sym = checker.getAliasedSymbol(sym);
      }
      const decl = sym?.valueDeclaration;
      if (
        decl &&
        ts.isVariableDeclaration(decl) &&
        ts.isIdentifier(decl.name) &&
        decl.initializer
      ) {
        // Top-level `const` discipline: declaration must sit under a
        // VariableStatement directly inside a SourceFile, with the
        // `const` flag set on its declarationList. Body-local `const`s
        // are handled by the let-elimination machinery, not here.
        const stmt = decl.parent.parent;
        if (ts.isVariableStatement(stmt) && ts.isSourceFile(stmt.parent)) {
          const declFlags = ts.getCombinedNodeFlags(stmt.declarationList);
          const isConst = (declFlags & ts.NodeFlags.Const) !== 0;
          const declSf = stmt.parent;
          if (
            isConst &&
            !program.isSourceFileDefaultLibrary(declSf) &&
            !program.isSourceFileFromExternalLibrary(declSf) &&
            translateConstInitializer(decl.initializer) !== null
          ) {
            const localNames = referenced.get(decl) ?? new Set<string>();
            localNames.add(localCalleeName);
            referenced.set(decl, localNames);
          }
        }
      }
    }
    ts.forEachChild(n, visit);
  }
  visit(fnNode.body);

  const result: ExtractedConst[] = [];
  for (const [decl, localNames] of referenced) {
    if (!ts.isIdentifier(decl.name)) {
      continue;
    }
    const valueExpr = translateConstInitializer(decl.initializer!);
    if (valueExpr === null) {
      continue;
    }
    const tsType = checker.getTypeAtLocation(decl);
    const pantType = mapTsType(tsType, checker, strategy, synthCell);
    // Kebab from the *declaration's* name (not any local alias spelling)
    // so all aliases route to the same Pant rule. With
    // `import { ERR, ERR as Err }`, both `ERR` and `Err` should resolve
    // to one canonical kebab name, not two collision-suffixed copies.
    const baseName = toPantTermName(decl.name.text);
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
    let first = true;
    for (const localName of localNames) {
      result.push(
        first
          ? { tsName: localName, pantName, declaration, equation }
          : { tsName: localName, pantName },
      );
      first = false;
    }
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
  // Mirror `extractReferencedTypes`'s ambiguity check: when no class
  // hint was supplied and more than one class declares a method with
  // the requested name, refuse to bind to an arbitrary first match.
  // Otherwise const extraction could attach to the wrong method body
  // and pull in unrelated module consts.
  let matched: ts.MethodDeclaration | null = null;
  let methodMatches = 0;
  for (const cls of sourceFile.getClasses()) {
    if (classHint && cls.getName() !== classHint) {
      continue;
    }
    const methods = cls.getMethods().filter((m) => m.getName() === memberName);
    const method = methods.find((m) => m.hasBody()) ?? methods[0];
    if (method) {
      matched = method.compilerNode;
      methodMatches += 1;
      if (classHint) {
        return matched;
      }
    }
  }
  if (!classHint && methodMatches > 1) {
    throw new Error(
      `Ambiguous method name: ${memberName}. Use ClassName.methodName`,
    );
  }
  return matched;
}

/**
 * One user-defined function the consumer's body calls — extracted as
 * a Pant rule head (signature only, no body). The consumer's
 * translation needs the head declared so the call site's emitted
 * `App(Var("kebab-name"), args)` resolves; the function's body itself
 * stays opaque under EUF semantics. A future change could opt to also
 * translate the body for richer SMT reasoning, but signature-only is
 * sufficient to make `pant --check` accept the consumer's document.
 */
export interface ReferencedFunctionDecl {
  /**
   * The call-site identifier text — what the consumer body wrote at
   * the use site. With `import { foo as bar } from ...`, a single
   * declaration can be referenced by multiple local names (`foo`,
   * `bar`); each entry carries one local name so `paramNameMap` can
   * route every spelling to the same kebab'd Pant rule.
   */
  tsName: string;
  /** Kebab'd Pant name — what the call site emits. */
  pantName: string;
  /**
   * Pantagruel rule head (signature only, no body). Populated on the
   * first entry per underlying declaration; subsequent entries
   * (additional local-name aliases for the same declaration) leave
   * this undefined so the consumer pipeline emits one rule head, not
   * one per alias.
   */
  declaration?: PantRule;
}

/**
 * Walk a consumer function's body for free-function call expressions
 * (`fn(args)` where the callee is a bare Identifier) and resolve each
 * to its declaration via the TS symbol resolver. For each in-project
 * `FunctionDeclaration` (i.e., not in TS stdlib or node_modules and
 * not the consumer itself), translate its signature into a Pant rule
 * head via the existing `translateSignature` pipeline.
 *
 * Filters that drop a candidate:
 * - Method calls (`obj.method(...)`) — handled by the existing
 *   property-access dispatch and the JS_MATH / JS_STRING builtins.
 * - The consumer function itself — already emitted by the pipeline's
 *   primary signature pass (recursive calls are pre-existing rules).
 * - Ambient `declare function` decls — those flow through
 *   `extractAmbientFunctions` and the `*_AMBIENT` module path.
 * - Foreign declarations (TS stdlib, node_modules) — those are EUF
 *   uninterpreted at the SMT layer, or they go through builtins.ts.
 * - `translateSignature` returning unsupported / action — only pure
 *   rule shapes get emitted; mutating callees would need their own
 *   plumbing and aren't a dogfood concern today.
 *
 * Names are kebab'd at translation time (consistent with the call
 * dispatcher's `paramNames.get(...) ?? text` lookup), and the
 * pipeline thread the `tsName -> pantName` rename into
 * `paramNameMap` so call sites emit the kebab'd name. The rename
 * routing matches how `extractModuleConsts` already plumbs renames
 * for module-level constants.
 */
export function extractReferencedFunctions(
  sourceFile: SourceFile,
  consumerName: string,
  strategy: NumericStrategy,
  synthCell?: SynthCell,
): ReferencedFunctionDecl[] {
  const project = sourceFile.getProject();
  project.resolveSourceFileDependencies();
  const checker = getChecker(sourceFile);
  const program = project.getProgram().compilerObject;
  const consumerNode = findFunctionNode(sourceFile, consumerName);
  if (!consumerNode?.body) {
    return [];
  }

  // Collect (declaration, callSiteNames) pairs for in-project free-
  // function calls reached from the consumer's body. Keying by the
  // resolved declaration deduplicates aliased imports — `import {
  // foo as bar }; bar(); foo();` collapses to one rule head — while
  // the value (a Set of local identifiers) preserves every call-site
  // spelling so each one gets its own `paramNameMap` rename. Without
  // the alias spelling, body translation looks up `bar` and falls
  // through to a raw TS identifier, emitting an undeclared Pant
  // symbol.
  const referenced = new Map<ts.FunctionDeclaration, Set<string>>();
  function visit(n: ts.Node): void {
    if (ts.isCallExpression(n) && ts.isIdentifier(n.expression)) {
      const localCalleeName = n.expression.text;
      let sym = checker.getSymbolAtLocation(n.expression);
      // Follow import aliases. For an `import { foo } from "./mod.js"`
      // followed by `foo()`, `getSymbolAtLocation` returns the import
      // specifier symbol, not the underlying function declaration. The
      // alias chain ends at the original declaration's symbol.
      if (sym && sym.flags & ts.SymbolFlags.Alias) {
        sym = checker.getAliasedSymbol(sym);
      }
      const decl = sym?.declarations?.find(
        (d): d is ts.FunctionDeclaration =>
          ts.isFunctionDeclaration(d) && !!d.name,
      );
      if (decl && decl !== consumerNode) {
        const sf = decl.getSourceFile();
        if (
          !program.isSourceFileDefaultLibrary(sf) &&
          !program.isSourceFileFromExternalLibrary(sf) &&
          // Ambient `declare function` decls flow through
          // extractAmbientFunctions and the *_AMBIENT module path —
          // don't double-emit them as consumer-local rule heads.
          !decl.modifiers?.some((m) => m.kind === ts.SyntaxKind.DeclareKeyword)
        ) {
          const localNames = referenced.get(decl) ?? new Set<string>();
          localNames.add(localCalleeName);
          referenced.set(decl, localNames);
        }
      }
    }
    ts.forEachChild(n, visit);
  }
  visit(consumerNode.body);
  if (referenced.size === 0) {
    return [];
  }

  // Pre-build the compiler-SF -> ts-morph-SF map once. ts-morph has no
  // public reverse-lookup API, but each referenced sibling needs one
  // resolution; without the cache, the inner loop would re-scan
  // `getSourceFiles()` per sibling — O(siblings × files).
  const declSfMap = new Map<ts.SourceFile, SourceFile>();
  for (const sf of project.getSourceFiles()) {
    declSfMap.set(sf.compilerNode, sf);
  }
  const result: ReferencedFunctionDecl[] = [];
  for (const [decl, localNames] of referenced) {
    const tsMorphSf = declSfMap.get(decl.getSourceFile());
    if (!tsMorphSf) {
      continue;
    }
    // `translateSignature` looks up the function by its declared name
    // (the spelling in its source file); `localNames` are the call-
    // site spellings the consumer used.
    const declName = decl.name!.text;
    const sig = translateSignature(tsMorphSf, declName, strategy, synthCell);
    if (sig.declaration.kind !== "rule") {
      continue;
    }
    let first = true;
    for (const localName of localNames) {
      result.push(
        first
          ? {
              tsName: localName,
              pantName: sig.declaration.name,
              declaration: sig.declaration,
            }
          : {
              tsName: localName,
              pantName: sig.declaration.name,
            },
      );
      first = false;
    }
  }
  return result;
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
  visited: Set<ts.Symbol>,
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

  const rawSymbol = type.aliasSymbol ?? type.symbol;
  if (rawSymbol && !BUILTIN_NAMES.has(rawSymbol.name)) {
    const symbol = canonicalSymbol(rawSymbol, checker);
    if (visited.has(symbol)) {
      return;
    }
    visited.add(symbol);

    for (const prop of type.getProperties()) {
      const propType = checker.getTypeOfSymbol(prop);
      collectNamedTypes(propType, checker, visited);
    }
  }
}
