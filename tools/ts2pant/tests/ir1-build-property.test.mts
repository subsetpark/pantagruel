// @archlint.module test
// @archlint.domain ts2pant.ir1-build-property

/**
 * Unit tests for the M5 Patch 1 property-access L1 builder
 * (`buildL1MemberAccess` in `src/ir1-build.ts`).
 *
 * The builder canonicalizes TS `PropertyAccessExpression` to L1
 * `Member(receiverL1, qualifiedName)`. The qualified name comes from
 * `qualifyFieldAccess` — resolved owners qualify to the rule name
 * (`account-balance`); unresolved-non-ambiguous types fall back to
 * the bare kebab'd field name; ambiguous unions reject with
 * `unsupported`.
 */

import assert from "node:assert/strict";
import { before, describe, it } from "node:test";
import { Project } from "ts-morph";
import ts from "typescript";
import {
  COMPILER_OPTIONS,
  createSourceFileFromSource,
  getChecker,
} from "../src/extract.js";
import { lowerExpr } from "../src/ir-emit.js";
import type { IR1Expr } from "../src/ir1.js";
import {
  buildL1MemberAccess,
  createL1AssumptionEnv,
  type L1BuildContext,
} from "../src/ir1-build.js";
import { lowerL1Expr } from "../src/ir1-lower.js";
import { getAst, loadAst } from "../src/pant-wasm.js";
import type { UniqueSupply } from "../src/supply.js";
import { qualifyFieldAccess } from "../src/translate-body.js";
import {
  cellEmitSynth,
  cellRegisterName,
  IntStrategy,
  newSynthCell,
  toPantTermName,
} from "../src/translate-types.js";

before(async () => {
  await loadAst();
});

interface PropertyAccessSetup {
  node: ts.PropertyAccessExpression;
  ctx: L1BuildContext;
}

interface AccessSetup {
  node: ts.PropertyAccessExpression | ts.ElementAccessExpression;
  ctx: L1BuildContext;
}

interface DependencyAccessSetup {
  sourceFile: ts.SourceFile;
  checker: ts.TypeChecker;
  ctx: L1BuildContext;
}

/**
 * Parse a function declaration, return the single property-access
 * expression in its `return` statement plus an L1 build context. The
 * `paramNames` allocation mirrors `translateSignature`'s parameter
 * registration so the test exercises real Pant-name lookups.
 */
function setup(source: string): PropertyAccessSetup {
  const r = setupAccess(source);
  if (!ts.isPropertyAccessExpression(r.node)) {
    throw new Error(
      `setup: expected a PropertyAccessExpression, got ${ts.SyntaxKind[r.node.kind]}`,
    );
  }
  return { node: r.node, ctx: r.ctx };
}

/**
 * Like `setup` but returns either a PropertyAccess or ElementAccess.
 * Used by Patch 3 tests that exercise the string-literal element-access
 * arm of `buildL1MemberAccess`.
 */
function setupAccess(source: string): AccessSetup {
  const sourceFile = createSourceFileFromSource(source);
  const checker = getChecker(sourceFile);
  const fn = sourceFile.compilerNode.statements.find(ts.isFunctionDeclaration);
  if (!fn?.body) {
    throw new Error("setup: expected a function declaration with a body");
  }
  const synthCell = newSynthCell();
  const paramNames = new Map<string, string>();
  for (const p of fn.parameters) {
    if (ts.isIdentifier(p.name)) {
      const pantName = cellRegisterName(synthCell, toPantTermName(p.name.text));
      paramNames.set(p.name.text, pantName);
    }
  }
  const supply: UniqueSupply = { n: 0, synthCell };
  const ctx: L1BuildContext = {
    checker,
    strategy: IntStrategy,
    paramNames,
    state: undefined,
    supply,
    env: createL1AssumptionEnv(),
  };
  const stmt = fn.body.statements[0];
  if (!stmt || !ts.isReturnStatement(stmt) || !stmt.expression) {
    throw new Error("setup: expected a return statement with an expression");
  }
  const expr = stmt.expression;
  if (
    !ts.isPropertyAccessExpression(expr) &&
    !ts.isElementAccessExpression(expr)
  ) {
    throw new Error(
      `setup: expected a Property/ElementAccessExpression, got ${ts.SyntaxKind[expr.kind]}`,
    );
  }
  return { node: expr, ctx };
}

function setupDependencySource(
  source: string,
  dependencyDeclarations: string,
): DependencyAccessSetup {
  const project = new Project({
    compilerOptions: COMPILER_OPTIONS,
    useInMemoryFileSystem: true,
  });
  project.createSourceFile(
    "/project/node_modules/dep/package.json",
    JSON.stringify({ name: "dep", types: "index.d.ts" }),
  );
  project.createSourceFile(
    "/project/node_modules/dep/index.d.ts",
    dependencyDeclarations,
  );
  const sourceFile = project.createSourceFile("/project/src/main.ts", source);
  project.resolveSourceFileDependencies();
  const checker = project.getTypeChecker().compilerObject;
  const fn = sourceFile.compilerNode.statements.find(ts.isFunctionDeclaration);
  if (!fn?.body) {
    throw new Error("setupDependencySource: expected function with body");
  }
  const synthCell = newSynthCell();
  const paramNames = new Map<string, string>();
  for (const p of fn.parameters) {
    if (ts.isIdentifier(p.name)) {
      const pantName = cellRegisterName(synthCell, toPantTermName(p.name.text));
      paramNames.set(p.name.text, pantName);
    }
  }
  const supply: UniqueSupply = { n: 0, synthCell };
  return {
    sourceFile: sourceFile.compilerNode,
    checker,
    ctx: {
      checker,
      strategy: IntStrategy,
      paramNames,
      state: undefined,
      supply,
      env: createL1AssumptionEnv(),
    },
  };
}

function returnExpression(sourceFile: ts.SourceFile): ts.Expression {
  const fn = sourceFile.statements.find(ts.isFunctionDeclaration);
  const stmt = fn?.body?.statements.find(ts.isReturnStatement);
  if (!stmt?.expression) {
    throw new Error("returnExpression: expected return expression");
  }
  return stmt.expression;
}

function collectAccesses(
  node: ts.Node,
): Array<ts.PropertyAccessExpression | ts.ElementAccessExpression> {
  const out: Array<ts.PropertyAccessExpression | ts.ElementAccessExpression> =
    [];
  const visit = (current: ts.Node): void => {
    if (
      ts.isPropertyAccessExpression(current) ||
      ts.isElementAccessExpression(current)
    ) {
      out.push(current);
    }
    current.forEachChild(visit);
  };
  visit(node);
  return out;
}

function expectMember(result: IR1Expr | { unsupported: string }): {
  receiver: IR1Expr;
  name: string;
} {
  if ("unsupported" in result) {
    throw new Error(`expected Member, got unsupported: ${result.unsupported}`);
  }
  if (result.kind !== "member") {
    throw new Error(`expected Member kind, got ${result.kind}`);
  }
  return { receiver: result.receiver, name: result.name };
}

describe("ir1-build-property", () => {
  it("simple PropertyAccess builds Member", () => {
    const { node, ctx } = setup(
      `interface User { readonly name: string; }
       function f(u: User): string {
         return u.name;
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    const { receiver, name } = expectMember(result);
    assert.equal(name, "user--name");
    assert.equal(receiver.kind, "var");
    assert.equal(receiver.name, "u");
  });

  it("nested PropertyAccess chain builds nested Member", () => {
    const { node, ctx } = setup(
      `interface Owner { readonly name: string; }
       interface Document { readonly owner: Owner; }
       function f(d: Document): string {
         return d.owner.name;
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    const outer = expectMember(result);
    assert.equal(outer.name, "owner--name");
    // Inner Member is the receiver — proves the chain composes as
    // nested Member trees rather than a single flat embedded wrap.
    const inner = expectMember(outer.receiver);
    assert.equal(inner.name, "document--owner");
    assert.equal(inner.receiver.kind, "var");
    assert.equal(inner.receiver.name, "d");
  });

  it("ambiguous receiver returns unsupported", () => {
    const { node, ctx } = setup(
      `interface A { readonly id: number; }
       interface B { readonly id: number; }
       function f(x: A | B): number {
         return x.id;
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    assert.ok(
      "unsupported" in result,
      `expected unsupported, got Member: ${JSON.stringify(result)}`,
    );
    if ("unsupported" in result) {
      assert.match(
        result.unsupported,
        /field access on a non-discriminated union is not expressible in Pantagruel/u,
      );
    }
  });

  it("qualifier matches legacy qualifyFieldAccess output", () => {
    const { node, ctx } = setup(
      `interface Account { readonly balance: number; }
       function f(a: Account): number {
         return a.balance;
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    const { name } = expectMember(result);
    // The legacy emission always qualified `Account.balance` →
    // `account-balance`; M5 Member must preserve the qualifier.
    const legacyQualifier = qualifyFieldAccess(
      ctx.checker.getTypeAtLocation(node.expression),
      "balance",
      ctx.checker,
      ctx.strategy,
      ctx.supply.synthCell,
    );
    assert.equal(name, legacyQualifier);
    assert.equal(name, "account--balance");
  });

  it("parenthesized receiver strips to identical Member", () => {
    // `(a).owner` must build to the same Member shape as `a.owner` —
    // the L1-layering paren-stripping invariant for property access.
    const bare = setup(
      `interface Owner { readonly name: string; }
       interface Document { readonly owner: Owner; }
       function f(d: Document): Owner {
         return d.owner;
       }`,
    );
    const paren = setup(
      `interface Owner { readonly name: string; }
       interface Document { readonly owner: Owner; }
       function f(d: Document): Owner {
         return (d).owner;
       }`,
    );
    const bareOpaque = getAst().strExpr(
      lowerExpr(
        lowerL1Expr(buildL1MemberAccess(bare.node, bare.ctx) as IR1Expr),
      ),
    );
    const parenOpaque = getAst().strExpr(
      lowerExpr(
        lowerL1Expr(buildL1MemberAccess(paren.node, paren.ctx) as IR1Expr),
      ),
    );
    assert.equal(parenOpaque, bareOpaque);
  });

  it("nested parenthesization strips fully", () => {
    // `((a)).owner` strips through every paren layer.
    const bare = setup(
      `interface Owner { readonly name: string; }
       interface Document { readonly owner: Owner; }
       function f(d: Document): Owner {
         return d.owner;
       }`,
    );
    const paren = setup(
      `interface Owner { readonly name: string; }
       interface Document { readonly owner: Owner; }
       function f(d: Document): Owner {
         return ((d)).owner;
       }`,
    );
    const bareOpaque = getAst().strExpr(
      lowerExpr(
        lowerL1Expr(buildL1MemberAccess(bare.node, bare.ctx) as IR1Expr),
      ),
    );
    const parenOpaque = getAst().strExpr(
      lowerExpr(
        lowerL1Expr(buildL1MemberAccess(paren.node, paren.ctx) as IR1Expr),
      ),
    );
    assert.equal(parenOpaque, bareOpaque);
  });

  it("ambiguous receiver under signature-mode falls back to bare kebab", () => {
    // The signature path consumes ambiguous-owner cases as best-effort
    // (a deleted local `qualifyFieldAccess` in `translate-signature.ts`
    // documented this asymmetry). Surface it on the helper itself via
    // the `ambiguousOwnerFallback: "bare-kebab"` option so downstream
    // optional analyses (guard extraction) don't bail the whole
    // analysis on a single ambiguous accessor.
    const { node, ctx } = setup(
      `interface A { readonly id: number; }
       interface B { readonly id: number; }
       function f(x: A | B): number {
         return x.id;
       }`,
    );
    const result = buildL1MemberAccess(node, ctx, {
      ambiguousOwnerFallback: "bare-kebab",
    });
    const { name } = expectMember(result);
    // Bare kebab fallback — `id` -> `id` (no hyphenation needed) but
    // the canonical reduction of `toPantTermName("id")` is `id`.
    assert.equal(name, "id");
  });

  it("type-erasure wrapper is not stripped", () => {
    // `(a as A).owner` must NOT strip the `as A` cast — the cast is
    // load-bearing for the TS-checker type used by `qualifyFieldAccess`.
    // The receiver is an `AsExpression`; the inner expression's
    // declared type is `unknown` so `qualifyFieldAccess` would not
    // resolve to `Document`. Stripping would silently change the
    // qualifier, so the implementation must leave the cast in place.
    const { node, ctx } = setup(
      `interface Owner { readonly name: string; }
       interface Document { readonly owner: Owner; }
       function f(a: unknown): Owner {
         return (a as Document).owner;
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    // The receiver of the property access is the `AsExpression` node;
    // `unwrapParens` does NOT step into it. So `qualifyFieldAccess` is
    // called with `Document`'s type (the asserted type), producing the
    // qualified `document-owner` rule — exactly what the user
    // intended by writing the cast. A stripping regression would
    // invoke `qualifyFieldAccess` on `unknown` and fall back to the
    // bare `owner` name, observable as a divergent qualifier here.
    const { name } = expectMember(result);
    assert.equal(name, "document--owner");
  });

  // -------------------------------------------------------------------------
  // M5 Patch 3: string-literal ElementAccess collapses to canonical Member;
  // computed ElementAccess rejects with the specific reason.
  // -------------------------------------------------------------------------

  it("string-literal ElementAccess builds Member", () => {
    const { node, ctx } = setupAccess(
      `interface User { readonly name: string; }
       function f(u: User): string {
         return u["name"];
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    const { receiver, name } = expectMember(result);
    assert.equal(name, "user--name");
    // Mirrors the dotted-access bottom-out: receiver is a non-property
    assert.equal(receiver.kind, "var");
    assert.equal(receiver.name, "u");
  });

  it("nested string-literal ElementAccess composes", () => {
    const { node, ctx } = setupAccess(
      `interface Owner { readonly name: string; }
       interface Document { readonly owner: Owner; }
       function f(d: Document): string {
         return d["owner"]["name"];
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    const outer = expectMember(result);
    assert.equal(outer.name, "owner--name");
    // Inner Member proves the nested chain composes through the
    // string-literal recursion gate, not flattens to one bare leaf.
    const inner = expectMember(outer.receiver);
    assert.equal(inner.name, "document--owner");
    assert.equal(inner.receiver.kind, "var");
    assert.equal(inner.receiver.name, "d");
  });

  it("computed Identifier ElementAccess rejects with reason", () => {
    const { node, ctx } = setupAccess(
      `interface Account { readonly balance: number; }
       function f(a: Account, k: keyof Account): unknown {
         return a[k];
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    assert.ok(
      "unsupported" in result,
      `expected unsupported, got Member: ${JSON.stringify(result)}`,
    );
    if ("unsupported" in result) {
      assert.match(result.unsupported, /computed property access/u);
    }
  });

  it("computed expression ElementAccess rejects with reason", () => {
    const { node, ctx } = setupAccess(
      `interface T { readonly f: number; }
       function f(t: T): unknown {
         return t[1 + 1];
       }`,
    );
    const result = buildL1MemberAccess(node, ctx);
    assert.ok(
      "unsupported" in result,
      `expected unsupported, got Member: ${JSON.stringify(result)}`,
    );
    if ("unsupported" in result) {
      assert.match(result.unsupported, /computed property access/u);
    }
  });

  it('obj.f and obj["f"] build identical Member trees', () => {
    // Snapshot equivalence: the two surface forms collapse to the same
    // canonical Member output. Compare the lowered OpaqueExpr
    // serialization rather than IR structure so the test exercises the
    // full lower-to-Pant pipeline.
    const dotted = setupAccess(
      `interface User { readonly name: string; }
       function f(u: User): string {
         return u.name;
       }`,
    );
    const indexed = setupAccess(
      `interface User { readonly name: string; }
       function f(u: User): string {
         return u["name"];
       }`,
    );
    const dottedOpaque = getAst().strExpr(
      lowerExpr(
        lowerL1Expr(buildL1MemberAccess(dotted.node, dotted.ctx) as IR1Expr),
      ),
    );
    const indexedOpaque = getAst().strExpr(
      lowerExpr(
        lowerL1Expr(buildL1MemberAccess(indexed.node, indexed.ctx) as IR1Expr),
      ),
    );
    assert.equal(indexedOpaque, dottedOpaque);
  });

  it("no-substitution template-literal ElementAccess builds Member", () => {
    // ``obj[`field`]`` is the second accepted literal-key form
    // (`elementAccessLiteralKey` accepts both `StringLiteral` and
    // `NoSubstitutionTemplateLiteral`); its lowered output must match
    // the dotted form byte-for-byte, locking the template-literal
    // path against accidental regression.
    const { node, ctx } = setupAccess(
      "interface User { readonly name: string; }\n" +
        "function f(u: User): string {\n" +
        "  return u[`name`];\n" +
        "}",
    );
    const result = buildL1MemberAccess(node, ctx);
    const { receiver, name } = expectMember(result);
    assert.equal(name, "user--name");
    assert.equal(receiver.kind, "var");
    assert.equal(receiver.name, "u");

    const dotted = setupAccess(
      `interface User { readonly name: string; }
       function f(u: User): string {
         return u.name;
       }`,
    );
    const dottedOpaque = getAst().strExpr(
      lowerExpr(
        lowerL1Expr(buildL1MemberAccess(dotted.node, dotted.ctx) as IR1Expr),
      ),
    );
    const templateOpaque = getAst().strExpr(
      lowerExpr(lowerL1Expr(result as IR1Expr)),
    );
    assert.equal(templateOpaque, dottedOpaque);
  });

  it("foreign receiver property access lowers to synthesized accessor", () => {
    const { sourceFile, ctx } = setupDependencySource(
      `
        import type { DependencyContainer } from "dep";
        export function f(c: DependencyContainer) {
          return c.items;
        }
      `,
      `
        export interface DependencyItem {
          readonly label: string;
          readonly ready: boolean;
        }
        export interface DependencyContainer {
          readonly items: readonly DependencyItem[];
        }
      `,
    );
    const expr = returnExpression(sourceFile);
    assert.ok(ts.isPropertyAccessExpression(expr));

    const result = buildL1MemberAccess(expr, ctx);
    const { receiver, name } = expectMember(result);
    assert.equal(name, "items");
    assert.equal(receiver.kind, "var");
    assert.equal(receiver.name, "c");
    assert.deepEqual(cellEmitSynth(ctx.supply.synthCell!).decls, [
      { kind: "domain", name: "ForeignDependencyContainer" },
      { kind: "domain", name: "ForeignDependencyItem" },
      {
        kind: "rule",
        name: "items",
        params: [{ name: "c", type: "ForeignDependencyContainer" }],
        returnType: "[ForeignDependencyItem]",
      },
    ]);
  });

  it("nested foreign reads compose through synthesized accessors", () => {
    const { sourceFile, ctx } = setupDependencySource(
      `
        import type { VariableDeclaration } from "dep";
        export function f(decl: VariableDeclaration) {
          return decl.name.text;
        }
      `,
      `
        export interface Identifier {
          readonly text: string;
        }
        export interface VariableDeclaration {
          readonly name: Identifier;
        }
      `,
    );
    const expr = returnExpression(sourceFile);
    assert.ok(ts.isPropertyAccessExpression(expr));

    const result = buildL1MemberAccess(expr, ctx);
    const outer = expectMember(result);
    assert.equal(outer.name, "identifier-text");
    const inner = expectMember(outer.receiver);
    assert.equal(inner.name, "name");
    assert.deepEqual(
      cellEmitSynth(ctx.supply.synthCell!).decls.filter(
        (decl) => decl.kind === "rule",
      ),
      [
        {
          kind: "rule",
          name: "identifier-text",
          params: [{ name: "i", type: "ForeignIdentifier" }],
          returnType: "String",
        },
        {
          kind: "rule",
          name: "name",
          params: [{ name: "d", type: "ForeignVariableDeclaration" }],
          returnType: "ForeignIdentifier",
        },
      ],
    );
  });

  it("repeated foreign reads share one synthesized declaration", () => {
    const { sourceFile, ctx } = setupDependencySource(
      `
        import type { DependencyContainer } from "dep";
        export function f(c: DependencyContainer) {
          return c.items.length + c.items.length;
        }
      `,
      `
        export interface DependencyItem { readonly label: string; }
        export interface DependencyContainer {
          readonly items: readonly DependencyItem[];
        }
      `,
    );
    const accesses = collectAccesses(returnExpression(sourceFile)).filter(
      (access): access is ts.PropertyAccessExpression =>
        ts.isPropertyAccessExpression(access) && access.name.text === "items",
    );
    assert.equal(accesses.length, 2);

    for (const access of accesses) {
      const result = buildL1MemberAccess(access, ctx);
      assert.equal(expectMember(result).name, "items");
    }
    const rules = cellEmitSynth(ctx.supply.synthCell!).decls.filter(
      (decl) => decl.kind === "rule" && decl.name === "items",
    );
    assert.equal(rules.length, 1);
  });

  it("declList.declarations lowers through a TypeScript dependency accessor", () => {
    const { sourceFile, ctx } = setupDependencySource(
      `
        import type { VariableDeclarationList } from "dep";
        export function f(declList: VariableDeclarationList) {
          return declList.declarations;
        }
      `,
      `
        export interface VariableDeclaration {
          readonly name: Identifier;
        }
        export interface Identifier {
          readonly text: string;
        }
        export interface VariableDeclarationList {
          readonly declarations: readonly VariableDeclaration[];
        }
      `,
    );
    const expr = returnExpression(sourceFile);
    assert.ok(ts.isPropertyAccessExpression(expr));

    const result = buildL1MemberAccess(expr, ctx);
    assert.equal(expectMember(result).name, "declarations");
    assert.deepEqual(
      cellEmitSynth(ctx.supply.synthCell!).decls.filter(
        (decl) => decl.kind === "rule",
      ),
      [
        {
          kind: "rule",
          name: "declarations",
          params: [{ name: "dl", type: "ForeignVariableDeclarationList" }],
          returnType: "[ForeignVariableDeclaration]",
        },
      ],
    );
  });

  it("unsupported foreign property type refuses without dangling rules", () => {
    const { sourceFile, ctx } = setupDependencySource(
      `
        import type { DependencyContainer } from "dep";
        export function f(c: DependencyContainer) {
          return c.metadata;
        }
      `,
      `
        export interface DependencyContainer {
          readonly metadata: () => void;
        }
      `,
    );
    const expr = returnExpression(sourceFile);
    assert.ok(ts.isPropertyAccessExpression(expr));

    const result = buildL1MemberAccess(expr, ctx);
    assert.ok("unsupported" in result);
    if ("unsupported" in result) {
      assert.match(result.unsupported, /foreign accessor \.metadata/u);
      assert.match(result.unsupported, /unsupported property type/u);
    }
    const decls = cellEmitSynth(ctx.supply.synthCell!).decls;
    assert.deepEqual(
      decls.filter((decl) => decl.kind === "rule"),
      [],
    );
  });
});
