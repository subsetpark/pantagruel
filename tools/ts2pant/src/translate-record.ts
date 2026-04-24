import ts from "typescript";
import { type NameRegistry, registerName } from "./name-registry.js";
import type { OpaqueExpr } from "./pant-ast.js";
import { getAst } from "./pant-wasm.js";
import {
  bodyExpr,
  isBodyEffect,
  isBodyUnsupported,
  translateBodyExpr,
  type UniqueSupply,
} from "./translate-body.js";
import {
  cellRegisterName,
  fieldRuleName,
  isAnonymousRecord,
  isMapType,
  isSetType,
  lookupMapKV,
  mapTsType,
  type NumericStrategy,
  resolveRecordOwner,
  type SynthCell,
  UNSUPPORTED_ANONYMOUS_RECORD,
} from "./translate-types.js";
import type { PropResult } from "./types.js";

/**
 * Translate a function whose body returns an object literal
 * `{ f1: e1, f2: e2 }` into one equation per field of the return type.
 * Each equation shape: `all <params> | f_i (fn <args>) = e_i.` — the
 * field's accessor rule applied to the function application is equated
 * to the field's initializer expression.
 *
 * `new Set()` in a `[T]` field position is special-cased as "empty set"
 * and emits an assertion `all x: T | not (x in f_i (fn <args>))` instead
 * of an equation, since Pantagruel has no empty-list literal.
 *
 * Requirements:
 *   - Return type must be a named TypeScript interface/class/alias whose
 *     accessor rules are already declared elsewhere in the document.
 *   - The object literal must supply exactly one PropertyAssignment or
 *     ShorthandPropertyAssignment per declared field of the return type.
 *   - Initializers must translate as pure expressions (or be the
 *     `new Set()` special case above).
 */
export function translateRecordReturn(
  lit: ts.ObjectLiteralExpression,
  functionName: string,
  params: Array<{ name: string; type: string }>,
  fnNode: ts.FunctionDeclaration | ts.MethodDeclaration,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  scopedParams: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  synthCell: SynthCell | undefined,
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
): PropResult[] {
  const ast = getAst();

  // Resolve return type. Use the signature's declared return rather than
  // the object literal's inferred type (which would be
  // `{ f1: SetLike, ... }` with contextual widening not applied).
  const sig = checker.getSignatureFromDeclaration(fnNode);
  const returnType = sig?.getReturnType();
  if (!returnType) {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — cannot resolve return type for record return`,
      },
    ];
  }
  const returnSymbol = returnType.aliasSymbol ?? returnType.symbol;
  const returnTypeName = returnSymbol?.getName();
  if (!returnTypeName) {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — cannot resolve return type name for record return`,
      },
    ];
  }
  // Anonymous record return: the `mapTsType` branch during signature
  // translation has already registered the shape with the synth cell,
  // so the synthesized domain and its accessor rules are declared by
  // the time the body is translated. The field-emission loop below
  // works unchanged — it iterates `returnType.getProperties()` (which
  // enumerates the anonymous shape's declared fields just as well as
  // an interface's) and emits one equation per field, applying each
  // accessor rule to the function application.
  //
  // `isAnonymousRecord` inspects the underlying structural symbol
  // (`type.getSymbol()?.getName() === "__type"`), so it matches both
  // bare anonymous returns and alias-backed ones (`type Point = {x, y}`)
  // — the alias name doesn't correspond to a declared Pantagruel
  // domain, so we must resolve through the synth even when `aliasSymbol`
  // is set. Reject only when the cell is missing or upstream synth
  // registration failed.
  if (isAnonymousRecord(returnType)) {
    if (!synthCell) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — anonymous record return requires a synth cell`,
        },
      ];
    }
    if (returnType.getCallSignatures().length > 0) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — callable anonymous return type`,
        },
      ];
    }
    if (returnType.getConstructSignatures().length > 0) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — constructible anonymous return type`,
        },
      ];
    }
    if (
      returnType.getStringIndexType() !== undefined ||
      returnType.getNumberIndexType() !== undefined
    ) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — index-signature anonymous return type (unbounded dictionary, not a finite record)`,
        },
      ];
    }
    // Re-run the (idempotent) synth-mapping to confirm registration
    // actually succeeded for this shape. If a field type is unmangleable
    // the synth returns the failure sentinel rather than a domain name,
    // and the body emission below would otherwise reference accessor
    // rules that were never declared.
    const mapped = mapTsType(returnType, checker, strategy, synthCell);
    if (mapped === UNSUPPORTED_ANONYMOUS_RECORD) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — anonymous record return shape could not be synthesized`,
        },
      ];
    }
  }

  // Collect declared fields. For named interfaces this is the declared
  // property list; for anonymous records it's the same shape seen through
  // the structural type's properties. Canonical order matches the synth's
  // sorted order (by field name) so the emission stays deterministic.
  const declaredFields = returnType
    .getProperties()
    .map((prop) => ({
      name: prop.getName(),
      type: checker.getTypeOfSymbolAtLocation(prop, fnNode),
    }))
    .sort((a, b) => a.name.localeCompare(b.name));

  // Index literal properties by name. Reject unsupported property kinds
  // and duplicate keys (the spec requires exactly one assignment per
  // declared field).
  const literalByName = new Map<string, ts.Expression>();
  for (const prop of lit.properties) {
    if (ts.isPropertyAssignment(prop)) {
      if (!ts.isIdentifier(prop.name) && !ts.isStringLiteral(prop.name)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — record return with computed or non-simple key`,
          },
        ];
      }
      if (literalByName.has(prop.name.text)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — record return repeats field '${prop.name.text}'`,
          },
        ];
      }
      literalByName.set(prop.name.text, prop.initializer);
    } else if (ts.isShorthandPropertyAssignment(prop)) {
      if (literalByName.has(prop.name.text)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — record return repeats field '${prop.name.text}'`,
          },
        ];
      }
      literalByName.set(prop.name.text, prop.name);
    } else {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — record return with spread/method/accessor property`,
        },
      ];
    }
  }

  // Every declared field must be supplied. Extra fields on the literal
  // are flagged separately below.
  const missing = declaredFields
    .filter((f) => !literalByName.has(f.name))
    .map((f) => f.name);
  if (missing.length > 0) {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — record return missing field(s): ${missing.join(", ")}`,
      },
    ];
  }
  const extras = [...literalByName.keys()].filter(
    (n) => !declaredFields.some((f) => f.name === n),
  );
  if (extras.length > 0) {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — record return has extra field(s): ${extras.join(", ")}`,
      },
    ];
  }

  const argExprs = params.map((p) => ast.var(p.name));
  const fnApp = ast.app(ast.var(functionName), argExprs);

  // Rule parameters are implicitly in scope for body propositions, so we
  // don't re-quantify over them — emission matches the existing single-
  // equation path (`larger a b = cond ...`). Only fresh binders introduced
  // by a field's translation (e.g., the empty-set membership binder) are
  // quantified explicitly.
  //
  // Empty-set binders are *serialized* into the final assertion, so they
  // must be valid Pantagruel identifiers and must not capture the function's
  // own params. The synthCell branch already avoids both issues: its
  // registry was seeded by translateSignature with every param name, so
  // `cellRegisterName(synthCell, "x")` returns `x1` when `x` is a param.
  // The fallback (no synthCell — direct callers / tests) seeds a local
  // registry from `params` to preserve the same guarantees.
  let localRegistry: NameRegistry = {
    used: new Set(params.map((p) => p.name)),
  };
  const allocEmittedBinder = (hint: string): string => {
    if (synthCell) {
      return cellRegisterName(synthCell, hint);
    }
    const r = registerName(localRegistry, hint);
    localRegistry = r.registry;
    return r.name;
  };

  return emitRecordEquations(
    lit,
    fnApp,
    returnType,
    declaredFields,
    functionName,
    checker,
    strategy,
    scopedParams,
    supply,
    synthCell,
    applyConst,
    allocEmittedBinder,
  );
}

/**
 * Emit one equation per field of a record-typed receiver. Shared between
 * the top-level function return (receiver = function application) and
 * nested object-literal initializers (receiver = accessor application on
 * the outer record). Recurses when a field's initializer is itself an
 * object literal — translating a literal value under Pantagruel's
 * observational discipline requires decomposing it into per-accessor
 * equations, since there's no record-constructor expression to fall back
 * on.
 */
function emitRecordEquations(
  lit: ts.ObjectLiteralExpression,
  receiverExpr: OpaqueExpr,
  receiverType: ts.Type,
  declaredFields: Array<{ name: string; type: ts.Type }>,
  functionName: string,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  scopedParams: ReadonlyMap<string, string>,
  supply: UniqueSupply,
  synthCell: SynthCell | undefined,
  applyConst: (e: OpaqueExpr) => OpaqueExpr,
  allocEmittedBinder: (hint: string) => string,
): PropResult[] {
  // Stage A (named interface receiver): Map fields encode as a pair of
  // binary rules — `<field>Key(receiver, k)` membership predicate plus a
  // V-valued rule guarded by it (translate-types.ts interface-field branch).
  // Stage B (synthesized anonymous record receiver): Map fields encode as
  // a unary accessor returning a synthesized `KToVMap` domain, and the
  // map's key predicate lives on the synthesized domain, not the record.
  // We branch on this per Map-valued field below.
  const receiverIsAnon = isAnonymousRecord(receiverType);
  // Accessor rule symbols are qualified with the owning domain so that
  // distinct interfaces / synth shapes with a same-named field produce
  // distinct arity-1 rules under Pantagruel's positional coherence. The
  // same resolver runs at declaration time (translate-types.ts) and
  // at this body-emission site, so the two stay in lockstep for both
  // named interfaces and synthesized record domains.
  const ownerName = resolveRecordOwner(
    receiverType,
    checker,
    strategy,
    synthCell,
  );
  const ruleSymbol = (fieldName: string): string =>
    ownerName ? fieldRuleName(ownerName, fieldName) : fieldName;
  const ast = getAst();

  // Re-index the literal's properties by name for this level. Nested
  // calls each index their own literal. Apply the same exact-field
  // contract as the top-level record return: reject unsupported property
  // kinds, duplicate keys, missing fields, and extra fields.
  const literalByName = new Map<string, ts.Expression>();
  for (const prop of lit.properties) {
    if (ts.isPropertyAssignment(prop)) {
      if (!ts.isIdentifier(prop.name) && !ts.isStringLiteral(prop.name)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — nested record literal with computed or non-simple key`,
          },
        ];
      }
      if (literalByName.has(prop.name.text)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — nested record literal repeats field '${prop.name.text}'`,
          },
        ];
      }
      literalByName.set(prop.name.text, prop.initializer);
    } else if (ts.isShorthandPropertyAssignment(prop)) {
      if (literalByName.has(prop.name.text)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — nested record literal repeats field '${prop.name.text}'`,
          },
        ];
      }
      literalByName.set(prop.name.text, prop.name);
    } else {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — nested record literal with spread/method/accessor property`,
        },
      ];
    }
  }

  const extras = [...literalByName.keys()].filter(
    (n) => !declaredFields.some((f) => f.name === n),
  );
  if (extras.length > 0) {
    return [
      {
        kind: "unsupported",
        reason: `${functionName} — nested record literal has extra field(s): ${extras.join(", ")}`,
      },
    ];
  }

  const results: PropResult[] = [];
  for (const field of declaredFields) {
    const initializer = literalByName.get(field.name);
    if (!initializer) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — nested record literal missing field '${field.name}'`,
        },
      ];
    }
    const fieldApp = ast.app(ast.var(ruleSymbol(field.name)), [receiverExpr]);

    // `new Set()` → empty-set membership negation.
    if (isEmptySetConstruction(initializer)) {
      const elemType = getSetElementTypeName(
        field.type,
        checker,
        strategy,
        synthCell,
      );
      if (!elemType) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — new Set() initializer on non-set field '${field.name}'`,
          },
        ];
      }
      const binderName = allocEmittedBinder("x");
      const binderParam = ast.param(binderName, ast.tName(elemType));
      const body = ast.unop(
        ast.opNot(),
        ast.binop(ast.opIn(), ast.var(binderName), fieldApp),
      );
      results.push({
        kind: "assertion",
        quantifiers: [binderParam],
        body,
      });
      continue;
    }

    // `new Map()` → empty-map key-predicate negation. The predicate being
    // negated differs between Stage A and Stage B (see receiverIsAnon
    // comment above):
    //   Stage A: `all k: K | ~(<field>Key receiver k)` — the interface's
    //     own binary membership predicate.
    //   Stage B: `all k: K | ~(<kToVMapKey> (field receiver) k)` — the
    //     synthesized map's key predicate, applied to the synth-domain
    //     value that the unary accessor returns.
    if (isEmptyMapConstruction(initializer)) {
      if (!isMapType(field.type)) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — new Map() initializer on non-map field '${field.name}'`,
          },
        ];
      }
      const keyType = getMapKeyTypeName(
        field.type,
        checker,
        strategy,
        synthCell,
      );
      if (!keyType) {
        return [
          {
            kind: "unsupported",
            reason: `${functionName} — new Map() initializer on non-map field '${field.name}'`,
          },
        ];
      }
      const binderName = allocEmittedBinder("k");
      const binderParam = ast.param(binderName, ast.tName(keyType));
      let keyPredApp: OpaqueExpr;
      if (receiverIsAnon) {
        const vType = getMapValueTypeName(
          field.type,
          checker,
          strategy,
          synthCell,
        );
        const synthEntry =
          synthCell && vType
            ? lookupMapKV(synthCell.synth, keyType, vType)
            : undefined;
        if (!synthEntry) {
          return [
            {
              kind: "unsupported",
              reason: `${functionName} — new Map() on anonymous record field '${field.name}' with unregistered synth entry`,
            },
          ];
        }
        keyPredApp = ast.app(ast.var(synthEntry.names.keyPred), [
          fieldApp,
          ast.var(binderName),
        ]);
      } else {
        keyPredApp = ast.app(ast.var(`${ruleSymbol(field.name)}-key`), [
          receiverExpr,
          ast.var(binderName),
        ]);
      }
      const body = ast.unop(ast.opNot(), keyPredApp);
      results.push({
        kind: "assertion",
        quantifiers: [binderParam],
        body,
      });
      continue;
    }

    // Non-empty Map-valued initializer reaches the generic equation path
    // below as a unary `field(receiver) = <init>`. That shape is only
    // correct when the accessor is unary (Stage B) and the RHS has the
    // synthesized domain type; it's always wrong for Stage A binary
    // accessors, and for Stage B we don't currently translate any
    // Map-producing expression other than `new Map()`. Reject both up
    // front rather than emit invalid Pantagruel.
    if (isMapType(field.type)) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName} — Map-valued field '${field.name}' with non-empty initializer`,
        },
      ];
    }

    // Nested object literal → recursively decompose into per-subfield
    // equations with `fieldApp` as the new receiver. Pantagruel has no
    // record-constructor expression; the only way to specify a record
    // value is observationally (equations on its accessors).
    if (ts.isObjectLiteralExpression(initializer)) {
      const subFields = field.type
        .getProperties()
        .map((prop) => ({
          name: prop.getName(),
          type: checker.getTypeOfSymbolAtLocation(prop, initializer),
        }))
        .sort((a, b) => a.name.localeCompare(b.name));
      const subResults = emitRecordEquations(
        initializer,
        fieldApp,
        field.type,
        subFields,
        `${functionName}.${field.name}`,
        checker,
        strategy,
        scopedParams,
        supply,
        synthCell,
        applyConst,
        allocEmittedBinder,
      );
      const subUnsupported = subResults.find((p) => p.kind === "unsupported");
      if (subUnsupported) {
        return [subUnsupported];
      }
      results.push(...subResults);
      continue;
    }

    const body = translateBodyExpr(
      initializer,
      checker,
      strategy,
      scopedParams,
      undefined,
      supply,
    );
    if (isBodyUnsupported(body)) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName}.${field.name} — ${body.unsupported}`,
        },
      ];
    }
    if (isBodyEffect(body)) {
      return [
        {
          kind: "unsupported",
          reason: `${functionName}.${field.name} — effect outside statement position`,
        },
      ];
    }
    const rhs = applyConst(bodyExpr(body));
    results.push({
      kind: "equation",
      quantifiers: [],
      lhs: fieldApp,
      rhs,
    });
  }

  return results;
}

/** `new Set()` (zero args) — the only Set construction form we currently
 * translate. `new Set(iterable)` and subclass constructors are rejected. */
function isEmptySetConstruction(expr: ts.Expression): boolean {
  return (
    ts.isNewExpression(expr) &&
    ts.isIdentifier(expr.expression) &&
    expr.expression.text === "Set" &&
    (expr.arguments === undefined || expr.arguments.length === 0)
  );
}

/** Return the Pantagruel type name of `T` in a `Set<T>` / `ReadonlySet<T>`
 * / `[T]` (array) field, or null if the field isn't set-shaped. */
function getSetElementTypeName(
  fieldType: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
): string | null {
  if (isSetType(fieldType) || checker.isArrayType(fieldType)) {
    const typeArgs = checker.getTypeArguments(fieldType as ts.TypeReference);
    if (typeArgs.length === 1) {
      return mapTsType(typeArgs[0]!, checker, strategy, synthCell);
    }
  }
  return null;
}

/** `new Map()` (zero args) — the only Map construction form we currently
 * translate. `new Map(iterable)` and subclass constructors are rejected. */
function isEmptyMapConstruction(expr: ts.Expression): boolean {
  return (
    ts.isNewExpression(expr) &&
    ts.isIdentifier(expr.expression) &&
    expr.expression.text === "Map" &&
    (expr.arguments === undefined || expr.arguments.length === 0)
  );
}

/** Return the Pantagruel type name of `K` in a `Map<K, V>` /
 * `ReadonlyMap<K, V>` field, or null if the field isn't map-shaped. */
function getMapKeyTypeName(
  fieldType: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
): string | null {
  if (isMapType(fieldType)) {
    const typeArgs = checker.getTypeArguments(fieldType as ts.TypeReference);
    if (typeArgs.length === 2) {
      return mapTsType(typeArgs[0]!, checker, strategy, synthCell);
    }
  }
  return null;
}

/** Return the Pantagruel type name of `V` in a `Map<K, V>` /
 * `ReadonlyMap<K, V>` field, or null if the field isn't map-shaped. */
function getMapValueTypeName(
  fieldType: ts.Type,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  synthCell: SynthCell | undefined,
): string | null {
  if (isMapType(fieldType)) {
    const typeArgs = checker.getTypeArguments(fieldType as ts.TypeReference);
    if (typeArgs.length === 2) {
      return mapTsType(typeArgs[1]!, checker, strategy, synthCell);
    }
  }
  return null;
}
