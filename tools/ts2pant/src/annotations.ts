// @archlint.module core
// @archlint.domain ts2pant.annotations

import type { SourceFile } from "ts-morph";
import ts from "typescript";
import { findFunction } from "./translate-signature.js";

/** A raw Pantagruel proposition extracted from a @pant annotation. */
export interface PantAnnotation {
  text: string;
}

/** A type override from a @pant-type annotation. */
export interface PantTypeOverride {
  name: string;
  type: string;
}

/** Result of extracting annotations from a function's JSDoc comments. */
export interface AnnotationResult {
  propositions: PantAnnotation[];
  typeOverrides: PantTypeOverride[];
}

/**
 * Three line-oriented forms are recognised inside a `/** ... *\/` block
 * attached to a function declaration:
 *
 *   `\@pant <proposition>`        — Pantagruel proposition (may span
 *                                    multiple continuation lines; the
 *                                    JSDoc parser folds them into the
 *                                    tag's comment field naturally).
 *   `\@pant-begin ... \@pant-end` — Equivalent to a multi-line `\@pant`;
 *                                    retained for source-level
 *                                    readability. Lexically the
 *                                    `\@pant-begin` tag's comment field
 *                                    already carries the block body;
 *                                    `\@pant-end` is a closing marker
 *                                    with no payload.
 *   `\@pant-type name: Type`      — type override for a TS parameter.
 *
 * All lexical concerns (comment-range scanning, `* ` decorator strip,
 * tag-name tokenisation, continuation-line folding) are delegated to
 * the TypeScript compiler's JSDoc parser. The only grammar this module
 * owns is the `name: Type` split for `\@pant-type` and the trivial
 * dispatch between the four tag kinds.
 */
export function extractAnnotations(node: ts.Node): AnnotationResult {
  const propositions: PantAnnotation[] = [];
  const typeOverrides: PantTypeOverride[] = [];

  for (const tags of jsDocTagBlocks(node)) {
    let openBlockText: string | null = null;

    for (const tag of tags) {
      const name = tag.tagName.text;
      if (openBlockText !== null) {
        if (name === "pant-end") {
          const text = openBlockText.trim();
          if (text.length > 0) {
            propositions.push({ text });
          }
          openBlockText = null;
        }
        // Any other tag while a block is open is silently dropped —
        // mixing additional tags inside a `@pant-begin`/`@pant-end`
        // block is not part of the supported grammar.
        continue;
      }

      switch (name) {
        case "pant": {
          const text = tagComment(tag).trim();
          if (text.length > 0) {
            propositions.push({ text });
          }
          break;
        }
        case "pant-begin":
          openBlockText = tagComment(tag);
          break;
        case "pant-end":
          // Stray `@pant-end` without a matching `@pant-begin`: drop.
          break;
        case "pant-type": {
          const override = parseTypeOverride(tagComment(tag));
          if (override !== null) {
            typeOverrides.push(override);
          }
          break;
        }
        default:
          // Foreign JSDoc tags (@param, @returns, etc.) are not part of
          // this module's surface and pass through untouched.
          break;
      }
    }
  }

  return { propositions, typeOverrides };
}

/**
 * Split a `@pant-type` tag's comment into `name` and `type`. Returns
 * `null` when the shape doesn't match `<name>:<type>` with non-empty
 * halves after trimming.
 */
function parseTypeOverride(comment: string): PantTypeOverride | null {
  const colonIdx = comment.indexOf(":");
  if (colonIdx < 0) {
    return null;
  }
  const name = comment.slice(0, colonIdx).trim();
  const type = comment.slice(colonIdx + 1).trim();
  if (name.length === 0 || type.length === 0) {
    return null;
  }
  return { name, type };
}

/**
 * Extract the plain-text comment payload of a JSDoc tag. The compiler
 * represents tag comments as either a flat string or a `NodeArray` of
 * `JSDocComment` parts (when the comment contains embedded constructs
 * like `{@link ...}`); both shapes flatten to a single string by
 * concatenating the parts' `.text`.
 */
function tagComment(tag: ts.JSDocTag): string {
  const c = tag.comment;
  if (c === undefined) {
    return "";
  }
  if (typeof c === "string") {
    return c;
  }
  return c.map((part) => part.text).join("");
}

/**
 * Collect every JSDoc tag declared on `node` itself (not on its
 * ancestors). `ts.getJSDocTags` walks the inheritance chain to support
 * `@inheritDoc`, which is not behaviour we want here — each annotated
 * function owns its own propositions.
 */
function jsDocTagBlocks(node: ts.Node): readonly (readonly ts.JSDocTag[])[] {
  const blocks: ts.JSDocTag[][] = [];
  for (const child of ts.getJSDocCommentsAndTags(node)) {
    if (ts.isJSDoc(child) && child.tags !== undefined) {
      blocks.push([...child.tags]);
    }
  }
  return blocks;
}

// ===========================================================================
// Function-by-name extraction (unchanged surface)
// ===========================================================================

/** Combined proposition texts + type overrides from a function's JSDoc. */
export interface FunctionAnnotations {
  propositionTexts: string[];
  typeOverrides: Map<string, string>;
}

/**
 * Find a function by name and extract both \@pant propositions and
 * \@pant-type overrides in a single JSDoc pass.
 */
export function extractFunctionAnnotationsAndOverrides(
  sourceFile: SourceFile,
  functionName: string,
): FunctionAnnotations {
  const { node } = findFunction(sourceFile, functionName);
  const result = extractAnnotations(node);
  return {
    propositionTexts: result.propositions.map((p) => p.text),
    typeOverrides: new Map(result.typeOverrides.map((o) => [o.name, o.type])),
  };
}

/**
 * Convenience wrapper: find a function by name and extract its \@pant
 * proposition texts as plain strings.
 *
 * @pant extractFunctionAnnotations sourceFile functionName = propositionTexts (extractFunctionAnnotationsAndOverrides sourceFile functionName).
 */
export function extractFunctionAnnotations(
  sourceFile: SourceFile,
  functionName: string,
): string[] {
  return extractFunctionAnnotationsAndOverrides(sourceFile, functionName)
    .propositionTexts;
}

/**
 * Convenience wrapper: find a function by name and extract its \@pant-type
 * overrides as a Map from TS parameter name to Pantagruel type string.
 */
export function extractFunctionTypeOverrides(
  sourceFile: SourceFile,
  functionName: string,
): Map<string, string> {
  return extractFunctionAnnotationsAndOverrides(sourceFile, functionName)
    .typeOverrides;
}
