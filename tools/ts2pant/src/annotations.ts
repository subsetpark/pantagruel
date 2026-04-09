import ts from "typescript";

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
 * Parse @pant annotations from raw JSDoc comment text (the content
 * between the opening and closing delimiters).
 *
 * Recognises three forms:
 *   @pant <proposition>           — single-line proposition
 *   @pant-begin ... @pant-end     — multi-line proposition block
 *   @pant-type name: Type         — numeric type override
 */
export function parseAnnotations(text: string): AnnotationResult {
  const propositions: PantAnnotation[] = [];
  const typeOverrides: PantTypeOverride[] = [];

  // Strip leading `*` and whitespace from each line (JSDoc formatting).
  const lines = text.split("\n").map((l) => l.replace(/^\s*\*?\s?/, ""));

  let inBlock = false;
  let blockLines: string[] = [];

  for (const line of lines) {
    const trimmed = line.trimEnd();

    if (inBlock) {
      if (/^@pant-end\b/.test(trimmed.trimStart())) {
        const blockText = blockLines.join("\n").trim();
        if (blockText) {
          propositions.push({ text: blockText });
        }
        inBlock = false;
        blockLines = [];
      } else {
        blockLines.push(trimmed);
      }
      continue;
    }

    if (/^@pant-begin\b/.test(trimmed.trimStart())) {
      inBlock = true;
      blockLines = [];
      continue;
    }

    // @pant-type name: Type
    const typeMatch = trimmed.match(/^\s*@pant-type\s+(\w+)\s*:\s*(.+)$/);
    if (typeMatch) {
      const typeName = typeMatch[2].trim();
      if (typeName) {
        typeOverrides.push({ name: typeMatch[1], type: typeName });
      }
      continue;
    }

    // @pant <proposition>  (but not @pant-begin, @pant-end, @pant-type)
    const pantMatch = trimmed.match(/^\s*@pant\s+(.+)$/);
    if (pantMatch) {
      const propText = pantMatch[1].trim();
      if (propText) {
        propositions.push({ text: propText });
      }
      continue;
    }
  }

  return { propositions, typeOverrides };
}

/**
 * Extract @pant annotations from JSDoc comments attached to a TypeScript
 * AST node. Walks all leading comment ranges and parses any that are
 * JSDoc-style (`/** ... *​/`).
 */
export function extractAnnotations(
  node: ts.Node,
  sourceFile: ts.SourceFile,
): AnnotationResult {
  const result: AnnotationResult = { propositions: [], typeOverrides: [] };

  const fullText = sourceFile.getFullText();
  const commentRanges = ts.getLeadingCommentRanges(
    fullText,
    node.getFullStart(),
  );

  if (!commentRanges) return result;

  for (const range of commentRanges) {
    const commentText = fullText.slice(range.pos, range.end);

    // Only process JSDoc-style comments.
    if (!commentText.startsWith("/**")) continue;

    // Strip /** and */ delimiters.
    const inner = commentText.slice(3, -2);
    const parsed = parseAnnotations(inner);

    result.propositions.push(...parsed.propositions);
    result.typeOverrides.push(...parsed.typeOverrides);
  }

  return result;
}
