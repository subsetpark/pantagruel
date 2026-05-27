import ts from "typescript";

export interface ExtractedBlockConstBinding {
  tsName: string;
  initializer: ts.Expression;
}

export interface ExtractedBlockReturn {
  bindings: readonly ExtractedBlockConstBinding[];
  returnExpr: ts.Expression;
}

/**
 * Extract a block shaped as `(const name = expr;)* return expr;`.
 *
 * This helper is deliberately TS-AST-only: purity, TDZ, and lowering checks
 * stay with the caller because pure bodies, mutating bodies, and switch
 * clauses have different surrounding contexts.
 */
export function extractBlockReturn(
  block: ts.Block,
): ExtractedBlockReturn | null {
  return extractBlockReturnFromStatements(block.statements);
}

export function extractBlockReturnFromStatements(
  statements: readonly ts.Statement[],
): ExtractedBlockReturn | null {
  if (statements.length === 0) {
    return null;
  }
  const last = statements[statements.length - 1]!;
  if (!ts.isReturnStatement(last) || last.expression === undefined) {
    return null;
  }

  const bindings: ExtractedBlockConstBinding[] = [];
  for (const stmt of statements.slice(0, -1)) {
    if (
      !ts.isVariableStatement(stmt) ||
      !(stmt.declarationList.flags & ts.NodeFlags.Const)
    ) {
      return null;
    }
    for (const decl of stmt.declarationList.declarations) {
      if (!ts.isIdentifier(decl.name) || decl.initializer === undefined) {
        return null;
      }
      bindings.push({
        tsName: decl.name.text,
        initializer: decl.initializer,
      });
    }
  }

  return { bindings, returnExpr: last.expression };
}
