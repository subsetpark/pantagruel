import ts from "typescript";
import type { PantProposition, PantDeclaration } from "./types.js";
import { mapTsType, type NumericStrategy } from "./translate-types.js";
import { translateExpr, translateOperator, findFunction, classifyFunction, shortParamName } from "./translate-signature.js";

function isUnsupported(s: string): boolean {
  return s.startsWith("> UNSUPPORTED:");
}

/** Generate a binder name not already used by params. */
function freshBinder(paramNames: Map<string, string>): string {
  const used = new Set(paramNames.values());
  for (const candidate of ["x", "y", "z", "w", "v", "u", "t"]) {
    if (!used.has(candidate)) return candidate;
  }
  let i = 0;
  while (used.has(`x${i}`)) i++;
  return `x${i}`;
}

export interface TranslateBodyOptions {
  program: ts.Program;
  fileName: string;
  functionName: string;
  strategy: NumericStrategy;
  /** Declarations in scope — used for frame condition generation. */
  declarations?: PantDeclaration[];
}

/**
 * Translate a TypeScript function body to Pantagruel propositions.
 *
 * Pure functions: return expression becomes `all params | f args = <expr>`.
 * Mutating functions: property assignments become primed propositions,
 * plus frame conditions for unmodified rules.
 */
export function translateBody(opts: TranslateBodyOptions): PantProposition[] {
  const { program, fileName, functionName, strategy, declarations } = opts;
  const checker = program.getTypeChecker();
  const { node, className } = findFunction(program, fileName, functionName);
  const classification = classifyFunction(node, checker);

  // Build param name map (same logic as translateSignature)
  const paramNames = new Map<string, string>();
  const paramList: Array<{ name: string; type: string }> = [];

  const sig = checker.getSignatureFromDeclaration(node);

  if (className) {
    const existingParamNames = new Set(sig ? sig.getParameters().map((p) => p.name) : []);
    const pName = shortParamName(className, existingParamNames);
    paramNames.set("this", pName);
    paramList.push({ name: pName, type: className });
  }

  if (sig) {
    for (const param of sig.getParameters()) {
      const paramType = checker.getTypeOfSymbol(param);
      const typeName = mapTsType(paramType, checker, strategy);
      paramNames.set(param.name, param.name);
      paramList.push({ name: param.name, type: typeName });
    }
  }

  if (!node.body) return [];

  if (classification === "pure") {
    return translatePureBody(node, functionName, paramList, checker, strategy, paramNames);
  } else {
    return translateMutatingBody(node, functionName, paramList, checker, strategy, paramNames, declarations ?? []);
  }
}

function translatePureBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  functionName: string,
  params: Array<{ name: string; type: string }>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): PantProposition[] {
  if (!node.body) return [];

  const extracted = extractReturnExpression(node.body);
  if (!extracted) {
    const reason = describeRejectedBody(node.body);
    return [{ text: `> UNSUPPORTED: ${functionName} — ${reason}` }];
  }

  // Translate const bindings and add to paramNames for inline substitution
  const extendedParams = new Map(paramNames);
  for (const binding of extracted.bindings) {
    const translated = translateBodyExpr(binding.initializer, checker, strategy, extendedParams);
    if (isUnsupported(translated)) {
      return [{ text: translated }];
    }
    // Wrap compound expressions in parens to preserve grouping
    extendedParams.set(binding.name, translated.includes(" ") ? `(${translated})` : translated);
  }

  const args = params.map((p) => p.name).join(" ");
  const bindings = params.map((p) => `${p.name}: ${p.type}`).join(", ");
  const body = translateBodyExpr(extracted.returnExpr, checker, strategy, extendedParams);

  if (body.startsWith("> UNSUPPORTED:")) {
    return [{ text: body }];
  }

  const head = bindings ? `all ${bindings} | ` : "";
  const call = args ? `${functionName} ${args}` : functionName;
  return [{ text: `${head}${call} = ${body}` }];
}

interface ExtractedBody {
  bindings: Array<{ name: string; initializer: ts.Expression }>;
  returnExpr: ts.Expression;
}

/**
 * Extract the return expression from a function body, collecting any leading
 * const bindings with pure initializers for inline substitution.
 * Handles:
 *   - Single return statement
 *   - Leading const bindings + return statement
 *   - if/else with returns in both branches (produces a synthetic conditional)
 * Returns null if the body contains let/var bindings or effectful const initializers.
 */
function extractReturnExpression(body: ts.Block): ExtractedBody | null {
  // Skip guard statements (if-throw patterns) and find the meaningful return
  const stmts = body.statements.filter((s) => !isGuardStatement(s));

  if (stmts.length === 0) return null;

  const bindings: Array<{ name: string; initializer: ts.Expression }> = [];
  let i = 0;

  // Collect leading const bindings
  for (; i < stmts.length - 1; i++) {
    const stmt = stmts[i];
    if (!ts.isVariableStatement(stmt)) break;

    const declList = stmt.declarationList;
    // Reject let/var
    if (!(declList.flags & ts.NodeFlags.Const)) return null;

    for (const decl of declList.declarations) {
      // Must have a simple identifier name and an initializer
      if (!ts.isIdentifier(decl.name) || !decl.initializer) return null;
      // Reject effectful initializers
      if (expressionHasSideEffects(decl.initializer)) return null;
      bindings.push({ name: decl.name.text, initializer: decl.initializer });
    }
  }

  // The last statement must be a return or if/else-with-returns
  const last = stmts[i];
  // If we didn't consume all preceding statements as const bindings, reject
  if (i < stmts.length - 1) return null;

  if (ts.isReturnStatement(last) && last.expression) {
    return { bindings, returnExpr: last.expression };
  }
  if (ts.isIfStatement(last) && last.elseStatement) {
    return { bindings, returnExpr: last };
  }

  return null;
}

function describeRejectedBody(body: ts.Block): string {
  const stmts = body.statements.filter((s) => !isGuardStatement(s));
  if (stmts.length === 0) return "empty body";
  if (stmts.length > 1) {
    // Check for specific rejection reasons in leading statements
    for (const stmt of stmts) {
      if (ts.isVariableStatement(stmt)) {
        const declList = stmt.declarationList;
        if (!(declList.flags & ts.NodeFlags.Const)) {
          return "let/var bindings not supported";
        }
        for (const decl of declList.declarations) {
          if (decl.initializer && expressionHasSideEffects(decl.initializer)) {
            return "const binding with side-effectful initializer";
          }
        }
      }
    }
    return "local bindings or multiple statements before return";
  }
  const stmt = stmts[0];
  if (ts.isReturnStatement(stmt) && !stmt.expression) return "return without expression";
  return "non-translatable control flow";
}

function isGuardStatement(stmt: ts.Statement): boolean {
  if (!ts.isIfStatement(stmt)) return false;
  // if (...) { throw } without else
  if (!stmt.elseStatement && blockThrows(stmt.thenStatement)) {
    return !expressionHasSideEffects(stmt.expression);
  }
  // if (...) { ... } else { throw }
  if (stmt.elseStatement && blockThrows(stmt.elseStatement)) {
    // Only a guard if the condition and then-block have no side effects.
    // A side-effectful condition like `if (audit(a)) { throw ... }` must not be
    // skipped. A mutating then-branch like `if (ok) { a.balance = 1; } else { throw e; }`
    // must NOT be classified as a guard — collectAssignments() needs to see it.
    return (
      !expressionHasSideEffects(stmt.expression) &&
      blockHasNoSideEffects(stmt.thenStatement) &&
      !blockReturns(stmt.thenStatement)
    );
  }
  return false;
}

function variableStatementHasNoSideEffects(stmt: ts.VariableStatement): boolean {
  return stmt.declarationList.declarations.every(
    (decl) =>
      !decl.initializer || !expressionHasSideEffects(decl.initializer),
  );
}

function blockThrows(node: ts.Statement): boolean {
  if (ts.isThrowStatement(node)) return true;
  if (ts.isBlock(node)) {
    const stmts = node.statements;
    if (stmts.length === 0) return false;
    // Last statement must be a throw; all preceding must be side-effect-free
    // (variable declarations for building the error message, etc.)
    if (!ts.isThrowStatement(stmts[stmts.length - 1])) return false;
    return stmts
      .slice(0, -1)
      .every((s) => ts.isVariableStatement(s) && variableStatementHasNoSideEffects(s));
  }
  return false;
}

function blockReturns(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.some((s) => ts.isReturnStatement(s));
  }
  return ts.isReturnStatement(node);
}

/** Check that a statement/block contains no assignments or property writes. */
function blockHasNoSideEffects(node: ts.Statement): boolean {
  if (ts.isBlock(node)) {
    return node.statements.every((s) => blockHasNoSideEffects(s));
  }
  if (ts.isExpressionStatement(node)) {
    return !expressionHasSideEffects(node.expression);
  }
  // Variable declarations are fine only if initializers have no side effects
  if (ts.isVariableStatement(node)) {
    return variableStatementHasNoSideEffects(node);
  }
  // Return statements, throw statements are fine
  if (ts.isReturnStatement(node) || ts.isThrowStatement(node)) {
    return true;
  }
  // if/for/while/switch may contain mutations — treat as side-effectful
  return false;
}

/** Unwrap parentheses, type assertions, and non-null assertions to get the inner expression. */
function unwrapExpression(expr: ts.Expression): ts.Expression {
  while (
    ts.isParenthesizedExpression(expr) ||
    ts.isAsExpression(expr) ||
    ts.isSatisfiesExpression(expr) ||
    ts.isNonNullExpression(expr)
  ) {
    expr = expr.expression;
  }
  return expr;
}

function expressionHasSideEffects(expr: ts.Expression): boolean {
  expr = unwrapExpression(expr);

  if (ts.isBinaryExpression(expr)) {
    // Any assignment operator
    return (
      (expr.operatorToken.kind >= ts.SyntaxKind.EqualsToken &&
        expr.operatorToken.kind <= ts.SyntaxKind.CaretEqualsToken) ||
      expressionHasSideEffects(expr.left) ||
      expressionHasSideEffects(expr.right)
    );
  }
  if (ts.isCallExpression(expr)) return true;
  if (ts.isNewExpression(expr) || ts.isAwaitExpression(expr)) return true;
  if (ts.isPrefixUnaryExpression(expr) || ts.isPostfixUnaryExpression(expr)) {
    const op = expr.operator;
    return (
      op === ts.SyntaxKind.PlusPlusToken ||
      op === ts.SyntaxKind.MinusMinusToken ||
      expressionHasSideEffects(expr.operand)
    );
  }
  return ts.forEachChild(
    expr,
    (child) => (ts.isExpression(child) ? expressionHasSideEffects(child) : false),
  ) ?? false;
}

/**
 * Translate a TS expression to Pantagruel syntax, extending the base
 * translateExpr with support for ternary, array ops, and if/else as cond.
 */
export function translateBodyExpr(
  expr: ts.Expression | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): string {
  if (ts.isExpression(expr)) {
    expr = unwrapExpression(expr);
  }

  // if/else statement -> cond
  if (ts.isIfStatement(expr)) {
    return translateIfStatement(expr, checker, strategy, paramNames);
  }

  // Ternary: a ? b : c -> cond a => b, true => c
  if (ts.isConditionalExpression(expr)) {
    const cond = translateBodyExpr(expr.condition, checker, strategy, paramNames);
    if (isUnsupported(cond)) return cond;
    const whenTrue = translateBodyExpr(expr.whenTrue, checker, strategy, paramNames);
    if (isUnsupported(whenTrue)) return whenTrue;
    const whenFalse = translateBodyExpr(expr.whenFalse, checker, strategy, paramNames);
    if (isUnsupported(whenFalse)) return whenFalse;
    return `cond ${cond} => ${whenTrue}, true => ${whenFalse}`;
  }

  // Property access with special array operations
  if (ts.isPropertyAccessExpression(expr)) {
    const prop = expr.name.text;
    const obj = translateBodyExpr(expr.expression, checker, strategy, paramNames);
    if (isUnsupported(obj)) return obj;
    // .length -> #obj (array only)
    if (prop === "length") {
      const receiverType = checker.getTypeAtLocation(expr.expression);
      if (checker.isArrayType(receiverType)) {
        return `#${obj}`;
      }
    }
    // Regular property access: a.balance -> balance a
    return `${prop} ${obj}`;
  }

  // Call expression: handle .includes(), .filter().map(), etc.
  if (ts.isCallExpression(expr)) {
    return translateCallExpr(expr, checker, strategy, paramNames);
  }

  // Prefix unary: !x -> ~(x), -x -> -(x)
  if (ts.isPrefixUnaryExpression(expr)) {
    const operand = translateBodyExpr(expr.operand, checker, strategy, paramNames);
    if (isUnsupported(operand)) return operand;
    if (expr.operator === ts.SyntaxKind.ExclamationToken) {
      return `~(${operand})`;
    }
    if (expr.operator === ts.SyntaxKind.MinusToken) {
      return `-(${operand})`;
    }
  }

  // Binary expression
  if (ts.isBinaryExpression(expr)) {
    const op = translateOperator(expr.operatorToken.kind);
    if (op === "?") return `> UNSUPPORTED: operator ${ts.SyntaxKind[expr.operatorToken.kind]}`;
    const left = translateBodyExpr(expr.left, checker, strategy, paramNames);
    if (isUnsupported(left)) return left;
    const right = translateBodyExpr(expr.right, checker, strategy, paramNames);
    if (isUnsupported(right)) return right;
    return `${left} ${op} ${right}`;
  }

  // Fall through to base translateExpr for identifiers, literals, this, etc.
  if (ts.isExpression(expr)) {
    return translateExpr(expr, checker, strategy, paramNames);
  }

  return `> UNSUPPORTED: non-expression statement`;
}

function translateIfStatement(
  stmt: ts.IfStatement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): string {
  const cond = translateBodyExpr(stmt.expression, checker, strategy, paramNames);
  if (isUnsupported(cond)) return cond;
  const thenExpr = extractReturnFromBranch(stmt.thenStatement);
  const elseExpr = stmt.elseStatement
    ? extractReturnFromBranch(stmt.elseStatement)
    : null;

  if (thenExpr && elseExpr) {
    const thenStr = translateBodyExpr(thenExpr, checker, strategy, paramNames);
    if (isUnsupported(thenStr)) return thenStr;
    const elseStr = translateBodyExpr(elseExpr, checker, strategy, paramNames);
    if (isUnsupported(elseStr)) return elseStr;
    return `cond ${cond} => ${thenStr}, true => ${elseStr}`;
  }

  return `> UNSUPPORTED: if statement without return in both branches`;
}

function extractReturnFromBranch(stmt: ts.Statement): ts.Expression | null {
  if (ts.isReturnStatement(stmt) && stmt.expression) return stmt.expression;
  if (ts.isBlock(stmt)) {
    // Apply the same rule as extractReturnExpression: only allow a single
    // return (after filtering guards). Blocks with local declarations or
    // multiple non-guard statements are rejected so we don't leak
    // branch-scoped bindings into the generated proposition.
    const nonGuard = stmt.statements.filter((s) => !isGuardStatement(s));
    if (nonGuard.length === 1) {
      const s = nonGuard[0];
      if (ts.isReturnStatement(s) && s.expression) return s.expression;
    }
  }
  return null;
}

function translateCallExpr(
  expr: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): string {
  // Method calls: obj.method(args)
  if (ts.isPropertyAccessExpression(expr.expression)) {
    const methodName = expr.expression.name.text;
    const obj = expr.expression.expression;

    // .includes(x) -> x in obj (array only)
    if (methodName === "includes" && expr.arguments.length === 1) {
      const receiverType = checker.getTypeAtLocation(obj);
      if (!checker.isArrayType(receiverType)) {
        return `> UNSUPPORTED: non-array .includes()`;
      }
      const arg = translateBodyExpr(expr.arguments[0], checker, strategy, paramNames);
      if (isUnsupported(arg)) return arg;
      const objStr = translateBodyExpr(obj, checker, strategy, paramNames);
      if (isUnsupported(objStr)) return objStr;
      return `${arg} in ${objStr}`;
    }

    // .filter(p).map(f) -> each x: T, p x | f x
    if (methodName === "map" && ts.isCallExpression(obj)) {
      const filterResult = tryTranslateFilterMap(obj, expr, checker, strategy, paramNames);
      if (filterResult) return filterResult;
    }
  }

  // Unsupported call
  return `> UNSUPPORTED: ${expr.getText()}`;
}

function tryTranslateFilterMap(
  filterCall: ts.CallExpression,
  mapCall: ts.CallExpression,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
): string | null {
  if (!ts.isPropertyAccessExpression(filterCall.expression)) return null;
  if (filterCall.expression.name.text !== "filter") return null;
  if (filterCall.arguments.length !== 1 || mapCall.arguments.length !== 1) return null;

  const sourceObj = filterCall.expression.expression;
  const filterArg = filterCall.arguments[0];
  const mapArg = mapCall.arguments[0];

  // Only translate filter/map on actual arrays
  const sourceType = checker.getTypeAtLocation(sourceObj);
  if (!checker.isArrayType(sourceType)) return null;
  let elemTypeName = "?";
  const typeArgs = checker.getTypeArguments(sourceType as ts.TypeReference);
  if (typeArgs.length === 1) {
    elemTypeName = mapTsType(typeArgs[0], checker, strategy);
  }

  const varName = freshBinder(paramNames);
  const extendedParams = new Map(paramNames);
  extendedParams.set(varName, varName);

  // Extract predicate body from arrow function
  const filterBody = extractArrowBody(filterArg, varName, extendedParams, checker, strategy);
  const mapBody = extractArrowBody(mapArg, varName, extendedParams, checker, strategy);

  if (filterBody && mapBody) {
    if (isUnsupported(filterBody)) return filterBody;
    if (isUnsupported(mapBody)) return mapBody;
    return `(each ${varName}: ${elemTypeName}, ${filterBody} | ${mapBody})`;
  }

  return null;
}

function extractArrowBody(
  expr: ts.Expression,
  binderName: string,
  paramNames: Map<string, string>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
): string | null {
  if (!ts.isArrowFunction(expr)) return null;
  if (expr.parameters.length !== 1 || !ts.isIdentifier(expr.parameters[0].name)) {
    return `> UNSUPPORTED: filter/map callback must have exactly one identifier parameter`;
  }

  // Map arrow param to the fresh binder
  const arrowParams = new Map(paramNames);
  arrowParams.set(expr.parameters[0].name.text, binderName);

  if (ts.isBlock(expr.body)) {
    // Only allow a single return (after filtering guards), same rule as
    // extractReturnExpression — blocks with locals or multiple statements
    // would introduce free variables in the generated comprehension.
    const nonGuard = expr.body.statements.filter((s) => !isGuardStatement(s));
    if (nonGuard.length === 1) {
      const s = nonGuard[0];
      if (ts.isReturnStatement(s) && s.expression) {
        return translateBodyExpr(s.expression, checker, strategy, arrowParams);
      }
    }
    return null;
  }

  // Expression body
  return translateBodyExpr(expr.body, checker, strategy, arrowParams);
}

// --- Mutating function body translation ---

function translateMutatingBody(
  node: ts.FunctionDeclaration | ts.MethodDeclaration,
  functionName: string,
  params: Array<{ name: string; type: string }>,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  declarations: PantDeclaration[],
): PantProposition[] {
  if (!node.body) return [];

  const propositions: PantProposition[] = [];
  const modifiedRules = new Set<string>();

  // Collect property assignments
  const hasUnsupportedMutation = collectAssignments(node.body, checker, strategy, paramNames, propositions, modifiedRules);

  // Only generate frame conditions when all mutation shapes were translatable;
  // unsupported control flow (if/loop/switch) makes frames unsound.
  if (!hasUnsupportedMutation) {
    const frames = generateFrameConditions(modifiedRules, declarations);
    propositions.push(...frames);
  }

  return propositions;
}

/**
 * Collect property assignments from a block. Returns true if any unsupported
 * mutating control flow (if/loop/switch) was encountered, signalling that
 * frame condition generation should be suppressed.
 */
function collectAssignments(
  body: ts.Block | ts.Statement,
  checker: ts.TypeChecker,
  strategy: NumericStrategy,
  paramNames: Map<string, string>,
  propositions: PantProposition[],
  modifiedRules: Set<string>,
): boolean {
  let hasUnsupportedMutation = false;
  const stmts = ts.isBlock(body) ? Array.from(body.statements) : [body];

  for (const stmt of stmts) {
    // Skip guard statements (if-throw patterns)
    if (isGuardStatement(stmt)) continue;

    // Handle const bindings: translate initializer and add to paramNames
    if (ts.isVariableStatement(stmt)) {
      const declList = stmt.declarationList;
      if (declList.flags & ts.NodeFlags.Const) {
        let allPure = true;
        for (const decl of declList.declarations) {
          if (!ts.isIdentifier(decl.name) || !decl.initializer || expressionHasSideEffects(decl.initializer)) {
            allPure = false;
            break;
          }
        }
        if (allPure) {
          for (const decl of declList.declarations) {
            const name = (decl.name as ts.Identifier).text;
            const translated = translateBodyExpr(decl.initializer!, checker, strategy, paramNames);
            if (isUnsupported(translated)) {
              hasUnsupportedMutation = true;
              propositions.push({ text: translated });
              continue;
            }
            paramNames.set(name, translated.includes(" ") ? `(${translated})` : translated);
          }
          continue;
        }
      }
      // let/var or effectful const — fall through to existing rejection
    }

    if (ts.isExpressionStatement(stmt) && ts.isBinaryExpression(unwrapExpression(stmt.expression))) {
      const bin = unwrapExpression(stmt.expression) as ts.BinaryExpression;
      if (
        bin.operatorToken.kind === ts.SyntaxKind.EqualsToken &&
        ts.isPropertyAccessExpression(bin.left)
      ) {
        const prop = bin.left.name.text;
        const obj = translateBodyExpr(bin.left.expression, checker, strategy, paramNames);
        const val = translateBodyExpr(bin.right, checker, strategy, paramNames);
        if (
          obj.startsWith("> UNSUPPORTED:") ||
          val.startsWith("> UNSUPPORTED:")
        ) {
          hasUnsupportedMutation = true;
          propositions.push({
            text: obj.startsWith("> UNSUPPORTED:") ? obj : val,
          });
          continue;
        }
        propositions.push({ text: `${prop}' ${obj} = ${val}` });
        modifiedRules.add(prop);
      }
    }

    // Recurse into nested blocks
    if (ts.isBlock(stmt)) {
      if (collectAssignments(stmt, checker, strategy, paramNames, propositions, modifiedRules)) {
        hasUnsupportedMutation = true;
      }
    } else if (ts.isIfStatement(stmt)) {
      propositions.push({ text: `> UNSUPPORTED: conditional assignment (if/else)` });
      hasUnsupportedMutation = true;
    } else if (
      ts.isForStatement(stmt) ||
      ts.isForOfStatement(stmt) ||
      ts.isForInStatement(stmt) ||
      ts.isWhileStatement(stmt) ||
      ts.isDoStatement(stmt)
    ) {
      propositions.push({ text: `> UNSUPPORTED: loop assignment` });
      hasUnsupportedMutation = true;
    } else if (ts.isTryStatement(stmt)) {
      if (collectAssignments(stmt.tryBlock, checker, strategy, paramNames, propositions, modifiedRules)) {
        hasUnsupportedMutation = true;
      }
      if (stmt.catchClause) {
        if (collectAssignments(stmt.catchClause.block, checker, strategy, paramNames, propositions, modifiedRules)) {
          hasUnsupportedMutation = true;
        }
      }
      if (stmt.finallyBlock) {
        if (collectAssignments(stmt.finallyBlock, checker, strategy, paramNames, propositions, modifiedRules)) {
          hasUnsupportedMutation = true;
        }
      }
    } else if (ts.isSwitchStatement(stmt)) {
      propositions.push({ text: `> UNSUPPORTED: switch assignment` });
      hasUnsupportedMutation = true;
    }
  }

  return hasUnsupportedMutation;
}

/**
 * Generate frame conditions: for each rule in declarations not explicitly
 * modified, emit `all x: T | rule' x = rule x`.
 */
function generateFrameConditions(
  modifiedRules: Set<string>,
  declarations: PantDeclaration[],
): PantProposition[] {
  const frames: PantProposition[] = [];

  for (const decl of declarations) {
    if (decl.kind !== "rule") continue;
    if (modifiedRules.has(decl.name)) continue;

    if (decl.params.length === 0) {
      frames.push({ text: `${decl.name}' = ${decl.name}` });
    } else {
      const paramBindings = decl.params.map((p) => `${p.name}: ${p.type}`).join(", ");
      const paramArgs = decl.params.map((p) => p.name).join(" ");
      frames.push({
        text: `all ${paramBindings} | ${decl.name}' ${paramArgs} = ${decl.name} ${paramArgs}`,
      });
    }
  }

  return frames;
}
