// Pantagruel expression and proposition AST.
//
// Translation produces this AST; rendering to Pantagruel source text
// is a separate, purely structural pass via renderExpr / renderProp.

// ---------------------------------------------------------------------------
// Expression AST
// ---------------------------------------------------------------------------

export type PantExpr =
  | { kind: "var"; name: string }
  | { kind: "literal"; value: string }
  | { kind: "apply"; fn: string; args: PantExpr[] }
  | { kind: "primed-apply"; fn: string; args: PantExpr[] }
  | { kind: "binop"; op: string; left: PantExpr; right: PantExpr }
  | { kind: "unop"; op: string; operand: PantExpr }
  | { kind: "cardinality"; expr: PantExpr }
  | { kind: "membership"; element: PantExpr; collection: PantExpr }
  | { kind: "cond"; arms: { guard: PantExpr; value: PantExpr }[]; fallback: PantExpr }
  | { kind: "comprehension"; binder: string; type: string; predicate?: PantExpr; body: PantExpr }
  | { kind: "unsupported"; reason: string };

// ---------------------------------------------------------------------------
// Proposition AST
// ---------------------------------------------------------------------------

export interface Binding {
  name: string;
  type: string;
}

export type PantProp =
  | { kind: "equation"; quantifiers: Binding[]; lhs: PantExpr; rhs: PantExpr }
  | { kind: "unsupported"; reason: string }
  | { kind: "raw"; text: string };

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

export const Var = (name: string): PantExpr => ({ kind: "var", name });
export const Lit = (value: string): PantExpr => ({ kind: "literal", value });
export const Apply = (fn: string, ...args: PantExpr[]): PantExpr => ({ kind: "apply", fn, args });
export const PrimedApply = (fn: string, ...args: PantExpr[]): PantExpr => ({ kind: "primed-apply", fn, args });
export const Binop = (op: string, left: PantExpr, right: PantExpr): PantExpr => ({ kind: "binop", op, left, right });
export const Unop = (op: string, operand: PantExpr): PantExpr => ({ kind: "unop", op, operand });
export const Cardinality = (expr: PantExpr): PantExpr => ({ kind: "cardinality", expr });
export const Membership = (element: PantExpr, collection: PantExpr): PantExpr => ({ kind: "membership", element, collection });
export const Cond = (arms: { guard: PantExpr; value: PantExpr }[], fallback: PantExpr): PantExpr => ({ kind: "cond", arms, fallback });
export const Comprehension = (binder: string, type: string, body: PantExpr, predicate?: PantExpr): PantExpr => ({
  kind: "comprehension", binder, type, predicate, body,
});
export const Unsupported = (reason: string): PantExpr => ({ kind: "unsupported", reason });

export const Equation = (quantifiers: Binding[], lhs: PantExpr, rhs: PantExpr): PantProp => ({
  kind: "equation", quantifiers, lhs, rhs,
});
export const UnsupportedProp = (reason: string): PantProp => ({ kind: "unsupported", reason });
export const RawProp = (text: string): PantProp => ({ kind: "raw", text });

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/** True when the expression is compound and needs parens as an apply argument. */
function needsParens(expr: PantExpr): boolean {
  return expr.kind === "binop" || expr.kind === "cond" || expr.kind === "membership";
}

/** Render a PantExpr argument, wrapping in parens if compound. */
function renderArg(expr: PantExpr): string {
  const s = renderExpr(expr);
  return needsParens(expr) ? `(${s})` : s;
}

export function renderExpr(expr: PantExpr): string {
  switch (expr.kind) {
    case "var":
      return expr.name;
    case "literal":
      return expr.value;
    case "apply": {
      if (expr.args.length === 0) return expr.fn;
      return `${expr.fn} ${expr.args.map(renderArg).join(" ")}`;
    }
    case "primed-apply": {
      if (expr.args.length === 0) return `${expr.fn}'`;
      return `${expr.fn}' ${expr.args.map(renderArg).join(" ")}`;
    }
    case "binop":
      return `${renderArg(expr.left)} ${expr.op} ${renderArg(expr.right)}`;
    case "unop":
      return `${expr.op}(${renderExpr(expr.operand)})`;
    case "cardinality":
      return `#${renderArg(expr.expr)}`;
    case "membership":
      return `${renderArg(expr.element)} in ${renderArg(expr.collection)}`;
    case "cond": {
      const arms = expr.arms.map((a) => `${renderExpr(a.guard)} => ${renderExpr(a.value)}`);
      arms.push(`true => ${renderExpr(expr.fallback)}`);
      return `cond ${arms.join(", ")}`;
    }
    case "comprehension": {
      const pred = expr.predicate ? `, ${renderExpr(expr.predicate)}` : "";
      return `(each ${expr.binder}: ${expr.type}${pred} | ${renderExpr(expr.body)})`;
    }
    case "unsupported":
      return `> UNSUPPORTED: ${expr.reason}`;
  }
}

export function renderProp(prop: PantProp): string {
  switch (prop.kind) {
    case "equation": {
      const lhs = renderExpr(prop.lhs);
      const rhs = renderExpr(prop.rhs);
      if (prop.quantifiers.length === 0) {
        return `${lhs} = ${rhs}`;
      }
      const bindings = prop.quantifiers.map((b) => `${b.name}: ${b.type}`).join(", ");
      return `all ${bindings} | ${lhs} = ${rhs}`;
    }
    case "unsupported":
      return `> UNSUPPORTED: ${prop.reason}`;
    case "raw":
      return prop.text;
  }
}
