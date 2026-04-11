// Boolean operators: &&, ||, !

/** logical and → and */
export function and(a: boolean, b: boolean): boolean {
  return a && b;
}

/** logical or → or */
export function or(a: boolean, b: boolean): boolean {
  return a || b;
}

/** logical not → ~ */
export function not(a: boolean): boolean {
  return !a;
}

/** compound: && with ! → and ~() */
export function andNot(a: boolean, b: boolean): boolean {
  return a && !b;
}
