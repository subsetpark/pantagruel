// @archlint.module exempt
// @archlint.exempt-reason test-support

export function letOneReassignment(a: number, b: number): number {
  let x = a;
  x = b;
  return x;
}

export function letSeveralReassignments(a: number, b: number, c: number): number {
  let x = a;
  x = b;
  x = c + 1;
  return x;
}

export function letCompoundAdd(a: number): number {
  let x = a;
  x += 1;
  return x;
}

export function letCompoundMultiply(a: number): number {
  let x = a;
  x *= 2;
  return x;
}

export function letPrefixPostfix(a: number): number {
  let x = a;
  ++x;
  x++;
  return x;
}

export function letArrayDestructure(a: number, b: number): number {
  let x = a;
  let y = b;
  [x, y] = [y, x];
  return x + y;
}

export function letObjectDestructure(a: number, b: number): number {
  let x = a;
  let y = b;
  ({ x, y } = { x: y, y: x });
  return x - y;
}
