export function varBinding(a: number): number {
  var x = a + 1;
  return x;
}

export function varReassignment(a: number, b: number): number {
  var x = a;
  x = b;
  return x;
}
