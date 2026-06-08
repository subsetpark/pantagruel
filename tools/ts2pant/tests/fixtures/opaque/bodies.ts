// @archlint.module exempt
// @archlint.exempt-reason test-support

declare function consumeAny(value: any): any;

interface Box {
  total: number;
}

/**
 * @pant true.
 */
export function readUnknown(value: unknown): unknown {
  return value;
}

/**
 * @pant true.
 */
export function arithmeticContagion(value: any): any {
  return value + 1;
}

/**
 * @pant true.
 */
export function logicalContagion(value: any): any {
  return !value;
}

/**
 * @pant true.
 */
export function callContagion(value: any): any {
  return consumeAny(value);
}

/**
 * @pant true.
 */
export function memberContagion(value: any): any {
  return value.answer;
}

/**
 * @pant true.
 */
export function conditionalContagion(flag: boolean, value: any): any {
  return flag ? value : 0;
}

/**
 * @pant true.
 */
export function mutatingReturnContagion(box: Box, value: any): any {
  box.total = 1;
  return value + box.total;
}
