// @archlint.module exempt
// @archlint.exempt-reason test-support

interface TokenBox {
  value: string;
}

interface Circle {
  kind: "circle";
  radius: number;
}

function isStringByTypeof(value: unknown): value is string {
  return true;
}

function isTokenBoxByInstanceof(value: unknown): value is TokenBox {
  return true;
}

function isCircleByDiscriminator(value: unknown): value is Circle {
  return true;
}

function isNumberArrayByArrayIsArray(value: unknown): value is number[] {
  return true;
}

function isAnyArrayByArrayIsArray(value: unknown): value is any[] {
  return true;
}

/**
 * @pant true.
 */
export function typeofString(value: unknown): string {
  if (isStringByTypeof(value)) {
    return value.toUpperCase();
  }
  return "";
}

/**
 * @pant true.
 */
export function instanceofClass(value: unknown, fallback: TokenBox): TokenBox {
  if (isTokenBoxByInstanceof(value)) {
    return value;
  }
  return fallback;
}

/**
 * @pant true.
 */
export function discriminatorPredicate(value: unknown, fallback: Circle): Circle {
  if (isCircleByDiscriminator(value)) {
    return value;
  }
  return fallback;
}

/**
 * @pant true.
 */
export function arrayIsArray(value: unknown, fallback: number[]): number[] {
  if (isNumberArrayByArrayIsArray(value)) {
    return value;
  }
  return fallback;
}

/**
 * @pant true.
 */
export function arrayIsArrayUnmappable(value: unknown): unknown {
  if (isAnyArrayByArrayIsArray(value)) {
    return value;
  }
  return value;
}

/**
 * @pant true.
 */
export function unNarrowed(value: unknown): unknown {
  return value;
}

/**
 * @pant true.
 */
export function perUseSite(
  value: unknown,
  flag: boolean,
): string {
  if (isStringByTypeof(value)) {
    return value.trim();
  }
  if (isStringByTypeof(value)) {
    return value.toUpperCase();
  }
  return flag ? "" : "";
}
