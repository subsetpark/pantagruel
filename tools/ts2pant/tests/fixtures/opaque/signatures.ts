export declare function scalarUnknown(value: unknown): unknown;

export declare function scalarAny(value: any): any;

export declare function unknownArray(value: unknown[]): unknown[];

export declare function numberUnknownTuple(
  value: [number, unknown],
): [number, unknown];

export declare function stringUnknownMap(
  value: Map<string, unknown>,
): Map<string, unknown>;

export declare function numberUnknownUnion(
  value: number | unknown,
): number | unknown;

export declare function nullableUnknown(
  value: unknown | null,
): unknown | null;

export declare function anonymousRecord(
  value: { known: number; mystery: unknown },
): { known: number; mystery: unknown };
