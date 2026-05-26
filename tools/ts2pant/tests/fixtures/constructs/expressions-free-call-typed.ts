// Fixture: typed bare-identifier free-call declarations.

interface Profile {
  name: string;
  score: number;
}

declare function mix(a: number, b: number, label: string): number;
declare function lookupProfile(id: string): Profile;
declare function opaqueValue(): any;
declare function consumeAny(x: any): number;
declare function mysteryValue(): unknown;
declare function consumeUnknown(x: unknown): number;

export function ambientConcrete(a: number, b: number, label: string): number {
  return mix(a, b, label);
}

export function ambientReturnsInterface(id: string): string {
  return lookupProfile(id).name;
}

export function ambientAnyParam(): number {
  return consumeAny(opaqueValue());
}

export function ambientUnknownReturn(): number {
  return consumeUnknown(mysteryValue());
}
