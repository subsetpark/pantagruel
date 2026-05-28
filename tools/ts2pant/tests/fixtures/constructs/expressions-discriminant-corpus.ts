type Lit =
  | { kind: "string"; value: string }
  | { kind: "number"; value: number }
  | { kind: "boolean"; value: boolean };

export function litWidth(l: Lit): number {
  switch (l.kind) {
    case "string": {
      const value = l.value;
      return 1;
    }
    case "number": {
      const value = l.value;
      return 2;
    }
    case "boolean": {
      const value = l.value;
      return 3;
    }
    default:
      return 0;
  }
}

export function litValue(l: Lit): string | number | boolean {
  switch (l.kind) {
    case "string":
      return l.value;
    case "number":
      return l.value;
    case "boolean":
      return l.value;
    default:
      return "";
  }
}

type Res =
  | { kind: "resolved"; owner: string }
  | { kind: "none" }
  | { kind: "ambiguous" };

export function ownerOf(r: Res): string {
  if (r.kind === "resolved") return r.owner;
  return "";
}
