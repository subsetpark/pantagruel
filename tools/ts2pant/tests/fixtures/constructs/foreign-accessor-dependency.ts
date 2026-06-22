import {
  isReady,
  type ForeignDependencyContainer,
} from "../foreign-dependency/index.js";

export function foreignLabels(c: ForeignDependencyContainer): string[] {
  const labels: string[] = [];
  for (const item of c.items) {
    if (isReady(item)) {
      labels.push(item.label);
    }
  }
  return labels;
}
