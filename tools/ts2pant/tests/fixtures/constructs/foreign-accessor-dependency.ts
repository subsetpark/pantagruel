import type { DependencyContainer } from "../foreign-dependency/index.js";

export function foreignLabels(c: DependencyContainer): string[] {
  const labels: string[] = [];
  for (const item of c.items) {
    if (item.ready) {
      labels.push(item.label);
    }
  }
  return labels;
}
