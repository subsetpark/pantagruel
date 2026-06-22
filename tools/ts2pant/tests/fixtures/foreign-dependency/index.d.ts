export interface ForeignDependencyContainer {
  readonly items: readonly ForeignDependencyItem[];
}

export interface ForeignDependencyItem {
  readonly label: string;
  readonly ready: boolean;
}

export declare function isReady(item: ForeignDependencyItem): boolean;
