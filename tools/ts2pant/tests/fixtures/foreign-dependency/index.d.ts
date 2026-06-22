export interface DependencyContainer {
  readonly items: readonly DependencyItem[];
}

export interface DependencyItem {
  readonly label: string;
  readonly ready: boolean;
}

export declare function isReady(item: DependencyItem): boolean;
