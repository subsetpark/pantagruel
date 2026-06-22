export interface DependencyContainer {
  readonly items: readonly DependencyItem[];
}

export interface DependencyItem {
  readonly label: string;
  readonly ready: boolean;
  isLabeled(): boolean;
  hasReadyFlag(): boolean;
  hasIndex(index: number): boolean;
  getLabel(): string;
}

export declare function isReady(item: DependencyItem): boolean;
