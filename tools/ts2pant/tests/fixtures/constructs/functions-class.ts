// Class method patterns: pure and mutating methods with implicit this

export class Account {
  balance: number = 0;
  owner: string = "";

  /** pure class method → rule with synthetic Account param */
  getBalance(): number {
    return this.balance;
  }

  /** mutating class method → action with primed proposition */
  deposit(amount: number): void {
    this.balance = this.balance + amount;
  }
}
