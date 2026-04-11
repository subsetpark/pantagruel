// Mutating functions: assignments, frame conditions

interface Account {
  balance: number;
  owner: string;
}

/** single property assignment → primed proposition */
export function deposit(a: Account, amount: number): void {
  a.balance = a.balance + amount;
}

/** multiple assignments → multiple primed propositions */
export function transfer(a: Account, newOwner: string, fee: number): void {
  a.balance = a.balance - fee;
  a.owner = newOwner;
}

/** parenthesized assignment */
export function depositWrapped(a: Account, amount: number): void {
  (a.balance = a.balance + amount);
}

/** assignment to literal; owner preserved as frame condition */
export function reset(a: Account): void {
  a.balance = 0;
}
