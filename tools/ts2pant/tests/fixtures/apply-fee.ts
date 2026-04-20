interface Account {
  balance: number;
}

/**
 * Apply a fee to an account only when the fee is positive.
 * @pant fee > 0 -> balance' a = balance a - fee
 * @pant fee <= 0 -> balance' a = balance a
 */
function applyFee(a: Account, fee: number): void {
  if (fee > 0) {
    a.balance = a.balance - fee;
  }
}
