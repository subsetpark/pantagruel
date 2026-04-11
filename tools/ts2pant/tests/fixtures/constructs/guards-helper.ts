// Guard detection: call-graph following for guard extraction

interface Account {
  balance: number;
}

// --- Helper functions (not exported, not tested directly) ---

function validateAmount(amount: number): void {
  if (amount <= 0) {
    throw new Error("Amount must be positive");
  }
}

function requirePositive(n: number): void {
  if (n <= 0) {
    throw new Error("Must be positive");
  }
}

function validateAll(amount: number, balance: number): void {
  validateAmount(amount);
  requirePositive(balance);
}

const checkPositive = (n: number): void => {
  if (n <= 0) {
    throw new Error("Must be positive");
  }
};

// --- Exported test functions ---

/** direct call to validation helper → followed guard */
export function depositDirect(account: Account, amount: number): void {
  validateAmount(amount);
  account.balance = account.balance + amount;
}

/** formal-to-actual parameter substitution */
export function depositSubst(account: Account, amount: number): void {
  requirePositive(account.balance);
  account.balance = account.balance + amount;
}

/** multiple followed calls → combined guards */
export function depositMulti(account: Account, amount: number): void {
  validateAmount(amount);
  requirePositive(account.balance);
  account.balance = account.balance + amount;
}

/** two levels deep: validateAll → validateAmount → requirePositive */
export function depositDeep(account: Account, amount: number): void {
  validateAll(amount, account.balance);
  account.balance = account.balance + amount;
}

/** const-bound arrow function as guard helper */
export function depositConst(account: Account, amount: number): void {
  checkPositive(amount);
  account.balance = account.balance + amount;
}
