// Composite type mappings: arrays, tuples, unions, nested interfaces, enums

interface User {
  name: string;
}

interface Account {
  owner: User;
  balance: number;
}

interface Library {
  books: string[];
}

interface Address {
  city: string;
}

interface Customer {
  address: Address;
}

interface Order {
  customer: Customer;
  total: number;
}

type Point = [number, number];

interface Value {
  data: string;
}

type Result = Value | null;

enum Status {
  Active,
  Inactive,
  Pending,
}

interface UserWithStatus {
  name: string;
  status: Status;
}

/** array property → [String] */
export function getBooks(lib: Library): string[] {
  return lib.books;
}

/** nested interface → Account.owner: User */
export function getOwnerName(a: Account): string {
  return a.owner.name;
}

/** tuple alias → Int * Int */
export function getPoint(): Point {
  return [0, 0];
}

/** union alias → Value + Nothing */
export function getResult(v: Value | null): Value | null {
  return v;
}

/** transitive type following: Order → Customer → Address */
export function getCity(o: Order): string {
  return o.customer.address.city;
}

/** enum → domain */
export function getStatus(u: UserWithStatus): Status {
  return u.status;
}
