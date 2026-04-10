(** Type representation for Pantagruel *)

(** Internal type representation *)
type ty =
  | TyBool
  | TyNat
  | TyNat0
  | TyInt
  | TyReal
  | TyString
  | TyNothing
  | TyDomain of string  (** User-defined domain *)
  | TyList of ty
  | TyProduct of ty list  (** n-ary product *)
  | TySum of ty list  (** n-ary sum *)
  | TyFunc of ty list * ty option  (** params, return; None = Void *)
[@@deriving show, eq]

val builtin_of_name : string -> ty option
val builtin_type_names : string list

val format_ty : ty -> string
(** Pretty-print a type (human-readable) *)

val numeric_super : ty -> ty option
(** Immediate numeric supertype: Nat -> Nat0 -> Int -> Real *)

val is_numeric : ty -> bool

val is_subtype : ty -> ty -> bool
(** Is t1 a subtype of t2? (t1 fits where t2 is expected) *)

val lub_numeric : ty -> ty -> ty option
(** Least upper bound of two numeric types *)

(** Unification error *)
type unify_error =
  | TypeMismatch of ty * ty
  | ArityMismatch of int * int
  | NotAFunction of ty
  | NotAList of ty
  | NotAProduct of ty
  | NotNumeric of ty
[@@deriving show]

val join : ty -> ty -> (ty, unify_error) result
(** Compute the join (least upper bound) of two types *)

val compatible : ty -> ty -> bool
(** Check if two types are compatible (have a common supertype) *)
