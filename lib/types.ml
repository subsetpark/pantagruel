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

(** Centralized builtin type registry *)
let builtins =
  [
    ("Bool", TyBool);
    ("Nat", TyNat);
    ("Nat0", TyNat0);
    ("Int", TyInt);
    ("Real", TyReal);
    ("String", TyString);
    ("Nothing", TyNothing);
  ]

let builtin_of_name name = List.assoc_opt name builtins
let builtin_type_names = List.map fst builtins

(** Pretty-print a type (human-readable) *)
let rec format_ty = function
  | TyBool -> "Bool"
  | TyNat -> "Nat"
  | TyNat0 -> "Nat0"
  | TyInt -> "Int"
  | TyReal -> "Real"
  | TyString -> "String"
  | TyNothing -> "Nothing"
  | TyDomain name -> name
  | TyList t -> "[" ^ format_ty t ^ "]"
  | TyProduct ts -> String.concat " * " (List.map format_ty ts)
  | TySum ts -> String.concat " + " (List.map format_ty ts)
  | TyFunc (params, ret) ->
      let params_str = String.concat ", " (List.map format_ty params) in
      let ret_str = match ret with None -> "Void" | Some t -> format_ty t in
      "(" ^ params_str ^ ") -> " ^ ret_str

(** Immediate numeric supertype: Nat → Nat0 → Int → Real *)
let numeric_super = function
  | TyNat -> Some TyNat0
  | TyNat0 -> Some TyInt
  | TyInt -> Some TyReal
  | _ -> None

let is_numeric ty = numeric_super ty <> None || equal_ty ty TyReal

(** Is t1 a subtype of t2? (t1 fits where t2 is expected)
    - TyNothing is the bottom type (subtype of everything)
    - Numeric chain: Nat < Nat0 < Int < Real
    - Covariant through List, Product, Sum *)
let rec is_subtype t1 t2 =
  equal_ty t1 t2 || equal_ty t1 TyNothing
  || (match numeric_super t1 with Some up -> is_subtype up t2 | None -> false)
  ||
  match (t1, t2) with
  | TyList a, TyList b -> is_subtype a b
  | TyProduct as_, TyProduct bs ->
      List.length as_ = List.length bs && List.for_all2 is_subtype as_ bs
  | TySum as_, TySum bs ->
      List.length as_ = List.length bs && List.for_all2 is_subtype as_ bs
  | _ -> false

(** Least upper bound of two numeric types *)
let lub_numeric t1 t2 =
  if is_subtype t1 t2 then Some t2
  else if is_subtype t2 t1 then Some t1
  else None

(** Unification error *)
type unify_error =
  | TypeMismatch of ty * ty
  | ArityMismatch of int * int
  | NotAFunction of ty
  | NotAList of ty
  | NotAProduct of ty
  | NotNumeric of ty
[@@deriving show]

(** Compute the join (least upper bound) of two types. Used for inferring result
    types (e.g. arithmetic operand LUB). *)
let rec join t1 t2 : (ty, unify_error) result =
  if equal_ty t1 t2 then Ok t1
  else
    match lub_numeric t1 t2 with
    | Some lub -> Ok lub
    | None -> (
        match (t1, t2) with
        | TyList a, TyList b -> (
            match join a b with
            | Ok joined -> Ok (TyList joined)
            | Error e -> Error e)
        | TyProduct as_, TyProduct bs when List.length as_ = List.length bs -> (
            match
              List.map2 (fun a b -> join a b) as_ bs |> Util.sequence_results
            with
            | Ok joined -> Ok (TyProduct joined)
            | Error e -> Error e)
        | TySum as_, TySum bs when List.length as_ = List.length bs -> (
            match
              List.map2 (fun a b -> join a b) as_ bs |> Util.sequence_results
            with
            | Ok joined -> Ok (TySum joined)
            | Error e -> Error e)
        | _ -> Error (TypeMismatch (t1, t2)))

(** Check if two types are compatible (have a common supertype). Used for
    symmetric checks like = and != operands. *)
let compatible t1 t2 = Result.is_ok (join t1 t2)
