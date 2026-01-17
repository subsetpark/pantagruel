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
  | TyDomain of string            (** User-defined domain *)
  | TyList of ty
  | TyProduct of ty list          (** n-ary product *)
  | TySum of ty list              (** n-ary sum *)
  | TyFunc of ty list * ty option (** params, return; None = Void *)
[@@deriving show, eq]

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

(** Numeric hierarchy level: Nat < Nat0 < Int < Real *)
let numeric_level = function
  | TyNat -> Some 1
  | TyNat0 -> Some 2
  | TyInt -> Some 3
  | TyReal -> Some 4
  | _ -> None

let is_numeric ty = numeric_level ty <> None

(** Least upper bound of two numeric types *)
let lub_numeric t1 t2 =
  match numeric_level t1, numeric_level t2 with
  | Some l1, Some l2 ->
      (* Return the type with higher level *)
      if l1 >= l2 then Some t1 else Some t2
  | _ -> None

(** Check if t1 is a subtype of t2 (for numeric hierarchy only) *)
let is_subtype t1 t2 =
  if equal_ty t1 t2 then true
  else match numeric_level t1, numeric_level t2 with
    | Some l1, Some l2 -> l1 <= l2
    | _ -> false

(** Unification error *)
type unify_error =
  | TypeMismatch of ty * ty
  | ArityMismatch of int * int
  | NotAFunction of ty
  | NotAList of ty
  | NotAProduct of ty
  | NotNumeric of ty
[@@deriving show]

(** Unify two types, considering numeric subtyping *)
let rec unify t1 t2 : (ty, unify_error) result =
  if equal_ty t1 t2 then Ok t1
  else match lub_numeric t1 t2 with
    | Some lub -> Ok lub
    | None ->
        match t1, t2 with
        | TyList a, TyList b ->
            (match unify a b with
             | Ok unified -> Ok (TyList unified)
             | Error e -> Error e)
        | TyProduct as_, TyProduct bs when List.length as_ = List.length bs ->
            (match List.map2 (fun a b -> unify a b) as_ bs |> Util.sequence_results with
             | Ok unified -> Ok (TyProduct unified)
             | Error e -> Error e)
        | TySum as_, TySum bs when List.length as_ = List.length bs ->
            (match List.map2 (fun a b -> unify a b) as_ bs |> Util.sequence_results with
             | Ok unified -> Ok (TySum unified)
             | Error e -> Error e)
        | _ -> Error (TypeMismatch (t1, t2))

(** Check if two types are compatible (can be unified) *)
let compatible t1 t2 =
  Result.is_ok (unify t1 t2)
