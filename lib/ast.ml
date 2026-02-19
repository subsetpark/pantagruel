(** AST type definitions for Pantagruel *)

(** Source location *)
type loc = {
  file: string;
  line: int;
  col: int;
}
[@@deriving show, eq]

let dummy_loc = { file = "<unknown>"; line = 0; col = 0 }

(** Wrap a value with its location and optional doc comment *)
type 'a located = {
  loc: loc;
  value: 'a;
  doc: string list;  (** Doc comment lines (without leading >) *)
}
[@@deriving show, eq]

let located ?(doc=[]) loc value = { loc; value; doc }

(** Identifier types *)
type upper_ident = string  (* Domain names: User, Document *)
[@@deriving show, eq]

type lower_ident = string  (* Procedures/variables: owner, has-perm? *)
[@@deriving show, eq]

type qualified_name = upper_ident * string  (* MODULE.name *)
[@@deriving show, eq]

(** Type expressions (syntactic) *)
type type_expr =
  | TName of upper_ident             (** User, Bool, Nat *)
  | TQName of upper_ident * upper_ident  (** Module::Type *)
  | TList of type_expr               (** [T] *)
  | TProduct of type_expr list       (** A * B * C *)
  | TSum of type_expr list           (** A + B *)
[@@deriving show, eq]

(** Binary operators *)
type binop =
  | OpAnd    (** and *)
  | OpOr     (** or *)
  | OpImpl   (** -> implication *)
  | OpIff    (** <-> biconditional *)
  | OpEq     (** = *)
  | OpNeq    (** != *)
  | OpLt     (** < *)
  | OpGt     (** > *)
  | OpLe     (** <= *)
  | OpGe     (** >= *)
  | OpIn     (** in (membership) *)
  | OpSubset (** subset *)
  | OpAdd    (** + *)
  | OpSub    (** - *)
  | OpMul    (** * *)
  | OpDiv    (** / *)
[@@deriving show, eq]

(** Unary operators *)
type unop =
  | OpNot   (** not *)
  | OpNeg   (** unary minus *)
  | OpCard  (** # cardinality *)
[@@deriving show, eq]

(** Parameters in declarations and quantifiers *)
type param = {
  param_name: lower_ident;
  param_type: type_expr;
}
[@@deriving show, eq]

(** Guards: parameter bindings, membership bindings, or boolean expressions *)
type guard =
  | GParam of param           (** x: T *)
  | GIn of lower_ident * expr (** x in xs - binds x to element type of xs *)
  | GExpr of expr             (** boolean condition *)
[@@deriving show, eq]

(** Expressions *)
and expr =
  | EVar of lower_ident                           (** x *)
  | EDomain of upper_ident                        (** User as set *)
  | EQualified of qualified_name                  (** MODULE.name *)
  | ELitNat of int                                (** 42 *)
  | ELitReal of float                             (** 3.14 *)
  | ELitString of string                          (** "hello" *)
  | ELitBool of bool                              (** true, false *)
  | EApp of expr * expr list                      (** f x y *)
  | EPrimed of lower_ident                        (** f' *)
  | EOverride of lower_ident * (expr * expr) list (** f[a |-> b] *)
  | ETuple of expr list                           (** (a, b, c) *)
  | EProj of expr * int                           (** e.1, e.2 *)
  | EBinop of binop * expr * expr                 (** e1 op e2 *)
  | EUnop of unop * expr                          (** op e *)
  | EForall of param list * guard list * expr     (** forall x:T, g | e *)
  | EExists of param list * guard list * expr     (** exists x:T, g | e *)
[@@deriving show, eq]

(** Declarations in chapter heads *)
type declaration =
  | DeclDomain of upper_ident                (** User. *)
  | DeclAlias of upper_ident * type_expr     (** Point = Nat * Nat. *)
  | DeclProc of {
      name: lower_ident;
      params: param list;
      guards: guard list;
      return_type: type_expr option;  (** None = Void procedure *)
      context: upper_ident option;    (** Context annotation: "in Banking" *)
    }
  | DeclContext of upper_ident * lower_ident list  (** context Banking = { balance, owner }. *)
[@@deriving show, eq]

(** A chapter has a head (declarations) and body (propositions) *)
type chapter = {
  head: declaration located list;
  body: expr located list;
}
[@@deriving show, eq]

(** A complete document *)
type document = {
  module_name: upper_ident option;  (** None = standalone (no module system) *)
  imports: upper_ident located list;
  chapters: chapter list;
}
[@@deriving show, eq]
