(** AST type definitions for Pantagruel *)

type loc = { file : string; line : int; col : int } [@@deriving show, eq]
(** Source location *)

let dummy_loc = { file = "<unknown>"; line = 0; col = 0 }

type 'a located = {
  loc : loc;
  value : 'a;
  doc : string list list;
      (** Doc comment paragraphs (groups separated by blank lines) *)
  doc_adjacent : bool;
      (** Last doc group is on the line directly above this node *)
}
[@@deriving show, eq]
(** Wrap a value with its location and optional doc comment *)

let located ?(doc = []) ?(doc_adjacent = true) loc value =
  { loc; value; doc; doc_adjacent }

(** Identifier types *)
type upper_ident = Upper of string (* Domain names: User, Document *)
[@@unboxed] [@@deriving show, eq]

type lower_ident = Lower of string (* Rules/variables: owner, has-perm? *)
[@@unboxed] [@@deriving show, eq]

(** Extract the string from an upper identifier *)
let upper_name (Upper s) = s

(** Extract the string from a lower identifier *)
let lower_name (Lower s) = s

type qualified_name = upper_ident * string (* MODULE.name *)
[@@deriving show, eq]

(** Type expressions (syntactic) *)
type type_expr =
  | TName of upper_ident  (** User, Bool, Nat *)
  | TQName of upper_ident * upper_ident  (** Module::Type *)
  | TList of type_expr  (** [T] *)
  | TProduct of type_expr list  (** A * B * C *)
  | TSum of type_expr list  (** A + B *)
[@@deriving show, eq]

(** Binary operators *)
type binop =
  | OpAnd  (** and *)
  | OpOr  (** or *)
  | OpImpl  (** -> implication *)
  | OpIff  (** <-> biconditional *)
  | OpEq  (** = *)
  | OpNeq  (** ~= (also !=) *)
  | OpLt  (** < *)
  | OpGt  (** > *)
  | OpLe  (** <= *)
  | OpGe  (** >= *)
  | OpIn  (** in (membership) *)
  | OpSubset  (** subset *)
  | OpAdd  (** + *)
  | OpSub  (** - *)
  | OpMul  (** * *)
  | OpDiv  (** / *)
[@@deriving show, eq]

(** Unary operators *)
type unop =
  | OpNot  (** not *)
  | OpNeg  (** unary minus *)
  | OpCard  (** # cardinality *)
[@@deriving show, eq]

(** Aggregate combiners for over-each quantifiers *)
type combiner = CombAdd | CombMul | CombAnd | CombOr | CombMin | CombMax
[@@deriving show, eq]

type param = { param_name : lower_ident; param_type : type_expr }
[@@deriving show, eq]
(** Parameters in declarations and quantifiers. In [EForall] / [EExists] /
    [EEach], the accompanying [param list] carries purely syntactic metadata
    (the user-facing name and type annotation used by pretty-printers and error
    messages); binder identity is supplied by the Bindlib [Mbinder.t] that
    accompanies it. *)

(** Guards: parameter bindings, membership bindings, or boolean expressions.
    Inside a quantifier body, the [GParam] and [GIn] names still appear as plain
    [lower_ident] — only the top-level quantifier params go through the Bindlib
    binder. *)
type guard =
  | GParam of param  (** x: T *)
  | GIn of lower_ident * expr  (** x in xs - binds x to element type of xs *)
  | GExpr of expr  (** boolean condition *)

and guarded_body = guard list * expr
(** The body of a quantifier binder: the guard list plus the inner body
    expression. Both are wrapped inside the [Mbinder.t] so that capture-avoiding
    substitution of the quantifier's top-level parameters descends into guards
    and body uniformly. *)

(** Expressions *)
and expr =
  | EVar of lower_ident  (** x *)
  | EDomain of upper_ident  (** User as set *)
  | EQualified of qualified_name  (** MODULE.name *)
  | ELitNat of int  (** 42 *)
  | ELitReal of float  (** 3.14 *)
  | ELitString of string  (** "hello" *)
  | ELitBool of bool  (** true, false *)
  | EApp of expr * expr list  (** f x y *)
  | EPrimed of lower_ident  (** f' *)
  | EOverride of lower_ident * (expr * expr) list  (** f[a |-> b] *)
  | ETuple of expr list  (** (a, b, c) *)
  | EProj of expr * int  (** e.1, e.2 *)
  | EBinop of binop * expr * expr  (** e1 op e2 *)
  | EUnop of unop * expr  (** op e *)
  | EForall of (expr, guarded_body) Binder.Mbinder.t * param list
      (** forall x:T, g | e — the [Mbinder.t] binds the top-level params in
          (guards, body); the [param list] preserves user-facing names and type
          annotations for printing and error messages. *)
  | EExists of (expr, guarded_body) Binder.Mbinder.t * param list
      (** exists x:T, g | e *)
  | EEach of
      (expr, guarded_body) Binder.Mbinder.t * param list * combiner option
      (** each x:T, g | e; optional combiner for over-each *)
  | ECond of (expr * expr) list  (** cond arm => e, arm2 => e2 *)
  | EInitially of expr  (** initially e *)

(* ========================================================================== *)
(* Bindlib helpers for constructing and destructuring quantifier binders.     *)
(* ========================================================================== *)

(** Injection of a Bindlib variable into an AST expression — the [mkfree] for
    [Binder.Var.make] when the carrier is [expr]. *)
let mkfree_expr (v : expr Binder.Var.t) : expr =
  EVar (Lower (Binder.Var.name v))

(** Fresh variable keyed by the supplied user-facing name. *)
let fresh_var (name : string) : expr Binder.Var.t =
  Binder.Var.make mkfree_expr name

(** Box an expression, replacing free occurrences of [EVar (Lower n)] with the
    corresponding bound variable whenever [n] is in [env]. Outer bindings are
    shadowed by any inner quantifier's own parameters; this function rebinds
    nested quantifiers so that free occurrences captured by the outer binder are
    threaded through correctly. *)
let rec box_expr (env : (string * expr Binder.Var.t) list) (e : expr) :
    expr Binder.Box.t =
  match e with
  | EVar (Lower name) -> (
      match List.assoc_opt name env with
      | Some v -> Binder.Box.var v
      | None -> Binder.Box.pure e)
  | EPrimed _ | ELitNat _ | ELitReal _ | ELitString _ | ELitBool _ | EDomain _
  | EQualified _ ->
      Binder.Box.pure e
  | EApp (f, args) ->
      Binder.Box.apply2
        (fun f args -> EApp (f, args))
        (box_expr env f)
        (Binder.Box.list (List.map (box_expr env) args))
  | EBinop (op, e1, e2) ->
      Binder.Box.apply2
        (fun a b -> EBinop (op, a, b))
        (box_expr env e1) (box_expr env e2)
  | EUnop (op, e) -> Binder.Box.apply (fun a -> EUnop (op, a)) (box_expr env e)
  | ETuple es ->
      Binder.Box.apply
        (fun es -> ETuple es)
        (Binder.Box.list (List.map (box_expr env) es))
  | EProj (e, i) -> Binder.Box.apply (fun a -> EProj (a, i)) (box_expr env e)
  | EOverride (name, pairs) ->
      let pairs_box =
        List.map
          (fun (k, v) ->
            Binder.Box.apply2
              (fun k v -> (k, v))
              (box_expr env k) (box_expr env v))
          pairs
      in
      Binder.Box.apply
        (fun pairs -> EOverride (name, pairs))
        (Binder.Box.list pairs_box)
  | ECond arms ->
      let arms_box =
        List.map
          (fun (a, c) ->
            Binder.Box.apply2
              (fun a c -> (a, c))
              (box_expr env a) (box_expr env c))
          arms
      in
      Binder.Box.apply (fun arms -> ECond arms) (Binder.Box.list arms_box)
  | EInitially e -> Binder.Box.apply (fun e -> EInitially e) (box_expr env e)
  | EForall (mb, metas) ->
      box_quant env (fun mb -> EForall (mb, metas)) mb metas
  | EExists (mb, metas) ->
      box_quant env (fun mb -> EExists (mb, metas)) mb metas
  | EEach (mb, metas, comb) ->
      box_quant env (fun mb -> EEach (mb, metas, comb)) mb metas

and box_quant :
    'a.
    (string * expr Binder.Var.t) list ->
    ((expr, guarded_body) Binder.Mbinder.t -> expr) ->
    (expr, guarded_body) Binder.Mbinder.t ->
    param list ->
    expr Binder.Box.t =
 fun env make_expr mb metas ->
  let vars_arr, (guards, body) = Binder.Mbinder.unbind mb in
  (* Inner params shadow outer references by the same name. Filter them from
     [env] before recursing. *)
  let shadow_names =
    List.map (fun (p : param) -> lower_name p.param_name) metas
  in
  let env' = List.filter (fun (n, _) -> not (List.mem n shadow_names)) env in
  let guards_box = box_guards env' guards in
  let body_box = box_expr env' body in
  let body_pair_box =
    Binder.Box.apply2 (fun g b -> (g, b)) guards_box body_box
  in
  let rebound = Binder.Mbinder.bind vars_arr body_pair_box in
  Binder.Box.apply make_expr rebound

and box_guards env guards = Binder.Box.list (List.map (box_guard env) guards)

and box_guard env = function
  | GParam p -> Binder.Box.pure (GParam p)
  | GIn (name, e) -> Binder.Box.apply (fun e -> GIn (name, e)) (box_expr env e)
  | GExpr e -> Binder.Box.apply (fun e -> GExpr e) (box_expr env e)

(** Bind a quantifier: allocate a fresh [Var.t] for each param name, rewrite the
    guards and body to reference those vars, and return the resulting
    [Mbinder.t]. Guards and body are captured in entry order — so an outer
    [GParam] or [GIn] that shares a name with a top-level param will shadow its
    outer binding in subsequent guards only. *)
let bind_quant (params : param list) (guards : guard list) (body : expr) :
    (expr, guarded_body) Binder.Mbinder.t =
  let env =
    List.map
      (fun (p : param) ->
        let name = lower_name p.param_name in
        (name, fresh_var name))
      params
  in
  let vars_arr = Array.of_list (List.map snd env) in
  let body_pair_box =
    Binder.Box.apply2
      (fun g b -> (g, b))
      (box_guards env guards) (box_expr env body)
  in
  Binder.Box.unbox (Binder.Mbinder.bind vars_arr body_pair_box)

(** Unbind a quantifier: returns the parameter metadata with names refreshed to
    match the [Mbinder]'s internal vars, the guards, and the body. The returned
    [param] records keep their original [type_expr]; only the names are
    refreshed. Callers that need the raw [Var.t] array should use the
    [Binder.Mbinder.unbind] primitive directly. *)
let unbind_quant (mb : (expr, guarded_body) Binder.Mbinder.t)
    (metas : param list) : param list * guard list * expr =
  let vars_arr, (guards, body) = Binder.Mbinder.unbind mb in
  let n = Array.length vars_arr in
  let params =
    List.mapi
      (fun i (p : param) ->
        if i < n then
          { p with param_name = Lower (Binder.Var.name vars_arr.(i)) }
        else p)
      metas
  in
  (params, guards, body)

(** Smart constructors so that call sites can supply the old
    [(params, guards, body)] triple and receive an AST node with the binder
    properly closed. *)
let make_forall (params : param list) (guards : guard list) (body : expr) : expr
    =
  EForall (bind_quant params guards body, params)

let make_exists (params : param list) (guards : guard list) (body : expr) : expr
    =
  EExists (bind_quant params guards body, params)

let make_each (params : param list) (guards : guard list)
    (comb : combiner option) (body : expr) : expr =
  EEach (bind_quant params guards body, params, comb)

(* ========================================================================== *)
(* Hand-written show, pp, and equal for expr / guard / guarded_body.          *)
(* ppx_deriving cannot descend into [Binder.Mbinder.t], so we unbind each     *)
(* quantifier variant and format / compare alpha-equivalently.                *)
(* ========================================================================== *)

let rec pp_expr (fmt : Format.formatter) (e : expr) : unit =
  match e with
  | EVar (Lower n) -> Format.fprintf fmt "EVar (Lower %S)" n
  | EDomain (Upper n) -> Format.fprintf fmt "EDomain (Upper %S)" n
  | EQualified (Upper m, n) ->
      Format.fprintf fmt "EQualified ((Upper %S), %S)" m n
  | ELitNat n -> Format.fprintf fmt "ELitNat %d" n
  | ELitReal r -> Format.fprintf fmt "ELitReal %g" r
  | ELitString s -> Format.fprintf fmt "ELitString %S" s
  | ELitBool b -> Format.fprintf fmt "ELitBool %B" b
  | EApp (f, args) ->
      Format.fprintf fmt "EApp (@[%a,@ [@[%a@]]@])" pp_expr f
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp_expr)
        args
  | EPrimed (Lower n) -> Format.fprintf fmt "EPrimed (Lower %S)" n
  | EOverride (Lower n, pairs) ->
      Format.fprintf fmt "EOverride (Lower %S, [@[%a@]])" n
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" pp_expr k pp_expr v))
        pairs
  | ETuple es ->
      Format.fprintf fmt "ETuple [@[%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           pp_expr)
        es
  | EProj (e, i) -> Format.fprintf fmt "EProj (%a, %d)" pp_expr e i
  | EBinop (op, e1, e2) ->
      Format.fprintf fmt "EBinop (%a, %a, %a)" pp_binop op pp_expr e1 pp_expr e2
  | EUnop (op, e) -> Format.fprintf fmt "EUnop (%a, %a)" pp_unop op pp_expr e
  | EForall (mb, metas) ->
      let params, guards, body = unbind_quant mb metas in
      Format.fprintf fmt "EForall (@[%a,@ %a,@ %a@])" pp_params params pp_guards
        guards pp_expr body
  | EExists (mb, metas) ->
      let params, guards, body = unbind_quant mb metas in
      Format.fprintf fmt "EExists (@[%a,@ %a,@ %a@])" pp_params params pp_guards
        guards pp_expr body
  | EEach (mb, metas, comb) ->
      let params, guards, body = unbind_quant mb metas in
      Format.fprintf fmt "EEach (@[%a,@ %a,@ %a,@ %a@])" pp_params params
        pp_guards guards
        (fun fmt o ->
          match o with
          | None -> Format.fprintf fmt "None"
          | Some c -> Format.fprintf fmt "Some %a" pp_combiner c)
        comb pp_expr body
  | ECond arms ->
      Format.fprintf fmt "ECond [@[%a@]]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (a, c) -> Format.fprintf fmt "(%a, %a)" pp_expr a pp_expr c))
        arms
  | EInitially e -> Format.fprintf fmt "EInitially %a" pp_expr e

and pp_params fmt params =
  Format.fprintf fmt "[@[%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       pp_param)
    params

and pp_guards fmt gs =
  Format.fprintf fmt "[@[%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       pp_guard)
    gs

and pp_guard fmt = function
  | GParam p -> Format.fprintf fmt "GParam %a" pp_param p
  | GIn (Lower n, e) -> Format.fprintf fmt "GIn (Lower %S, %a)" n pp_expr e
  | GExpr e -> Format.fprintf fmt "GExpr %a" pp_expr e

let show_expr e = Format.asprintf "%a" pp_expr e
let show_guard g = Format.asprintf "%a" pp_guard g

(** Alpha-aware structural equality on expressions. Quantifier bodies are
    compared under fresh variables via [Binder.Mbinder.equal]; everything else
    is ordinary structural equality. *)
let rec equal_expr (e1 : expr) (e2 : expr) : bool =
  match (e1, e2) with
  | EVar a, EVar b -> equal_lower_ident a b
  | EDomain a, EDomain b -> equal_upper_ident a b
  | EQualified (m1, n1), EQualified (m2, n2) ->
      equal_upper_ident m1 m2 && String.equal n1 n2
  | ELitNat a, ELitNat b -> Int.equal a b
  | ELitReal a, ELitReal b -> Float.equal a b
  | ELitString a, ELitString b -> String.equal a b
  | ELitBool a, ELitBool b -> Bool.equal a b
  | EApp (f1, a1), EApp (f2, a2) ->
      equal_expr f1 f2 && List.equal equal_expr a1 a2
  | EPrimed a, EPrimed b -> equal_lower_ident a b
  | EOverride (n1, p1), EOverride (n2, p2) ->
      equal_lower_ident n1 n2
      && List.equal
           (fun (k1, v1) (k2, v2) -> equal_expr k1 k2 && equal_expr v1 v2)
           p1 p2
  | ETuple a, ETuple b -> List.equal equal_expr a b
  | EProj (a, i1), EProj (b, i2) -> Int.equal i1 i2 && equal_expr a b
  | EBinop (o1, a1, b1), EBinop (o2, a2, b2) ->
      equal_binop o1 o2 && equal_expr a1 a2 && equal_expr b1 b2
  | EUnop (o1, a1), EUnop (o2, a2) -> equal_unop o1 o2 && equal_expr a1 a2
  | EForall (m1, p1), EForall (m2, p2) ->
      equal_params p1 p2 && Binder.Mbinder.equal equal_guarded_body m1 m2
  | EExists (m1, p1), EExists (m2, p2) ->
      equal_params p1 p2 && Binder.Mbinder.equal equal_guarded_body m1 m2
  | EEach (m1, p1, c1), EEach (m2, p2, c2) ->
      equal_params p1 p2
      && Option.equal equal_combiner c1 c2
      && Binder.Mbinder.equal equal_guarded_body m1 m2
  | ECond a, ECond b ->
      List.equal
        (fun (a1, c1) (a2, c2) -> equal_expr a1 a2 && equal_expr c1 c2)
        a b
  | EInitially a, EInitially b -> equal_expr a b
  | ( ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _
      | ELitString _ | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _
      | EProj _ | EBinop _ | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _
      | EInitially _ ),
      _ ) ->
      false

and equal_params p1 p2 =
  (* Param equality ignores param names — only the type annotations are
     meaningful metadata; the names are set by whatever unbinding happens
     first and are not stable across calls. *)
  List.length p1 = List.length p2
  && List.for_all2
       (fun (a : param) (b : param) ->
         equal_type_expr a.param_type b.param_type)
       p1 p2

and equal_guarded_body (g1, b1) (g2, b2) =
  List.equal equal_guard g1 g2 && equal_expr b1 b2

and equal_guard g1 g2 =
  match (g1, g2) with
  | GParam p1, GParam p2 -> equal_param p1 p2
  | GIn (n1, e1), GIn (n2, e2) -> equal_lower_ident n1 n2 && equal_expr e1 e2
  | GExpr e1, GExpr e2 -> equal_expr e1 e2
  | (GParam _ | GIn _ | GExpr _), _ -> false

(** Declarations in chapter heads *)
type declaration =
  | DeclDomain of upper_ident  (** User. *)
  | DeclAlias of upper_ident * type_expr  (** Point = Nat * Nat. *)
  | DeclRule of {
      name : lower_ident;
      params : param list;
      guards : guard list;
      return_type : type_expr;
      contexts : upper_ident list;
          (** Context footprint: "{Accounts} balance ..." *)
    }
  | DeclAction of {
      label : string;
      params : param list;
      guards : guard list;
      contexts : upper_ident list;  (** Contexts in "Ctx1, Ctx2 ~> action" *)
    }
  | DeclClosure of {
      name : lower_ident;
      param : param;
      return_type : type_expr;
      target : lower_ident;
    }

(* Hand-written show / pp / equal for [declaration] (mutually touches
   [guard list] and [expr] — which no longer derive show/eq). *)

let pp_declaration fmt = function
  | DeclDomain (Upper n) -> Format.fprintf fmt "DeclDomain (Upper %S)" n
  | DeclAlias (Upper n, t) ->
      Format.fprintf fmt "DeclAlias (Upper %S, %a)" n pp_type_expr t
  | DeclRule { name = Lower n; params; guards; return_type; contexts } ->
      Format.fprintf fmt
        "DeclRule {@[name = Lower %S;@ params = %a;@ guards = %a;@ return_type \
         = %a;@ contexts = [@[%a@]]@]}"
        n pp_params params pp_guards guards pp_type_expr return_type
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (Upper u) -> Format.fprintf fmt "Upper %S" u))
        contexts
  | DeclAction { label; params; guards; contexts } ->
      Format.fprintf fmt
        "DeclAction {@[label = %S;@ params = %a;@ guards = %a;@ contexts = \
         [@[%a@]]@]}"
        label pp_params params pp_guards guards
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
           (fun fmt (Upper u) -> Format.fprintf fmt "Upper %S" u))
        contexts
  | DeclClosure { name = Lower n; param; return_type; target = Lower t } ->
      Format.fprintf fmt
        "DeclClosure {@[name = Lower %S;@ param = %a;@ return_type = %a;@ \
         target = Lower %S@]}"
        n pp_param param pp_type_expr return_type t

let show_declaration d = Format.asprintf "%a" pp_declaration d

let equal_declaration (d1 : declaration) (d2 : declaration) : bool =
  match (d1, d2) with
  | DeclDomain a, DeclDomain b -> equal_upper_ident a b
  | DeclAlias (a, t1), DeclAlias (b, t2) ->
      equal_upper_ident a b && equal_type_expr t1 t2
  | ( DeclRule
        { name = n1; params = p1; guards = g1; return_type = r1; contexts = c1 },
      DeclRule
        { name = n2; params = p2; guards = g2; return_type = r2; contexts = c2 }
    ) ->
      equal_lower_ident n1 n2 && equal_params p1 p2
      && List.equal equal_guard g1 g2
      && equal_type_expr r1 r2
      && List.equal equal_upper_ident c1 c2
  | ( DeclAction { label = l1; params = p1; guards = g1; contexts = c1 },
      DeclAction { label = l2; params = p2; guards = g2; contexts = c2 } ) ->
      String.equal l1 l2 && equal_params p1 p2
      && List.equal equal_guard g1 g2
      && List.equal equal_upper_ident c1 c2
  | ( DeclClosure { name = n1; param = p1; return_type = r1; target = t1 },
      DeclClosure { name = n2; param = p2; return_type = r2; target = t2 } ) ->
      equal_lower_ident n1 n2 && equal_param p1 p2 && equal_type_expr r1 r2
      && equal_lower_ident t1 t2
  | (DeclDomain _ | DeclAlias _ | DeclRule _ | DeclAction _ | DeclClosure _), _
    ->
      false

type chapter = {
  head : declaration located list;
  body : expr located list;
  checks : expr located list;
      (** Proof-obligation propositions (after [check] keyword) *)
  trailing_docs : string list list;
      (** Doc comments after the last body proposition (before where/EOF) *)
}
(** A chapter has a head (declarations), body (propositions), and optional check
    block (entailment goals) *)

let pp_chapter fmt { head; body; checks; trailing_docs } =
  Format.fprintf fmt
    "{@[head = [@[%a@]];@ body = [@[%a@]];@ checks = [@[%a@]];@ trailing_docs \
     = [@[%a@]]@]}"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (pp_located pp_declaration))
    head
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (pp_located pp_expr))
    body
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (pp_located pp_expr))
    checks
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (fun fmt para ->
         Format.fprintf fmt "[@[%a@]]"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
              (fun fmt s -> Format.fprintf fmt "%S" s))
           para))
    trailing_docs

let show_chapter c = Format.asprintf "%a" pp_chapter c

let equal_chapter (c1 : chapter) (c2 : chapter) : bool =
  List.equal (equal_located equal_declaration) c1.head c2.head
  && List.equal (equal_located equal_expr) c1.body c2.body
  && List.equal (equal_located equal_expr) c1.checks c2.checks
  && List.equal (List.equal String.equal) c1.trailing_docs c2.trailing_docs

type document = {
  module_name : upper_ident option;  (** None = standalone (no module system) *)
  imports : upper_ident located list;
  contexts : upper_ident located list;  (** Module-level context declarations *)
  chapters : chapter list;
}
(** A complete document *)

let pp_document fmt { module_name; imports; contexts; chapters } =
  Format.fprintf fmt
    "{@[module_name = %a;@ imports = [@[%a@]];@ contexts = [@[%a@]];@ chapters \
     = [@[%a@]]@]}"
    (fun fmt o ->
      match o with
      | None -> Format.fprintf fmt "None"
      | Some u -> Format.fprintf fmt "Some %a" pp_upper_ident u)
    module_name
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (pp_located pp_upper_ident))
    imports
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       (pp_located pp_upper_ident))
    contexts
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@ ")
       pp_chapter)
    chapters

let show_document d = Format.asprintf "%a" pp_document d

let equal_document (d1 : document) (d2 : document) : bool =
  Option.equal equal_upper_ident d1.module_name d2.module_name
  && List.equal (equal_located equal_upper_ident) d1.imports d2.imports
  && List.equal (equal_located equal_upper_ident) d1.contexts d2.contexts
  && List.equal equal_chapter d1.chapters d2.chapters
