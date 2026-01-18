(** Pretty printer for Pantagruel documents *)

open Ast

(** Print a type expression *)
let rec pp_type_expr = function
  | TName name -> name
  | TList t -> "[" ^ pp_type_expr t ^ "]"
  | TProduct ts ->
      String.concat " * " (List.map pp_type_expr_atom ts)
  | TSum ts ->
      String.concat " + " (List.map pp_type_expr_product ts)

and pp_type_expr_atom = function
  | TName name -> name
  | TList t -> "[" ^ pp_type_expr t ^ "]"
  | t -> "(" ^ pp_type_expr t ^ ")"

and pp_type_expr_product = function
  | TProduct ts -> String.concat " * " (List.map pp_type_expr_atom ts)
  | t -> pp_type_expr_atom t

(** Print a binary operator *)
let pp_binop = function
  | OpAnd -> "and"
  | OpOr -> "or"
  | OpImpl -> "->"
  | OpIff -> "<->"
  | OpEq -> "="
  | OpNeq -> "!="
  | OpLt -> "<"
  | OpGt -> ">"
  | OpLe -> "<="
  | OpGe -> ">="
  | OpIn -> "in"
  | OpSubset -> "subset"
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"

(** Print a unary operator *)
let pp_unop = function
  | OpNot -> "not "
  | OpNeg -> "-"
  | OpCard -> "#"

(** Print an expression with proper precedence handling *)
let rec pp_expr = function
  | EForall (params, guards, body) ->
      "all " ^ pp_quant_params_guards params guards ^ " | " ^ pp_expr body
  | EExists (params, guards, body) ->
      "some " ^ pp_quant_params_guards params guards ^ " | " ^ pp_expr body
  | e -> pp_biconditional e

and pp_biconditional = function
  | EBinop (OpIff, e1, e2) ->
      pp_implication e1 ^ " <-> " ^ pp_implication e2
  | e -> pp_implication e

and pp_quant_params_guards params guards =
  let param_strs = List.map pp_param params in
  let guard_strs = List.filter_map (function
    | GParam p -> Some (pp_param p)
    | GIn (name, e) -> Some (name ^ " in " ^ pp_term e)
    | GExpr e -> Some (pp_conjunction e)
  ) guards in
  String.concat ", " (param_strs @ guard_strs)

and pp_param p =
  p.param_name ^ ": " ^ pp_type_expr p.param_type

and pp_implication = function
  | EBinop (OpImpl, e1, e2) ->
      pp_disjunction e1 ^ " -> " ^ pp_expr e2
  | e -> pp_disjunction e

and pp_disjunction = function
  | EBinop (OpOr, e1, e2) ->
      pp_disjunction e1 ^ " or " ^ pp_conjunction e2
  | e -> pp_conjunction e

and pp_conjunction = function
  | EBinop (OpAnd, e1, e2) ->
      pp_conjunction e1 ^ " and " ^ pp_negation e2
  | e -> pp_negation e

and pp_negation = function
  | EUnop (OpNot, e) -> "not " ^ pp_negation e
  | e -> pp_comparison e

and pp_comparison = function
  | EBinop ((OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe | OpIn | OpSubset) as op, e1, e2) ->
      pp_term e1 ^ " " ^ pp_binop op ^ " " ^ pp_term e2
  | e -> pp_term e

and pp_term = function
  | EBinop ((OpAdd | OpSub) as op, e1, e2) ->
      pp_term e1 ^ " " ^ pp_binop op ^ " " ^ pp_factor e2
  | e -> pp_factor e

and pp_factor = function
  | EBinop ((OpMul | OpDiv) as op, e1, e2) ->
      pp_factor e1 ^ " " ^ pp_binop op ^ " " ^ pp_unary e2
  | e -> pp_unary e

and pp_unary = function
  | EUnop (OpCard, e) -> "#" ^ pp_unary e
  | EUnop (OpNeg, e) -> "-" ^ pp_unary e
  | e -> pp_primary e

and pp_primary = function
  | EApp (f, args) when args <> [] ->
      pp_atom f ^ " " ^ String.concat " " (List.map pp_atom args)
  | e -> pp_atom e

and pp_atom = function
  | EVar name -> name
  | EDomain name -> name
  | EQualified (m, name) -> m ^ "::" ^ name
  | ELitNat n -> string_of_int n
  | ELitReal r ->
      (* Ensure floats always have digits after decimal point *)
      let s = string_of_float r in
      if String.ends_with ~suffix:"." s then s ^ "0" else s
  | ELitString s -> "\"" ^ String.escaped s ^ "\""
  | ELitBool true -> "true"
  | ELitBool false -> "false"
  | EPrimed name -> name ^ "'"
  | EOverride (name, pairs) ->
      name ^ "[" ^ String.concat ", " (List.map (fun (k, v) ->
        pp_expr k ^ " |-> " ^ pp_expr v
      ) pairs) ^ "]"
  | ETuple es ->
      "(" ^ String.concat ", " (List.map pp_expr es) ^ ")"
  | EProj (e, n) ->
      pp_atom e ^ "." ^ string_of_int n
  | EApp (f, []) -> pp_atom f
  | e -> "(" ^ pp_expr e ^ ")"

(** Print a guard *)
let pp_guard = function
  | GParam p -> pp_param p
  | GIn (name, e) -> name ^ " in " ^ pp_term e
  | GExpr e -> pp_conjunction e  (* Guards use conjunction level *)

(** Print a declaration *)
let pp_declaration = function
  | DeclDomain name -> name ^ "."
  | DeclAlias (name, te) -> name ^ " = " ^ pp_type_expr te ^ "."
  | DeclProc { name; params; guards; return_type } ->
      let parts = [name] in
      let parts = if params <> [] then
        parts @ [" " ^ String.concat ", " (List.map pp_param params)]
      else parts in
      let parts = if guards <> [] then
        parts @ [", " ^ String.concat ", " (List.map pp_guard guards)]
      else parts in
      let parts = match return_type with
        | None -> parts
        | Some te -> parts @ [" => " ^ pp_type_expr te]
      in
      String.concat "" parts ^ "."

(** Print doc comments *)
let pp_docs docs =
  String.concat "\n" (List.map (fun d -> "> " ^ d) docs)

(** Print a declaration with its doc comments *)
let pp_declaration_with_docs d =
  let decl_str = pp_declaration d.Ast.value in
  if d.doc = [] then decl_str
  else pp_docs d.doc ^ "\n" ^ decl_str

(** Print a proposition with its doc comments *)
let pp_proposition_with_docs p =
  let prop_str = pp_expr p.Ast.value ^ "." in
  if p.doc = [] then prop_str
  else pp_docs p.doc ^ "\n" ^ prop_str

(** Print a chapter *)
let pp_chapter chapter =
  let head_lines = List.map pp_declaration_with_docs chapter.head in
  let body_lines = List.map pp_proposition_with_docs chapter.body in
  String.concat "\n" head_lines ^ "\n---\n" ^ String.concat "\n" body_lines

(** Print a document *)
let pp_document doc =
  let imports = List.map (fun i -> "import " ^ i.value ^ ".") doc.imports in
  let chapters = List.map pp_chapter doc.chapters in
  let parts = ["module " ^ doc.module_name ^ "."] @ imports @
              [String.concat "\n\nwhere\n\n" chapters] in
  String.concat "\n" parts ^ "\n"

(** Output document to stdout *)
let output doc =
  print_string (pp_document doc)
