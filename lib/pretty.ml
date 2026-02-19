(** Pretty printer for Pantagruel documents *)

open Ast
open Format

let pp_sep_comma fmt () = fprintf fmt ", "
let pp_sep_space fmt () = pp_print_char fmt ' '

(** Print a type expression *)
let rec pp_type_expr fmt = function
  | TName name -> pp_print_string fmt name
  | TQName (m, name) -> fprintf fmt "%s::%s" m name
  | TList t -> fprintf fmt "[%a]" pp_type_expr t
  | TProduct ts ->
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " * ")
        pp_type_expr_atom fmt ts
  | TSum ts ->
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " + ")
        pp_type_expr_product fmt ts

and pp_type_expr_atom fmt = function
  | TName name -> pp_print_string fmt name
  | TQName (m, name) -> fprintf fmt "%s::%s" m name
  | TList t -> fprintf fmt "[%a]" pp_type_expr t
  | t -> fprintf fmt "(%a)" pp_type_expr t

and pp_type_expr_product fmt = function
  | TProduct ts ->
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " * ")
        pp_type_expr_atom fmt ts
  | t -> pp_type_expr_atom fmt t

(** Print a binary operator *)
let pp_binop fmt = function
  | OpAnd -> pp_print_string fmt "and"
  | OpOr -> pp_print_string fmt "or"
  | OpImpl -> pp_print_string fmt "->"
  | OpIff -> pp_print_string fmt "<->"
  | OpEq -> pp_print_char fmt '='
  | OpNeq -> pp_print_string fmt "!="
  | OpLt -> pp_print_char fmt '<'
  | OpGt -> pp_print_char fmt '>'
  | OpLe -> pp_print_string fmt "<="
  | OpGe -> pp_print_string fmt ">="
  | OpIn -> pp_print_string fmt "in"
  | OpSubset -> pp_print_string fmt "subset"
  | OpAdd -> pp_print_char fmt '+'
  | OpSub -> pp_print_char fmt '-'
  | OpMul -> pp_print_char fmt '*'
  | OpDiv -> pp_print_char fmt '/'

(** Print an expression with proper precedence handling *)
let rec pp_expr fmt = function
  | EForall (params, guards, body) ->
      fprintf fmt "all %a | %a" pp_quant_params_guards (params, guards) pp_expr
        body
  | EExists (params, guards, body) ->
      fprintf fmt "some %a | %a" pp_quant_params_guards (params, guards) pp_expr
        body
  | e -> pp_biconditional fmt e

and pp_biconditional fmt = function
  | EBinop (OpIff, e1, e2) ->
      fprintf fmt "%a <-> %a" pp_implication e1 pp_implication e2
  | e -> pp_implication fmt e

and pp_quant_params_guards fmt (params, guards) =
  let items =
    List.map (fun p -> `Param p) params
    @ List.filter_map
        (function
          | GParam p -> Some (`Param p)
          | GIn (name, e) -> Some (`In (name, e))
          | GExpr e -> Some (`Guard e))
        guards
  in
  pp_print_list ~pp_sep:pp_sep_comma
    (fun fmt -> function
      | `Param p -> pp_param fmt p
      | `In (name, e) -> fprintf fmt "%s in %a" name pp_term e
      | `Guard e -> pp_conjunction fmt e)
    fmt items

and pp_param fmt p = fprintf fmt "%s: %a" p.param_name pp_type_expr p.param_type

and pp_implication fmt = function
  | EBinop (OpImpl, e1, e2) ->
      fprintf fmt "%a -> %a" pp_disjunction e1 pp_implication e2
  | e -> pp_disjunction fmt e

and pp_disjunction fmt = function
  | EBinop (OpOr, e1, e2) ->
      fprintf fmt "%a or %a" pp_disjunction e1 pp_conjunction e2
  | e -> pp_conjunction fmt e

and pp_conjunction fmt = function
  | EBinop (OpAnd, e1, e2) ->
      fprintf fmt "%a and %a" pp_conjunction e1 pp_negation e2
  | e -> pp_negation fmt e

and pp_negation fmt = function
  | EUnop (OpNot, e) -> fprintf fmt "not %a" pp_negation e
  | e -> pp_comparison fmt e

and pp_comparison fmt = function
  | EBinop
      ( ((OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe | OpIn | OpSubset) as op),
        e1,
        e2 ) ->
      fprintf fmt "%a %a %a" pp_term e1 pp_binop op pp_term e2
  | e -> pp_term fmt e

and pp_term fmt = function
  | EBinop (((OpAdd | OpSub) as op), e1, e2) ->
      fprintf fmt "%a %a %a" pp_term e1 pp_binop op pp_factor e2
  | e -> pp_factor fmt e

and pp_factor fmt = function
  | EBinop (((OpMul | OpDiv) as op), e1, e2) ->
      fprintf fmt "%a %a %a" pp_factor e1 pp_binop op pp_unary e2
  | e -> pp_unary fmt e

and pp_unary fmt = function
  | EUnop (OpCard, e) -> fprintf fmt "#%a" pp_unary e
  | EUnop (OpNeg, e) -> fprintf fmt "-%a" pp_unary e
  | e -> pp_primary fmt e

and pp_primary fmt = function
  | EApp (f, args) when args <> [] ->
      fprintf fmt "%a %a" pp_atom f
        (pp_print_list ~pp_sep:pp_sep_space pp_atom)
        args
  | e -> pp_atom fmt e

and pp_atom fmt = function
  | EVar name -> pp_print_string fmt name
  | EDomain name -> pp_print_string fmt name
  | EQualified (m, name) -> fprintf fmt "%s::%s" m name
  | ELitNat n -> pp_print_int fmt n
  | ELitReal r ->
      let s = string_of_float r in
      let s = if String.ends_with ~suffix:"." s then s ^ "0" else s in
      pp_print_string fmt s
  | ELitString s -> fprintf fmt "\"%s\"" (String.escaped s)
  | ELitBool b -> pp_print_bool fmt b
  | EPrimed name -> fprintf fmt "%s'" name
  | EOverride (name, pairs) ->
      fprintf fmt "%s[%a]" name
        (pp_print_list ~pp_sep:pp_sep_comma (fun fmt (k, v) ->
             fprintf fmt "%a |-> %a" pp_expr k pp_expr v))
        pairs
  | ETuple es ->
      fprintf fmt "(%a)" (pp_print_list ~pp_sep:pp_sep_comma pp_expr) es
  | EProj (e, n) -> fprintf fmt "%a.%d" pp_atom e n
  | EApp (f, []) -> pp_atom fmt f
  | e -> fprintf fmt "(%a)" pp_expr e

(** Print a guard *)
let pp_guard fmt = function
  | GParam p -> pp_param fmt p
  | GIn (name, e) -> fprintf fmt "%s in %a" name pp_term e
  | GExpr e -> pp_conjunction fmt e

(** Print a declaration *)
let pp_declaration fmt = function
  | DeclDomain name -> fprintf fmt "%s." name
  | DeclAlias (name, te) -> fprintf fmt "%s = %a." name pp_type_expr te
  | DeclProc { name; params; guards; return_type; contexts; context } ->
      if contexts <> [] then
        fprintf fmt "{%a} "
          (pp_print_list ~pp_sep:pp_sep_comma pp_print_string)
          contexts;
      pp_print_string fmt name;
      if params <> [] then
        fprintf fmt " %a" (pp_print_list ~pp_sep:pp_sep_comma pp_param) params;
      if guards <> [] then
        fprintf fmt ", %a" (pp_print_list ~pp_sep:pp_sep_comma pp_guard) guards;
      (match return_type with
      | None -> ()
      | Some te -> fprintf fmt " => %a" pp_type_expr te);
      (match context with None -> () | Some ctx -> fprintf fmt " in %s" ctx);
      pp_print_char fmt '.'

(* --- String-returning wrappers --- *)

(** Format to string with a wide margin so the formatter never wraps *)
let to_string pp x =
  let buf = Buffer.create 256 in
  let fmt = formatter_of_buffer buf in
  pp_set_margin fmt 10000;
  pp fmt x;
  pp_print_flush fmt ();
  Buffer.contents buf

let str_expr = to_string pp_expr
let str_declaration = to_string pp_declaration
let str_type_expr = to_string pp_type_expr

(** Print doc comments *)
let pp_docs fmt docs = List.iter (fun d -> fprintf fmt "> %s@\n" d) docs

(** Print a declaration with its doc comments *)
let pp_declaration_with_docs fmt d =
  if d.doc <> [] then pp_docs fmt d.doc;
  fprintf fmt "%a@\n" pp_declaration d.value

(** Print a proposition with its doc comments *)
let pp_proposition_with_docs fmt p =
  if p.doc <> [] then pp_docs fmt p.doc;
  fprintf fmt "%a.@\n" pp_expr p.value

(** Print a chapter *)
let pp_chapter fmt chapter =
  List.iter (pp_declaration_with_docs fmt) chapter.head;
  fprintf fmt "---@\n";
  List.iter (pp_proposition_with_docs fmt) chapter.body

(** Print a document *)
let pp_document fmt doc =
  (match doc.module_name with
  | Some name -> fprintf fmt "module %s.@\n" name
  | None -> ());
  List.iter (fun i -> fprintf fmt "import %s.@\n" i.value) doc.imports;
  List.iter (fun c -> fprintf fmt "context %s.@\n" c.value) doc.contexts;
  List.iteri
    (fun i chapter ->
      if i > 0 then fprintf fmt "@\nwhere@\n@\n";
      pp_chapter fmt chapter)
    doc.chapters

(** Output document to stdout *)
let output doc =
  pp_set_margin std_formatter 10000;
  pp_document std_formatter doc;
  pp_print_flush std_formatter ()

(* --- Formatting with wrapping and consistent style --- *)

(** Wrap text to max width, preserving words *)
let wrap_text width text =
  let words = String.split_on_char ' ' text in
  let rec go line_len acc = function
    | [] -> List.rev acc
    | word :: rest ->
        let wlen = String.length word in
        if line_len + 1 + wlen > width && line_len > 0 then
          go wlen (word :: "\n" :: acc) rest
        else if line_len = 0 then go wlen (word :: acc) rest
        else go (line_len + 1 + wlen) (word :: " " :: acc) rest
  in
  String.concat "" (go 0 [] words)

(** Format doc comments with wrapping *)
let pp_docs_wrapped width fmt docs =
  let wrapped = List.map (wrap_text (width - 2)) docs in
  (* -2 for "> " prefix *)
  let lines = List.concat_map (String.split_on_char '\n') wrapped in
  List.iter (fun line -> fprintf fmt "> %s@\n" line) lines

(** Format a chapter with consistent style *)
let format_chapter ?(width = 80) fmt chapter =
  let prev_had_doc = ref false in
  List.iter
    (fun decl ->
      let has_doc = decl.doc <> [] in
      if has_doc && !prev_had_doc then fprintf fmt "@\n";
      if has_doc then pp_docs_wrapped width fmt decl.doc;
      fprintf fmt "%a@\n" pp_declaration decl.value;
      prev_had_doc := has_doc)
    chapter.head;
  fprintf fmt "---@\n";
  List.iter
    (fun prop ->
      if prop.doc <> [] then pp_docs_wrapped width fmt prop.doc;
      fprintf fmt "%a.@\n" pp_expr prop.value)
    chapter.body

(** Format a document with consistent style *)
let format_document ?(width = 80) fmt doc =
  (match doc.module_name with
  | Some name -> fprintf fmt "module %s.@\n" name
  | None -> ());
  List.iter (fun imp -> fprintf fmt "import %s.@\n" imp.value) doc.imports;
  List.iter (fun ctx -> fprintf fmt "context %s.@\n" ctx.value) doc.contexts;
  if doc.imports <> [] || doc.contexts <> [] then fprintf fmt "@\n";
  List.iteri
    (fun i chapter ->
      if i > 0 then fprintf fmt "@\nwhere@\n@\n";
      format_chapter ~width fmt chapter)
    doc.chapters

(** Output formatted document to stdout *)
let output_formatted ?(width = 80) doc =
  pp_set_margin std_formatter 10000;
  format_document ~width std_formatter doc;
  pp_print_flush std_formatter ()
