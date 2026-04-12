(** Pretty printer for Pantagruel documents *)

open Ast
open Format

let pp_sep_comma fmt () = fprintf fmt ", "
let pp_sep_space fmt () = pp_print_char fmt ' '

(** Print a type expression *)
let rec pp_type_expr fmt = function
  | TName (Upper name) -> pp_print_string fmt name
  | TQName (Upper m, Upper name) -> fprintf fmt "%s::%s" m name
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
  | TName (Upper name) -> pp_print_string fmt name
  | TQName (Upper m, Upper name) -> fprintf fmt "%s::%s" m name
  | TList t -> fprintf fmt "[%a]" pp_type_expr t
  | (TProduct _ | TSum _) as t -> fprintf fmt "(%a)" pp_type_expr t

and pp_type_expr_product fmt = function
  | TProduct ts ->
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt " * ")
        pp_type_expr_atom fmt ts
  | (TName _ | TQName _ | TList _ | TSum _) as t -> pp_type_expr_atom fmt t

(** Print a binary operator *)
let pp_binop fmt = function
  | OpAnd -> pp_print_string fmt "and"
  | OpOr -> pp_print_string fmt "or"
  | OpImpl -> pp_print_string fmt "->"
  | OpIff -> pp_print_string fmt "<->"
  | OpEq -> pp_print_char fmt '='
  | OpNeq -> pp_print_string fmt "~="
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

(** Print a combiner keyword *)
let pp_combiner fmt = function
  | CombAdd -> pp_print_char fmt '+'
  | CombMul -> pp_print_char fmt '*'
  | CombAnd -> pp_print_string fmt "and"
  | CombOr -> pp_print_string fmt "or"
  | CombMin -> pp_print_string fmt "min"
  | CombMax -> pp_print_string fmt "max"

(** Print an expression with proper precedence handling *)
let rec pp_expr fmt = function
  | EInitially e -> fprintf fmt "initially %a" pp_expr e
  | EForall (params, guards, body) ->
      fprintf fmt "all %a | %a" pp_quant_params_guards (params, guards) pp_expr
        body
  | EExists (params, guards, body) ->
      fprintf fmt "some %a | %a" pp_quant_params_guards (params, guards) pp_expr
        body
  | EEach (params, guards, None, body) ->
      fprintf fmt "each %a | %a" pp_quant_params_guards (params, guards) pp_expr
        body
  | EEach (params, guards, Some comb, body) ->
      fprintf fmt "%a over each %a | %a" pp_combiner comb pp_quant_params_guards
        (params, guards) pp_expr body
  | ECond arms ->
      fprintf fmt "cond %a"
        (pp_print_list ~pp_sep:pp_sep_comma (fun fmt (arm, cons) ->
             fprintf fmt "%a => %a" pp_biconditional arm pp_biconditional cons))
        arms
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop _ | EUnop _ ) as e ->
      pp_biconditional fmt e

and pp_biconditional fmt = function
  | EBinop (OpIff, e1, e2) ->
      fprintf fmt "%a <-> %a" pp_implication e1 pp_implication e2
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop
        ( ( OpAnd | OpOr | OpImpl | OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe
          | OpIn | OpSubset | OpAdd | OpSub | OpMul | OpDiv ),
          _,
          _ )
    | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e
    ->
      pp_implication fmt e

and pp_quant_params_guards fmt (params, guards) =
  let items =
    List.map (fun p -> `Param p) params
    @ List.filter_map
        (function
          | GParam p -> Some (`Param p)
          | GIn (Lower name, e) -> Some (`In (name, e))
          | GExpr e -> Some (`Guard e))
        guards
  in
  pp_print_list ~pp_sep:pp_sep_comma
    (fun fmt -> function
      | `Param p -> pp_param fmt p
      | `In (name, e) -> fprintf fmt "%s in %a" name pp_term e
      | `Guard e -> pp_conjunction fmt e)
    fmt items

and pp_param fmt p =
  fprintf fmt "%s: %a" (lower_name p.param_name) pp_type_expr p.param_type

and pp_implication fmt = function
  | EBinop (OpImpl, e1, e2) ->
      fprintf fmt "%a -> %a" pp_disjunction e1 pp_implication e2
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop
        ( ( OpAnd | OpOr | OpIff | OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe
          | OpIn | OpSubset | OpAdd | OpSub | OpMul | OpDiv ),
          _,
          _ )
    | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e
    ->
      pp_disjunction fmt e

and pp_disjunction fmt = function
  | EBinop (OpOr, e1, e2) ->
      fprintf fmt "%a or %a" pp_disjunction e1 pp_conjunction e2
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop
        ( ( OpAnd | OpImpl | OpIff | OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe
          | OpIn | OpSubset | OpAdd | OpSub | OpMul | OpDiv ),
          _,
          _ )
    | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e
    ->
      pp_conjunction fmt e

and pp_conjunction fmt = function
  | EBinop (OpAnd, e1, e2) ->
      fprintf fmt "%a and %a" pp_conjunction e1 pp_negation e2
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop
        ( ( OpOr | OpImpl | OpIff | OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe
          | OpIn | OpSubset | OpAdd | OpSub | OpMul | OpDiv ),
          _,
          _ )
    | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e
    ->
      pp_negation fmt e

and pp_negation fmt = function
  | EUnop (OpNot, e) -> fprintf fmt "~%a" pp_atom e
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop _
    | EUnop ((OpNeg | OpCard), _)
    | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e ->
      pp_comparison fmt e

and pp_comparison fmt = function
  | EBinop
      ( ((OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe | OpIn | OpSubset) as op),
        e1,
        e2 ) ->
      fprintf fmt "%a %a %a" pp_term e1 pp_binop op pp_term e2
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop
        ((OpAnd | OpOr | OpImpl | OpIff | OpAdd | OpSub | OpMul | OpDiv), _, _)
    | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e
    ->
      pp_term fmt e

and pp_term fmt = function
  | EBinop (((OpAdd | OpSub) as op), e1, e2) ->
      fprintf fmt "%a %a %a" pp_term e1 pp_binop op pp_factor e2
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop
        ( ( OpAnd | OpOr | OpImpl | OpIff | OpEq | OpNeq | OpLt | OpGt | OpLe
          | OpGe | OpIn | OpSubset | OpMul | OpDiv ),
          _,
          _ )
    | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e
    ->
      pp_factor fmt e

and pp_factor fmt = function
  | EBinop (((OpMul | OpDiv) as op), e1, e2) ->
      fprintf fmt "%a %a %a" pp_factor e1 pp_binop op pp_unary e2
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop
        ( ( OpAnd | OpOr | OpImpl | OpIff | OpEq | OpNeq | OpLt | OpGt | OpLe
          | OpGe | OpIn | OpSubset | OpAdd | OpSub ),
          _,
          _ )
    | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e
    ->
      pp_unary fmt e

and pp_unary fmt = function
  | EUnop (OpCard, e) -> fprintf fmt "#%a" pp_atom e
  | EUnop (OpNeg, e) -> fprintf fmt "-%a" pp_atom e
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop _
    | EUnop (OpNot, _)
    | EForall _ | EExists _ | EEach _ | ECond _ | EInitially _ ) as e ->
      pp_primary fmt e

and pp_primary fmt = function
  | EApp (f, args) when args <> [] ->
      fprintf fmt "%a %a" pp_atom f
        (pp_print_list ~pp_sep:pp_sep_space pp_atom)
        args
  | ( EVar _ | EDomain _ | EQualified _ | ELitNat _ | ELitReal _ | ELitString _
    | ELitBool _ | EApp _ | EPrimed _ | EOverride _ | ETuple _ | EProj _
    | EBinop _ | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _
    | EInitially _ ) as e ->
      pp_atom fmt e

and pp_atom fmt = function
  | EVar (Lower name) -> pp_print_string fmt name
  | EDomain (Upper name) -> pp_print_string fmt name
  | EQualified (Upper m, name) -> fprintf fmt "%s::%s" m name
  | ELitNat n -> pp_print_int fmt n
  | ELitReal r ->
      let s = string_of_float r in
      let s = if String.ends_with ~suffix:"." s then s ^ "0" else s in
      pp_print_string fmt s
  | ELitString s -> fprintf fmt "\"%s\"" (String.escaped s)
  | ELitBool b -> pp_print_bool fmt b
  | EPrimed (Lower name) -> fprintf fmt "%s'" name
  | EOverride (Lower name, pairs) ->
      fprintf fmt "%s[%a]" name
        (pp_print_list ~pp_sep:pp_sep_comma (fun fmt (k, v) ->
             fprintf fmt "%a |-> %a" pp_expr k pp_expr v))
        pairs
  | ETuple es ->
      fprintf fmt "(%a)" (pp_print_list ~pp_sep:pp_sep_comma pp_expr) es
  | EProj (e, n) -> fprintf fmt "%a.%d" pp_atom e n
  | EApp (f, []) -> pp_atom fmt f
  | ( EApp _ | EBinop _ | EUnop _ | EForall _ | EExists _ | EEach _ | ECond _
    | EInitially _ ) as e ->
      fprintf fmt "(%a)" pp_expr e

(** Print a guard *)
let pp_guard fmt = function
  | GParam p -> pp_param fmt p
  | GIn (Lower name, e) -> fprintf fmt "%s in %a" name pp_term e
  | GExpr e -> pp_conjunction fmt e

(** Print a declaration *)
let pp_declaration fmt = function
  | DeclDomain (Upper name) -> fprintf fmt "%s." name
  | DeclAlias (Upper name, te) -> fprintf fmt "%s = %a." name pp_type_expr te
  | DeclRule { name = Lower name; params; guards; return_type; contexts } ->
      if contexts <> [] then
        fprintf fmt "{%a} "
          (pp_print_list ~pp_sep:pp_sep_comma (fun fmt c ->
               pp_print_string fmt (upper_name c)))
          contexts;
      pp_print_string fmt name;
      if params <> [] then
        fprintf fmt " %a" (pp_print_list ~pp_sep:pp_sep_comma pp_param) params;
      if guards <> [] then
        fprintf fmt ", %a" (pp_print_list ~pp_sep:pp_sep_comma pp_guard) guards;
      fprintf fmt " => %a." pp_type_expr return_type
  | DeclAction { label; params; guards; contexts } ->
      (match contexts with
      | [] -> fprintf fmt "~> "
      | ctxs ->
          fprintf fmt "%a ~> "
            (pp_print_list ~pp_sep:pp_sep_comma (fun fmt c ->
                 pp_print_string fmt (upper_name c)))
            ctxs);
      pp_print_string fmt label;
      if params <> [] then
        fprintf fmt " @@ %a"
          (pp_print_list ~pp_sep:pp_sep_comma pp_param)
          params;
      if guards <> [] then
        fprintf fmt ", %a" (pp_print_list ~pp_sep:pp_sep_comma pp_guard) guards;
      pp_print_char fmt '.'
  | DeclClosure { name = Lower name; param; return_type; target = Lower target }
    ->
      fprintf fmt "%s %a => %a = closure %s." name pp_param param pp_type_expr
        return_type target

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

(* --- Layout-based document output --- *)

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

(** A layout item is an atomic unit of document output. After formatting, there
    is exactly one blank line between each layout item. *)
type layout_item =
  | LDocs of string list  (** A contiguous block of doc comment lines *)
  | LDecls of declaration located list  (** Contiguous declarations *)
  | LProps of expr located list  (** Contiguous propositions *)
  | LSeparator  (** --- *)
  | LCheck  (** check *)
  | LWhere  (** where *)

(** Split a list of located nodes into layout items. Doc comments on nodes
    become separate LDocs items; consecutive nodes without doc comments form
    contiguous groups. *)
let layout_split ~make_group items =
  let result = ref [] in
  let current = ref [] in
  let flush () =
    if !current <> [] then begin
      result := make_group (List.rev !current) :: !result;
      current := []
    end
  in
  List.iter
    (fun item ->
      if item.doc <> [] then begin
        flush ();
        List.iter (fun group -> result := LDocs group :: !result) item.doc
      end;
      current := item :: !current)
    items;
  flush ();
  List.rev !result

let layout_of_chapter chapter =
  layout_split ~make_group:(fun ds -> LDecls ds) chapter.head
  @ [ LSeparator ]
  @ layout_split ~make_group:(fun ps -> LProps ps) chapter.body
  @ (if chapter.checks <> [] then
       [ LCheck ]
       @ layout_split ~make_group:(fun ps -> LProps ps) chapter.checks
     else [])
  @ List.map (fun group -> LDocs group) chapter.trailing_docs

let layout_of_document doc =
  List.concat
    (List.mapi
       (fun i ch -> (if i > 0 then [ LWhere ] else []) @ layout_of_chapter ch)
       doc.chapters)

(** Render layout items with a blank line between each *)
let render_layout ?(width = 80) fmt items =
  List.iteri
    (fun i item ->
      if i > 0 then fprintf fmt "@\n";
      match item with
      | LDocs lines ->
          let wrapped = List.map (wrap_text (width - 2)) lines in
          let all_lines = List.concat_map (String.split_on_char '\n') wrapped in
          List.iter (fun line -> fprintf fmt "> %s@\n" line) all_lines
      | LDecls decls ->
          List.iter (fun d -> fprintf fmt "%a@\n" pp_declaration d.value) decls
      | LProps props ->
          List.iter (fun p -> fprintf fmt "%a.@\n" pp_expr p.value) props
      | LSeparator -> fprintf fmt "---@\n"
      | LCheck -> fprintf fmt "check@\n"
      | LWhere -> fprintf fmt "where@\n")
    items

(** Output a document with consistent layout *)
let output_document ?(width = 80) fmt doc =
  (match doc.module_name with
  | Some (Upper name) -> fprintf fmt "module %s.@\n" name
  | None -> ());
  List.iter
    (fun imp -> fprintf fmt "import %s.@\n" (upper_name imp.value))
    doc.imports;
  List.iter
    (fun ctx -> fprintf fmt "context %s.@\n" (upper_name ctx.value))
    doc.contexts;
  let items = layout_of_document doc in
  let has_preamble =
    doc.module_name <> None || doc.imports <> [] || doc.contexts <> []
  in
  if has_preamble && items <> [] then fprintf fmt "@\n";
  render_layout ~width fmt items

(** Output formatted document to stdout *)
let output_formatted ?(width = 80) doc =
  pp_set_margin std_formatter 10000;
  output_document ~width std_formatter doc;
  pp_print_flush std_formatter ()

(** Output document to stdout (default width) *)
let output doc = output_formatted ~width:10000 doc
