(** Rich Markdown output for Pantagruel documents *)

open Ast
open Format
module StringSet = Set.Make (String)

(** Extract rule names from environment *)
let rule_names_of_env env =
  Env.StringMap.fold
    (fun name entry acc ->
      match entry.Env.kind with
      | Env.KRule _ -> StringSet.add name acc
      | _ -> acc)
    env.Env.terms StringSet.empty

(* --- Rich markdown expression rendering with unicode operators --- *)

let pp_type_expr fmt te =
  let rec go fmt = function
    | TName name -> fprintf fmt "`%s`" name
    | TQName (m, name) -> fprintf fmt "`%s`::`%s`" m name
    | TList t -> fprintf fmt "[%a]" go t
    | TProduct ts ->
        pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " × ") go_atom fmt ts
    | TSum ts ->
        pp_print_list
          ~pp_sep:(fun fmt () -> fprintf fmt " + ")
          go_product fmt ts
  and go_atom fmt = function
    | TName name -> fprintf fmt "`%s`" name
    | TQName (m, name) -> fprintf fmt "`%s`::`%s`" m name
    | TList t -> fprintf fmt "[%a]" go t
    | t -> fprintf fmt "(%a)" go t
  and go_product fmt = function
    | TProduct ts ->
        pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " × ") go_atom fmt ts
    | t -> go_atom fmt t
  in
  go fmt te

let pp_binop fmt = function
  | OpAnd -> pp_print_string fmt "∧"
  | OpOr -> pp_print_string fmt "∨"
  | OpImpl -> pp_print_string fmt "→"
  | OpIff -> pp_print_string fmt "↔"
  | OpEq -> pp_print_char fmt '='
  | OpNeq -> pp_print_string fmt "≠"
  | OpLt -> pp_print_char fmt '<'
  | OpGt -> pp_print_char fmt '>'
  | OpLe -> pp_print_string fmt "≤"
  | OpGe -> pp_print_string fmt "≥"
  | OpIn -> pp_print_string fmt "∈"
  | OpSubset -> pp_print_string fmt "⊆"
  | OpAdd -> pp_print_char fmt '+'
  | OpSub -> pp_print_char fmt '-'
  | OpMul -> pp_print_string fmt "·"
  | OpDiv -> pp_print_char fmt '/'

let pp_name procs fmt name =
  if StringSet.mem name procs then fprintf fmt "**%s**" name
  else fprintf fmt "*%s*" name

let pp_param fmt p =
  fprintf fmt "*%s*: %a" p.param_name pp_type_expr p.param_type

let pp_params_sep fmt () = fprintf fmt ", "

let rec pp_expr procs fmt = function
  | EForall (params, guards, body) ->
      fprintf fmt "∀ %a · %a" (pp_quant_bindings procs) (params, guards)
        (pp_expr procs) body
  | EExists (params, guards, body) ->
      fprintf fmt "∃ %a · %a" (pp_quant_bindings procs) (params, guards)
        (pp_expr procs) body
  | e -> pp_biconditional procs fmt e

and pp_quant_bindings procs fmt (params, guards) =
  let items =
    List.map (fun p -> `Param p) params
    @ List.filter_map
        (function
          | GParam p -> Some (`Param p)
          | GIn (name, e) -> Some (`In (name, e))
          | GExpr e -> Some (`Guard e))
        guards
  in
  pp_print_list ~pp_sep:pp_params_sep
    (fun fmt -> function
      | `Param p -> pp_param fmt p
      | `In (name, e) -> fprintf fmt "*%s* ∈ %a" name (pp_term procs) e
      | `Guard e -> pp_conjunction procs fmt e)
    fmt items

and pp_biconditional procs fmt = function
  | EBinop (OpIff, e1, e2) ->
      fprintf fmt "%a ↔ %a" (pp_implication procs) e1 (pp_implication procs) e2
  | e -> pp_implication procs fmt e

and pp_implication procs fmt = function
  | EBinop (OpImpl, e1, e2) ->
      fprintf fmt "%a → %a" (pp_disjunction procs) e1 (pp_implication procs) e2
  | e -> pp_disjunction procs fmt e

and pp_disjunction procs fmt = function
  | EBinop (OpOr, e1, e2) ->
      fprintf fmt "%a ∨ %a" (pp_disjunction procs) e1 (pp_conjunction procs) e2
  | e -> pp_conjunction procs fmt e

and pp_conjunction procs fmt = function
  | EBinop (OpAnd, e1, e2) ->
      fprintf fmt "%a ∧ %a" (pp_conjunction procs) e1 (pp_negation procs) e2
  | e -> pp_negation procs fmt e

and pp_negation procs fmt = function
  | EUnop (OpNot, e) -> fprintf fmt "¬%a" (pp_negation procs) e
  | e -> pp_comparison procs fmt e

and pp_comparison procs fmt = function
  | EBinop
      ( ((OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe | OpIn | OpSubset) as op),
        e1,
        e2 ) ->
      fprintf fmt "%a %a %a" (pp_term procs) e1 pp_binop op (pp_term procs) e2
  | e -> pp_term procs fmt e

and pp_term procs fmt = function
  | EBinop (((OpAdd | OpSub) as op), e1, e2) ->
      fprintf fmt "%a %a %a" (pp_term procs) e1 pp_binop op (pp_factor procs) e2
  | e -> pp_factor procs fmt e

and pp_factor procs fmt = function
  | EBinop (((OpMul | OpDiv) as op), e1, e2) ->
      fprintf fmt "%a %a %a" (pp_factor procs) e1 pp_binop op (pp_unary procs)
        e2
  | e -> pp_unary procs fmt e

and pp_unary procs fmt = function
  | EUnop (OpCard, e) -> fprintf fmt "#%a" (pp_unary procs) e
  | EUnop (OpNeg, e) -> fprintf fmt "-%a" (pp_unary procs) e
  | e -> pp_primary procs fmt e

and pp_primary procs fmt = function
  | EApp (f, args) when args <> [] ->
      fprintf fmt "%a %a" (pp_atom procs) f
        (pp_print_list ~pp_sep:pp_print_space (pp_atom procs))
        args
  | e -> pp_atom procs fmt e

and pp_atom procs fmt = function
  | EVar name -> pp_name procs fmt name
  | EDomain name -> fprintf fmt "`%s`" name
  | EQualified (m, name) -> fprintf fmt "`%s`::%a" m (pp_name procs) name
  | ELitNat n -> pp_print_int fmt n
  | ELitReal r ->
      let s = string_of_float r in
      let s = if String.ends_with ~suffix:"." s then s ^ "0" else s in
      pp_print_string fmt s
  | ELitString s -> fprintf fmt "\"%s\"" (String.escaped s)
  | ELitBool b -> pp_print_bool fmt b
  | EPrimed name -> fprintf fmt "%a′" (pp_name procs) name
  | EOverride (name, pairs) ->
      fprintf fmt "%a[%a]" (pp_name procs) name
        (pp_print_list ~pp_sep:pp_params_sep (fun fmt (k, v) ->
             fprintf fmt "%a ↦ %a" (pp_expr procs) k (pp_expr procs) v))
        pairs
  | ETuple es ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:pp_params_sep (pp_expr procs))
        es
  | EProj (e, n) -> fprintf fmt "%a.%d" (pp_atom procs) e n
  | EApp (f, []) -> pp_atom procs fmt f
  | e -> fprintf fmt "(%a)" (pp_expr procs) e

let pp_guard procs fmt = function
  | GParam p -> pp_param fmt p
  | GIn (name, e) -> fprintf fmt "*%s* ∈ %a" name (pp_term procs) e
  | GExpr e -> pp_conjunction procs fmt e

let pp_declaration procs fmt = function
  | DeclDomain name -> fprintf fmt "`%s`." name
  | DeclAlias (name, te) -> fprintf fmt "`%s` = %a." name pp_type_expr te
  | DeclRule { name; params; guards; return_type; contexts } ->
      if contexts <> [] then
        fprintf fmt "{%a} "
          (pp_print_list ~pp_sep:pp_params_sep (fun fmt c ->
               fprintf fmt "**`%s`**" c))
          contexts;
      fprintf fmt "**%s**" name;
      if params <> [] then
        fprintf fmt " %a" (pp_print_list ~pp_sep:pp_params_sep pp_param) params;
      if guards <> [] then
        fprintf fmt ", %a"
          (pp_print_list ~pp_sep:pp_params_sep (pp_guard procs))
          guards;
      fprintf fmt " ⇒ %a." pp_type_expr return_type
  | DeclAction { label; params; guards; context } ->
      (match context with
      | Some ctx -> fprintf fmt "**`%s`** ↝ " ctx
      | None -> fprintf fmt "↝ ");
      fprintf fmt "%s" label;
      if params <> [] then
        fprintf fmt " %a" (pp_print_list ~pp_sep:pp_params_sep pp_param) params;
      if guards <> [] then
        fprintf fmt ", %a"
          (pp_print_list ~pp_sep:pp_params_sep (pp_guard procs))
          guards;
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

let md_expr procs e = to_string (pp_expr procs) e
let md_declaration procs d = to_string (pp_declaration procs) d

(* --- Markdown document rendering --- *)

let rec drop n lst =
  if n <= 0 then lst
  else match lst with [] -> [] | _ :: rest -> drop (n - 1) rest

let pp_doc_comment fmt doc =
  let pp_paragraph fmt para =
    fprintf fmt "> %s" (String.concat " " para)
  in
  pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@\n>@\n") pp_paragraph fmt
    doc

let pp_decl_with_doc procs ~skip_doc_groups fmt d =
  let doc = drop skip_doc_groups d.doc in
  if doc <> [] then fprintf fmt "%a@\n@\n" pp_doc_comment doc;
  fprintf fmt "%a@\n@\n" (pp_declaration procs) d.value

let pp_chapter procs ?(skip_first_doc_groups = 0) ~total_chapters chapter_num
    fmt chapter =
  if total_chapters > 1 then fprintf fmt "## Chapter %d@\n@\n" chapter_num;

  let first_line =
    match chapter.head with d :: _ -> Some d.loc.line | [] -> None
  in

  let domains, rule_decls, action_decls =
    List.fold_left
      (fun (ds, rs, acts) decl ->
        match decl.value with
        | DeclDomain _ | DeclAlias _ -> (decl :: ds, rs, acts)
        | DeclRule _ -> (ds, decl :: rs, acts)
        | DeclAction _ -> (ds, rs, decl :: acts))
      ([], [], []) chapter.head
  in
  let domains = List.rev domains in
  let rule_decls = List.rev rule_decls in
  let action_decls = List.rev action_decls in

  let skip_for d =
    if skip_first_doc_groups > 0 && Some d.loc.line = first_line then
      skip_first_doc_groups
    else 0
  in

  let section_header_doc decls =
    match decls with
    | first :: _ ->
        let doc = drop (skip_for first) first.doc in
        if doc <> [] && not first.doc_adjacent then Some doc else None
    | [] -> None
  in

  let pp_domain_name fmt d =
    match d.value with
    | DeclDomain n | DeclAlias (n, _) -> fprintf fmt "`%s`" n
    | _ -> ()
  in

  if domains <> [] then begin
    fprintf fmt "### Domains@\n@\n";
    match section_header_doc domains with
    | Some doc ->
        fprintf fmt "%a@\n@\n" pp_doc_comment doc;
        pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_domain_name
          fmt domains;
        fprintf fmt "@\n@\n"
    | None ->
        let any_docs =
          List.exists (fun d -> drop (skip_for d) d.doc <> []) domains
        in
        if any_docs then
          List.iter
            (fun d ->
              pp_decl_with_doc procs ~skip_doc_groups:(skip_for d) fmt d)
            domains
        else begin
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_domain_name
            fmt domains;
          fprintf fmt "@\n@\n"
        end
  end;

  if rule_decls <> [] then begin
    fprintf fmt "### Rules@\n@\n";
    List.iter
      (fun d -> pp_decl_with_doc procs ~skip_doc_groups:(skip_for d) fmt d)
      rule_decls
  end;

  if action_decls <> [] then begin
    fprintf fmt "### Action@\n@\n";
    List.iter
      (fun d -> pp_decl_with_doc procs ~skip_doc_groups:(skip_for d) fmt d)
      action_decls
  end;

  if chapter.body <> [] then begin
    fprintf fmt "---@\n@\n";
    List.iter
      (fun prop ->
        if prop.doc <> [] then fprintf fmt "%a@\n@\n" pp_doc_comment prop.doc;
        fprintf fmt "%a.@\n@\n" (pp_expr procs) prop.value)
      chapter.body
  end

let pp_document procs fmt doc =
  let skip_first_doc_groups =
    match doc.chapters with
    | { head = first :: _; _ } :: _ when first.doc <> [] ->
        if not first.doc_adjacent then begin
          (* All doc groups float above the declaration — module-level doc *)
          List.iter
            (fun para -> fprintf fmt "%s@\n@\n" (String.concat "\n" para))
            first.doc;
          List.length first.doc
        end
        else
          let n = List.length first.doc in
          if n > 1 then begin
            (* Multiple groups: earlier groups are module doc, last is item doc *)
            List.iteri
              (fun i para ->
                if i < n - 1 then
                  fprintf fmt "%s@\n@\n" (String.concat "\n" para))
              first.doc;
            n - 1
          end
          else 0
    | _ -> 0
  in

  if doc.imports <> [] then begin
    fprintf fmt "**Imports:** %a@\n@\n"
      (pp_print_list
         ~pp_sep:(fun fmt () -> fprintf fmt ", ")
         (fun fmt i -> pp_print_string fmt i.value))
      doc.imports
  end;

  if doc.contexts <> [] then begin
    let any_docs = List.exists (fun c -> c.doc <> []) doc.contexts in
    fprintf fmt "### Contexts@\n@\n";
    if any_docs then
      List.iter
        (fun c ->
          if c.doc <> [] then fprintf fmt "%a@\n@\n" pp_doc_comment c.doc;
          fprintf fmt "**`%s`**@\n@\n" c.value)
        doc.contexts
    else begin
      fprintf fmt "%a@\n@\n"
        (pp_print_list
           ~pp_sep:(fun fmt () -> fprintf fmt ", ")
           (fun fmt c -> fprintf fmt "**`%s`**" c.value))
        doc.contexts
    end
  end;

  let total_chapters = List.length doc.chapters in
  List.iteri
    (fun i ch ->
      let skip_first_doc_groups =
        if i = 0 then skip_first_doc_groups else 0
      in
      pp_chapter procs ~skip_first_doc_groups ~total_chapters (i + 1) fmt ch)
    doc.chapters

let md_document procs doc = to_string (pp_document procs) doc

(** Output Markdown to stdout *)
let output env doc =
  let procs = rule_names_of_env env in
  pp_set_margin std_formatter 10000;
  pp_document procs std_formatter doc;
  pp_print_flush std_formatter ()
