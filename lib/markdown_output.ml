(** Markdown + LaTeX output for Pantagruel documents *)

open Ast

(* --- LaTeX helpers --- *)

(** Escape LaTeX special characters in text mode *)
let latex_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '\\' -> Buffer.add_string buf "\\textbackslash{}"
    | '{' -> Buffer.add_string buf "\\{"
    | '}' -> Buffer.add_string buf "\\}"
    | '$' -> Buffer.add_string buf "\\$"
    | '%' -> Buffer.add_string buf "\\%"
    | '&' -> Buffer.add_string buf "\\&"
    | '#' -> Buffer.add_string buf "\\#"
    | '_' -> Buffer.add_string buf "\\_"
    | '^' -> Buffer.add_string buf "\\^{}"
    | '~' -> Buffer.add_string buf "\\~{}"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Render identifier in roman text (for procedure/domain names) *)
let latex_ident name =
  "\\text{" ^ latex_escape name ^ "}"

(* --- LaTeX type expression rendering --- *)

let rec latex_type_expr = function
  | TName name -> latex_ident name
  | TList t -> "[" ^ latex_type_expr t ^ "]"
  | TProduct ts ->
      String.concat " \\times " (List.map latex_type_expr_atom ts)
  | TSum ts ->
      String.concat " + " (List.map latex_type_expr_atom ts)

and latex_type_expr_atom = function
  | TName _ | TList _ as t -> latex_type_expr t
  | t -> "(" ^ latex_type_expr t ^ ")"

(* --- LaTeX operator rendering --- *)

let latex_binop = function
  | OpAnd -> " \\land "
  | OpOr -> " \\lor "
  | OpImpl -> " \\rightarrow "
  | OpIff -> " \\leftrightarrow "
  | OpEq -> " = "
  | OpNeq -> " \\neq "
  | OpLt -> " < "
  | OpGt -> " > "
  | OpLe -> " \\leq "
  | OpGe -> " \\geq "
  | OpIn -> " \\in "
  | OpSubset -> " \\subseteq "
  | OpAdd -> " + "
  | OpSub -> " - "
  | OpMul -> " \\cdot "
  | OpDiv -> " / "

(* --- LaTeX expression rendering with proper precedence --- *)

let rec latex_expr = function
  | EForall (params, guards, body) ->
      "\\forall " ^ latex_quant_params_guards params guards ^ ".\\ " ^ latex_expr body
  | EExists (params, guards, body) ->
      "\\exists " ^ latex_quant_params_guards params guards ^ ".\\ " ^ latex_expr body
  | e -> latex_biconditional e

and latex_biconditional = function
  | EBinop (OpIff, e1, e2) ->
      latex_implication e1 ^ latex_binop OpIff ^ latex_implication e2
  | e -> latex_implication e

and latex_quant_params_guards params guards =
  let param_strs = List.map latex_param params in
  let guard_strs = List.filter_map (function
    | GParam p -> Some (latex_param p)
    | GIn (name, e) -> Some (name ^ " \\in " ^ latex_term e)
    | GExpr e -> Some (latex_conjunction e)
  ) guards in
  String.concat ",\\, " (param_strs @ guard_strs)

and latex_param p =
  p.param_name ^ " : " ^ latex_type_expr p.param_type

and latex_implication = function
  | EBinop (OpImpl, e1, e2) ->
      latex_disjunction e1 ^ latex_binop OpImpl ^ latex_expr e2
  | e -> latex_disjunction e

and latex_disjunction = function
  | EBinop (OpOr, e1, e2) ->
      latex_disjunction e1 ^ latex_binop OpOr ^ latex_conjunction e2
  | e -> latex_conjunction e

and latex_conjunction = function
  | EBinop (OpAnd, e1, e2) ->
      latex_conjunction e1 ^ latex_binop OpAnd ^ latex_negation e2
  | e -> latex_negation e

and latex_negation = function
  | EUnop (OpNot, e) -> "\\neg " ^ latex_negation e
  | e -> latex_comparison e

and latex_comparison = function
  | EBinop ((OpEq | OpNeq | OpLt | OpGt | OpLe | OpGe | OpIn | OpSubset) as op, e1, e2) ->
      latex_term e1 ^ latex_binop op ^ latex_term e2
  | e -> latex_term e

and latex_term = function
  | EBinop ((OpAdd | OpSub) as op, e1, e2) ->
      latex_term e1 ^ latex_binop op ^ latex_factor e2
  | e -> latex_factor e

and latex_factor = function
  | EBinop ((OpMul | OpDiv) as op, e1, e2) ->
      latex_factor e1 ^ latex_binop op ^ latex_unary e2
  | e -> latex_unary e

and latex_unary = function
  | EUnop (OpCard, e) -> "|" ^ latex_unary e ^ "|"
  | EUnop (OpNeg, e) -> "-" ^ latex_unary e
  | e -> latex_primary e

and latex_primary = function
  | EApp (EVar name, args) when args <> [] ->
      (* Function application: render function name in roman *)
      latex_ident name ^ "\\ " ^ String.concat "\\ " (List.map latex_atom args)
  | EApp (f, args) when args <> [] ->
      latex_atom f ^ "\\ " ^ String.concat "\\ " (List.map latex_atom args)
  | e -> latex_atom e

and latex_atom = function
  | EVar name -> name  (* variables stay italic *)
  | EDomain name -> latex_ident name
  | EQualified (m, name) -> latex_ident (m ^ "::" ^ name)
  | ELitNat n -> string_of_int n
  | ELitReal r ->
      let s = string_of_float r in
      if String.ends_with ~suffix:"." s then s ^ "0" else s
  | ELitString s -> "\\text{\"" ^ latex_escape s ^ "\"}"
  | ELitBool true -> latex_ident "true"
  | ELitBool false -> latex_ident "false"
  | EPrimed name -> latex_ident name ^ "'"
  | EOverride (name, pairs) ->
      latex_ident name ^ "[" ^ String.concat ",\\, " (List.map (fun (k, v) ->
        latex_expr k ^ " \\mapsto " ^ latex_expr v
      ) pairs) ^ "]"
  | ETuple es ->
      "(" ^ String.concat ",\\, " (List.map latex_expr es) ^ ")"
  | EProj (e, n) ->
      latex_atom e ^ "." ^ string_of_int n
  | EApp (f, []) -> latex_atom f
  | e -> "(" ^ latex_expr e ^ ")"

(* --- LaTeX declaration signature rendering --- *)

let latex_guard = function
  | GParam p -> latex_param p
  | GIn (name, e) -> name ^ " \\in " ^ latex_term e
  | GExpr e -> latex_conjunction e

let latex_decl_signature = function
  | DeclDomain name -> latex_ident name
  | DeclAlias (name, te) -> latex_ident name ^ " = " ^ latex_type_expr te
  | DeclProc { name; params; guards; return_type } ->
      let parts = [latex_ident name] in
      let parts = if params <> [] then
        parts @ ["\\ " ^ String.concat ",\\, " (List.map latex_param params)]
      else parts in
      let parts = if guards <> [] then
        parts @ [",\\, " ^ String.concat ",\\, " (List.map latex_guard guards)]
      else parts in
      let parts = match return_type with
        | None -> parts
        | Some te -> parts @ [" \\Rightarrow " ^ latex_type_expr te]
      in
      String.concat "" parts

(* --- Markdown document rendering --- *)

let md_chapter ?(skip_first_doc=false) ~total_chapters chapter_num chapter =
  let buf = Buffer.create 1024 in
  (* Only show chapter headers for multi-chapter documents *)
  if total_chapters > 1 then
    Buffer.add_string buf (Printf.sprintf "## Chapter %d\n\n" chapter_num);

  (* Get line number of first declaration to identify it *)
  let first_line = match chapter.head with
    | d :: _ -> Some d.loc.line
    | [] -> None
  in

  (* Separate declarations by kind *)
  let domains, aliases, procs = List.fold_left (fun (ds, as_, ps) decl ->
    match decl.value with
    | DeclDomain _ -> (decl :: ds, as_, ps)
    | DeclAlias _ -> (ds, decl :: as_, ps)
    | DeclProc _ -> (ds, as_, decl :: ps)
  ) ([], [], []) chapter.head in
  let domains = List.rev domains in
  let aliases = List.rev aliases in
  let procs = List.rev procs in

  (* Should we skip doc for this declaration? Check by line number *)
  let should_skip_doc d =
    skip_first_doc && Some d.loc.line = first_line
  in

  (* Domains - compact comma-separated list *)
  if domains <> [] then begin
    Buffer.add_string buf "### Domains\n\n";
    let domain_strs = List.map (fun d ->
      let name = match d.value with DeclDomain n -> n | _ -> "" in
      Printf.sprintf "$%s$" (latex_ident name)
    ) domains in
    Buffer.add_string buf (String.concat ", " domain_strs);
    Buffer.add_string buf "\n\n"
  end;

  (* Type aliases *)
  if aliases <> [] then begin
    Buffer.add_string buf "### Types\n\n";
    List.iter (fun a ->
      if a.doc <> [] && not (should_skip_doc a) then
        Buffer.add_string buf (Printf.sprintf "*%s*\n\n" (String.concat " " a.doc));
      Buffer.add_string buf (Printf.sprintf "$$%s$$\n\n" (latex_decl_signature a.value))
    ) aliases
  end;

  (* Procedures *)
  if procs <> [] then begin
    Buffer.add_string buf "### Procedures\n\n";
    List.iter (fun p ->
      if p.doc <> [] && not (should_skip_doc p) then
        Buffer.add_string buf (Printf.sprintf "*%s*\n\n" (String.concat " " p.doc));
      Buffer.add_string buf (Printf.sprintf "$$%s$$\n\n" (latex_decl_signature p.value))
    ) procs
  end;

  (* Propositions *)
  if chapter.body <> [] then begin
    Buffer.add_string buf "### Propositions\n\n";
    List.iter (fun prop ->
      if prop.doc <> [] then
        Buffer.add_string buf (Printf.sprintf "*%s*\n\n" (String.concat " " prop.doc));
      Buffer.add_string buf "\\begin{dmath*}\n";
      Buffer.add_string buf (latex_expr prop.value);
      Buffer.add_string buf "\n\\end{dmath*}\n\n"
    ) chapter.body
  end;

  Buffer.contents buf

let md_document doc =
  let buf = Buffer.create 4096 in

  (* YAML frontmatter for pandoc *)
  Buffer.add_string buf "---\n";
  Buffer.add_string buf (Printf.sprintf "title: \"Module %s\"\n" doc.module_name);
  Buffer.add_string buf "classoption:\n";
  Buffer.add_string buf "  - fleqn\n";
  Buffer.add_string buf "  - 11pt\n";
  Buffer.add_string buf "geometry:\n";
  Buffer.add_string buf "  - margin=1in\n";
  Buffer.add_string buf "header-includes:\n";
  Buffer.add_string buf "  - \\usepackage{breqn}\n";
  Buffer.add_string buf "  - \\setlength{\\mathindent}{2em}\n";
  Buffer.add_string buf "  - \\setlength{\\parskip}{0.5em}\n";
  Buffer.add_string buf "  - \\setlength{\\parindent}{0pt}\n";
  Buffer.add_string buf "colorlinks: true\n";
  Buffer.add_string buf "linkcolor: blue\n";
  Buffer.add_string buf "urlcolor: blue\n";
  Buffer.add_string buf "---\n\n";

  (* Title *)
  Buffer.add_string buf (Printf.sprintf "# Module %s\n\n" doc.module_name);

  (* Module-level doc comments from first declaration if any *)
  let has_module_doc = match doc.chapters with
    | { head = first :: _; _ } :: _ when first.doc <> [] ->
        Buffer.add_string buf (String.concat "\n" first.doc);
        Buffer.add_string buf "\n\n";
        true
    | _ -> false
  in

  (* Imports *)
  if doc.imports <> [] then begin
    Buffer.add_string buf "**Imports:** ";
    Buffer.add_string buf (String.concat ", " (List.map (fun i -> i.value) doc.imports));
    Buffer.add_string buf "\n\n"
  end;

  (* Chapters *)
  let total_chapters = List.length doc.chapters in
  List.iteri (fun i ch ->
    (* Skip first doc for first chapter if we showed it as module doc *)
    let skip_first_doc = (i = 0) && has_module_doc in
    Buffer.add_string buf (md_chapter ~skip_first_doc ~total_chapters (i + 1) ch)
  ) doc.chapters;

  Buffer.contents buf

(** Output Markdown to stdout *)
let output doc =
  print_string (md_document doc)
