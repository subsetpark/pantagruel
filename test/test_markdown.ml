(** Markdown output tests *)

open Alcotest
open Pantagruel

let parse = Test_util.parse

let render str =
  let doc = parse str in
  match
    Collect.collect_all
      ~base_env:(Env.empty (Option.value ~default:"" doc.module_name))
      doc
  with
  | Error e -> fail (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Error e -> fail (Check.show_type_error e)
      | Ok () ->
          let procs = Markdown_output.rule_names_of_env env in
          (procs, doc))

let render_expr str =
  let procs, doc = render str in
  match doc.Ast.chapters with
  | [ { body = [ prop ]; _ } ] -> Markdown_output.md_expr procs prop.value
  | _ -> fail "expected single proposition"

let render_decl str =
  let procs, doc = render str in
  match doc.Ast.chapters with
  | [ { head = [ decl ]; _ } ] ->
      Markdown_output.md_declaration procs decl.value
  | _ -> fail "expected single declaration"

let render_doc str =
  let procs, doc = render str in
  Markdown_output.md_document procs doc

(* --- Expression rendering --- *)

let test_var_italic () =
  let md =
    render_expr
      "module T.\nUser.\nf u: User => Bool.\n---\nall u: User | f u.\n"
  in
  check string "var italic" "∀ *u*: `User` · **f** *u*" md

let test_proc_bold () =
  let md =
    render_expr "module T.\nUser.\nactive u: User => Bool.\n---\nactive u.\n"
  in
  (* 'u' is a rule param, so it's a variable; 'active' is a rule *)
  check string "rule bold" "**active** *u*" md

let test_nullary_rule_bold () =
  let md =
    render_expr
      "module T.\nUser.\nnobody => User.\n---\nall u: User | u = nobody.\n"
  in
  check string "nullary bold" "∀ *u*: `User` · *u* = **nobody**" md

let test_primed_rule () =
  let md =
    render_expr
      {|module T.
User.
Document.
owner d: Document => User.
~> Check out @ u: User, d: Document.
---
owner' d = u.
|}
  in
  check string "primed rule bold" "**owner**′ *d* = *u*" md

let test_override () =
  let md =
    render_expr
      {|module T.
Key.
Value.
mapping k: Key => Value.
---
all k: Key, v: Value | mapping[k |-> v] k = v.
|}
  in
  check string "override"
    "∀ *k*: `Key`, *v*: `Value` · **mapping**[*k* ↦ *v*] *k* = *v*" md

(* --- Operators --- *)

let test_logical_ops () =
  let md =
    render_expr "module T.\nFoo.\nf => Bool.\ng => Bool.\n---\nf and g.\n"
  in
  check string "and" "**f** ∧ **g**" md;
  let md =
    render_expr "module T.\nFoo.\nf => Bool.\ng => Bool.\n---\nf or g.\n"
  in
  check string "or" "**f** ∨ **g**" md;
  let md =
    render_expr "module T.\nFoo.\nf => Bool.\ng => Bool.\n---\nf -> g.\n"
  in
  check string "impl" "**f** → **g**" md;
  let md =
    render_expr "module T.\nFoo.\nf => Bool.\ng => Bool.\n---\nf <-> g.\n"
  in
  check string "iff" "**f** ↔ **g**" md

let test_negation () =
  let md = render_expr "module T.\nFoo.\nf => Bool.\n---\n~f.\n" in
  check string "not" "¬**f**" md

let test_comparison_ops () =
  let md = render_expr "module T.\nFoo.\n---\n1 != 2.\n" in
  check string "neq" "1 ≠ 2" md;
  let md = render_expr "module T.\nFoo.\n---\n1 <= 2.\n" in
  check string "le" "1 ≤ 2" md;
  let md = render_expr "module T.\nFoo.\n---\n1 >= 2.\n" in
  check string "ge" "1 ≥ 2" md

let test_membership () =
  let md = render_expr "module T.\nUser.\n---\nall u: User | u in User.\n" in
  check string "in" "∀ *u*: `User` · *u* ∈ `User`" md

let test_subset () =
  let md =
    render_expr
      "module T.\nItem.\n---\nall xs: [Item], ys: [Item] | xs subset ys.\n"
  in
  check string "subset" "∀ *xs*: [`Item`], *ys*: [`Item`] · *xs* ⊆ *ys*" md

let test_addition () =
  let md =
    render_expr "module T.\nFoo.\n---\nall x: Nat, y: Nat | x + y >= 0.\n"
  in
  check string "add" "∀ *x*: `Nat`, *y*: `Nat` · *x* + *y* ≥ 0" md

let test_subtraction () =
  let md =
    render_expr "module T.\nFoo.\n---\nall x: Nat, y: Nat | x - y >= 0.\n"
  in
  check string "sub" "∀ *x*: `Nat`, *y*: `Nat` · *x* - *y* ≥ 0" md

let test_multiply () =
  let md =
    render_expr "module T.\nFoo.\n---\nall x: Nat, y: Nat | x * y >= 0.\n"
  in
  check string "mul dot" "∀ *x*: `Nat`, *y*: `Nat` · *x* · *y* ≥ 0" md

let test_division () =
  let md =
    render_expr "module T.\nFoo.\n---\nall x: Nat, y: Nat | x / y >= 0.\n"
  in
  check string "div" "∀ *x*: `Nat`, *y*: `Nat` · *x* / *y* ≥ 0" md

let test_lt_gt () =
  let md = render_expr "module T.\nFoo.\n---\n1 < 2.\n" in
  check string "lt" "1 < 2" md;
  let md = render_expr "module T.\nFoo.\n---\n2 > 1.\n" in
  check string "gt" "2 > 1" md

let test_neg () =
  let md = render_expr "module T.\nFoo.\nn => Nat.\n---\n(0 - n) >= 0.\n" in
  check string "neg" "0 - **n** ≥ 0" md

let test_cardinality () =
  let md = render_expr "module T.\nUser.\n---\n#User >= 0.\n" in
  check string "card" "#`User` ≥ 0" md

let test_quantifiers () =
  let md = render_expr "module T.\nUser.\n---\nall u: User | true.\n" in
  check string "forall" "∀ *u*: `User` · true" md;
  let md =
    render_expr
      "module T.\n\
       User.\n\
       active u: User => Bool.\n\
       ---\n\
       some u: User | active u.\n"
  in
  check string "exists" "∃ *u*: `User` · **active** *u*" md

let test_each () =
  let md =
    render_expr
      "module T.\n\
       User.\n\
       Role.\n\
       role u: User => Role.\n\
       ---\n\
       all r: Role | r in (each u: User | role u).\n"
  in
  check string "each" "∀ *r*: `Role` · *r* ∈ (each *u*: `User` · **role** *u*)"
    md

let test_cond () =
  let md =
    render_expr
      {|module T.
User.
active u: User => Bool.
---
all u: User | cond active u => true, true => false.
|}
  in
  check string "cond" "∀ *u*: `User` · cond **active** *u* ⇒ true, true ⇒ false"
    md

let test_initially () =
  let md =
    render_expr "module T.\nFoo.\ncount => Nat.\n---\ninitially count = 0.\n"
  in
  check string "initially" "initially **count** = 0" md

let test_proj () =
  let md = render_expr "module T.\nFoo.\np => Nat * Nat.\n---\np.1 >= 0.\n" in
  check string "proj" "**p**.1 ≥ 0" md

let test_tuple () =
  let md = render_expr "module T.\nFoo.\np => Nat * Nat.\n---\np = (1, 2).\n" in
  check string "tuple" "**p** = (1, 2)" md

let has_substring_ haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if String.sub haystack i nlen = needle then found := true
    done;
    !found

let test_lit_real () =
  let md = render_expr "module T.\nFoo.\n---\n3.14 > 0.\n" in
  (* OCaml string_of_float may vary but should include 3.14 *)
  check bool "real contains 3.14" true (has_substring_ md "3.14")

let test_lit_string () =
  let md = render_expr "module T.\nFoo.\n---\n\"hello\" = \"hello\".\n" in
  check string "string literal" "\"hello\" = \"hello\"" md

let test_membership_guard () =
  let md =
    render_expr
      {|module T.
Item.
price i: Item => Nat.
items => [Item].
---
all i in items | price i > 0.
|}
  in
  check string "membership guard" "∀ *i* ∈ **items** · **price** *i* > 0" md

(* --- Type expressions --- *)

let test_type_backticks () =
  let md = render_decl "module T.\nPoint = Nat * Nat.\n---\n" in
  check string "product type" "`Point` = `Nat` × `Nat`." md

let test_type_list () =
  let md = render_decl "module T.\nNames = [String].\n---\n" in
  check string "list type" "`Names` = [`String`]." md

let test_type_sum () =
  let _, doc = render "module T.\nFoo.\nBar.\nEither = Foo + Bar.\n---\n" in
  match doc.Ast.chapters with
  | [ { head = [ _; _; decl ]; _ } ] ->
      let procs = Markdown_output.StringSet.empty in
      let md = Markdown_output.md_declaration procs decl.value in
      check string "sum type" "`Either` = `Foo` + `Bar`." md
  | _ -> fail "expected three declarations"

(* --- Declaration rendering --- *)

let test_decl_domain () =
  let md = render_decl "module T.\nUser.\n---\n" in
  check string "domain decl" "`User`." md

let test_decl_rule_with_return () =
  let _, doc = render "module T.\nUser.\nname u: User => String.\n---\n" in
  let procs = Markdown_output.StringSet.singleton "name" in
  match doc.Ast.chapters with
  | [ { head = [ _; decl ]; _ } ] ->
      let md = Markdown_output.md_declaration procs decl.value in
      check string "rule decl" "**name** *u*: `User` ⇒ `String`." md
  | _ -> fail "expected two declarations"

let test_decl_action () =
  let _, doc =
    render
      "module T.\nUser.\nDocument.\n~> Check out @ u: User, d: Document.\n---\n"
  in
  let procs = Markdown_output.StringSet.empty in
  match doc.Ast.chapters with
  | [ { head = [ _; _; decl ]; _ } ] ->
      let md = Markdown_output.md_declaration procs decl.value in
      check string "action decl" "↝ Check out *u*: `User`, *d*: `Document`." md
  | _ -> fail "expected three declarations"

let test_decl_closure () =
  let _, doc =
    render
      "module T.\n\
       Block.\n\
       parent b: Block => Block + Nothing.\n\
       ancestor b: Block => [Block] = closure parent.\n\
       ---\n"
  in
  let procs = Markdown_output.StringSet.of_list [ "parent"; "ancestor" ] in
  match doc.Ast.chapters with
  | [ { head = _ :: _ :: decl :: _; _ } ] ->
      let md = Markdown_output.md_declaration procs decl.value in
      check string "closure decl"
        "**ancestor** *b*: `Block` ⇒ [`Block`] = closure **parent**." md
  | _ -> fail "expected closure declaration"

let test_decl_action_with_context () =
  let _, doc =
    render
      {|module T.
context Banking.
User.
Banking ~> Withdraw @ u: User.
---
|}
  in
  let procs = Markdown_output.StringSet.empty in
  match doc.Ast.chapters with
  | [ { head = _ :: decl :: _; _ } ] ->
      let md = Markdown_output.md_declaration procs decl.value in
      check string "action with context" "**`Banking`** ↝ Withdraw *u*: `User`."
        md
  | _ -> fail "expected action declaration"

(* --- Document structure --- *)

(* Helper: check a substring exists in output *)
let has_substring haystack needle =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if String.sub haystack i nlen = needle then found := true
    done;
    !found

let test_doc_domains_backtick () =
  let md = render_doc "module T.\nUser.\nDocument.\n---\n" in
  check bool "domains backtick" true (has_substring md "`User`, `Document`")

let test_doc_comments_blockquote () =
  let md =
    render_doc
      {|module T.
User.
> Check if active
active u: User => Bool.
---
|}
  in
  check bool "rule blockquote" true (has_substring md "> Check if active")

let test_domain_doc_comments () =
  let md =
    render_doc
      {|module T.
Foo.
> A person who uses the system
User.
> A file in the system
Document.
---
|}
  in
  check bool "domain blockquote 1" true
    (has_substring md "> A person who uses the system");
  check bool "domain blockquote 2" true
    (has_substring md "> A file in the system");
  check bool "domain rendered" true (has_substring md "`User`.");
  check bool "domain rendered" true (has_substring md "`Document`.")

let test_doc_propositions_not_indented () =
  let md = render_doc "module T.\nUser.\n---\nall u: User | true.\n" in
  let lines = String.split_on_char '\n' md in
  let prop_lines = List.filter (fun l -> has_substring l "∀") lines in
  check bool "not indented" true (List.length prop_lines > 0);
  List.iter
    (fun l ->
      check bool "no leading spaces" true (String.length l > 0 && l.[0] <> ' '))
    prop_lines

let test_doc_chapter_headers () =
  let md = render_doc "module T.\nFoo.\n---\n\nwhere\n\nBar.\n---\n" in
  check bool "chapter 1" true (has_substring md "## Chapter 1");
  check bool "chapter 2" true (has_substring md "## Chapter 2")

let test_doc_section_headings () =
  let md =
    render_doc
      {|module T.
User.
Point = Nat * Nat.
name u: User => String.
---
all u: User | true.
|}
  in
  check bool "domains heading" true (has_substring md "### Domains");
  check bool "alias in domains" true (has_substring md "`Point`");
  check bool "rules heading" true (has_substring md "### Rules");
  check bool "propositions separator" true (has_substring md "---")

let test_multi_chapter_where () =
  let md =
    render_doc
      {|module T.
context C.
User.
{C} name u: User => String.
---
all u: User | name u = name u.

where

C ~> Rename @ u: User.
---
name' u = name u.
|}
  in
  check bool "chapter 1" true (has_substring md "## Chapter 1");
  check bool "chapter 2" true (has_substring md "## Chapter 2");
  check bool "action rendered" true (has_substring md "Rename");
  check bool "has rules section" true (has_substring md "### Rules");
  check bool "has action section" true (has_substring md "### Action")

let test_builtin_types_backtick () =
  let md =
    render_doc
      "module T.\nFoo.\nn => Nat.\ns => String.\nb => Bool.\n---\nn >= 0.\n"
  in
  check bool "Nat backtick" true (has_substring md "`Nat`");
  check bool "String backtick" true (has_substring md "`String`");
  check bool "Bool backtick" true (has_substring md "`Bool`")

let () =
  run "Markdown"
    [
      ( "expressions",
        [
          test_case "var italic, rule bold" `Quick test_var_italic;
          test_case "rule bold" `Quick test_proc_bold;
          test_case "nullary rule bold" `Quick test_nullary_rule_bold;
          test_case "primed rule" `Quick test_primed_rule;
          test_case "override" `Quick test_override;
        ] );
      ( "operators",
        [
          test_case "logical" `Quick test_logical_ops;
          test_case "negation" `Quick test_negation;
          test_case "comparison" `Quick test_comparison_ops;
          test_case "membership" `Quick test_membership;
          test_case "subset" `Quick test_subset;
          test_case "addition" `Quick test_addition;
          test_case "subtraction" `Quick test_subtraction;
          test_case "multiply" `Quick test_multiply;
          test_case "division" `Quick test_division;
          test_case "lt_gt" `Quick test_lt_gt;
          test_case "neg" `Quick test_neg;
          test_case "cardinality" `Quick test_cardinality;
          test_case "quantifiers" `Quick test_quantifiers;
          test_case "each" `Quick test_each;
          test_case "cond" `Quick test_cond;
          test_case "initially" `Quick test_initially;
          test_case "proj" `Quick test_proj;
          test_case "tuple" `Quick test_tuple;
          test_case "lit_real" `Quick test_lit_real;
          test_case "lit_string" `Quick test_lit_string;
          test_case "membership guard" `Quick test_membership_guard;
        ] );
      ( "type expressions",
        [
          test_case "product" `Quick test_type_backticks;
          test_case "list" `Quick test_type_list;
          test_case "sum" `Quick test_type_sum;
        ] );
      ( "declarations",
        [
          test_case "domain" `Quick test_decl_domain;
          test_case "rule with return" `Quick test_decl_rule_with_return;
          test_case "action" `Quick test_decl_action;
          test_case "closure" `Quick test_decl_closure;
          test_case "action with context" `Quick test_decl_action_with_context;
        ] );
      ( "document",
        [
          test_case "domains backtick" `Quick test_doc_domains_backtick;
          test_case "doc comments blockquote" `Quick
            test_doc_comments_blockquote;
          test_case "domain doc comments" `Quick test_domain_doc_comments;
          test_case "propositions not indented" `Quick
            test_doc_propositions_not_indented;
          test_case "chapter headers" `Quick test_doc_chapter_headers;
          test_case "multi chapter where" `Quick test_multi_chapter_where;
          test_case "section headings" `Quick test_doc_section_headings;
          test_case "builtin types backtick" `Quick test_builtin_types_backtick;
        ] );
    ]
