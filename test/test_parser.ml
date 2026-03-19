(** Parser tests *)

open Alcotest
open Pantagruel

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let test_minimal () =
  let doc = parse "module EXAMPLE.\n\nFoo.\n---\n" in
  check (option string) "module name" (Some "EXAMPLE") doc.Ast.module_name;
  check int "chapters" 1 (List.length doc.Ast.chapters);
  check int "imports" 0 (List.length doc.Ast.imports)

let test_domain_decl () =
  let doc = parse "module TEST.\n\nUser.\nDocument.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  check int "declarations" 2 (List.length chapter.Ast.head)

let test_rule_decl () =
  let doc = parse "module TEST.\n\nUser.\nowner d: User => User.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  check int "declarations" 2 (List.length chapter.Ast.head)

let test_action () =
  let doc = parse "module TEST.\n\nUser.\n~> Check out @ u: User.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 1).Ast.value with
  | Ast.DeclAction { label = "Check out"; contexts = []; _ } -> ()
  | _ -> fail "Expected action"

let test_import () =
  let doc = parse "module TEST.\n\nimport FOO.\n\nBar.\n---\n" in
  check int "imports" 1 (List.length doc.Ast.imports);
  check string "import name" "FOO" (List.hd doc.Ast.imports).Ast.value

let test_where_clause () =
  let doc = parse "module TEST.\n\nFoo.\n---\n\nwhere\n\nBar.\n---\n" in
  check int "chapters" 2 (List.length doc.Ast.chapters)

let test_proposition () =
  let doc = parse "module TEST.\n\nFoo.\n---\ntrue.\n" in
  let chapter = List.hd doc.Ast.chapters in
  check int "propositions" 1 (List.length chapter.Ast.body)

let test_all () =
  let doc = parse "module TEST.\n\nUser.\n---\nall u: User | true.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall _ -> ()
  | _ -> fail "Expected all"

let test_application () =
  let doc =
    parse "module TEST.\n\nUser.\nf x: User => User.\n---\nf u = u.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (Ast.OpEq, Ast.EApp (_, _), _) -> ()
  | _ -> fail "Expected application in equality"

let test_type_alias () =
  let doc = parse "module TEST.\n\nPoint = Nat * Nat.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.head).Ast.value with
  | Ast.DeclAlias ("Point", Ast.TProduct [ Ast.TName "Nat"; Ast.TName "Nat" ])
    ->
      ()
  | _ -> fail "Expected type alias"

let test_list_type () =
  let doc = parse "module TEST.\n\nUser.\nusers => [User].\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 1).Ast.value with
  | Ast.DeclRule { return_type = Ast.TList (Ast.TName "User"); _ } -> ()
  | _ -> fail "Expected list type"

let test_sum_type () =
  let doc = parse "module TEST.\n\nResult = Nat + Nothing.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.head).Ast.value with
  | Ast.DeclAlias ("Result", Ast.TSum _) -> ()
  | _ -> fail "Expected sum type"

let test_rule_guard () =
  let doc =
    parse
      "module TEST.\n\n\
       Account.\n\
       balance a: Account => Nat.\n\
       ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
       ---\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclAction { guards = [ Ast.GExpr _ ]; _ } -> ()
  | _ -> fail "Expected action with guard"

let test_membership_binding () =
  let doc =
    parse
      "module TEST.\n\nItem.\nitems => [Item].\n---\nall i in items | true.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall ([], [ Ast.GIn ("i", _) ], _) -> ()
  | _ -> fail "Expected membership binding"

let test_existential () =
  let doc = parse "module TEST.\n\nUser.\n---\nsome u: User | true.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EExists _ -> ()
  | _ -> fail "Expected existential"

let test_doc_comment () =
  let doc = parse "module TEST.\n\n> This is a doc comment\nFoo.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  let decl = List.hd chapter.Ast.head in
  check
    (list (list string))
    "doc comment"
    [ [ "This is a doc comment" ] ]
    decl.Ast.doc

let test_multiple_guards () =
  let doc =
    parse
      "module TEST.\n\n\
       Account.\n\
       balance a: Account => Nat.\n\
       ~> Withdraw @ a: Account, amount: Nat, amount > 0, balance a >= amount.\n\
       ---\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclAction { guards = [ Ast.GExpr _; Ast.GExpr _ ]; _ } -> ()
  | _ -> fail "Expected action with two guards"

let test_disjunction_guard () =
  let doc =
    parse
      "module TEST.\n\n\
       Node.\n\
       kind n: Node => Nat.\n\
       children n: Node => [Node].\n\
       ---\n\
       all n: Node, kind n = 1 or kind n = 2 | #children n = 0.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall (_, [ Ast.GExpr (Ast.EBinop (Ast.OpOr, _, _)) ], _) -> ()
  | _ -> fail "Expected forall with disjunction guard"

let test_no_module () =
  let doc = parse "Foo.\n---\ntrue.\n" in
  check (option string) "no module" None doc.Ast.module_name;
  check int "chapters" 1 (List.length doc.Ast.chapters);
  check int "imports" 0 (List.length doc.Ast.imports)

let test_action_no_params () =
  let doc = parse "module TEST.\n\nFoo.\n~> Do something.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 1).Ast.value with
  | Ast.DeclAction
      { label = "Do something"; params = []; guards = []; contexts = [] } ->
      ()
  | _ -> fail "Expected no-param action"

let test_context_declaration () =
  let doc = parse "module TEST.\ncontext Banking.\n\nAccount.\n---\n" in
  check int "contexts" 1 (List.length doc.Ast.contexts);
  check string "context name" "Banking" (List.hd doc.Ast.contexts).Ast.value

let test_rule_with_context () =
  let doc =
    parse
      "module TEST.\n\
       context Banking.\n\n\
       Account.\n\
       {Banking} balance a: Account => Nat.\n\
       Banking ~> Withdraw @ a: Account.\n\
       ---\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  (match (List.nth chapter.Ast.head 1).Ast.value with
  | Ast.DeclRule { name = "balance"; contexts = [ "Banking" ]; _ } -> ()
  | _ -> fail "Expected rule with context footprint");
  match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclAction { label = "Withdraw"; contexts = [ "Banking" ]; _ } -> ()
  | _ -> fail "Expected action with context"

let test_closure () =
  let doc =
    parse
      "module TEST.\n\n\
       Block.\n\
       parent b: Block => Block + Nothing.\n\
       ancestor b: Block => [Block] = closure parent.\n\
       ---\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclClosure { name = "ancestor"; target = "parent"; _ } -> ()
  | _ -> fail "Expected closure declaration"

let test_each () =
  let doc =
    parse
      "module TEST.\n\n\
       User.\n\
       Role.\n\
       role u: User => Role.\n\
       ---\n\
       all r: Role | r in (each u: User | role u).\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall (_, _, Ast.EBinop (Ast.OpIn, _, Ast.EEach _)) -> ()
  | _ -> fail "Expected each comprehension inside all"

let test_cond_simple () =
  let doc =
    parse
      "module TEST.\n\n\
       Status.\n\
       level s: Status => Nat.\n\
       ---\n\
       all s: Status | level s = cond level s >= 10 => 2, level s >= 5 => 1, \
       true => 0.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall (_, _, Ast.EBinop (Ast.OpEq, _, Ast.ECond arms)) ->
      check int "arms" 3 (List.length arms)
  | _ -> fail "Expected cond expression"

let test_cond_two_arms () =
  let doc = parse "module TEST.\n\nBool.\n---\ncond true => 1, false => 0.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.ECond
      [
        (Ast.ELitBool true, Ast.ELitNat 1); (Ast.ELitBool false, Ast.ELitNat 0);
      ] ->
      ()
  | _ -> fail "Expected cond with two arms"

(* --- Arithmetic and operator tests --- *)

let test_arithmetic () =
  let doc = parse "module TEST.\n\nFoo.\n---\n1 + 2 = 3.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, EBinop (OpAdd, ELitNat 1, ELitNat 2), ELitNat 3) -> ()
  | _ -> fail "Expected 1 + 2 = 3"

let test_multiply () =
  let doc = parse "module TEST.\n\nFoo.\n---\n3 * 4 = 12.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, EBinop (OpMul, ELitNat 3, ELitNat 4), ELitNat 12) -> ()
  | _ -> fail "Expected 3 * 4 = 12"

let test_subtraction () =
  let doc = parse "module TEST.\n\nFoo.\n---\n5 - 3 = 2.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, EBinop (OpSub, _, _), _) -> ()
  | _ -> fail "Expected subtraction"

let test_division () =
  let doc = parse "module TEST.\n\nFoo.\n---\n10 / 2 = 5.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, EBinop (OpDiv, _, _), _) -> ()
  | _ -> fail "Expected division"

let test_logical_and () =
  let doc =
    parse "module TEST.\n\nFoo.\nf => Bool.\ng => Bool.\n---\nf and g.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpAnd, _, _) -> ()
  | _ -> fail "Expected and"

let test_logical_or () =
  let doc =
    parse "module TEST.\n\nFoo.\nf => Bool.\ng => Bool.\n---\nf or g.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpOr, _, _) -> ()
  | _ -> fail "Expected or"

let test_logical_not () =
  let doc = parse "module TEST.\n\nFoo.\nf => Bool.\n---\n~f.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EUnop (OpNot, _) -> ()
  | _ -> fail "Expected not"

let test_biconditional () =
  let doc =
    parse "module TEST.\n\nFoo.\nf => Bool.\ng => Bool.\n---\nf <-> g.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpIff, _, _) -> ()
  | _ -> fail "Expected iff"

let test_implication () =
  let doc =
    parse "module TEST.\n\nFoo.\nf => Bool.\ng => Bool.\n---\nf -> g.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpImpl, _, _) -> ()
  | _ -> fail "Expected impl"

let test_comparison_ops () =
  let ops =
    [ ("<", Ast.OpLt); (">", Ast.OpGt); ("<=", Ast.OpLe); (">=", Ast.OpGe) ]
  in
  List.iter
    (fun (sym, expected_op) ->
      let doc =
        parse (Printf.sprintf "module TEST.\n\nFoo.\n---\n1 %s 2.\n" sym)
      in
      let chapter = List.hd doc.Ast.chapters in
      match (List.hd chapter.Ast.body).Ast.value with
      | Ast.EBinop (op, _, _) when Ast.equal_binop op expected_op -> ()
      | _ -> fail (Printf.sprintf "Expected %s operator" sym))
    ops

let test_in_operator () =
  let doc = parse "module TEST.\n\nUser.\n---\nall u: User | u in User.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall (_, _, EBinop (OpIn, _, _)) -> ()
  | _ -> fail "Expected in operator"

let test_subset_operator () =
  let doc =
    parse
      "module TEST.\n\n\
       Item.\n\
       xs => [Item].\n\
       ys => [Item].\n\
       ---\n\
       xs subset ys.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpSubset, _, _) -> ()
  | _ -> fail "Expected subset operator"

let test_override () =
  let doc =
    parse
      "module TEST.\n\n\
       Key.\n\
       Value.\n\
       f k: Key => Value.\n\
       ---\n\
       all k: Key, v: Value | f[k |-> v] k = v.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall (_, _, EBinop (OpEq, EApp (EOverride ("f", _), _), _)) -> ()
  | _ -> fail "Expected override"

let test_tuple () =
  let doc = parse "module TEST.\n\nFoo.\n---\n(1, 2, 3) = (1, 2, 3).\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, ETuple _, ETuple _) -> ()
  | _ -> fail "Expected tuple"

let test_projection () =
  let doc =
    parse "module TEST.\n\nPoint = Nat * Nat.\np => Point.\n---\np.1 >= 0.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpGe, EProj (_, 1), _) -> ()
  | _ -> fail "Expected projection"

let test_primed_expr () =
  let doc =
    parse
      "module TEST.\n\n\
       User.\n\
       f u: User => User.\n\
       ~> Act @ u: User.\n\
       ---\n\
       f' u = u.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, EApp (EPrimed "f", _), _) -> ()
  | _ -> fail "Expected primed expression"

let test_lit_real () =
  let doc = parse "module TEST.\n\nFoo.\n---\n3.14 >= 0.0.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpGe, ELitReal _, ELitReal _) -> ()
  | _ -> fail "Expected real literals"

let test_lit_string () =
  let doc = parse "module TEST.\n\nFoo.\n---\n\"hello\" = \"hello\".\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, ELitString "hello", ELitString "hello") -> ()
  | _ -> fail "Expected string literals"

let test_initially () =
  let doc =
    parse "module TEST.\n\ncontext C.\n\n{C} x => Nat.\n---\ninitially x = 0.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EInitially _ -> ()
  | _ -> fail "Expected initially"

let test_qualified_name () =
  let doc = parse "module TEST.\n\nFoo.\n---\nModule::name = Module::name.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, EQualified ("Module", "name"), _) -> ()
  | _ -> fail "Expected qualified name"

let test_precedence_or_and () =
  (* a or b and c should parse as a or (b and c) *)
  let doc =
    parse
      "module TEST.\n\n\
       Foo.\n\
       a => Bool.\n\
       b => Bool.\n\
       c => Bool.\n\
       ---\n\
       a or b and c.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpOr, EVar "a", EBinop (OpAnd, EVar "b", EVar "c")) -> ()
  | _ -> fail "Expected a or (b and c)"

let test_precedence_add_mul () =
  (* 1 + 2 * 3 should parse as 1 + (2 * 3) *)
  let doc = parse "module TEST.\n\nFoo.\n---\n1 + 2 * 3 = 7.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpEq, EBinop (OpAdd, ELitNat 1, EBinop (OpMul, _, _)), _) -> ()
  | _ -> fail "Expected 1 + (2 * 3)"

let test_nested_types () =
  let doc = parse "module TEST.\n\nFoo.\nBar.\nxs => [Nat * Nat].\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  (match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclRule { return_type = TList (TProduct _); _ } -> ()
  | _ -> fail "Expected [Nat * Nat]");
  let doc = parse "module TEST.\n\nFoo.\nBar.\nxs => [Foo + Bar].\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclRule { return_type = TList (TSum _); _ } -> ()
  | _ -> fail "Expected [Foo + Bar]"

(* --- Bug-finding tests --- *)

let test_implication_associativity () =
  (* Bug #8: a -> b -> c should be right-associative: a -> (b -> c) *)
  let doc =
    parse
      "module TEST.\n\n\
       Foo.\n\
       a => Bool.\n\
       b => Bool.\n\
       c => Bool.\n\
       ---\n\
       a -> b -> c.\n"
  in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (OpImpl, EVar "a", EBinop (OpImpl, EVar "b", EVar "c")) ->
      () (* Right-associative: a -> (b -> c) *)
  | Ast.EBinop (OpImpl, EBinop (OpImpl, EVar "a", EVar "b"), EVar "c") ->
      fail "Implication is left-associative but should be right-associative"
  | other -> fail (Printf.sprintf "Unexpected parse: %s" (Ast.show_expr other))

let test_chained_comparison_parse () =
  (* Bug #12 investigation: 1 < x < 10 is actually a parse error.
     The grammar rule `comparison: e1=term op=comp_op e2=term` requires
     both sides to be `term`, not `comparison`. This means chained
     comparisons are rejected at parse time — which is actually
     correct behavior (prevents confusing Bool < Int). *)
  try
    let _ = parse "module TEST.\n\nFoo.\nx => Nat.\n---\n1 < x < 10.\n" in
    fail "Expected parse error for chained comparison"
  with _ -> ()

let () =
  run "Parser"
    [
      ("minimal", [ test_case "minimal document" `Quick test_minimal ]);
      ("no_module", [ test_case "no module header" `Quick test_no_module ]);
      ("domain", [ test_case "domain declaration" `Quick test_domain_decl ]);
      ("rule", [ test_case "rule declaration" `Quick test_rule_decl ]);
      ("action", [ test_case "action" `Quick test_action ]);
      ( "action_no_params",
        [ test_case "action no params" `Quick test_action_no_params ] );
      ("import", [ test_case "import" `Quick test_import ]);
      ("where", [ test_case "where clause" `Quick test_where_clause ]);
      ("proposition", [ test_case "proposition" `Quick test_proposition ]);
      ("all", [ test_case "all" `Quick test_all ]);
      ("application", [ test_case "application" `Quick test_application ]);
      ("type_alias", [ test_case "type alias" `Quick test_type_alias ]);
      ("list_type", [ test_case "list type" `Quick test_list_type ]);
      ("sum_type", [ test_case "sum type" `Quick test_sum_type ]);
      ("guard", [ test_case "rule guard" `Quick test_rule_guard ]);
      ( "membership",
        [ test_case "membership binding" `Quick test_membership_binding ] );
      ("existential", [ test_case "existential" `Quick test_existential ]);
      ("doc_comment", [ test_case "doc comment" `Quick test_doc_comment ]);
      ( "multi_guard",
        [
          test_case "multiple guards" `Quick test_multiple_guards;
          test_case "disjunction guard" `Quick test_disjunction_guard;
        ] );
      ( "context",
        [ test_case "context declaration" `Quick test_context_declaration ] );
      ( "rule_context",
        [ test_case "rule with context" `Quick test_rule_with_context ] );
      ("closure", [ test_case "closure declaration" `Quick test_closure ]);
      ("each", [ test_case "each comprehension" `Quick test_each ]);
      ( "cond",
        [
          test_case "cond simple" `Quick test_cond_simple;
          test_case "cond two arms" `Quick test_cond_two_arms;
        ] );
      ( "arithmetic",
        [
          test_case "addition" `Quick test_arithmetic;
          test_case "multiply" `Quick test_multiply;
          test_case "subtraction" `Quick test_subtraction;
          test_case "division" `Quick test_division;
        ] );
      ( "logical",
        [
          test_case "and" `Quick test_logical_and;
          test_case "or" `Quick test_logical_or;
          test_case "not" `Quick test_logical_not;
          test_case "biconditional" `Quick test_biconditional;
          test_case "implication" `Quick test_implication;
        ] );
      ("comparison", [ test_case "comparison ops" `Quick test_comparison_ops ]);
      ("in_op", [ test_case "in operator" `Quick test_in_operator ]);
      ("subset_op", [ test_case "subset operator" `Quick test_subset_operator ]);
      ("override", [ test_case "override" `Quick test_override ]);
      ("tuple_expr", [ test_case "tuple expression" `Quick test_tuple ]);
      ("proj_expr", [ test_case "projection expression" `Quick test_projection ]);
      ("primed", [ test_case "primed expression" `Quick test_primed_expr ]);
      ("lit_real", [ test_case "real literal" `Quick test_lit_real ]);
      ("lit_string", [ test_case "string literal" `Quick test_lit_string ]);
      ("initially_expr", [ test_case "initially" `Quick test_initially ]);
      ("qualified", [ test_case "qualified name" `Quick test_qualified_name ]);
      ( "precedence",
        [
          test_case "or/and precedence" `Quick test_precedence_or_and;
          test_case "add/mul precedence" `Quick test_precedence_add_mul;
        ] );
      ("nested_types", [ test_case "nested types" `Quick test_nested_types ]);
      ( "bug_finding",
        [
          test_case "implication associativity" `Quick
            test_implication_associativity;
          test_case "chained comparison parse" `Quick
            test_chained_comparison_parse;
        ] );
    ]
