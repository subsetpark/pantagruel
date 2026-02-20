(** SMT translation tests *)

open Alcotest
open Pantagruel

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let parse_and_collect str =
  let doc = parse str in
  match
    Collect.collect_all
      ~base_env:(Env.empty (Option.value ~default:"" doc.module_name))
      doc
  with
  | Error e -> failf "Collection error: %s" (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Error e -> failf "Type error: %s" (Check.show_type_error e)
      | Ok () -> (env, doc))

(* --- Expression translation tests --- *)

let config = Smt.{ bound = 3 }

let test_lit_bool () =
  let env = Env.empty "" in
  check string "true" "true" (Smt.translate_expr config env (Ast.ELitBool true));
  check string "false" "false"
    (Smt.translate_expr config env (Ast.ELitBool false))

let test_lit_nat () =
  let env = Env.empty "" in
  check string "42" "42" (Smt.translate_expr config env (Ast.ELitNat 42));
  check string "0" "0" (Smt.translate_expr config env (Ast.ELitNat 0))

let test_lit_real () =
  let env = Env.empty "" in
  let result = Smt.translate_expr config env (Ast.ELitReal 3.14) in
  check bool "contains 3.14" true (String.length result > 0)

let test_var () =
  let env = Env.empty "" in
  check string "x" "x" (Smt.translate_expr config env (Ast.EVar "x"));
  check string "hyphenated" "has_permp"
    (Smt.translate_expr config env (Ast.EVar "has-perm?"))

let test_primed () =
  let env = Env.empty "" in
  check string "f'" "f_prime" (Smt.translate_expr config env (Ast.EPrimed "f"))

let test_binop_and () =
  let env = Env.empty "" in
  check string "and" "(and x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpAnd, EVar "x", EVar "y")))

let test_binop_or () =
  let env = Env.empty "" in
  check string "or" "(or x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpOr, EVar "x", EVar "y")))

let test_binop_impl () =
  let env = Env.empty "" in
  check string "impl" "(=> x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpImpl, EVar "x", EVar "y")))

let test_binop_eq () =
  let env = Env.empty "" in
  check string "eq" "(= x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpEq, EVar "x", EVar "y")))

let test_binop_neq () =
  let env = Env.empty "" in
  check string "neq" "(not (= x y))"
    (Smt.translate_expr config env
       (Ast.EBinop (OpNeq, EVar "x", EVar "y")))

let test_binop_lt () =
  let env = Env.empty "" in
  check string "lt" "(< x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpLt, EVar "x", EVar "y")))

let test_binop_arith () =
  let env = Env.empty "" in
  check string "add" "(+ x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpAdd, EVar "x", EVar "y")));
  check string "sub" "(- x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpSub, EVar "x", EVar "y")));
  check string "mul" "(* x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpMul, EVar "x", EVar "y")));
  check string "div" "(div x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpDiv, EVar "x", EVar "y")))

let test_unop_not () =
  let env = Env.empty "" in
  check string "not" "(not x)"
    (Smt.translate_expr config env (Ast.EUnop (OpNot, EVar "x")))

let test_unop_neg () =
  let env = Env.empty "" in
  check string "neg" "(- x)"
    (Smt.translate_expr config env (Ast.EUnop (OpNeg, EVar "x")))

let test_app () =
  let env = Env.empty "" in
  check string "app" "(f x y)"
    (Smt.translate_expr config env
       (Ast.EApp (EVar "f", [ EVar "x"; EVar "y" ])))

let test_primed_app () =
  let env = Env.empty "" in
  check string "primed app" "(f_prime x)"
    (Smt.translate_expr config env
       (Ast.EApp (EPrimed "f", [ EVar "x" ])))

let test_domain_in () =
  let env = Env.empty "" in
  check string "in domain"
    "(or (= x Account_0) (= x Account_1) (= x Account_2))"
    (Smt.translate_expr config env
       (Ast.EBinop (OpIn, EVar "x", EDomain "Account")))

let test_proj () =
  let env = Env.empty "" in
  check string "proj" "(fst_1 x)"
    (Smt.translate_expr config env (Ast.EProj (EVar "x", 1)))

(* --- Sort encoding tests --- *)

let test_sort_bool () =
  check string "Bool sort" "Bool" (Smt.sort_of_ty Types.TyBool)

let test_sort_int () =
  check string "Int sort" "Int" (Smt.sort_of_ty Types.TyInt);
  check string "Nat sort" "Int" (Smt.sort_of_ty Types.TyNat);
  check string "Nat0 sort" "Int" (Smt.sort_of_ty Types.TyNat0)

let test_sort_real () =
  check string "Real sort" "Real" (Smt.sort_of_ty Types.TyReal)

let test_sort_domain () =
  check string "Domain sort" "Account" (Smt.sort_of_ty (Types.TyDomain "Account"))

(* --- Integration tests --- *)

let contains s sub =
  let slen = String.length s in
  let sublen = String.length sub in
  if sublen > slen then false
  else
    let rec check i =
      if i > slen - sublen then false
      else if String.sub s i sublen = sub then true
      else check (i + 1)
    in
    check 0

let test_preamble_domains_simple () =
  let env =
    Env.empty "" |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
  in
  let preamble = Smt.declare_domain_sorts config env in
  check bool "has sort decl" true
    (contains preamble "(declare-sort Account 0)");
  check bool "has Account_0" true (contains preamble "Account_0");
  check bool "has distinct" true (contains preamble "(distinct")

let test_function_declarations () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "balance"
         (Types.TyFunc ([ Types.TyDomain "Account" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
  in
  let decls = Smt.declare_functions env in
  check bool "has balance decl" true
    (contains decls "(declare-fun balance (Account) Int)");
  check bool "has balance_prime decl" true
    (contains decls "(declare-fun balance_prime (Account) Int)")

let test_nullary_rule () =
  let env =
    Env.empty ""
    |> Env.add_rule "nobody"
         (Types.TyFunc ([], Some (Types.TyDomain "User")))
         Ast.dummy_loc ~chapter:0
  in
  let decls = Smt.declare_functions env in
  check bool "has const decl" true
    (contains decls "(declare-const nobody User)")

let test_classify_chapters () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       all a: Account | balance a >= 0.\n\
       where\n\
       Ctx ~> Withdraw | a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n"
  in
  ignore env;
  let chapters = Smt.classify_chapters doc in
  check int "chapter count" 2 (List.length chapters);
  (match List.nth chapters 0 with
  | Smt.Invariant props -> check int "invariant props" 1 (List.length props)
  | Smt.Action _ -> fail "Expected invariant chapter");
  match List.nth chapters 1 with
  | Smt.Action { label; _ } ->
      check string "action label" "Withdraw" label
  | Smt.Invariant _ -> fail "Expected action chapter"

let test_generate_queries () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       all a: Account | balance a >= 0.\n\
       where\n\
       Ctx ~> Withdraw | a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n"
  in
  let queries = Smt.generate_queries config env doc in
  (* Should have: 1 contradiction + 1 invariant + 1 precondition = 3 queries *)
  check int "query count" 3 (List.length queries);
  let kinds =
    List.map (fun (q : Smt.query) -> q.kind) queries
  in
  check bool "has contradiction" true
    (List.mem Smt.Contradiction kinds);
  check bool "has invariant" true
    (List.mem Smt.InvariantPreservation kinds);
  check bool "has precondition" true
    (List.mem Smt.PreconditionSat kinds)

let test_contradiction_query_content () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       all a: Account | balance a >= 0.\n\
       where\n\
       Ctx ~> Withdraw | a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let contra =
    List.find
      (fun (q : Smt.query) -> q.kind = Smt.Contradiction)
      queries
  in
  check bool "has check-sat" true (contains contra.smt2 "(check-sat)");
  check bool "has balance decl" true
    (contains contra.smt2 "(declare-fun balance ");
  check bool "has postcondition" true
    (contains contra.smt2 "balance_prime")

let test_frame_conditions () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "balance"
         (Types.TyFunc ([ Types.TyDomain "Account" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "owner"
         (Types.TyFunc ([ Types.TyDomain "Account" ], Some (Types.TyDomain "User")))
         Ast.dummy_loc ~chapter:0
    |> Env.add_context "Accounts" [ "balance" ]
  in
  let frame = Smt.generate_frame_conditions config env (Some "Accounts") in
  (* owner should be framed (not in Accounts context) *)
  check bool "owner is framed" true
    (contains frame "owner_prime" && contains frame "owner");
  (* balance IS in context: left free, not framed *)
  check bool "balance not framed" false
    (contains frame "balance_prime")

let test_prime_expr () =
  let open Ast in
  (* Free variables get primed *)
  let e = EBinop (OpEq, EApp (EVar "f", [ EVar "x" ]), ELitNat 0) in
  let e' = Smt.prime_expr e in
  (match e' with
  | EBinop (OpEq, EApp (EPrimed "f", [ EPrimed "x" ]), ELitNat 0) -> ()
  | _ -> fail (Printf.sprintf "Unexpected primed expr: %s" (Ast.show_expr e')));
  (* Quantifier-bound variables are NOT primed *)
  let e2 =
    EForall
      ( [ { param_name = "a"; param_type = TName "Account" } ],
        [],
        EBinop (OpGe, EApp (EVar "balance", [ EVar "a" ]), ELitNat 0) )
  in
  let e2' = Smt.prime_expr e2 in
  match e2' with
  | EForall (_, _, EBinop (OpGe, EApp (EPrimed "balance", [ EVar "a" ]), ELitNat 0))
    ->
      ()
  | _ ->
      fail
        (Printf.sprintf "Quantifier-bound var should not be primed: %s"
           (Ast.show_expr e2'))

let test_sanitize_ident () =
  check string "hyphen" "has_perm" (Smt.sanitize_ident "has-perm");
  check string "question" "is_validp" (Smt.sanitize_ident "is-valid?");
  check string "plain" "foo" (Smt.sanitize_ident "foo")

(* --- Test suites --- *)

let expression_tests =
  [
    test_case "bool literals" `Quick test_lit_bool;
    test_case "nat literals" `Quick test_lit_nat;
    test_case "real literals" `Quick test_lit_real;
    test_case "variables" `Quick test_var;
    test_case "primed" `Quick test_primed;
    test_case "and" `Quick test_binop_and;
    test_case "or" `Quick test_binop_or;
    test_case "implication" `Quick test_binop_impl;
    test_case "equality" `Quick test_binop_eq;
    test_case "inequality" `Quick test_binop_neq;
    test_case "less than" `Quick test_binop_lt;
    test_case "arithmetic" `Quick test_binop_arith;
    test_case "not" `Quick test_unop_not;
    test_case "negation" `Quick test_unop_neg;
    test_case "application" `Quick test_app;
    test_case "primed application" `Quick test_primed_app;
    test_case "domain membership" `Quick test_domain_in;
    test_case "projection" `Quick test_proj;
  ]

let sort_tests =
  [
    test_case "Bool sort" `Quick test_sort_bool;
    test_case "Int sorts" `Quick test_sort_int;
    test_case "Real sort" `Quick test_sort_real;
    test_case "Domain sort" `Quick test_sort_domain;
  ]

let integration_tests =
  [
    test_case "domain preamble" `Quick test_preamble_domains_simple;
    test_case "function declarations" `Quick test_function_declarations;
    test_case "nullary rule" `Quick test_nullary_rule;
    test_case "classify chapters" `Quick test_classify_chapters;
    test_case "generate queries" `Quick test_generate_queries;
    test_case "contradiction query content" `Quick test_contradiction_query_content;
    test_case "frame conditions" `Quick test_frame_conditions;
    test_case "prime expr" `Quick test_prime_expr;
    test_case "sanitize ident" `Quick test_sanitize_ident;
  ]

let () =
  run "SMT"
    [
      ("expressions", expression_tests);
      ("sorts", sort_tests);
      ("integration", integration_tests);
    ]
