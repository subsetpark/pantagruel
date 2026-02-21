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

let config = Smt.{ bound = 3; steps = 5; domain_bounds = Env.StringMap.empty }

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
    (Smt.translate_expr config env (Ast.EBinop (OpAnd, EVar "x", EVar "y")))

let test_binop_or () =
  let env = Env.empty "" in
  check string "or" "(or x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpOr, EVar "x", EVar "y")))

let test_binop_impl () =
  let env = Env.empty "" in
  check string "impl" "(=> x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpImpl, EVar "x", EVar "y")))

let test_binop_eq () =
  let env = Env.empty "" in
  check string "eq" "(= x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpEq, EVar "x", EVar "y")))

let test_binop_neq () =
  let env = Env.empty "" in
  check string "neq" "(not (= x y))"
    (Smt.translate_expr config env (Ast.EBinop (OpNeq, EVar "x", EVar "y")))

let test_binop_lt () =
  let env = Env.empty "" in
  check string "lt" "(< x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpLt, EVar "x", EVar "y")))

let test_binop_arith () =
  let env = Env.empty "" in
  check string "add" "(+ x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpAdd, EVar "x", EVar "y")));
  check string "sub" "(- x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpSub, EVar "x", EVar "y")));
  check string "mul" "(* x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpMul, EVar "x", EVar "y")));
  check string "div" "(div x y)"
    (Smt.translate_expr config env (Ast.EBinop (OpDiv, EVar "x", EVar "y")))

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
    (Smt.translate_expr config env (Ast.EApp (EPrimed "f", [ EVar "x" ])))

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
  check string "Domain sort" "Account"
    (Smt.sort_of_ty (Types.TyDomain "Account"))

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
  let env = Env.empty "" |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0 in
  let preamble = Smt.declare_domain_sorts config env in
  check bool "has sort decl" true (contains preamble "(declare-sort Account 0)");
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
  | Smt.Action { label; _ } -> check string "action label" "Withdraw" label
  | Smt.Invariant _ -> fail "Expected action chapter"

let test_generate_queries () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       initially all a: Account | balance a = 100.\n\
       all a: Account | balance a >= 0.\n\
       where\n\
       Ctx ~> Withdraw | a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n"
  in
  let queries = Smt.generate_queries config env doc in
  (* Should have: 1 invariant consistency + 1 init consistency + 1 init-invariant
     + 1 contradiction + 1 invariant + 1 precondition + 1 bmc-invariant
     + 1 bmc-deadlock = 8 queries *)
  check int "query count" 8 (List.length queries);
  let kinds = List.map (fun (q : Smt.query) -> q.kind) queries in
  check bool "has invariant consistency" true
    (List.mem Smt.InvariantConsistency kinds);
  check bool "has contradiction" true (List.mem Smt.Contradiction kinds);
  check bool "has invariant" true (List.mem Smt.InvariantPreservation kinds);
  check bool "has precondition" true (List.mem Smt.PreconditionSat kinds);
  check bool "has bmc deadlock" true (List.mem Smt.BMCDeadlock kinds)

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
    List.find (fun (q : Smt.query) -> q.kind = Smt.Contradiction) queries
  in
  check bool "has check-sat" true (contains contra.smt2 "(check-sat)");
  check bool "has balance decl" true
    (contains contra.smt2 "(declare-fun balance ");
  check bool "has postcondition" true (contains contra.smt2 "balance_prime");
  check bool "has produce-unsat-cores" true
    (contains contra.smt2 "(set-option :produce-unsat-cores true)");
  check bool "has get-unsat-core" true (contains contra.smt2 "(get-unsat-core)");
  check bool "has named postcond" true (contains contra.smt2 ":named postcond_");
  check bool "assertion_names non-empty" true (contra.assertion_names <> [])

let test_frame_conditions () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "balance"
         (Types.TyFunc ([ Types.TyDomain "Account" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "owner"
         (Types.TyFunc
            ([ Types.TyDomain "Account" ], Some (Types.TyDomain "User")))
         Ast.dummy_loc ~chapter:0
    |> Env.add_context "Accounts" [ "balance" ]
  in
  let frame = Smt.generate_frame_conditions config env (Some "Accounts") in
  (* owner should be framed (not in Accounts context) *)
  check bool "owner is framed" true
    (contains frame "owner_prime" && contains frame "owner");
  (* balance IS in context: left free, not framed *)
  check bool "balance not framed" false (contains frame "balance_prime")

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
  | EForall
      (_, _, EBinop (OpGe, EApp (EPrimed "balance", [ EVar "a" ]), ELitNat 0))
    ->
      ()
  | _ ->
      fail
        (Printf.sprintf "Quantifier-bound var should not be primed: %s"
           (Ast.show_expr e2'))

let test_card_domain () =
  let env = Env.empty "" |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0 in
  check string "#Domain = bound" "3"
    (Smt.translate_expr config env (Ast.EUnop (OpCard, EDomain "Account")))

let test_card_list () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([], Some (Types.TyList (Types.TyDomain "Account"))))
         Ast.dummy_loc ~chapter:0
  in
  let result =
    Smt.translate_expr config env (Ast.EUnop (OpCard, EVar "active"))
  in
  (* Should expand to sum of ite over domain elements *)
  check bool "has ite" true (contains result "(ite (select active");
  check bool "has Account_0" true (contains result "Account_0");
  check bool "has Account_2" true (contains result "Account_2")

let test_subset () =
  let env =
    Env.empty ""
    |> Env.add_domain "Item" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "xs"
         (Types.TyFunc ([], Some (Types.TyList (Types.TyDomain "Item"))))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "ys"
         (Types.TyFunc ([], Some (Types.TyList (Types.TyDomain "Item"))))
         Ast.dummy_loc ~chapter:0
  in
  let result =
    Smt.translate_expr config env (Ast.EBinop (OpSubset, EVar "xs", EVar "ys"))
  in
  (* Should expand over domain elements *)
  check bool "has select xs" true (contains result "(select xs");
  check bool "has select ys" true (contains result "(select ys");
  check bool "has Item_0" true (contains result "Item_0")

let test_in_list () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([], Some (Types.TyList (Types.TyDomain "Account"))))
         Ast.dummy_loc ~chapter:0
    |> Env.add_var "a" (Types.TyDomain "Account")
  in
  let result =
    Smt.translate_expr config env (Ast.EBinop (OpIn, EVar "a", EVar "active"))
  in
  check string "in list" "(select active a)" result

let test_sanitize_ident () =
  check string "hyphen" "has_perm" (Smt.sanitize_ident "has-perm");
  check string "question" "is_validp" (Smt.sanitize_ident "is-valid?");
  check string "plain" "foo" (Smt.sanitize_ident "foo")

let test_domain_standalone () =
  let env = Env.empty "" in
  check_raises "EDomain standalone raises"
    (Failure
       "SMT translation: EDomain 'Account' appeared in standalone position")
    (fun () -> ignore (Smt.translate_expr config env (Ast.EDomain "Account")))

let test_card_non_domain_list () =
  let env =
    Env.empty ""
    |> Env.add_rule "nums"
         (Types.TyFunc ([], Some (Types.TyList Types.TyInt)))
         Ast.dummy_loc ~chapter:0
  in
  let result =
    Smt.translate_expr config env (Ast.EUnop (OpCard, EVar "nums"))
  in
  (* Non-domain list card should emit warning and 0 fallback *)
  check bool "has WARNING" true (contains result "WARNING");
  check bool "has 0 fallback" true (contains result "0")

let test_subset_domain_rhs () =
  (* Ensure OpSubset with EDomain on RHS doesn't trigger standalone failwith *)
  let env =
    Env.empty ""
    |> Env.add_domain "Item" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "xs"
         (Types.TyFunc ([], Some (Types.TyList (Types.TyDomain "Item"))))
         Ast.dummy_loc ~chapter:0
  in
  let result =
    Smt.translate_expr config env
      (Ast.EBinop (OpSubset, EVar "xs", EDomain "Item"))
  in
  check bool "has select xs" true (contains result "(select xs");
  check bool "has Item_0" true (contains result "Item_0")

(* --- Value terms tests --- *)

let test_build_value_terms () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "balance"
         (Types.TyFunc ([ Types.TyDomain "Account" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
  in
  let params = [ Ast.{ param_name = "a"; param_type = TName "Account" } ] in
  let terms = Smt.build_value_terms config env params in
  check bool "has param a" true (List.mem "a" terms);
  check bool "has (balance a)" true (List.mem "(balance a)" terms);
  check bool "has (balance_prime a)" true (List.mem "(balance_prime a)" terms)

let test_build_value_terms_nullary () =
  let env =
    Env.empty ""
    |> Env.add_rule "count"
         (Types.TyFunc ([], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
  in
  let terms = Smt.build_value_terms config env [] in
  check bool "has count" true (List.mem "count" terms);
  check bool "has count_prime" true (List.mem "count_prime" terms)

let test_contradiction_query_has_get_value () =
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
    List.find (fun (q : Smt.query) -> q.kind = Smt.Contradiction) queries
  in
  check bool "has get-value" true (contains contra.smt2 "(get-value (")

let test_invariant_query_has_get_value () =
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
  let inv =
    List.find
      (fun (q : Smt.query) -> q.kind = Smt.InvariantPreservation)
      queries
  in
  check bool "has get-value" true (contains inv.smt2 "(get-value (")

let test_precondition_query_no_get_value () =
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
  let precon =
    List.find (fun (q : Smt.query) -> q.kind = Smt.PreconditionSat) queries
  in
  check bool "no get-value" false (contains precon.smt2 "(get-value (")

(* --- Solver parsing tests --- *)

let test_parse_get_value_simple () =
  let input = "((a Account_0)\n (amount 5))" in
  let pairs = Solver.parse_get_value input in
  check int "pair count" 2 (List.length pairs);
  check (pair string string) "first" ("a", "Account_0") (List.nth pairs 0);
  check (pair string string) "second" ("amount", "5") (List.nth pairs 1)

let test_parse_get_value_applied () =
  let input = "((a Account_0)\n ((balance a) 10)\n ((balance_prime a) 5))" in
  let pairs = Solver.parse_get_value input in
  check int "pair count" 3 (List.length pairs);
  check (pair string string) "param" ("a", "Account_0") (List.nth pairs 0);
  check (pair string string) "balance" ("(balance a)", "10") (List.nth pairs 1);
  check (pair string string) "balance'" ("(balance_prime a)", "5")
    (List.nth pairs 2)

let test_parse_get_value_empty () =
  let pairs = Solver.parse_get_value "" in
  check int "empty" 0 (List.length pairs)

let test_translate_display_name () =
  check string "plain" "a" (Solver.translate_display_name "a");
  check string "prime" "balance'"
    (Solver.translate_display_name "balance_prime");
  check string "applied" "balance a"
    (Solver.translate_display_name "(balance a)");
  check string "applied prime" "balance' a"
    (Solver.translate_display_name "(balance_prime a)")

let test_format_counterexample () =
  let values =
    [ ("a", "Account_0"); ("(balance a)", "10"); ("(balance_prime a)", "5") ]
  in
  let result = Solver.format_counterexample values in
  check bool "has Before:" true (contains result "Before:");
  check bool "has Action:" true (contains result "Action:");
  check bool "has After:" true (contains result "After:");
  check bool "has a = Account_0" true (contains result "a = Account_0");
  check bool "has balance a = 10" true (contains result "balance a = 10");
  check bool "has balance' a = 5" true (contains result "balance' a = 5")

let test_format_counterexample_empty () =
  check string "empty" "" (Solver.format_counterexample [])

let test_translate_value () =
  check string "z3 internal" "Account_0"
    (Solver.translate_value "Account!val!0");
  check string "our constant" "Account_0" (Solver.translate_value "Account_0");
  check string "integer" "5" (Solver.translate_value "5");
  check string "no trailing digits" "foo_bar" (Solver.translate_value "foo_bar");
  check string "negative sexp" "-1" (Solver.translate_value "(- 1)");
  check string "negative large" "-42" (Solver.translate_value "(- 42)")

let test_format_counterexample_filters_unchanged () =
  (* balance a = 10 and balance_prime a = 10 should both be filtered *)
  let values =
    [
      ("a", "Account_0");
      ("(balance a)", "10");
      ("(balance_prime a)", "10");
      ("(owner a)", "User_0");
      ("(owner_prime a)", "User_1");
    ]
  in
  let result = Solver.format_counterexample values in
  check bool "has a = Account" true (contains result "a = Account");
  check bool "no balance a (unchanged)" false (contains result "balance a");
  check bool "has owner a" true (contains result "owner a = User");
  check bool "has owner' a" true (contains result "owner' a = User")

let test_invariant_query_has_text () =
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
  let inv =
    List.find
      (fun (q : Smt.query) -> q.kind = Smt.InvariantPreservation)
      queries
  in
  check bool "invariant_text is non-empty" true (inv.invariant_text <> "");
  check bool "invariant_text has balance" true
    (contains inv.invariant_text "balance")

(* --- Unsat core tests --- *)

let test_parse_unsat_core_simple () =
  let core = Solver.parse_unsat_core "(postcond_0 postcond_1)" in
  check int "core count" 2 (List.length core);
  check string "first" "postcond_0" (List.nth core 0);
  check string "second" "postcond_1" (List.nth core 1)

let test_parse_unsat_core_empty () =
  check (list string) "empty string" [] (Solver.parse_unsat_core "");
  check (list string) "empty parens" [] (Solver.parse_unsat_core "()")

let test_parse_unsat_core_single () =
  let core = Solver.parse_unsat_core "(inv_0)" in
  check int "single" 1 (List.length core);
  check string "name" "inv_0" (List.hd core)

let test_format_unsat_core () =
  let names =
    [
      ("postcond_0", "balance' a = balance a + 1");
      ("postcond_1", "balance' a = balance a - 1");
      ("frame_2", "owner' = owner (frame)");
    ]
  in
  let result = Solver.format_unsat_core [ "postcond_0"; "postcond_1" ] names in
  check bool "has Conflicting" true (contains result "Conflicting constraints:");
  check bool "has first postcond" true
    (contains result "balance' a = balance a + 1");
  check bool "has second postcond" true
    (contains result "balance' a = balance a - 1");
  check bool "no frame" false (contains result "frame")

let test_format_unsat_core_empty () =
  check string "no core" "" (Solver.format_unsat_core [] []);
  check string "no matches" ""
    (Solver.format_unsat_core [ "unknown_0" ] [ ("postcond_0", "x") ])

let test_contradiction_query_named_assertions () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       where\n\
       Ctx ~> BadAction | a: Account.\n\
       ---\n\
       balance' a = balance a + 1.\n\
       balance' a = balance a - 1.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let contra =
    List.find (fun (q : Smt.query) -> q.kind = Smt.Contradiction) queries
  in
  (* Should have named postconditions *)
  let postcond_names =
    List.filter
      (fun (name, _) ->
        String.length name >= 8 && String.sub name 0 8 = "postcond")
      contra.assertion_names
  in
  check bool "has postcond names" true (List.length postcond_names >= 2);
  (* No frame conditions here since balance is the only function and it's in context *)
  check bool "has produce-unsat-cores" true
    (contains contra.smt2 "(set-option :produce-unsat-cores true)");
  check bool "has get-unsat-core" true (contains contra.smt2 "(get-unsat-core)")

let test_precondition_query_named_assertions () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       all a: Account | balance a >= 0.\n\
       where\n\
       Ctx ~> ImpossibleAction | a: Account, balance a < 0.\n\
       ---\n\
       balance' a = 0.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let precon =
    List.find (fun (q : Smt.query) -> q.kind = Smt.PreconditionSat) queries
  in
  check bool "has produce-unsat-cores" true
    (contains precon.smt2 "(set-option :produce-unsat-cores true)");
  check bool "has get-unsat-core" true (contains precon.smt2 "(get-unsat-core)");
  (* Should have named invariants and preconditions *)
  let inv_names =
    List.filter
      (fun (name, _) -> String.length name >= 3 && String.sub name 0 3 = "inv")
      precon.assertion_names
  in
  let precond_names =
    List.filter
      (fun (name, _) ->
        String.length name >= 7 && String.sub name 0 7 = "precond")
      precon.assertion_names
  in
  check bool "has inv names" true (List.length inv_names >= 1);
  check bool "has precond names" true (List.length precond_names >= 1);
  (* Invariant text should mention "Invariant:" prefix *)
  let inv_text = snd (List.hd inv_names) in
  check bool "inv has prefix" true (contains inv_text "Invariant:")

(* --- BMC tests --- *)

let test_replace_word () =
  check string "simple" "f_s0" (Smt.replace_word ~from:"f" ~to_:"f_s0" "f");
  check string "in parens" "(f_s0 x)"
    (Smt.replace_word ~from:"f" ~to_:"f_s0" "(f x)");
  check string "no partial" "foo" (Smt.replace_word ~from:"f" ~to_:"f_s0" "foo");
  check string "multiple" "(and f_s0 f_s0)"
    (Smt.replace_word ~from:"f" ~to_:"f_s0" "(and f f)");
  check string "prime first" "(f_prime x)"
    (Smt.replace_word ~from:"f" ~to_:"f_s0" "(f_prime x)")

let test_rename_smt_for_step () =
  let env =
    Env.empty ""
    |> Env.add_rule "balance"
         (Types.TyFunc ([ Types.TyDomain "Account" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
  in
  let smt = "(>= (balance a) 0)" in
  let renamed = Smt.rename_smt_for_step env smt 2 in
  check bool "has balance_s2" true (contains renamed "balance_s2");
  check bool "no bare balance" false (contains renamed "(balance a)");
  (* Test primed renaming *)
  let smt2 = "(= (balance_prime a) (- (balance a) 1))" in
  let renamed2 = Smt.rename_smt_for_step env smt2 1 in
  check bool "has balance_s2 (prime)" true (contains renamed2 "balance_s2");
  check bool "has balance_s1 (base)" true (contains renamed2 "balance_s1")

let test_generate_bmc_queries () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       all a: Account | balance a >= 0.\n\
       initially all a: Account | balance a = 0.\n\
       where\n\
       Ctx ~> Withdraw | a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n\
       all b: Account | b != a -> balance' b = balance b.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let bmc_queries =
    List.filter (fun (q : Smt.query) -> q.kind = Smt.BMCInvariant) queries
  in
  check bool "has BMC queries" true (List.length bmc_queries >= 1);
  let bmc = List.hd bmc_queries in
  check bool "has check-sat" true (contains bmc.smt2 "(check-sat)");
  check bool "has step 0" true (contains bmc.smt2 "balance_s0");
  check bool "has step 1" true (contains bmc.smt2 "balance_s1");
  check bool "has initial state" true (contains bmc.smt2 "Initial state");
  check bool "has transition" true (contains bmc.smt2 "Transition step");
  check bool "has violated" true (contains bmc.smt2 "Invariant violated")

let test_no_bmc_without_init () =
  (* Spec without init props should produce no BMC queries *)
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
  let bmc_queries =
    List.filter (fun (q : Smt.query) -> q.kind = Smt.BMCInvariant) queries
  in
  check int "no BMC queries without init" 0 (List.length bmc_queries)

let test_format_bmc_counterexample () =
  let values =
    [ ("balance_s0", "0"); ("balance_s1", "5"); ("balance_s2", "3") ]
  in
  let result = Solver.format_bmc_counterexample values in
  check bool "has Step 0" true (contains result "Step 0");
  check bool "has Step 1" true (contains result "Step 1");
  check bool "has Step 2" true (contains result "Step 2");
  check bool "has balance = 0" true (contains result "balance = 0");
  check bool "has balance = 5" true (contains result "balance = 5")

let test_format_bmc_counterexample_applied () =
  let values =
    [ ("(balance_s0 Account_0)", "10"); ("(balance_s1 Account_0)", "5") ]
  in
  let result = Solver.format_bmc_counterexample values in
  check bool "has Step 0" true (contains result "Step 0");
  check bool "has Step 1" true (contains result "Step 1");
  check bool "has balance Account_0 = 10" true
    (contains result "balance Account_0 = 10")

(* --- Domain bounds tests --- *)

let test_compute_domain_bounds () =
  let env, _doc =
    parse_and_collect
      "module Test.\n\
       Color.\n\
       red => Color.\n\
       green => Color.\n\
       blue => Color.\n\
       yellow => Color.\n\
       purple => Color.\n\
       Account.\n\
       admin => Account.\n\
       ---\n\
       true.\n"
  in
  let bounds = Smt.compute_domain_bounds 3 env in
  (* Color has 5 constants > default bound 3, so it should be in the map *)
  check (option int) "Color bound" (Some 5)
    (Env.StringMap.find_opt "Color" bounds);
  (* Account has 1 constant <= 3, so it should NOT be in the map *)
  check (option int) "Account bound (not bumped)" None
    (Env.StringMap.find_opt "Account" bounds)

let test_bound_for () =
  let bounds = Env.StringMap.singleton "Color" 5 in
  let config = Smt.{ bound = 3; steps = 5; domain_bounds = bounds } in
  check int "Color uses override" 5 (Smt.bound_for config "Color");
  check int "Account uses default" 3 (Smt.bound_for config "Account")

let test_card_domain_with_bounds () =
  let env = Env.empty "" |> Env.add_domain "Color" Ast.dummy_loc ~chapter:0 in
  let bounds = Env.StringMap.singleton "Color" 5 in
  let cfg = Smt.{ bound = 3; steps = 5; domain_bounds = bounds } in
  check string "#Domain with per-domain bound" "5"
    (Smt.translate_expr cfg env (Ast.EUnop (OpCard, EDomain "Color")))

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
    test_case "list membership" `Quick test_in_list;
    test_case "projection" `Quick test_proj;
    test_case "cardinality domain" `Quick test_card_domain;
    test_case "cardinality list" `Quick test_card_list;
    test_case "subset" `Quick test_subset;
    test_case "domain standalone raises" `Quick test_domain_standalone;
    test_case "cardinality non-domain list" `Quick test_card_non_domain_list;
    test_case "subset with domain RHS" `Quick test_subset_domain_rhs;
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
    test_case "contradiction query content" `Quick
      test_contradiction_query_content;
    test_case "frame conditions" `Quick test_frame_conditions;
    test_case "prime expr" `Quick test_prime_expr;
    test_case "sanitize ident" `Quick test_sanitize_ident;
  ]

let value_terms_tests =
  [
    test_case "build value terms" `Quick test_build_value_terms;
    test_case "build value terms nullary" `Quick test_build_value_terms_nullary;
    test_case "contradiction query has get-value" `Quick
      test_contradiction_query_has_get_value;
    test_case "invariant query has get-value" `Quick
      test_invariant_query_has_get_value;
    test_case "invariant query has invariant text" `Quick
      test_invariant_query_has_text;
    test_case "precondition query no get-value" `Quick
      test_precondition_query_no_get_value;
  ]

let solver_parsing_tests =
  [
    test_case "parse get-value simple" `Quick test_parse_get_value_simple;
    test_case "parse get-value applied" `Quick test_parse_get_value_applied;
    test_case "parse get-value empty" `Quick test_parse_get_value_empty;
    test_case "translate display name" `Quick test_translate_display_name;
    test_case "format counterexample" `Quick test_format_counterexample;
    test_case "format counterexample empty" `Quick
      test_format_counterexample_empty;
    test_case "translate domain value" `Quick test_translate_value;
    test_case "format counterexample filters unchanged" `Quick
      test_format_counterexample_filters_unchanged;
  ]

let unsat_core_tests =
  [
    test_case "parse unsat core simple" `Quick test_parse_unsat_core_simple;
    test_case "parse unsat core empty" `Quick test_parse_unsat_core_empty;
    test_case "parse unsat core single" `Quick test_parse_unsat_core_single;
    test_case "format unsat core" `Quick test_format_unsat_core;
    test_case "format unsat core empty" `Quick test_format_unsat_core_empty;
    test_case "contradiction named assertions" `Quick
      test_contradiction_query_named_assertions;
    test_case "precondition named assertions" `Quick
      test_precondition_query_named_assertions;
  ]

let bmc_tests =
  [
    test_case "replace_word" `Quick test_replace_word;
    test_case "rename_smt_for_step" `Quick test_rename_smt_for_step;
    test_case "generate BMC queries" `Quick test_generate_bmc_queries;
    test_case "no BMC without init" `Quick test_no_bmc_without_init;
    test_case "format BMC counterexample" `Quick test_format_bmc_counterexample;
    test_case "format BMC counterexample applied" `Quick
      test_format_bmc_counterexample_applied;
  ]

let domain_bounds_tests =
  [
    test_case "compute domain bounds" `Quick test_compute_domain_bounds;
    test_case "bound_for helper" `Quick test_bound_for;
    test_case "cardinality with per-domain bound" `Quick
      test_card_domain_with_bounds;
  ]

(* --- Comprehension tests --- *)

let test_in_forall_comprehension () =
  (* y in (all x: D | f x) → disjunction over domain elements *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Role" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "role"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some (Types.TyDomain "Role")))
         Ast.dummy_loc ~chapter:0
    |> Env.add_var "r" (Types.TyDomain "Role")
  in
  let expr =
    Ast.EBinop
      ( OpIn,
        EVar "r",
        EForall
          ( [ { param_name = "u"; param_type = TName "User" } ],
            [],
            EApp (EVar "role", [ EVar "u" ]) ) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has (= r (role User_0))" true
    (contains result "(= r (role User_0))");
  check bool "has (= r (role User_1))" true
    (contains result "(= r (role User_1))");
  check bool "has or" true (contains result "(or")

let test_in_forall_comprehension_guarded () =
  (* y in (all x: D, g x | f x) → includes guard in expansion *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Role" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "role"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some (Types.TyDomain "Role")))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
    |> Env.add_var "r" (Types.TyDomain "Role")
  in
  let expr =
    Ast.EBinop
      ( OpIn,
        EVar "r",
        EForall
          ( [ { param_name = "u"; param_type = TName "User" } ],
            [ GExpr (EApp (EVar "active", [ EVar "u" ])) ],
            EApp (EVar "role", [ EVar "u" ]) ) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has guard (active User_0)" true
    (contains result "(active User_0)");
  check bool "has (= r (role User_0))" true
    (contains result "(= r (role User_0))");
  check bool "has and (guard + eq)" true (contains result "(and")

let test_in_membership_comprehension () =
  (* y in (all x in xs | f x) → includes membership guard *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Role" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "role"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some (Types.TyDomain "Role")))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "admins"
         (Types.TyFunc ([], Some (Types.TyList (Types.TyDomain "User"))))
         Ast.dummy_loc ~chapter:0
    |> Env.add_var "r" (Types.TyDomain "Role")
  in
  let expr =
    Ast.EBinop
      ( OpIn,
        EVar "r",
        EForall
          ([], [ GIn ("u", EVar "admins") ], EApp (EVar "role", [ EVar "u" ]))
      )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has select admins" true (contains result "(select admins");
  check bool "has (= r (role User_0))" true
    (contains result "(= r (role User_0))");
  check bool "has or" true (contains result "(or")

let test_card_forall_comprehension () =
  (* #(all x: D | f x) → count over range domain *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Role" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "role"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some (Types.TyDomain "Role")))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EUnop
      ( OpCard,
        EForall
          ( [ { param_name = "u"; param_type = TName "User" } ],
            [],
            EApp (EVar "role", [ EVar "u" ]) ) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has ite" true (contains result "(ite");
  check bool "has Role_0" true (contains result "Role_0");
  check bool "has +" true (contains result "(+")

let test_forall_comprehension_standalone_fails () =
  (* Non-Bool EForall in standalone position → error *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Role" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "role"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some (Types.TyDomain "Role")))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EForall
      ( [ { param_name = "u"; param_type = TName "User" } ],
        [],
        EApp (EVar "role", [ EVar "u" ]) )
  in
  check_raises "comprehension standalone raises"
    (Failure
       "SMT translation: comprehension (all ... | non-Bool) in standalone \
        position; use inside 'in', '#', or 'subset'") (fun () ->
      ignore (Smt.translate_expr config env expr))

let test_exists_comprehension_standalone_fails () =
  (* Non-Bool EExists in standalone position → error *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Role" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "role"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some (Types.TyDomain "Role")))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EExists
      ( [ { param_name = "u"; param_type = TName "User" } ],
        [],
        EApp (EVar "role", [ EVar "u" ]) )
  in
  check_raises "some comprehension standalone raises"
    (Failure
       "SMT translation: non-Bool 'some' comprehension not supported in SMT")
    (fun () -> ignore (Smt.translate_expr config env expr))

let comprehension_tests =
  [
    test_case "in forall comprehension" `Quick test_in_forall_comprehension;
    test_case "in forall guarded" `Quick test_in_forall_comprehension_guarded;
    test_case "in membership comprehension" `Quick
      test_in_membership_comprehension;
    test_case "card forall comprehension" `Quick test_card_forall_comprehension;
    test_case "forall standalone fails" `Quick
      test_forall_comprehension_standalone_fails;
    test_case "exists standalone fails" `Quick
      test_exists_comprehension_standalone_fails;
  ]

let () =
  run "SMT"
    [
      ("expressions", expression_tests);
      ("sorts", sort_tests);
      ("integration", integration_tests);
      ("value_terms", value_terms_tests);
      ("solver_parsing", solver_parsing_tests);
      ("unsat_core", unsat_core_tests);
      ("bmc", bmc_tests);
      ("domain_bounds", domain_bounds_tests);
      ("comprehensions", comprehension_tests);
    ]
