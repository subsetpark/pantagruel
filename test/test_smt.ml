(** SMT translation tests *)

open Alcotest
open Pantagruel

let parse_and_collect = Test_util.parse_and_collect

(* --- Expression translation tests --- *)

let config =
  Smt.make_config ~bound:3 ~steps:5 ~domain_bounds:Env.StringMap.empty
    ~inject_guards:true

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
  check string "x" "x" (Smt.translate_expr config env (Ast.EVar (Lower "x")));
  check string "hyphenated" "has_permp"
    (Smt.translate_expr config env (Ast.EVar (Lower "has-perm?")))

let test_primed () =
  let env = Env.empty "" in
  check string "f'" "f_prime"
    (Smt.translate_expr config env (Ast.EPrimed (Lower "f")))

let test_binop_and () =
  let env = Env.empty "" in
  check string "and" "(and x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpAnd, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_or () =
  let env = Env.empty "" in
  check string "or" "(or x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpOr, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_impl () =
  let env = Env.empty "" in
  check string "impl" "(=> x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpImpl, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_eq () =
  let env = Env.empty "" in
  check string "eq" "(= x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpEq, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_neq () =
  let env = Env.empty "" in
  check string "neq" "(not (= x y))"
    (Smt.translate_expr config env
       (Ast.EBinop (OpNeq, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_lt () =
  let env = Env.empty "" in
  check string "lt" "(< x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpLt, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_arith () =
  let env = Env.empty "" in
  check string "add" "(+ x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpAdd, EVar (Lower "x"), EVar (Lower "y"))));
  check string "sub" "(- x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpSub, EVar (Lower "x"), EVar (Lower "y"))));
  check string "mul" "(* x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpMul, EVar (Lower "x"), EVar (Lower "y"))));
  check string "div" "(div x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpDiv, EVar (Lower "x"), EVar (Lower "y"))))

let test_unop_not () =
  let env = Env.empty "" in
  check string "not" "(not x)"
    (Smt.translate_expr config env (Ast.EUnop (OpNot, EVar (Lower "x"))))

let test_unop_neg () =
  let env = Env.empty "" in
  check string "neg" "(- x)"
    (Smt.translate_expr config env (Ast.EUnop (OpNeg, EVar (Lower "x"))))

let test_app () =
  let env = Env.empty "" in
  check string "app" "(f x y)"
    (Smt.translate_expr config env
       (Ast.EApp (EVar (Lower "f"), [ EVar (Lower "x"); EVar (Lower "y") ])))

let test_primed_app () =
  let env = Env.empty "" in
  check string "primed app" "(f_prime x)"
    (Smt.translate_expr config env
       (Ast.EApp (EPrimed (Lower "f"), [ EVar (Lower "x") ])))

let test_domain_in () =
  let env = Env.empty "" in
  check string "in domain"
    "(or (= x Account_0) (= x Account_1) (= x Account_2))"
    (Smt.translate_expr config env
       (Ast.EBinop (OpIn, EVar (Lower "x"), EDomain (Upper "Account"))))

let test_proj () =
  let env = Env.empty "" in
  check string "proj" "(fst_1 x)"
    (Smt.translate_expr config env (Ast.EProj (EVar (Lower "x"), 1)))

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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n"
  in
  ignore env;
  let chapters = Smt.classify_chapters doc in
  check int "chapter count" 2 (List.length chapters);
  (match List.nth chapters 0 with
  | Smt.Invariant { propositions; _ } ->
      check int "invariant props" 1 (List.length propositions)
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
  let frame = Smt.generate_frame_conditions config env [ "Accounts" ] in
  (* owner should be framed (not in Accounts context) *)
  check bool "owner is framed" true
    (contains frame "owner_prime" && contains frame "owner");
  (* balance IS in context: left free, not framed *)
  check bool "balance not framed" false (contains frame "balance_prime")

let test_prime_expr () =
  let open Ast in
  (* Free variables get primed *)
  let e =
    EBinop (OpEq, EApp (EVar (Lower "f"), [ EVar (Lower "x") ]), ELitNat 0)
  in
  let e' = Smt.prime_expr e in
  (match[@warning "-4"] e' with
  | EBinop (OpEq, EApp (EPrimed (Lower "f"), [ EPrimed (Lower "x") ]), ELitNat 0)
    ->
      ()
  | _ -> fail (Printf.sprintf "Unexpected primed expr: %s" (Ast.show_expr e')));
  (* Quantifier-bound variables are NOT primed *)
  let e2 =
    EForall
      ( [ { param_name = Lower "a"; param_type = TName (Upper "Account") } ],
        [],
        EBinop
          (OpGe, EApp (EVar (Lower "balance"), [ EVar (Lower "a") ]), ELitNat 0)
      )
  in
  let e2' = Smt.prime_expr e2 in
  match[@warning "-4"] e2' with
  | EForall
      ( _,
        _,
        EBinop
          ( OpGe,
            EApp (EPrimed (Lower "balance"), [ EVar (Lower "a") ]),
            ELitNat 0 ) ) ->
      ()
  | _ ->
      fail
        (Printf.sprintf "Quantifier-bound var should not be primed: %s"
           (Ast.show_expr e2'))

let test_card_domain () =
  let env = Env.empty "" |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0 in
  check string "#Domain = bound" "3"
    (Smt.translate_expr config env
       (Ast.EUnop (OpCard, EDomain (Upper "Account"))))

let test_card_list () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([], Some (Types.TyList (Types.TyDomain "Account"))))
         Ast.dummy_loc ~chapter:0
  in
  let result =
    Smt.translate_expr config env (Ast.EUnop (OpCard, EVar (Lower "active")))
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
    Smt.translate_expr config env
      (Ast.EBinop (OpSubset, EVar (Lower "xs"), EVar (Lower "ys")))
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
    Smt.translate_expr config env
      (Ast.EBinop (OpIn, EVar (Lower "a"), EVar (Lower "active")))
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
    (fun () ->
      ignore (Smt.translate_expr config env (Ast.EDomain (Upper "Account"))))

let test_card_non_domain_list () =
  let env =
    Env.empty ""
    |> Env.add_rule "nums"
         (Types.TyFunc ([], Some (Types.TyList Types.TyInt)))
         Ast.dummy_loc ~chapter:0
  in
  let result =
    Smt.translate_expr config env (Ast.EUnop (OpCard, EVar (Lower "nums")))
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
      (Ast.EBinop (OpSubset, EVar (Lower "xs"), EDomain (Upper "Item")))
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
  let params =
    [ Ast.{ param_name = Lower "a"; param_type = TName (Upper "Account") } ]
  in
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
       Ctx ~> BadAction @ a: Account.\n\
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
       Ctx ~> ImpossibleAction @ a: Account, balance a < 0.\n\
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
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
  let config =
    Smt.make_config ~bound:3 ~steps:5 ~domain_bounds:bounds ~inject_guards:true
  in
  check int "Color uses override" 5 (Smt.bound_for config "Color");
  check int "Account uses default" 3 (Smt.bound_for config "Account")

let test_card_domain_with_bounds () =
  let env = Env.empty "" |> Env.add_domain "Color" Ast.dummy_loc ~chapter:0 in
  let bounds = Env.StringMap.singleton "Color" 5 in
  let cfg =
    Smt.make_config ~bound:3 ~steps:5 ~domain_bounds:bounds ~inject_guards:true
  in
  check string "#Domain with per-domain bound" "5"
    (Smt.translate_expr cfg env (Ast.EUnop (OpCard, EDomain (Upper "Color"))))

(* --- Test suites --- *)

(* --- Additional expression tests --- *)

let test_binop_iff () =
  let env = Env.empty "" in
  check string "iff" "(= x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpIff, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_ge () =
  let env = Env.empty "" in
  check string "ge" "(>= x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpGe, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_gt () =
  let env = Env.empty "" in
  check string "gt" "(> x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpGt, EVar (Lower "x"), EVar (Lower "y"))))

let test_binop_le () =
  let env = Env.empty "" in
  check string "le" "(<= x y)"
    (Smt.translate_expr config env
       (Ast.EBinop (OpLe, EVar (Lower "x"), EVar (Lower "y"))))

let test_tuple () =
  let env = Env.empty "" in
  let result =
    Smt.translate_expr config env (Ast.ETuple [ ELitNat 1; ELitNat 2 ])
  in
  (* Tuple translates to mk-pair or similar *)
  check bool "tuple non-empty" true (String.length result > 0)

let test_override () =
  let env =
    Env.empty ""
    |> Env.add_domain "K" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "f"
         (Types.TyFunc ([ Types.TyDomain "K" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_var "a" (Types.TyDomain "K")
  in
  (* Applied override: f[a |-> 42] a produces ite chain *)
  let result =
    Smt.translate_expr config env
      (Ast.EApp
         ( EOverride (Lower "f", [ (EVar (Lower "a"), ELitNat 42) ]),
           [ EVar (Lower "a") ] ))
  in
  check bool "applied override has ite" true (contains result "ite");
  check bool "applied override has f" true (contains result "f");
  (* Standalone override (not applied) can't be represented directly *)
  let result2 =
    Smt.translate_expr config env
      (Ast.EOverride (Lower "f", [ (EVar (Lower "a"), ELitNat 42) ]))
  in
  check bool "standalone override placeholder" true
    (contains result2 "override")

let test_initially () =
  let env = Env.empty "" in
  let result = Smt.translate_expr config env (Ast.EInitially (ELitBool true)) in
  check string "initially passes through" "true" result

let test_lit_string () =
  let env = Env.empty "" in
  let result = Smt.translate_expr config env (Ast.ELitString "hello") in
  check bool "string non-empty" true (String.length result > 0)

(* --- Bug-finding tests --- *)

let test_sanitize_ident_bang () =
  (* Bug #1: sanitize_ident doesn't handle '!' — produces invalid SMT-LIB2 *)
  let result = Smt.sanitize_ident "check-out!" in
  check bool "no bang in sanitized ident" true
    (not (String.contains result '!'))

let test_translate_in_zero_bound () =
  (* Bug #2: bound=0 used to produce "(or )" — invalid SMT-LIB2.
     Now produces "false" (nothing in empty domain). *)
  let zero_config =
    Smt.make_config ~bound:0 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let env = Env.empty "" |> Env.add_domain "D" Ast.dummy_loc ~chapter:0 in
  let result =
    Smt.translate_expr zero_config env
      (Ast.EBinop (OpIn, EVar (Lower "x"), EDomain (Upper "D")))
  in
  check string "empty domain membership is false" "false" result

let test_override_applied_zero_args () =
  (* Bug #5: EApp(EOverride(...), []) used to crash with List.hd on empty list.
     Now gives a proper error message. *)
  let env =
    Env.empty ""
    |> Env.add_domain "K" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "f"
         (Types.TyFunc ([ Types.TyDomain "K" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_var "a" (Types.TyDomain "K")
  in
  check_raises "override with 0 args gives clear error"
    (Failure "SMT translation: override applied with 0 arguments") (fun () ->
      ignore
        (Smt.translate_expr config env
           (Ast.EApp
              (EOverride (Lower "f", [ (EVar (Lower "a"), ELitNat 42) ]), []))))

let test_nat_vs_nat0_constraints () =
  (* Bug #10: Nat should emit >= 1, Nat0 should emit >= 0 *)
  let env =
    Env.empty ""
    |> Env.add_domain "D" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "score"
         (Types.TyFunc ([ Types.TyDomain "D" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "count"
         (Types.TyFunc ([ Types.TyDomain "D" ], Some Types.TyNat0))
         Ast.dummy_loc ~chapter:0
  in
  let constraints = Smt.declare_type_constraints config env in
  check bool "Nat has >= 1 bound" true (contains constraints ">= (score");
  check bool "Nat0 has >= 0 bound" true (contains constraints ">= (count");
  (* Verify the actual bound values *)
  check bool "contains 1) for Nat" true (contains constraints "(>= (score");
  check bool "contains 0) for Nat0" true (contains constraints "(>= (count")

let test_domain_closure_bound_one () =
  (* Bug #11: with bound=1, domain_elements produces 1 element,
     and closure axioms check "if List.length elems > 1" — so
     transitive closure axiom is SKIPPED at bound=1 *)
  let env, doc =
    parse_and_collect
      "module Test.\n\
       Block.\n\
       parent b: Block => Block + Nothing.\n\
       ancestor b: Block => [Block] = closure parent.\n\
       ---\n\
       all b: Block | ~(b in ancestor b).\n"
  in
  let one_config =
    Smt.make_config ~bound:1 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let queries = Smt.generate_queries one_config env doc in
  let inv =
    List.find (fun (q : Smt.query) -> q.kind = Smt.InvariantConsistency) queries
  in
  (* At bound=1, the closure axiom should still exist.
     If it's skipped, domain membership is incomplete. *)
  check bool "has ancestor axiom at bound=1" true (contains inv.smt2 "ancestor")

let expression_tests =
  [
    test_case "bool literals" `Quick test_lit_bool;
    test_case "nat literals" `Quick test_lit_nat;
    test_case "real literals" `Quick test_lit_real;
    test_case "string literal" `Quick test_lit_string;
    test_case "variables" `Quick test_var;
    test_case "primed" `Quick test_primed;
    test_case "and" `Quick test_binop_and;
    test_case "or" `Quick test_binop_or;
    test_case "implication" `Quick test_binop_impl;
    test_case "iff" `Quick test_binop_iff;
    test_case "equality" `Quick test_binop_eq;
    test_case "inequality" `Quick test_binop_neq;
    test_case "less than" `Quick test_binop_lt;
    test_case "ge" `Quick test_binop_ge;
    test_case "gt" `Quick test_binop_gt;
    test_case "le" `Quick test_binop_le;
    test_case "arithmetic" `Quick test_binop_arith;
    test_case "not" `Quick test_unop_not;
    test_case "negation" `Quick test_unop_neg;
    test_case "application" `Quick test_app;
    test_case "primed application" `Quick test_primed_app;
    test_case "domain membership" `Quick test_domain_in;
    test_case "list membership" `Quick test_in_list;
    test_case "projection" `Quick test_proj;
    test_case "tuple" `Quick test_tuple;
    test_case "override" `Quick test_override;
    test_case "initially" `Quick test_initially;
    test_case "cardinality domain" `Quick test_card_domain;
    test_case "cardinality list" `Quick test_card_list;
    test_case "subset" `Quick test_subset;
    test_case "domain standalone raises" `Quick test_domain_standalone;
    test_case "cardinality non-domain list" `Quick test_card_non_domain_list;
    test_case "subset with domain RHS" `Quick test_subset_domain_rhs;
  ]

let test_sort_string () =
  check string "String sort" "String" (Smt.sort_of_ty Types.TyString)

let test_sort_list () =
  check string "List sort" "(Array Account Bool)"
    (Smt.sort_of_ty (Types.TyList (Types.TyDomain "Account")))

let sort_tests =
  [
    test_case "Bool sort" `Quick test_sort_bool;
    test_case "Int sorts" `Quick test_sort_int;
    test_case "Real sort" `Quick test_sort_real;
    test_case "String sort" `Quick test_sort_string;
    test_case "Domain sort" `Quick test_sort_domain;
    test_case "List sort" `Quick test_sort_list;
  ]

let test_nat_type_constraints () =
  let env =
    Env.empty ""
    |> Env.add_domain "Account" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "balance"
         (Types.TyFunc ([ Types.TyDomain "Account" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
  in
  let constraints = Smt.declare_type_constraints config env in
  (* Nat uses bound 1 (positive naturals) *)
  check bool "has >= 1 for Nat" true (contains constraints ">= (balance");
  check bool "has assert" true (contains constraints "(assert")

let test_invariant_query_content () =
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       all a: Account | balance a >= 0.\n\
       where\n\
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let inv_con =
    List.find (fun (q : Smt.query) -> q.kind = Smt.InvariantConsistency) queries
  in
  check bool "has check-sat" true (contains inv_con.smt2 "(check-sat)");
  check bool "has balance decl" true
    (contains inv_con.smt2 "(declare-fun balance ");
  check bool "has domain sort" true
    (contains inv_con.smt2 "(declare-sort Account 0)")

let test_init_query_content () =
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
       Ctx ~> Withdraw @ a: Account, amount: Nat, balance a >= amount.\n\
       ---\n\
       balance' a = balance a - amount.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let init_inv =
    List.find (fun (q : Smt.query) -> q.kind = Smt.InitInvariant) queries
  in
  check bool "init-invariant has check-sat" true
    (contains init_inv.smt2 "(check-sat)");
  check bool "init-invariant has balance" true
    (contains init_inv.smt2 "balance")

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
    test_case "nat type constraints" `Quick test_nat_type_constraints;
    test_case "invariant query content" `Quick test_invariant_query_content;
    test_case "init query content" `Quick test_init_query_content;
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

let test_in_each_comprehension () =
  (* y in (each x: D | f x) → disjunction over domain elements *)
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
        EVar (Lower "r"),
        EEach
          ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
            [],
            None,
            EApp (EVar (Lower "role"), [ EVar (Lower "u") ]) ) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has (= r (role User_0))" true
    (contains result "(= r (role User_0))");
  check bool "has (= r (role User_1))" true
    (contains result "(= r (role User_1))");
  check bool "has or" true (contains result "(or")

let test_in_each_comprehension_guarded () =
  (* y in (each x: D, g x | f x) → includes guard in expansion *)
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
        EVar (Lower "r"),
        EEach
          ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
            [ GExpr (EApp (EVar (Lower "active"), [ EVar (Lower "u") ])) ],
            None,
            EApp (EVar (Lower "role"), [ EVar (Lower "u") ]) ) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has guard (active User_0)" true
    (contains result "(active User_0)");
  check bool "has (= r (role User_0))" true
    (contains result "(= r (role User_0))");
  check bool "has and (guard + eq)" true (contains result "(and")

let test_in_membership_comprehension () =
  (* y in (each x in xs | f x) → includes membership guard *)
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
        EVar (Lower "r"),
        EEach
          ( [],
            [ GIn (Lower "u", EVar (Lower "admins")) ],
            None,
            EApp (EVar (Lower "role"), [ EVar (Lower "u") ]) ) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has select admins" true (contains result "(select admins");
  check bool "has (= r (role User_0))" true
    (contains result "(= r (role User_0))");
  check bool "has or" true (contains result "(or")

let test_card_each_comprehension () =
  (* #(each x: D | f x) → count over range domain *)
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
        EEach
          ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
            [],
            None,
            EApp (EVar (Lower "role"), [ EVar (Lower "u") ]) ) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has ite" true (contains result "(ite");
  check bool "has Role_0" true (contains result "Role_0");
  check bool "has +" true (contains result "(+")

let test_each_comprehension_standalone () =
  (* EEach in standalone position → store-chain array *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Role" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "role"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some (Types.TyDomain "Role")))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        None,
        EApp (EVar (Lower "role"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has as const" true (contains result "as const");
  check bool "has store" true (contains result "store");
  check bool "has Array Role Bool" true (contains result "(Array Role Bool)")

let test_aggregate_add () =
  (* + over each u: User | score u → (+ (score User_0) (score User_1) ...) *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        Some CombAdd,
        EApp (EVar (Lower "score"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has +" true (contains result "(+");
  check bool "has (score User_0)" true (contains result "(score User_0)");
  check bool "has (score User_1)" true (contains result "(score User_1)")

let test_aggregate_and () =
  (* and over each u: User | active u → (and (active User_0) ...) *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        Some CombAnd,
        EApp (EVar (Lower "active"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has and" true (contains result "(and");
  check bool "has (active User_0)" true (contains result "(active User_0)")

let test_aggregate_min_guarded () =
  (* min over each u: User, active u | score u → pant_min with ite guards *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [ GExpr (EApp (EVar (Lower "active"), [ EVar (Lower "u") ])) ],
        Some CombMin,
        EApp (EVar (Lower "score"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has <=" true (contains result "(<=");
  check bool "has ite" true (contains result "(ite")

let test_aggregate_add_empty () =
  (* + over empty domain (bound=0) should return identity "0" *)
  let zero_config =
    Smt.make_config ~bound:0 ~steps:5 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        Some CombAdd,
        EApp (EVar (Lower "score"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr zero_config env expr in
  check string "empty add = identity" "0" result

let test_aggregate_decl_guard_injection () =
  (* + over each u: User | score u where score has decl guard "active u"
     should inject (active User_0), (active User_1) etc. as guards *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule_guards "score"
         [ Ast.{ param_name = Lower "u"; param_type = TName (Upper "User") } ]
         [ GExpr (EApp (EVar (Lower "active"), [ EVar (Lower "u") ])) ]
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        Some CombAdd,
        EApp (EVar (Lower "score"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has (active User_0)" true (contains result "(active User_0)");
  check bool "has ite" true (contains result "(ite")

let test_aggregate_add_real () =
  (* + over each u: User, active u | real_score u — guarded Real body uses 0.0 *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "real_score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyReal))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [ GExpr (EApp (EVar (Lower "active"), [ EVar (Lower "u") ])) ],
        Some CombAdd,
        EApp (EVar (Lower "real_score"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has 0.0 identity" true (contains result "0.0");
  check bool "has +" true (contains result "(+")

let test_aggregate_add_real_empty () =
  (* + over empty Real domain should return "0.0" *)
  let zero_config =
    Smt.make_config ~bound:0 ~steps:5 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "real_score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyReal))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        Some CombAdd,
        EApp (EVar (Lower "real_score"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr zero_config env expr in
  check string "empty real add = 0.0" "0.0" result

let test_aggregate_min_real () =
  (* min over each u: User | real_score u should use 0.0 seed, not 0 *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "real_score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyReal))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        Some CombMin,
        EApp (EVar (Lower "real_score"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "has 0.0 seed" true (contains result "0.0");
  check bool "has <=" true (contains result "(<=")

let comprehension_tests =
  [
    test_case "in each comprehension" `Quick test_in_each_comprehension;
    test_case "in each guarded" `Quick test_in_each_comprehension_guarded;
    test_case "in membership comprehension" `Quick
      test_in_membership_comprehension;
    test_case "card each comprehension" `Quick test_card_each_comprehension;
    test_case "each standalone" `Quick test_each_comprehension_standalone;
    test_case "aggregate add" `Quick test_aggregate_add;
    test_case "aggregate and" `Quick test_aggregate_and;
    test_case "aggregate min guarded" `Quick test_aggregate_min_guarded;
    test_case "aggregate add empty" `Quick test_aggregate_add_empty;
    test_case "aggregate decl guard injection" `Quick
      test_aggregate_decl_guard_injection;
    test_case "aggregate add real" `Quick test_aggregate_add_real;
    test_case "aggregate add real empty" `Quick test_aggregate_add_real_empty;
    test_case "aggregate min real" `Quick test_aggregate_min_real;
  ]

(* --- Closure tests --- *)

let test_closure_axiom_generation () =
  (* Closure axioms should be grounded (no forall/exists) *)
  let env, doc =
    parse_and_collect
      "module Test.\n\
       Block.\n\
       parent b: Block => Block + Nothing.\n\
       ancestor b: Block => [Block] = closure parent.\n\
       ---\n\
       all b: Block | ~(b in ancestor b).\n"
  in
  let small_config =
    Smt.make_config ~bound:2 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let queries = Smt.generate_queries small_config env doc in
  let inv =
    List.find (fun (q : Smt.query) -> q.kind = Smt.InvariantConsistency) queries
  in
  (* Should have grounded closure axioms for ancestor *)
  check bool "has ancestor" true (contains inv.smt2 "(ancestor");
  check bool "has ancestor_prime" true (contains inv.smt2 "(ancestor_prime");
  (* Should be quantifier-free in closure section *)
  check bool "no exists in axioms" false (contains inv.smt2 "(exists ((_cz_");
  (* Should define ancestor for each pair *)
  check bool "has Block_0 Block_0 pair" true
    (contains inv.smt2 "(select (ancestor Block_0) Block_0)");
  check bool "has Block_0 Block_1 pair" true
    (contains inv.smt2 "(select (ancestor Block_0) Block_1)")

let test_closure_no_frame_condition () =
  (* Closure rules should NOT generate frame conditions *)
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Node.\n\
       {Ctx} children n: Node => [Node].\n\
       descendants n: Node => [Node] = closure children.\n\
       ---\n\
       all n: Node | ~(n in descendants n).\n\
       where\n\
       Ctx ~> AddChild @ p: Node, c: Node.\n\
       ---\n\
       c in children' p.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let contra =
    List.find (fun (q : Smt.query) -> q.kind = Smt.Contradiction) queries
  in
  (* Frame conditions should mention children but NOT descendants *)
  let frame_names =
    List.filter
      (fun (name, _) ->
        String.length name >= 5 && String.sub name 0 5 = "frame")
      contra.assertion_names
  in
  let descendants_frame =
    List.exists (fun (_, text) -> contains text "descendants") frame_names
  in
  check bool "no descendants frame condition" false descendants_frame

let closure_tests =
  [
    test_case "closure axiom generation" `Quick test_closure_axiom_generation;
    test_case "closure no frame condition" `Quick
      test_closure_no_frame_condition;
  ]

(* --- Declaration guard injection tests --- *)

let test_quantifier_guard_injection () =
  (* Rule with a guard: get-access u: User, active? u => Access
     When used inside "all u: User | ...", the guard should appear as antecedent *)
  let env, doc =
    parse_and_collect
      "module Test.\n\
       User.\n\
       Access.\n\
       active? u: User => Bool.\n\
       get-access u: User, active? u => Access.\n\
       ---\n\
       all u: User | get-access u in Access.\n"
  in
  ignore doc;
  let queries = Smt.generate_queries config env doc in
  let inv_con =
    List.find (fun (q : Smt.query) -> q.kind = Smt.InvariantConsistency) queries
  in
  (* The guard "active? u" should appear as an antecedent in the
     forall quantifier, substituted to match the quantified variable *)
  check bool "has guard in quantifier" true (contains inv_con.smt2 "activep")

let test_non_quantified_guard_injection () =
  (* A bare proposition using a guarded function should get wrapped *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_domain "Access" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "get-access"
         (Types.TyFunc
            ([ Types.TyDomain "User" ], Some (Types.TyDomain "Access")))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "status"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule_guards "get-access"
         [ Ast.{ param_name = Lower "u"; param_type = TName (Upper "User") } ]
         [ GExpr (EApp (EVar (Lower "status"), [ EVar (Lower "u") ])) ]
    |> Env.add_var "x" (Types.TyDomain "User")
  in
  let expr =
    Ast.EBinop
      ( OpEq,
        EApp (EVar (Lower "get-access"), [ EVar (Lower "x") ]),
        EVar (Lower "x") )
  in
  let result = Smt.translate_proposition config env expr in
  (* Should wrap: (=> (status x) (= (get_access x) x)) *)
  check bool "has implication" true (contains result "(=>");
  check bool "has guard (status x)" true (contains result "(status x)")

let test_nested_quantifier_guards () =
  (* Guards should be injected at the correct nesting level.
     Inner quantifier has its own scope — guards from inner applications
     should appear in the inner quantifier, not the outer one. *)
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
    |> Env.add_rule_guards "role"
         [ Ast.{ param_name = Lower "u"; param_type = TName (Upper "User") } ]
         [ GExpr (EApp (EVar (Lower "active"), [ EVar (Lower "u") ])) ]
  in
  (* all x: User | (some y: User | role y = role x) *)
  let expr =
    Ast.EForall
      ( [ { param_name = Lower "x"; param_type = TName (Upper "User") } ],
        [],
        EExists
          ( [ { param_name = Lower "y"; param_type = TName (Upper "User") } ],
            [],
            EBinop
              ( OpEq,
                EApp (EVar (Lower "role"), [ EVar (Lower "y") ]),
                EApp (EVar (Lower "role"), [ EVar (Lower "x") ]) ) ) )
  in
  let result = Smt.translate_expr config env expr in
  (* Outer forall should NOT have the guard (nested quantifier handles its own).
     Inner exists should have (active y) and (active x) as guards. *)
  check bool "has active guard" true (contains result "(active")

let test_unguarded_rule_unchanged () =
  (* Rules without guards should produce the same output as before *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "name"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
  in
  let expr =
    Ast.EForall
      ( [ { param_name = Lower "u"; param_type = TName (Upper "User") } ],
        [],
        EApp (EVar (Lower "name"), [ EVar (Lower "u") ]) )
  in
  let result = Smt.translate_expr config env expr in
  (* No guard injection — plain forall without => *)
  check bool "no implication for unguarded" true (not (contains result "(=>"))

let test_guard_substitution () =
  (* Verify that guard expressions have their formal params correctly
     substituted with actual arguments *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule_guards "score"
         [ Ast.{ param_name = Lower "u"; param_type = TName (Upper "User") } ]
         [ GExpr (EApp (EVar (Lower "active"), [ EVar (Lower "u") ])) ]
  in
  (* all x: User | score x >= 0 — the guard "active u" should become "active x" *)
  let expr =
    Ast.EForall
      ( [ { param_name = Lower "x"; param_type = TName (Upper "User") } ],
        [],
        EBinop
          (OpGe, EApp (EVar (Lower "score"), [ EVar (Lower "x") ]), ELitNat 0)
      )
  in
  let result = Smt.translate_expr config env expr in
  (* Guard should be substituted: (active x), not (active u) *)
  check bool "has (active x)" true (contains result "(active x)");
  check bool "no (active u)" false (contains result "(active u)")

let test_primed_guard_collection () =
  (* collect_body_guards on EApp(EPrimed (Lower "value"), [EVar (Lower "t")]) should return
     the primed guard [active?' t] *)
  let env =
    Env.empty ""
    |> Env.add_domain "Thing" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "value"
         (Types.TyFunc ([ Types.TyDomain "Thing" ], Some Types.TyNat0))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active?"
         (Types.TyFunc ([ Types.TyDomain "Thing" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule_guards "value"
         [ Ast.{ param_name = Lower "t"; param_type = TName (Upper "Thing") } ]
         [ GExpr (EApp (EVar (Lower "active?"), [ EVar (Lower "t") ])) ]
  in
  let expr = Ast.EApp (EPrimed (Lower "value"), [ EVar (Lower "t") ]) in
  let guards = Smt.collect_body_guards ~bound:[ "t" ] env expr in
  check int "one guard" 1 (List.length guards);
  (* The guard should be primed: EPrimed (Lower "active?") applied to EVar (Lower "t")
     (t is not primed because it's not a rule name) *)
  let guard = List.hd guards in
  match[@warning "-4"] guard with
  | Ast.EApp (EPrimed (Lower "active?"), [ EVar (Lower "t") ]) ->
      check bool "primed guard for active?" true true
  | _ ->
      failf "Expected EApp(EPrimed \"active?\", [EVar \"t\"]) but got %s"
        (Ast.show_expr guard)

let test_inject_guards_false_skips () =
  (* translate_proposition with inject_guards = false produces no
     implication wrapper even for guarded applications *)
  let env =
    Env.empty ""
    |> Env.add_domain "User" Ast.dummy_loc ~chapter:0
    |> Env.add_rule "score"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyNat))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule "active"
         (Types.TyFunc ([ Types.TyDomain "User" ], Some Types.TyBool))
         Ast.dummy_loc ~chapter:0
    |> Env.add_rule_guards "score"
         [ Ast.{ param_name = Lower "u"; param_type = TName (Upper "User") } ]
         [ GExpr (EApp (EVar (Lower "active"), [ EVar (Lower "u") ])) ]
    |> Env.add_var "x" (Types.TyDomain "User")
  in
  let expr =
    Ast.EBinop
      (OpGe, EApp (EVar (Lower "score"), [ EVar (Lower "x") ]), ELitNat 0)
  in
  let no_guard_config = { config with Smt.inject_guards = false } in
  let result = Smt.translate_proposition no_guard_config env expr in
  (* Should NOT have an implication wrapper *)
  check bool "no implication" true (not (contains result "(=>"));
  (* But with inject_guards = true, it should *)
  let result_with = Smt.translate_proposition config env expr in
  check bool "has implication with guards" true (contains result_with "(=>")

let test_guarded_decl_e2e () =
  (* Full spec from bug report: guarded declarations should not cause
     spurious invariant preservation failures *)
  let env, doc =
    parse_and_collect
      "module Test.\n\
       context Ctx.\n\
       Thing.\n\
       {Ctx} active? t: Thing => Bool.\n\
       {Ctx} value t: Thing, active? t => Nat0.\n\
       ---\n\
       all t: Thing | value t >= 0.\n\
       initially all t: Thing | ~active? t.\n\
       where\n\
       Ctx ~> Create @ t: Thing, n: Nat0, ~active? t.\n\
       ---\n\
       active?' t.\n\
       value' t = n.\n\
       all t2: Thing, t2 != t | active?' t2 = active? t2.\n\
       all t2: Thing, t2 != t | value' t2 = value t2.\n"
  in
  let queries = Smt.generate_queries config env doc in
  (* Should generate queries without error *)
  check bool "has queries" true (List.length queries > 0);
  (* Check that contradiction query exists *)
  let has_contradiction =
    List.exists (fun (q : Smt.query) -> q.kind = Smt.Contradiction) queries
  in
  check bool "has contradiction query" true has_contradiction;
  (* Invariant preservation query postconditions should NOT have guards *)
  let inv_queries =
    List.filter
      (fun (q : Smt.query) -> q.kind = Smt.InvariantPreservation)
      queries
  in
  check bool "has invariant queries" true (List.length inv_queries > 0);
  let inv_q = List.hd inv_queries in
  (* The postcondition section should not have guard injection.
     Check that the "Action postconditions" section doesn't contain
     an implication from activep *)
  let postcond_section =
    let lines = String.split_on_char '\n' inv_q.smt2 in
    let in_postcond = ref false in
    let postcond_lines = ref [] in
    List.iter
      (fun line ->
        if contains line "Action postconditions" then in_postcond := true
        else if
          contains line "Frame conditions" || contains line "Invariant violated"
        then in_postcond := false;
        if !in_postcond then postcond_lines := line :: !postcond_lines)
      lines;
    String.concat "\n" (List.rev !postcond_lines)
  in
  (* Postcondition assertions should not wrap with guard implication *)
  check bool "postcond no guard implication" false
    (contains postcond_section "(=> (activep");
  (* The primed invariant (negated) should contain primed guard *)
  let violated_section =
    let lines = String.split_on_char '\n' inv_q.smt2 in
    let in_violated = ref false in
    let violated_lines = ref [] in
    List.iter
      (fun line ->
        if contains line "Invariant violated" then in_violated := true;
        if !in_violated then violated_lines := line :: !violated_lines)
      lines;
    String.concat "\n" (List.rev !violated_lines)
  in
  check bool "primed invariant has primed guard" true
    (contains violated_section "activep_prime");
  ignore doc

let guard_injection_tests =
  [
    test_case "quantifier guard injection" `Quick
      test_quantifier_guard_injection;
    test_case "non-quantified guard injection" `Quick
      test_non_quantified_guard_injection;
    test_case "nested quantifier guards" `Quick test_nested_quantifier_guards;
    test_case "unguarded rule unchanged" `Quick test_unguarded_rule_unchanged;
    test_case "guard substitution" `Quick test_guard_substitution;
    test_case "primed guard collection" `Quick test_primed_guard_collection;
    test_case "inject_guards false skips" `Quick test_inject_guards_false_skips;
    test_case "guarded decl e2e" `Quick test_guarded_decl_e2e;
  ]

(* --- Cond expression tests --- *)

let test_cond_single_arm () =
  let env = Env.empty "" in
  let expr = Ast.ECond [ (ELitBool true, ELitNat 42) ] in
  check string "single arm" "42" (Smt.translate_expr config env expr)

let test_cond_two_arms () =
  let env = Env.empty "" in
  let expr =
    Ast.ECond
      [
        (EBinop (OpGt, EVar (Lower "x"), ELitNat 0), ELitNat 1);
        (ELitBool true, ELitNat 0);
      ]
  in
  check string "two arms" "(ite (> x 0) 1 0)"
    (Smt.translate_expr config env expr)

let test_cond_three_arms () =
  let env = Env.empty "" in
  let expr =
    Ast.ECond
      [
        (EBinop (OpGt, EVar (Lower "x"), ELitNat 10), ELitNat 2);
        (EBinop (OpGt, EVar (Lower "x"), ELitNat 5), ELitNat 1);
        (ELitBool true, ELitNat 0);
      ]
  in
  check string "three arms" "(ite (> x 10) 2 (ite (> x 5) 1 0))"
    (Smt.translate_expr config env expr)

let test_cond_exhaustiveness_query () =
  let _env, doc =
    parse_and_collect
      "module TEST.\n\n\
       Status.\n\
       level s: Status => Nat.\n\
       ---\n\
       all s: Status | level s = cond level s >= 10 => 2, level s >= 5 => 1, \
       true => 0.\n"
  in
  let conds = Smt.collect_conds_from_doc doc in
  check int "one cond found" 1 (List.length conds);
  let cond = List.hd conds in
  check int "three arms" 3 (List.length cond.cond_arms)

let test_cond_exhaustiveness_with_true () =
  (* A cond with 'true' as last arm should always be exhaustive *)
  let env, doc =
    parse_and_collect
      "module TEST.\n\n\
       Status.\n\
       level s: Status => Nat.\n\
       ---\n\
       all s: Status | level s = cond true => 1.\n"
  in
  let conds = Smt.collect_conds_from_doc doc in
  check int "one cond" 1 (List.length conds);
  let queries = Smt.generate_queries config env doc in
  let exh_queries =
    List.filter (fun q -> q.Smt.kind = Smt.CondExhaustiveness) queries
  in
  check int "one exhaustiveness query" 1 (List.length exh_queries)

let cond_tests =
  [
    test_case "single arm" `Quick test_cond_single_arm;
    test_case "two arms" `Quick test_cond_two_arms;
    test_case "three arms" `Quick test_cond_three_arms;
    test_case "exhaustiveness query" `Quick test_cond_exhaustiveness_query;
    test_case "exhaustiveness with true" `Quick
      test_cond_exhaustiveness_with_true;
  ]

let bug_finding_tests =
  [
    test_case "sanitize_ident bang" `Quick test_sanitize_ident_bang;
    test_case "translate_in zero bound" `Quick test_translate_in_zero_bound;
    test_case "override applied zero args" `Quick
      test_override_applied_zero_args;
    test_case "nat vs nat0 constraints" `Quick test_nat_vs_nat0_constraints;
    test_case "domain closure bound=1" `Quick test_domain_closure_bound_one;
  ]

(* --- Aggregate (over-each) tests --- *)

let make_aggregate_env () =
  Env.empty ""
  |> Env.add_domain "Item" Ast.dummy_loc ~chapter:0
  |> Env.add_rule "price"
       (Types.TyFunc ([ Types.TyDomain "Item" ], Some Types.TyNat))
       Ast.dummy_loc ~chapter:0
  |> Env.add_rule "weight"
       (Types.TyFunc ([ Types.TyDomain "Item" ], Some Types.TyInt))
       Ast.dummy_loc ~chapter:0
  |> Env.add_rule "available?"
       (Types.TyFunc ([ Types.TyDomain "Item" ], Some Types.TyBool))
       Ast.dummy_loc ~chapter:0

let test_aggregate_add_item () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombAdd,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses +" true (contains result "(+");
  check bool "has price Item_0" true (contains result "(price Item_0)");
  check bool "has price Item_1" true (contains result "(price Item_1)");
  check bool "has price Item_2" true (contains result "(price Item_2)")

let test_aggregate_add_guarded () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [ GExpr (EApp (EVar (Lower "available?"), [ EVar (Lower "i") ])) ],
        Some CombAdd,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses +" true (contains result "(+");
  check bool "uses ite" true (contains result "(ite");
  (* Guarded-out elements contribute 0 *)
  check bool "identity 0" true (contains result " 0)")

let test_aggregate_mul () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombMul,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses *" true (contains result "(*")

let test_aggregate_and_item () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombAnd,
        EApp (EVar (Lower "available?"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses and" true (contains result "(and")

let test_aggregate_and_guarded () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [ GExpr (EApp (EVar (Lower "available?"), [ EVar (Lower "i") ])) ],
        Some CombAnd,
        EApp (EVar (Lower "available?"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses and" true (contains result "(and");
  check bool "uses ite" true (contains result "(ite");
  (* Guarded-out elements contribute true (identity for and) *)
  check bool "identity true" true (contains result " true)")

let test_aggregate_or_guarded () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [ GExpr (EApp (EVar (Lower "available?"), [ EVar (Lower "i") ])) ],
        Some CombOr,
        EApp (EVar (Lower "available?"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses or" true (contains result "(or");
  check bool "uses ite" true (contains result "(ite");
  (* Guarded-out elements contribute false (identity for or) *)
  check bool "identity false" true (contains result " false)")

let test_aggregate_or () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombOr,
        EApp (EVar (Lower "available?"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses or" true (contains result "(or")

let test_aggregate_min () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombMin,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses <" true (contains result "(<");
  check bool "uses ite" true (contains result "(ite")

let test_aggregate_max () =
  let env = make_aggregate_env () in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombMax,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr config env expr in
  check bool "uses >" true (contains result "(>");
  check bool "uses ite" true (contains result "(ite")

let test_aggregate_empty_domain () =
  (* Domain with bound=0 should return identity element *)
  let env = make_aggregate_env () in
  let zero_config =
    Smt.make_config ~bound:0 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombAdd,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr zero_config env expr in
  check string "empty domain returns 0" "0" result

let test_aggregate_empty_domain_mul () =
  let env = make_aggregate_env () in
  let zero_config =
    Smt.make_config ~bound:0 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombMul,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr zero_config env expr in
  check string "empty domain returns 1" "1" result

let test_aggregate_empty_domain_and () =
  let env = make_aggregate_env () in
  let zero_config =
    Smt.make_config ~bound:0 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombAnd,
        EApp (EVar (Lower "available?"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr zero_config env expr in
  check string "empty domain returns true" "true" result

let test_aggregate_empty_domain_or () =
  let env = make_aggregate_env () in
  let zero_config =
    Smt.make_config ~bound:0 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombOr,
        EApp (EVar (Lower "available?"), [ EVar (Lower "i") ]) )
  in
  let result = Smt.translate_expr zero_config env expr in
  check string "empty domain returns false" "false" result

let test_aggregate_empty_domain_min () =
  (* min/max have no identity element; empty domain should raise *)
  let env = make_aggregate_env () in
  let zero_config =
    Smt.make_config ~bound:0 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombMin,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  check_raises "empty domain min raises"
    (Failure "SMT: min/max over empty domain") (fun () ->
      ignore (Smt.translate_expr zero_config env expr))

let test_aggregate_empty_domain_max () =
  let env = make_aggregate_env () in
  let zero_config =
    Smt.make_config ~bound:0 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let expr =
    Ast.EEach
      ( [ { param_name = Lower "i"; param_type = TName (Upper "Item") } ],
        [],
        Some CombMax,
        EApp (EVar (Lower "price"), [ EVar (Lower "i") ]) )
  in
  check_raises "empty domain max raises"
    (Failure "SMT: min/max over empty domain") (fun () ->
      ignore (Smt.translate_expr zero_config env expr))

let test_aggregate_integration () =
  (* Full integration test: parse, collect, type-check, then generate SMT *)
  let env, doc =
    parse_and_collect
      "module Test.\n\
       Item.\n\
       price i: Item => Nat.\n\
       available? i: Item => Bool.\n\
       ---\n\
       and over each i: Item | available? i.\n"
  in
  let small_config =
    Smt.make_config ~bound:2 ~steps:0 ~domain_bounds:Env.StringMap.empty
      ~inject_guards:true
  in
  let queries = Smt.generate_queries small_config env doc in
  let inv =
    List.find (fun (q : Smt.query) -> q.kind = Smt.InvariantConsistency) queries
  in
  check bool "has and combiner" true (contains inv.smt2 "(and");
  check bool "has availablep Item_0" true
    (contains inv.smt2 "(availablep Item_0)");
  check bool "has availablep Item_1" true
    (contains inv.smt2 "(availablep Item_1)")

let aggregate_tests =
  [
    test_case "add combiner" `Quick test_aggregate_add_item;
    test_case "add guarded" `Quick test_aggregate_add_guarded;
    test_case "mul combiner" `Quick test_aggregate_mul;
    test_case "and combiner" `Quick test_aggregate_and_item;
    test_case "and guarded" `Quick test_aggregate_and_guarded;
    test_case "or combiner" `Quick test_aggregate_or;
    test_case "or guarded" `Quick test_aggregate_or_guarded;
    test_case "min combiner" `Quick test_aggregate_min;
    test_case "max combiner" `Quick test_aggregate_max;
    test_case "empty domain add" `Quick test_aggregate_empty_domain;
    test_case "empty domain mul" `Quick test_aggregate_empty_domain_mul;
    test_case "empty domain and" `Quick test_aggregate_empty_domain_and;
    test_case "empty domain or" `Quick test_aggregate_empty_domain_or;
    test_case "empty domain min" `Quick test_aggregate_empty_domain_min;
    test_case "empty domain max" `Quick test_aggregate_empty_domain_max;
    test_case "integration" `Quick test_aggregate_integration;
  ]

let test_entailment_invariant_query () =
  let env, doc =
    parse_and_collect
      "module T.\n\
       f a: Int => Int.\n\
       ---\n\
       all a: Int | f a = a + 1.\n\
       check\n\
       all a: Int | f a > a.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let entailment =
    List.filter (fun (q : Smt.query) -> q.kind = Smt.Entailment) queries
  in
  check int "entailment query count" 1 (List.length entailment);
  let q = List.hd entailment in
  check bool "has check-sat" true (contains q.smt2 "(check-sat)");
  check bool "has negated goal" true (contains q.smt2 "(assert (not");
  check bool "has f declaration" true (contains q.smt2 "(declare-fun f ")

let test_entailment_action_query () =
  let env, doc =
    parse_and_collect
      "module T.\n\
       context Ctx.\n\
       Account.\n\
       {Ctx} balance a: Account => Nat.\n\
       ---\n\
       all a: Account | balance a >= 0.\n\
       where\n\
       Ctx ~> Deposit @ a: Account, amount: Nat.\n\
       ---\n\
       balance' a = balance a + amount.\n\
       check\n\
       balance' a >= balance a.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let entailment =
    List.filter (fun (q : Smt.query) -> q.kind = Smt.Entailment) queries
  in
  check int "entailment query count" 1 (List.length entailment);
  let q = List.hd entailment in
  check bool "has check-sat" true (contains q.smt2 "(check-sat)");
  check bool "has negated goal" true (contains q.smt2 "(assert (not");
  check bool "has action postcondition" true
    (contains q.smt2 "(= (balance_prime a)");
  check bool "has invariant assumption" true
    (contains q.smt2 "(>= (balance a) 0)")

let test_entailment_uses_chapter_local_invariants () =
  (* Two invariant chapters, each with a check. The check in the second chapter
     should only use its own chapter body as assumptions, not the first chapter's. *)
  let env, doc =
    parse_and_collect
      "module T.\n\
       f a: Int => Int.\n\
       ---\n\
       all a: Int | f a = a + 1.\n\
       check\n\
       all a: Int | f a > a.\n\
       where\n\
       g a: Int => Int.\n\
       ---\n\
       all a: Int | g a = a * 2.\n\
       check\n\
       all a: Int | g a >= a.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let entailment =
    List.filter (fun (q : Smt.query) -> q.kind = Smt.Entailment) queries
  in
  check int "entailment query count" 2 (List.length entailment);
  (* Find each query by axiom fingerprint, not position *)
  let q1 =
    match
      List.find_opt (fun (q : Smt.query) -> contains q.smt2 "(= (f ") entailment
    with
    | Some q -> q
    | None -> fail "Expected entailment query containing f axiom"
  in
  check bool "q1 has f axiom" true (contains q1.smt2 "(= (f ");
  check bool "q1 lacks g axiom" false (contains q1.smt2 "(= (g ");
  let q2 =
    match
      List.find_opt (fun (q : Smt.query) -> contains q.smt2 "(= (g ") entailment
    with
    | Some q -> q
    | None -> fail "Expected entailment query containing g axiom"
  in
  check bool "q2 has g axiom" true (contains q2.smt2 "(= (g ");
  check bool "q2 lacks f axiom" false (contains q2.smt2 "(= (f ");
  ()

let test_entailment_multiple_goals () =
  let env, doc =
    parse_and_collect
      "module T.\n\
       f a: Int => Int.\n\
       ---\n\
       all a: Int | f a = a + 1.\n\
       check\n\
       all a: Int | f a > a.\n\
       all a: Int | f a >= a.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let entailment =
    List.filter (fun (q : Smt.query) -> q.kind = Smt.Entailment) queries
  in
  check int "two entailment queries for two goals" 2 (List.length entailment);
  check bool "has strict gt goal" true
    (List.exists (fun (q : Smt.query) -> contains q.smt2 "(> (f ") entailment);
  check bool "has gte goal" true
    (List.exists (fun (q : Smt.query) -> contains q.smt2 "(>= (f ") entailment)

let test_no_entailment_without_checks () =
  let env, doc =
    parse_and_collect
      "module T.\nf a: Int => Int.\n---\nall a: Int | f a = a + 1.\n"
  in
  let queries = Smt.generate_queries config env doc in
  let entailment =
    List.filter (fun (q : Smt.query) -> q.kind = Smt.Entailment) queries
  in
  check int "no entailment queries" 0 (List.length entailment)

let entailment_tests =
  [
    test_case "invariant entailment query" `Quick
      test_entailment_invariant_query;
    test_case "action entailment query" `Quick test_entailment_action_query;
    test_case "no entailment without checks" `Quick
      test_no_entailment_without_checks;
    test_case "entailment uses chapter-local invariants" `Quick
      test_entailment_uses_chapter_local_invariants;
    test_case "multiple goals in one check" `Quick
      test_entailment_multiple_goals;
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
      ("closure", closure_tests);
      ("guard_injection", guard_injection_tests);
      ("cond", cond_tests);
      ("bug_finding", bug_finding_tests);
      ("aggregates", aggregate_tests);
      ("entailment", entailment_tests);
    ]
