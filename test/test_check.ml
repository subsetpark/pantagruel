(** Type checker tests *)

open Alcotest
open Pantagruel

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let check_ok str =
  let doc = parse str in
  match
    Collect.collect_all
      ~base_env:(Env.empty (Option.value ~default:"" doc.module_name))
      doc
  with
  | Error e -> fail (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Ok () -> ()
      | Error e -> fail (Check.show_type_error e))

let check_fails str =
  let doc = parse str in
  match
    Collect.collect_all
      ~base_env:(Env.empty (Option.value ~default:"" doc.module_name))
      doc
  with
  | Error _ -> () (* Collection error is also a failure *)
  | Ok env -> (
      match Check.check_document env doc with
      | Ok () -> fail "Expected type error"
      | Error _ -> ())

(** Check that a document type-checks given a pre-built base_env *)
let check_ok_with_env base_env str =
  let doc = parse str in
  match Collect.collect_all ~base_env doc with
  | Error e -> fail (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Ok () -> ()
      | Error e -> fail (Check.show_type_error e))

(** Check that a document fails to type-check given a pre-built base_env *)
let check_fails_with_env base_env str =
  let doc = parse str in
  match Collect.collect_all ~base_env doc with
  | Error _ -> ()
  | Ok env -> (
      match Check.check_document env doc with
      | Ok () -> fail "Expected error"
      | Error _ -> ())

(** Check that a document type-checks but emits warnings *)
let check_warns str =
  let doc = parse str in
  match
    Collect.collect_all
      ~base_env:(Env.empty (Option.value ~default:"" doc.module_name))
      doc
  with
  | Error e -> fail (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Ok () -> if Check.get_warnings () = [] then fail "Expected warning"
      | Error e -> fail (Check.show_type_error e))

(** Check that a document fails with a specific error type *)
let check_error_with_env base_env str pred =
  let doc = parse str in
  match Collect.collect_all ~base_env doc with
  | Error _ -> () (* Collection error also OK *)
  | Ok env -> (
      match Check.check_document env doc with
      | Ok () -> fail "Expected error"
      | Error e ->
          if not (pred e) then
            fail
              (Printf.sprintf "Wrong error type: %s" (Check.show_type_error e)))

(** Build a simulated import environment. Constructs an env as if modules had
    been imported via add_import. *)
let make_import_env imports =
  let base = Env.empty "" in
  List.fold_left
    (fun env (mod_name, types, terms) ->
      let mod_env =
        let e = Env.empty mod_name in
        let e =
          List.fold_left
            (fun e (name, kind) ->
              let entry =
                {
                  Env.kind;
                  loc = Ast.dummy_loc;
                  module_origin = None;
                  decl_chapter = 0;
                }
              in
              { e with Env.types = Env.StringMap.add name entry e.Env.types })
            e types
        in
        List.fold_left
          (fun e (name, kind) ->
            let entry =
              {
                Env.kind;
                loc = Ast.dummy_loc;
                module_origin = None;
                decl_chapter = 0;
              }
            in
            { e with Env.terms = Env.StringMap.add name entry e.Env.terms })
          e terms
      in
      Env.add_import env mod_env mod_name)
    base imports

(* Valid documents *)
let test_minimal () = check_ok "module TEST.\n\nFoo.\n---\n"
let test_bool_literal () = check_ok "module TEST.\n\nFoo.\n---\ntrue.\nfalse.\n"

let test_simple_all () =
  check_ok "module TEST.\n\nUser.\n---\nall u: User | true.\n"

let test_rule_application () =
  check_ok
    "module TEST.\n\nUser.\nf u: User => Bool.\n---\nall u: User | f u.\n"

let test_equality () =
  check_ok "module TEST.\n\nUser.\n---\nall u: User | u = u.\n"

let test_numeric_literal () =
  check_ok "module TEST.\n\nFoo.\n---\n1 = 1.\n0 = 0.\n"

let test_numeric_comparison () = check_ok "module TEST.\n\nFoo.\n---\n1 < 2.\n"

let test_membership () =
  check_ok "module TEST.\n\nUser.\n---\nall u: User | u in User.\n"

let test_cardinality () = check_ok "module TEST.\n\nUser.\n---\n#User >= 0.\n"

let test_nullary_rule () =
  check_ok
    "module TEST.\n\nUser.\nnobody => User.\n---\nall u: User | u = nobody.\n"

let test_action_primed () =
  check_ok
    {|module TEST.

User.
Document.
owner d: Document => User.
nobody => User.
~> Check out | u: User, d: Document.
---
owner' d = u.
|}

let test_primed_nullary_rule () =
  check_ok
    {|module TEST.
context State.

{State} x => Int.
~> Step.
---
x' = x + 1.
|}

let test_type_alias () = check_ok "module TEST.\n\nPoint = Nat * Nat.\n---\n"

let test_tuple () =
  check_ok
    {|module TEST.

Point = Nat * Nat.
origin => Point.
---
origin = (0, 0).
|}

let test_projection () =
  check_ok {|module TEST.

Point = Nat * Nat.
p => Point.
---
p.1 >= 0.
|}

let test_rule_guard () =
  check_ok
    {|module TEST.

Account.
balance a: Account => Nat.
~> Withdraw | a: Account, amount: Nat, balance a >= amount.
---
balance' a = balance a - amount.
|}

let test_membership_binding () =
  check_ok
    {|module TEST.

Item.
price i: Item => Nat.
items => [Item].
---
all i in items | price i > 0.
some i in items | price i < 100.
|}

let test_rule_params_visible_in_body () =
  (* Rule parameters should be visible in chapter body *)
  check_ok
    {|module TEST.

Item.
price i: Item => Nat.
---
i in Item.
price i >= 0.
|}

let test_quantifier_scoping () =
  (* Quantifier-bound variable visible in body *)
  check_ok
    {|module TEST.

Item.
price i: Item => Nat.
---
all x: Item | price x > 0.
|}

let test_existential () =
  check_ok
    {|module TEST.

User.
active u: User => Bool.
---
some u: User | active u.
|}

let test_nested_quantifiers () =
  check_ok
    {|module TEST.

Item.
related i: Item, j: Item => Bool.
---
all i: Item | some j: Item | related i j.
|}

(* Invalid documents *)
let test_unbound_variable () =
  check_fails
    "module TEST.\n\nFoo.\n---\nall u: User | u = u.\n" (* User not declared *)

let test_type_mismatch () =
  check_fails {|module TEST.

User.
n => Nat.
---
all u: User | u = n.
|}

let test_arity_mismatch () =
  check_fails
    {|module TEST.

User.
f u: User, v: User => Bool.
---
all u: User | f u.
|}

let test_not_a_function () =
  check_fails {|module TEST.

User.
---
all u: User | u u.
|}

let test_prime_outside_action () =
  check_fails
    {|module TEST.

User.
f u: User => User.
---
all u: User | f' u = u.
|}

let test_prime_on_variable () =
  check_fails
    {|module TEST.

User.
~> Do thing | u: User.
---
all u: User | u' = u.
|}

let test_multiple_actions () =
  check_fails
    {|module TEST.

User.
~> Action 1 | u: User.
~> Action 2 | u: User.
---
|}

let test_recursive_alias () =
  check_fails "module TEST.\n\nList = Nat * List.\n---\n"

let test_guard_not_boolean () =
  (* Guard expression must be boolean *)
  check_fails
    {|module TEST.

Account.
balance a: Account => Nat.
~> Withdraw | a: Account, amount: Nat, balance a.
---
|}

let test_membership_binding_not_list () =
  (* x in xs requires xs to be a list *)
  check_fails {|module TEST.

Item.
item => Item.
---
all i in item | true.
|}

let test_quantifier_var_not_visible_outside () =
  (* Variable bound in quantifier not visible outside *)
  check_fails
    {|module TEST.

Item.
price i: Item => Nat.
---
all myvar: Item | true.
price myvar > 0.
|}

let test_guard_uses_params () =
  (* Guard can reference action parameters *)
  check_ok
    {|module TEST.

Account.
balance a: Account => Nat.
~> Withdraw | a: Account, amount: Nat, amount > 0.
---
|}

let test_shadowing_same_type () =
  (* Rebinding with same type is OK *)
  check_ok
    {|module TEST.

Item.
price i: Item => Nat.
---
all i: Item | price i > 0.
|}

let test_shadowing_different_type () =
  (* Rebinding with different type is a warning *)
  check_warns
    {|module TEST.

Item.
price i: Item => Nat.
---
all i: Nat | i > 0.
|}

let test_shadowing_in_membership () =
  (* Membership binding also checked for shadowing *)
  check_warns
    {|module TEST.

Item.
price i: Item => Nat.
items => [Nat].
---
all i in items | i > 0.
|}

(* --- Env unit tests for add_import --- *)

let test_env_single_import () =
  (* One import providing "Widget" domain → appears in flat maps *)
  let env =
    make_import_env
      [
        ( "LIB",
          [ ("Widget", Env.KDomain) ],
          [
            ( "make-widget",
              Env.KRule (Types.TyFunc ([], Some (Types.TyDomain "Widget"))) );
          ] );
      ]
  in
  check bool "type in flat map" true
    (Option.is_some (Env.lookup_type "Widget" env));
  check bool "term in flat map" true
    (Option.is_some (Env.lookup_term "make-widget" env));
  check bool "not ambiguous type" true
    (Option.is_none (Env.ambiguous_type_modules "Widget" env));
  check bool "not ambiguous term" true
    (Option.is_none (Env.ambiguous_term_modules "make-widget" env))

let test_env_ambiguous_import () =
  (* Two imports both providing "Item" → NOT in flat maps, IS ambiguous *)
  let env =
    make_import_env
      [
        ( "MOD_A",
          [ ("Item", Env.KDomain) ],
          [ ("process", Env.KRule (Types.TyFunc ([], Some Types.TyBool))) ] );
        ( "MOD_B",
          [ ("Item", Env.KDomain) ],
          [ ("process", Env.KRule (Types.TyFunc ([], Some Types.TyNat))) ] );
      ]
  in
  check bool "ambiguous type not in flat map" true
    (Option.is_none (Env.lookup_type "Item" env));
  check bool "ambiguous term not in flat map" true
    (Option.is_none (Env.lookup_term "process" env));
  check bool "ambiguous type detected" true
    (Option.is_some (Env.ambiguous_type_modules "Item" env));
  check bool "ambiguous term detected" true
    (Option.is_some (Env.ambiguous_term_modules "process" env))

let test_env_qualified_lookup () =
  (* Qualified lookup finds the right module even when ambiguous *)
  let env =
    make_import_env
      [
        ("MOD_A", [ ("Item", Env.KDomain) ], []);
        ("MOD_B", [ ("Item", Env.KDomain) ], []);
      ]
  in
  check bool "qualified type MOD_A" true
    (Option.is_some (Env.lookup_qualified_type "MOD_A" "Item" env));
  check bool "qualified type MOD_B" true
    (Option.is_some (Env.lookup_qualified_type "MOD_B" "Item" env));
  check bool "qualified type wrong module" true
    (Option.is_none (Env.lookup_qualified_type "MOD_C" "Item" env));
  check bool "qualified type wrong name" true
    (Option.is_none (Env.lookup_qualified_type "MOD_A" "Other" env))

let test_env_disjoint_imports () =
  (* Two imports with no overlap → both unambiguous *)
  let env =
    make_import_env
      [
        ("MOD_A", [ ("Foo", Env.KDomain) ], []);
        ("MOD_B", [ ("Bar", Env.KDomain) ], []);
      ]
  in
  check bool "Foo unambiguous" true (Option.is_some (Env.lookup_type "Foo" env));
  check bool "Bar unambiguous" true (Option.is_some (Env.lookup_type "Bar" env));
  check bool "Foo not ambiguous" true
    (Option.is_none (Env.ambiguous_type_modules "Foo" env));
  check bool "Bar not ambiguous" true
    (Option.is_none (Env.ambiguous_type_modules "Bar" env))

(* --- Integration tests: imports with collect + check --- *)

let test_unambiguous_import_type () =
  (* Imported domain usable as type in declarations *)
  let env = make_import_env [ ("LIB", [ ("Widget", Env.KDomain) ], []) ] in
  check_ok_with_env env {|
f w: Widget => Bool.
---
all w: Widget | f w.
|}

let test_unambiguous_import_term () =
  (* Imported rule usable in expressions *)
  let env =
    make_import_env
      [
        ( "LIB",
          [ ("Widget", Env.KDomain) ],
          [
            ( "make-widget",
              Env.KRule (Types.TyFunc ([], Some (Types.TyDomain "Widget"))) );
          ] );
      ]
  in
  check_ok_with_env env {|
Widget.
---
make-widget in Widget.
|}

let test_ambiguous_import_type_fails () =
  (* Ambiguous type used unqualified → error *)
  let env =
    make_import_env
      [
        ("MOD_A", [ ("Item", Env.KDomain) ], []);
        ("MOD_B", [ ("Item", Env.KDomain) ], []);
      ]
  in
  check_fails_with_env env {|
f i: Item => Bool.
---
|}

let test_ambiguous_import_term_fails () =
  (* Ambiguous term used unqualified → AmbiguousName error *)
  let env =
    make_import_env
      [
        ( "MOD_A",
          [ ("Foo", Env.KDomain) ],
          [ ("process", Env.KRule (Types.TyFunc ([], Some Types.TyBool))) ] );
        ( "MOD_B",
          [ ("Foo", Env.KDomain) ],
          [ ("process", Env.KRule (Types.TyFunc ([], Some Types.TyBool))) ] );
      ]
  in
  check_error_with_env env {|
Foo.
---
process.
|} (function
    | Check.AmbiguousName ("process", _, _) -> true
    | _ -> false)

let test_qualified_type_resolves_ambiguity () =
  (* TQName resolves ambiguous type *)
  let env =
    make_import_env
      [
        ("MOD_A", [ ("Item", Env.KDomain) ], []);
        ("MOD_B", [ ("Item", Env.KDomain) ], []);
      ]
  in
  check_ok_with_env env {|
f i: MOD_A::Item => Bool.
---
|}

let test_qualified_term_resolves_ambiguity () =
  (* EQualified resolves ambiguous term *)
  let env =
    make_import_env
      [
        ( "MOD_A",
          [],
          [ ("count", Env.KRule (Types.TyFunc ([], Some Types.TyNat))) ] );
        ( "MOD_B",
          [],
          [ ("count", Env.KRule (Types.TyFunc ([], Some Types.TyNat))) ] );
      ]
  in
  check_ok_with_env env {|
Foo.
---
MOD_A::count >= 0.
|}

let test_qualified_domain_in_expr () =
  (* EQualified for a domain in expression position → [Domain] *)
  let env =
    make_import_env
      [
        ("MOD_A", [ ("Item", Env.KDomain) ], []);
        ("MOD_B", [ ("Item", Env.KDomain) ], []);
      ]
  in
  check_ok_with_env env {|
Foo.
---
#MOD_A::Item >= 0.
|}

let test_unbound_qualified_type () =
  (* Wrong module name in TQName → error *)
  let env = make_import_env [ ("LIB", [ ("Widget", Env.KDomain) ], []) ] in
  check_fails_with_env env {|
f w: WRONG::Widget => Bool.
---
|}

let test_unbound_qualified_term () =
  (* Wrong module name in EQualified → UnboundQualified error *)
  let env =
    make_import_env
      [
        ( "LIB",
          [],
          [ ("count", Env.KRule (Types.TyFunc ([], Some Types.TyNat))) ] );
      ]
  in
  check_error_with_env env {|
Foo.
---
WRONG::count >= 0.
|} (function
    | Check.UnboundQualified ("WRONG", "count", _) -> true
    | _ -> false)

let test_local_domain_shadows_import () =
  (* Local domain declaration shadows an imported one *)
  let env = make_import_env [ ("LIB", [ ("Item", Env.KDomain) ], []) ] in
  check_ok_with_env env {|
Item.
---
all i: Item | i in Item.
|}

let test_local_rule_shadows_import () =
  (* Local rule shadows an imported one *)
  let env =
    make_import_env
      [
        ( "LIB",
          [ ("Widget", Env.KDomain) ],
          [ ("count", Env.KRule (Types.TyFunc ([], Some Types.TyNat))) ] );
      ]
  in
  check_ok_with_env env {|
Widget.
count => Bool.
---
count.
|}

let test_local_alias_shadows_import () =
  (* Local alias shadows an imported domain *)
  let env = make_import_env [ ("LIB", [ ("Coord", Env.KDomain) ], []) ] in
  check_ok_with_env env
    {|
Coord = Nat * Nat.
origin => Coord.
---
origin = (0, 0).
|}

let test_local_duplicate_type_still_errors () =
  (* Two local declarations with same name still error *)
  check_fails {|
Foo.
Foo.
---
|}

let test_local_duplicate_proc_still_errors () =
  (* Two local rules with same name still error *)
  check_fails {|
Foo.
do-thing f: Foo => Bool.
do-thing f: Foo => Nat.
---
|}

(* --- Context tests --- *)

let test_context_priming_succeeds () =
  (* Priming a function that is in the active context succeeds *)
  check_ok
    {|module TEST.
context Banking.

Account.
{Banking} balance a: Account => Nat.
{Banking} owner a: Account => Account.
---
all a: Account | balance a >= 0.

where

Banking ~> Withdraw | a: Account.
---
balance' a = balance a - 1.
owner' a = a.
|}

let test_context_extracontextual_fails () =
  (* Priming a function outside the context is rejected *)
  check_fails
    {|module TEST.
context Banking.

Account.
{Banking} balance a: Account => Nat.
owner a: Account => Account.
---
all a: Account | balance a >= 0.

where

Banking ~> Withdraw | a: Account.
---
owner' a = a.
|}

let test_footprint_on_action () =
  (* {Ctx} before ~> is a parse error *)
  try
    let _ =
      parse
        {|module TEST.
context Banking.

Account.
{Banking} ~> Do thing | a: Account.
---
|}
    in
    fail "Expected parse error"
  with _ -> ()

let test_action_not_last_in_chapter () =
  (* Action must appear last in chapter head *)
  check_fails
    {|module TEST.

User.
~> Do thing | u: User.
helper u: User => Bool.
---
|}

let test_undefined_context () =
  (* Referencing a non-existent context *)
  check_fails
    {|module TEST.

Account.
NoSuchContext ~> Withdraw | a: Account.
---
|}

let test_no_context_priming_unrestricted () =
  (* Without a context, priming is unrestricted (existing behavior) *)
  check_ok
    {|module TEST.

Account.
balance a: Account => Nat.
owner a: Account => Account.
~> Withdraw | a: Account.
---
balance' a = balance a - 1.
owner' a = a.
|}

let test_qualified_type_in_alias () =
  (* TQName used in a type alias definition *)
  let env =
    make_import_env
      [
        ( "GEO",
          [
            ( "Coord",
              Env.KAlias (Types.TyProduct [ Types.TyReal; Types.TyReal ]) );
          ],
          [] );
      ]
  in
  check_ok_with_env env {|
Point = GEO::Coord.
p => Point.
---
p.1 >= 0.0.
|}

let test_qualified_type_in_proc_param () =
  (* TQName in rule parameter type *)
  let env = make_import_env [ ("LIB", [ ("Widget", Env.KDomain) ], []) ] in
  check_ok_with_env env {|
process w: LIB::Widget => Bool.
---
|}

let test_qualified_type_in_return () =
  (* TQName in rule return type *)
  let env = make_import_env [ ("LIB", [ ("Widget", Env.KDomain) ], []) ] in
  check_ok_with_env env
    {|
make-widget => LIB::Widget.
---
make-widget in LIB::Widget.
|}

let () =
  run "Check"
    [
      ( "valid",
        [
          test_case "minimal" `Quick test_minimal;
          test_case "bool literal" `Quick test_bool_literal;
          test_case "simple all" `Quick test_simple_all;
          test_case "rule application" `Quick test_rule_application;
          test_case "equality" `Quick test_equality;
          test_case "numeric literal" `Quick test_numeric_literal;
          test_case "numeric comparison" `Quick test_numeric_comparison;
          test_case "membership" `Quick test_membership;
          test_case "cardinality" `Quick test_cardinality;
          test_case "nullary rule" `Quick test_nullary_rule;
          test_case "action primed" `Quick test_action_primed;
          test_case "primed nullary rule" `Quick test_primed_nullary_rule;
          test_case "type alias" `Quick test_type_alias;
          test_case "tuple" `Quick test_tuple;
          test_case "projection" `Quick test_projection;
          test_case "rule guard" `Quick test_rule_guard;
          test_case "membership binding" `Quick test_membership_binding;
          test_case "rule params visible" `Quick
            test_rule_params_visible_in_body;
          test_case "quantifier scoping" `Quick test_quantifier_scoping;
          test_case "existential" `Quick test_existential;
          test_case "nested quantifiers" `Quick test_nested_quantifiers;
          test_case "guard uses params" `Quick test_guard_uses_params;
          test_case "shadowing same type" `Quick test_shadowing_same_type;
        ] );
      ( "invalid",
        [
          test_case "unbound variable" `Quick test_unbound_variable;
          test_case "type mismatch" `Quick test_type_mismatch;
          test_case "arity mismatch" `Quick test_arity_mismatch;
          test_case "not a function" `Quick test_not_a_function;
          test_case "prime outside action" `Quick test_prime_outside_action;
          test_case "prime on variable" `Quick test_prime_on_variable;
          test_case "multiple actions" `Quick test_multiple_actions;
          test_case "recursive alias" `Quick test_recursive_alias;
          test_case "guard not boolean" `Quick test_guard_not_boolean;
          test_case "membership not list" `Quick
            test_membership_binding_not_list;
          test_case "quantifier var escapes" `Quick
            test_quantifier_var_not_visible_outside;
          test_case "shadowing different type" `Quick
            test_shadowing_different_type;
          test_case "shadowing in membership" `Quick
            test_shadowing_in_membership;
          test_case "local duplicate type" `Quick
            test_local_duplicate_type_still_errors;
          test_case "local duplicate rule" `Quick
            test_local_duplicate_proc_still_errors;
        ] );
      ( "env import",
        [
          test_case "single import" `Quick test_env_single_import;
          test_case "ambiguous import" `Quick test_env_ambiguous_import;
          test_case "qualified lookup" `Quick test_env_qualified_lookup;
          test_case "disjoint imports" `Quick test_env_disjoint_imports;
        ] );
      ( "import resolution",
        [
          test_case "unambiguous type" `Quick test_unambiguous_import_type;
          test_case "unambiguous term" `Quick test_unambiguous_import_term;
          test_case "ambiguous type fails" `Quick
            test_ambiguous_import_type_fails;
          test_case "ambiguous term fails" `Quick
            test_ambiguous_import_term_fails;
          test_case "qualified type resolves" `Quick
            test_qualified_type_resolves_ambiguity;
          test_case "qualified term resolves" `Quick
            test_qualified_term_resolves_ambiguity;
          test_case "qualified domain in expr" `Quick
            test_qualified_domain_in_expr;
          test_case "unbound qualified type" `Quick test_unbound_qualified_type;
          test_case "unbound qualified term" `Quick test_unbound_qualified_term;
          test_case "local domain shadows" `Quick
            test_local_domain_shadows_import;
          test_case "local rule shadows" `Quick test_local_rule_shadows_import;
          test_case "local alias shadows" `Quick test_local_alias_shadows_import;
          test_case "qualified in alias" `Quick test_qualified_type_in_alias;
          test_case "qualified in param" `Quick
            test_qualified_type_in_proc_param;
          test_case "qualified in return" `Quick test_qualified_type_in_return;
        ] );
      ( "context",
        [
          test_case "priming in context" `Quick test_context_priming_succeeds;
          test_case "extracontextual fails" `Quick
            test_context_extracontextual_fails;
          test_case "footprint on action" `Quick test_footprint_on_action;
          test_case "action not last" `Quick test_action_not_last_in_chapter;
          test_case "undefined context" `Quick test_undefined_context;
          test_case "no context unrestricted" `Quick
            test_no_context_priming_unrestricted;
        ] );
    ]
