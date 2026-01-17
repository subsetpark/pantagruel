(** Type checker tests *)

open Alcotest
open Pantagruel

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let check_ok str =
  let doc = parse str in
  match Collect.collect_all doc with
  | Error e -> fail (Collect.show_collect_error e)
  | Ok env ->
      match Check.check_document env doc with
      | Ok () -> ()
      | Error e -> fail (Check.show_type_error e)

let check_fails str =
  let doc = parse str in
  match Collect.collect_all doc with
  | Error _ -> ()  (* Collection error is also a failure *)
  | Ok env ->
      match Check.check_document env doc with
      | Ok () -> fail "Expected type error"
      | Error _ -> ()

(* Valid documents *)
let test_minimal () =
  check_ok "module TEST.\n\nFoo.\n---\n"

let test_bool_literal () =
  check_ok "module TEST.\n\nFoo.\n---\ntrue.\nfalse.\n"

let test_simple_forall () =
  check_ok "module TEST.\n\nUser.\n---\nforall u: User. true.\n"

let test_procedure_application () =
  check_ok "module TEST.\n\nUser.\nf u: User => Bool.\n---\nforall u: User. f u.\n"

let test_equality () =
  check_ok "module TEST.\n\nUser.\n---\nforall u: User. u = u.\n"

let test_numeric_literal () =
  check_ok "module TEST.\n\nFoo.\n---\n1 = 1.\n0 = 0.\n"

let test_numeric_comparison () =
  check_ok "module TEST.\n\nFoo.\n---\n1 < 2.\n"

let test_membership () =
  check_ok "module TEST.\n\nUser.\n---\nforall u: User. u in User.\n"

let test_cardinality () =
  check_ok "module TEST.\n\nUser.\n---\n#User >= 0.\n"

let test_nullary_procedure () =
  check_ok "module TEST.\n\nUser.\nnobody => User.\n---\nforall u: User. u = nobody.\n"

let test_void_procedure_primed () =
  check_ok {|module TEST.

User.
Document.
owner d: Document => User.
nobody => User.
check-out u: User, d: Document.
---
owner' d = u.
|}

let test_type_alias () =
  check_ok "module TEST.\n\nPoint = Nat * Nat.\n---\n"

let test_tuple () =
  check_ok {|module TEST.

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

(* Invalid documents *)
let test_unbound_variable () =
  check_fails "module TEST.\n\nFoo.\n---\nforall u: User. u = u.\n"  (* User not declared *)

let test_type_mismatch () =
  check_fails {|module TEST.

User.
n => Nat.
---
forall u: User. u = n.
|}

let test_arity_mismatch () =
  check_fails {|module TEST.

User.
f u: User, v: User => Bool.
---
forall u: User. f u.
|}

let test_not_a_function () =
  check_fails {|module TEST.

User.
---
forall u: User. u u.
|}

let test_prime_outside_void () =
  check_fails {|module TEST.

User.
f u: User => User.
---
forall u: User. f' u = u.
|}

let test_prime_on_variable () =
  check_fails {|module TEST.

User.
do-thing u: User.
---
forall u: User. u' = u.
|}

let test_multiple_void_procs () =
  check_fails {|module TEST.

User.
action1 u: User.
action2 u: User.
---
|}

let test_recursive_alias () =
  check_fails "module TEST.\n\nList = Nat * List.\n---\n"

let () =
  run "Check" [
    "valid", [
      test_case "minimal" `Quick test_minimal;
      test_case "bool literal" `Quick test_bool_literal;
      test_case "simple forall" `Quick test_simple_forall;
      test_case "procedure application" `Quick test_procedure_application;
      test_case "equality" `Quick test_equality;
      test_case "numeric literal" `Quick test_numeric_literal;
      test_case "numeric comparison" `Quick test_numeric_comparison;
      test_case "membership" `Quick test_membership;
      test_case "cardinality" `Quick test_cardinality;
      test_case "nullary procedure" `Quick test_nullary_procedure;
      test_case "void procedure primed" `Quick test_void_procedure_primed;
      test_case "type alias" `Quick test_type_alias;
      test_case "tuple" `Quick test_tuple;
      test_case "projection" `Quick test_projection;
    ];
    "invalid", [
      test_case "unbound variable" `Quick test_unbound_variable;
      test_case "type mismatch" `Quick test_type_mismatch;
      test_case "arity mismatch" `Quick test_arity_mismatch;
      test_case "not a function" `Quick test_not_a_function;
      test_case "prime outside void" `Quick test_prime_outside_void;
      test_case "prime on variable" `Quick test_prime_on_variable;
      test_case "multiple void procs" `Quick test_multiple_void_procs;
      test_case "recursive alias" `Quick test_recursive_alias;
    ];
  ]
