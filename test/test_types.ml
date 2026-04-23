(** Unit tests and property-based tests for Types module *)

open Alcotest
open Pantagruel

(* --- Testable for ty --- *)

let ty_testable =
  testable
    (fun fmt t -> Format.pp_print_string fmt (Types.format_ty t))
    Types.equal_ty

(* --- format_ty tests --- *)

let test_format_ty_bool () = check string "Bool" "Bool" (Types.format_ty TyBool)
let test_format_ty_nat () = check string "Nat" "Nat" (Types.format_ty TyNat)
let test_format_ty_nat0 () = check string "Nat0" "Nat0" (Types.format_ty TyNat0)
let test_format_ty_int () = check string "Int" "Int" (Types.format_ty TyInt)
let test_format_ty_real () = check string "Real" "Real" (Types.format_ty TyReal)

let test_format_ty_string () =
  check string "String" "String" (Types.format_ty TyString)

let test_format_ty_nothing () =
  check string "Nothing" "Nothing" (Types.format_ty TyNothing)

let test_format_ty_domain () =
  check string "Domain" "User" (Types.format_ty (TyDomain "User"))

let test_format_ty_list () =
  check string "List" "[Nat]" (Types.format_ty (TyList TyNat))

let test_format_ty_product () =
  check string "Product" "Nat * Bool"
    (Types.format_ty (TyProduct [ TyNat; TyBool ]))

let test_format_ty_sum () =
  check string "Sum" "Nat + Nothing"
    (Types.format_ty (TySum [ TyNat; TyNothing ]))

let test_format_ty_func () =
  check string "Func" "(Nat, Bool) -> Int"
    (Types.format_ty (TyFunc ([ TyNat; TyBool ], Some TyInt)));
  check string "Void func" "(Nat) -> Void"
    (Types.format_ty (TyFunc ([ TyNat ], None)))

(* --- is_numeric tests --- *)

let test_is_numeric () =
  check bool "Nat" true (Types.is_numeric TyNat);
  check bool "Nat0" true (Types.is_numeric TyNat0);
  check bool "Int" true (Types.is_numeric TyInt);
  check bool "Real" true (Types.is_numeric TyReal);
  check bool "Bool" false (Types.is_numeric TyBool);
  check bool "String" false (Types.is_numeric TyString);
  check bool "Domain" false (Types.is_numeric (TyDomain "X"));
  check bool "Nothing" false (Types.is_numeric TyNothing);
  check bool "List" false (Types.is_numeric (TyList TyNat))

(* --- is_subtype tests --- *)

let test_subtype_reflexive () =
  check bool "Bool" true (Types.is_subtype TyBool TyBool);
  check bool "Nat" true (Types.is_subtype TyNat TyNat);
  check bool "Domain" true (Types.is_subtype (TyDomain "X") (TyDomain "X"))

let test_subtype_numeric_chain () =
  check bool "Nat < Nat0" true (Types.is_subtype TyNat TyNat0);
  check bool "Nat < Int" true (Types.is_subtype TyNat TyInt);
  check bool "Nat < Real" true (Types.is_subtype TyNat TyReal);
  check bool "Nat0 < Int" true (Types.is_subtype TyNat0 TyInt);
  check bool "Nat0 < Real" true (Types.is_subtype TyNat0 TyReal);
  check bool "Int < Real" true (Types.is_subtype TyInt TyReal);
  (* not the other way *)
  check bool "Real !< Nat" false (Types.is_subtype TyReal TyNat);
  check bool "Int !< Nat" false (Types.is_subtype TyInt TyNat)

let test_subtype_nothing_bottom () =
  check bool "Nothing < Bool" true (Types.is_subtype TyNothing TyBool);
  check bool "Nothing < Nat" true (Types.is_subtype TyNothing TyNat);
  check bool "Nothing < Domain" true (Types.is_subtype TyNothing (TyDomain "X"));
  check bool "Nothing < List" true (Types.is_subtype TyNothing (TyList TyNat));
  check bool "Bool !< Nothing" false (Types.is_subtype TyBool TyNothing)

let test_subtype_covariant () =
  check bool "List covariant" true
    (Types.is_subtype (TyList TyNat) (TyList TyInt));
  check bool "Product covariant" true
    (Types.is_subtype (TyProduct [ TyNat; TyNat ]) (TyProduct [ TyInt; TyInt ]));
  check bool "Sum covariant" true
    (Types.is_subtype (TySum [ TyNat; TyNat ]) (TySum [ TyInt; TyInt ]))

let test_subtype_non_subtype () =
  check bool "Bool !< Nat" false (Types.is_subtype TyBool TyNat);
  check bool "String !< Bool" false (Types.is_subtype TyString TyBool);
  check bool "Domain !< Nat" false (Types.is_subtype (TyDomain "X") TyNat);
  check bool "Product length mismatch" false
    (Types.is_subtype (TyProduct [ TyNat ]) (TyProduct [ TyNat; TyNat ]))

(* --- lub_numeric tests --- *)

let test_lub_numeric () =
  check (option ty_testable) "Nat, Nat" (Some TyNat)
    (Types.lub_numeric TyNat TyNat);
  check (option ty_testable) "Nat, Nat0" (Some TyNat0)
    (Types.lub_numeric TyNat TyNat0);
  check (option ty_testable) "Nat, Int" (Some TyInt)
    (Types.lub_numeric TyNat TyInt);
  check (option ty_testable) "Int, Real" (Some TyReal)
    (Types.lub_numeric TyInt TyReal);
  check (option ty_testable) "Nat, Real" (Some TyReal)
    (Types.lub_numeric TyNat TyReal);
  check (option ty_testable) "Bool, Nat" None (Types.lub_numeric TyBool TyNat)

(* --- join tests --- *)

let test_join_equal () =
  check
    (result ty_testable Alcotest.reject)
    "same type" (Ok TyBool) (Types.join TyBool TyBool)

let test_join_numeric () =
  check
    (result ty_testable Alcotest.reject)
    "Nat+Nat0" (Ok TyNat0) (Types.join TyNat TyNat0);
  check
    (result ty_testable Alcotest.reject)
    "Int+Real" (Ok TyReal) (Types.join TyInt TyReal)

let test_join_list () =
  check
    (result ty_testable Alcotest.reject)
    "List join" (Ok (TyList TyNat0))
    (Types.join (TyList TyNat) (TyList TyNat0))

let test_join_product () =
  check
    (result ty_testable Alcotest.reject)
    "Product join"
    (Ok (TyProduct [ TyInt; TyNat0 ]))
    (Types.join (TyProduct [ TyNat; TyNat ]) (TyProduct [ TyInt; TyNat0 ]))

let test_join_sum () =
  check
    (result ty_testable Alcotest.reject)
    "Sum join"
    (Ok (TySum [ TyInt; TyReal ]))
    (Types.join (TySum [ TyNat; TyInt ]) (TySum [ TyInt; TyReal ]))

let test_join_mismatch () =
  check bool "Bool vs Nat" true (Result.is_error (Types.join TyBool TyNat));
  check bool "String vs Int" true (Result.is_error (Types.join TyString TyInt))

(* --- compatible tests --- *)

let test_compatible () =
  check bool "same" true (Types.compatible TyBool TyBool);
  check bool "numeric" true (Types.compatible TyNat TyInt);
  check bool "incompatible" false (Types.compatible TyBool TyNat);
  check bool "incompatible 2" false (Types.compatible TyString (TyDomain "X"))

(* --- Property-based tests --- *)

let gen_ty = Test_util.gen_ty
let print_ty = Test_util.print_ty
let print_pair (a, b) = Printf.sprintf "(%s, %s)" (print_ty a) (print_ty b)

let print_triple (a, b, c) =
  Printf.sprintf "(%s, %s, %s)" (print_ty a) (print_ty b) (print_ty c)

let test_subtype_reflexivity =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"subtype reflexivity" ~count:200 ~print:print_ty
       gen_ty (fun t -> Types.is_subtype t t))

let test_subtype_transitivity =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"subtype transitivity" ~count:500
       ~print:print_triple (QCheck2.Gen.triple gen_ty gen_ty gen_ty)
       (fun (a, b, c) ->
         if Types.is_subtype a b && Types.is_subtype b c then
           Types.is_subtype a c
         else true (* vacuously true *)))

let test_join_commutativity =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"join commutativity" ~count:500 ~print:print_pair
       (QCheck2.Gen.pair gen_ty gen_ty) (fun (a, b) ->
         match[@warning "-4"] (Types.join a b, Types.join b a) with
         | Ok x, Ok y -> Types.equal_ty x y
         | Error _, Error _ -> true (* both fail = commutative *)
         | _ -> false))

let test_join_reflexivity =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"join reflexivity" ~count:200 ~print:print_ty
       gen_ty (fun t -> Types.join t t = Ok t))

let test_compatible_symmetry =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"compatible symmetry" ~count:500 ~print:print_pair
       (QCheck2.Gen.pair gen_ty gen_ty) (fun (a, b) ->
         Types.compatible a b = Types.compatible b a))

let test_subtype_implies_compatible =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"subtype implies compatible" ~count:500
       ~print:print_pair (QCheck2.Gen.pair gen_ty gen_ty) (fun (a, b) ->
         if Types.is_subtype a b then Types.compatible a b else true))

let test_join_upper_bound =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"join is upper bound" ~count:500 ~print:print_pair
       (QCheck2.Gen.pair gen_ty gen_ty) (fun (a, b) ->
         match Types.join a b with
         | Ok lub -> Types.is_subtype a lub && Types.is_subtype b lub
         | Error _ -> true))

let () =
  run "Types"
    [
      ( "format_ty",
        [
          test_case "Bool" `Quick test_format_ty_bool;
          test_case "Nat" `Quick test_format_ty_nat;
          test_case "Nat0" `Quick test_format_ty_nat0;
          test_case "Int" `Quick test_format_ty_int;
          test_case "Real" `Quick test_format_ty_real;
          test_case "String" `Quick test_format_ty_string;
          test_case "Nothing" `Quick test_format_ty_nothing;
          test_case "Domain" `Quick test_format_ty_domain;
          test_case "List" `Quick test_format_ty_list;
          test_case "Product" `Quick test_format_ty_product;
          test_case "Sum" `Quick test_format_ty_sum;
          test_case "Func" `Quick test_format_ty_func;
        ] );
      ("is_numeric", [ test_case "all variants" `Quick test_is_numeric ]);
      ( "is_subtype",
        [
          test_case "reflexive" `Quick test_subtype_reflexive;
          test_case "numeric chain" `Quick test_subtype_numeric_chain;
          test_case "Nothing bottom" `Quick test_subtype_nothing_bottom;
          test_case "covariant" `Quick test_subtype_covariant;
          test_case "non-subtype" `Quick test_subtype_non_subtype;
        ] );
      ("lub_numeric", [ test_case "pairs" `Quick test_lub_numeric ]);
      ( "join",
        [
          test_case "equal" `Quick test_join_equal;
          test_case "numeric" `Quick test_join_numeric;
          test_case "list" `Quick test_join_list;
          test_case "product" `Quick test_join_product;
          test_case "sum" `Quick test_join_sum;
          test_case "mismatch" `Quick test_join_mismatch;
        ] );
      ("compatible", [ test_case "pairs" `Quick test_compatible ]);
      ( "property",
        [
          test_subtype_reflexivity;
          test_subtype_transitivity;
          test_join_commutativity;
          test_join_reflexivity;
          test_compatible_symmetry;
          test_subtype_implies_compatible;
          test_join_upper_bound;
        ] );
    ]
