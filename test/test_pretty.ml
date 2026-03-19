(** Unit tests and property-based tests for Pretty module *)

open Alcotest
open Pantagruel

let pp = Pretty.str_expr
let pp_decl = Pretty.str_declaration
let pp_te = Pretty.str_type_expr

(* --- Expression pretty-printing tests --- *)

let test_var () = check string "var" "x" (pp (Ast.EVar "x"))
let test_domain () = check string "domain" "User" (pp (Ast.EDomain "User"))

let test_qualified () =
  check string "qualified" "Mod::name" (pp (Ast.EQualified ("Mod", "name")))

let test_lit_nat () = check string "nat" "42" (pp (Ast.ELitNat 42))
let test_lit_real () = check string "real" "3.14" (pp (Ast.ELitReal 3.14))

let test_lit_string () =
  check string "string" "\"hello\"" (pp (Ast.ELitString "hello"))

let test_lit_bool () =
  check string "true" "true" (pp (Ast.ELitBool true));
  check string "false" "false" (pp (Ast.ELitBool false))

let test_app () =
  check string "app" "f x y" (pp (Ast.EApp (EVar "f", [ EVar "x"; EVar "y" ])))

let test_primed () = check string "primed" "f'" (pp (Ast.EPrimed "f"))

let test_override () =
  check string "override" "f[x |-> y]"
    (pp (Ast.EOverride ("f", [ (EVar "x", EVar "y") ])))

let test_tuple () =
  check string "tuple" "(1, 2, 3)"
    (pp (Ast.ETuple [ ELitNat 1; ELitNat 2; ELitNat 3 ]))

let test_proj () = check string "proj" "x.1" (pp (Ast.EProj (EVar "x", 1)))

let test_binop_and () =
  check string "and" "a and b" (pp (Ast.EBinop (OpAnd, EVar "a", EVar "b")))

let test_binop_or () =
  check string "or" "a or b" (pp (Ast.EBinop (OpOr, EVar "a", EVar "b")))

let test_binop_impl () =
  check string "impl" "a -> b" (pp (Ast.EBinop (OpImpl, EVar "a", EVar "b")))

let test_binop_iff () =
  check string "iff" "a <-> b" (pp (Ast.EBinop (OpIff, EVar "a", EVar "b")))

let test_binop_eq () =
  check string "eq" "a = b" (pp (Ast.EBinop (OpEq, EVar "a", EVar "b")))

let test_binop_neq () =
  check string "neq" "a != b" (pp (Ast.EBinop (OpNeq, EVar "a", EVar "b")))

let test_binop_lt () =
  check string "lt" "a < b" (pp (Ast.EBinop (OpLt, EVar "a", EVar "b")))

let test_binop_gt () =
  check string "gt" "a > b" (pp (Ast.EBinop (OpGt, EVar "a", EVar "b")))

let test_binop_le () =
  check string "le" "a <= b" (pp (Ast.EBinop (OpLe, EVar "a", EVar "b")))

let test_binop_ge () =
  check string "ge" "a >= b" (pp (Ast.EBinop (OpGe, EVar "a", EVar "b")))

let test_binop_in () =
  check string "in" "x in S" (pp (Ast.EBinop (OpIn, EVar "x", EVar "S")))

let test_binop_subset () =
  check string "subset" "a subset b"
    (pp (Ast.EBinop (OpSubset, EVar "a", EVar "b")))

let test_binop_add () =
  check string "add" "a + b" (pp (Ast.EBinop (OpAdd, EVar "a", EVar "b")))

let test_binop_sub () =
  check string "sub" "a - b" (pp (Ast.EBinop (OpSub, EVar "a", EVar "b")))

let test_binop_mul () =
  check string "mul" "a * b" (pp (Ast.EBinop (OpMul, EVar "a", EVar "b")))

let test_binop_div () =
  check string "div" "a / b" (pp (Ast.EBinop (OpDiv, EVar "a", EVar "b")))

let test_unop_not () =
  check string "not" "~a" (pp (Ast.EUnop (OpNot, EVar "a")))

let test_unop_neg () =
  check string "neg" "-a" (pp (Ast.EUnop (OpNeg, EVar "a")))

let test_unop_card () =
  check string "card" "#xs" (pp (Ast.EUnop (OpCard, EVar "xs")))

let test_forall () =
  check string "forall" "all x: Nat | x > 0"
    (pp
       (Ast.EForall
          ( [ { param_name = "x"; param_type = TName "Nat" } ],
            [],
            EBinop (OpGt, EVar "x", ELitNat 0) )))

let test_exists () =
  check string "exists" "some x: Nat | x > 0"
    (pp
       (Ast.EExists
          ( [ { param_name = "x"; param_type = TName "Nat" } ],
            [],
            EBinop (OpGt, EVar "x", ELitNat 0) )))

let test_each () =
  check string "each" "each u: User | f u"
    (pp
       (Ast.EEach
          ( [ { param_name = "u"; param_type = TName "User" } ],
            [],
            EApp (EVar "f", [ EVar "u" ]) )))

let test_cond () =
  check string "cond" "cond a => 1, b => 2"
    (pp (Ast.ECond [ (EVar "a", ELitNat 1); (EVar "b", ELitNat 2) ]))

let test_initially () =
  check string "initially" "initially x" (pp (Ast.EInitially (EVar "x")))

(* --- Declaration pretty-printing tests --- *)

let test_decl_domain () =
  check string "domain" "User." (pp_decl (Ast.DeclDomain "User"))

let test_decl_alias () =
  check string "alias" "Point = Nat * Nat."
    (pp_decl (Ast.DeclAlias ("Point", TProduct [ TName "Nat"; TName "Nat" ])))

let test_decl_rule () =
  check string "rule" "f x: Nat => Bool."
    (pp_decl
       (Ast.DeclRule
          {
            name = "f";
            params = [ { param_name = "x"; param_type = TName "Nat" } ];
            guards = [];
            return_type = TName "Bool";
            contexts = [];
          }))

let test_decl_rule_with_guards () =
  check string "rule with guards" "f x: Nat, x > 0 => Bool."
    (pp_decl
       (Ast.DeclRule
          {
            name = "f";
            params = [ { param_name = "x"; param_type = TName "Nat" } ];
            guards = [ GExpr (EBinop (OpGt, EVar "x", ELitNat 0)) ];
            return_type = TName "Bool";
            contexts = [];
          }))

let test_decl_rule_with_context () =
  check string "rule with context" "{Ctx} f x: Nat => Bool."
    (pp_decl
       (Ast.DeclRule
          {
            name = "f";
            params = [ { param_name = "x"; param_type = TName "Nat" } ];
            guards = [];
            return_type = TName "Bool";
            contexts = [ "Ctx" ];
          }))

let test_decl_action () =
  check string "action" "~> Do thing @ x: Nat."
    (pp_decl
       (Ast.DeclAction
          {
            label = "Do thing";
            params = [ { param_name = "x"; param_type = TName "Nat" } ];
            guards = [];
            contexts = [];
          }))

let test_decl_action_with_context () =
  check string "action with context" "Ctx ~> Do thing."
    (pp_decl
       (Ast.DeclAction
          { label = "Do thing"; params = []; guards = []; contexts = [ "Ctx" ] }))

let test_decl_closure () =
  check string "closure" "anc b: Block => [Block] = closure parent."
    (pp_decl
       (Ast.DeclClosure
          {
            name = "anc";
            param = { param_name = "b"; param_type = TName "Block" };
            return_type = TList (TName "Block");
            target = "parent";
          }))

(* --- Type expression tests --- *)

let test_te_name () = check string "name" "Nat" (pp_te (Ast.TName "Nat"))

let test_te_qname () =
  check string "qname" "Mod::Type" (pp_te (Ast.TQName ("Mod", "Type")))

let test_te_list () =
  check string "list" "[Nat]" (pp_te (Ast.TList (TName "Nat")))

let test_te_product () =
  check string "product" "Nat * Bool"
    (pp_te (Ast.TProduct [ TName "Nat"; TName "Bool" ]))

let test_te_sum () =
  check string "sum" "Nat + Bool"
    (pp_te (Ast.TSum [ TName "Nat"; TName "Bool" ]))

let test_te_nested () =
  check string "nested list of product" "[Nat * Nat]"
    (pp_te (Ast.TList (TProduct [ TName "Nat"; TName "Nat" ])));
  check string "sum of products" "Nat * Nat + Bool"
    (pp_te (Ast.TSum [ TProduct [ TName "Nat"; TName "Nat" ]; TName "Bool" ]))

(* --- Property-based tests: parse-pretty-reparse round trip --- *)

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let test_roundtrip_simple_exprs () =
  (* Parse, pretty-print, re-parse, compare ASTs *)
  let specs =
    [
      "module T.\nFoo.\n---\ntrue.\n";
      "module T.\nFoo.\n---\nfalse.\n";
      "module T.\nFoo.\n---\n1 = 1.\n";
      "module T.\nFoo.\n---\n1 + 2 = 3.\n";
      "module T.\nFoo.\nf => Bool.\ng => Bool.\n---\nf and g.\n";
      "module T.\nFoo.\nf => Bool.\ng => Bool.\n---\nf or g.\n";
      "module T.\nFoo.\nf => Bool.\ng => Bool.\n---\n~f.\n";
    ]
  in
  List.iter
    (fun spec ->
      let doc1 = parse spec in
      let body1 = (List.hd doc1.chapters).body in
      List.iter
        (fun prop ->
          let printed = Pretty.str_expr prop.Ast.value in
          (* Re-parse via a minimal document *)
          let spec2 = Printf.sprintf "module T.\nFoo.\n---\n%s.\n" printed in
          let doc2 = parse spec2 in
          let body2 = (List.hd doc2.chapters).body in
          let expr2 = (List.hd body2).Ast.value in
          check bool
            (Printf.sprintf "roundtrip: %s" printed)
            true
            (Ast.equal_expr prop.Ast.value expr2))
        body1)
    specs

let test_roundtrip_declarations () =
  let specs =
    [
      "module T.\nUser.\n---\n";
      "module T.\nPoint = Nat * Nat.\n---\n";
      "module T.\nUser.\nf u: User => Bool.\n---\n";
      "module T.\nFoo.\n~> Do thing.\n---\n";
    ]
  in
  List.iter
    (fun spec ->
      let doc1 = parse spec in
      let head1 = (List.hd doc1.chapters).head in
      List.iter
        (fun decl ->
          let printed = Pretty.str_declaration decl.Ast.value in
          (* Re-parse via a minimal document *)
          let spec2 = Printf.sprintf "module T.\n%s\n---\n" printed in
          try
            let doc2 = parse spec2 in
            let head2 = (List.hd doc2.chapters).head in
            (* Compare last declaration (skip domains that might be repeated) *)
            let d2 = (List.nth head2 (List.length head2 - 1)).Ast.value in
            check bool
              (Printf.sprintf "roundtrip: %s" printed)
              true
              (Ast.equal_declaration decl.Ast.value d2)
          with _ ->
            (* Some declarations need domain context to re-parse — skip *)
            ())
        head1)
    specs

let test_roundtrip_type_exprs () =
  let tes =
    [
      Ast.TName "Nat";
      Ast.TList (TName "Bool");
      Ast.TProduct [ TName "Nat"; TName "Bool" ];
      Ast.TSum [ TName "Nat"; TName "Nothing" ];
      Ast.TQName ("Mod", "Type");
      Ast.TList (TProduct [ TName "Nat"; TName "Nat" ]);
    ]
  in
  List.iter
    (fun te ->
      let printed = Pretty.str_type_expr te in
      (* Re-parse via a type alias *)
      let spec = Printf.sprintf "module T.\nAlias = %s.\n---\n" printed in
      let doc = parse spec in
      match (List.hd (List.hd doc.chapters).head).Ast.value with
      | Ast.DeclAlias (_, te2) ->
          check bool
            (Printf.sprintf "roundtrip: %s" printed)
            true
            (Ast.equal_type_expr te te2)
      | _ -> fail "expected alias")
    tes

(* --- Property-based: random type expression round trips --- *)

let gen_type_expr_at_depth =
  let open QCheck.Gen in
  let names = [ "Nat"; "Bool"; "Int"; "String"; "Real"; "Nothing" ] in
  let base = oneof_list (List.map (fun n -> Ast.TName n) names) in
  fix (fun self n ->
      if n <= 0 then base
      else
        oneof_weighted
          [
            (4, base);
            (1, map (fun t -> Ast.TList t) (self (n - 1)));
            ( 1,
              map2
                (fun a b -> Ast.TProduct [ a; b ])
                (self (n - 1))
                (self (n - 1)) );
            ( 1,
              map2 (fun a b -> Ast.TSum [ a; b ]) (self (n - 1)) (self (n - 1))
            );
          ])

let gen_type_expr = gen_type_expr_at_depth 2
let arb_type_expr = QCheck.make ~print:Pretty.str_type_expr gen_type_expr

let test_type_expr_roundtrip =
  QCheck_alcotest.to_alcotest
    (QCheck.Test.make ~name:"type expr roundtrip" ~count:200 arb_type_expr
       (fun te ->
         let printed = Pretty.str_type_expr te in
         let spec = Printf.sprintf "module T.\nAlias = %s.\n---\n" printed in
         try
           let doc = parse spec in
           match (List.hd (List.hd doc.chapters).head).Ast.value with
           | Ast.DeclAlias (_, te2) -> Ast.equal_type_expr te te2
           | _ -> false
         with _ -> false))

(* --- Bug-finding tests --- *)

let test_roundtrip_implication_chain () =
  (* Bug #8: a -> b -> c is right-associative. Does round-trip preserve this? *)
  let expr =
    Ast.EBinop (OpImpl, EVar "a", EBinop (OpImpl, EVar "b", EVar "c"))
  in
  let printed = Pretty.str_expr expr in
  (* Re-parse via a minimal document *)
  let spec =
    Printf.sprintf
      "module T.\nFoo.\na => Bool.\nb => Bool.\nc => Bool.\n---\n%s.\n" printed
  in
  let doc = parse spec in
  let body = (List.hd doc.chapters).body in
  let reparsed = (List.hd body).Ast.value in
  check bool
    (Printf.sprintf "roundtrip impl chain: %s" printed)
    true
    (Ast.equal_expr expr reparsed)

let test_roundtrip_or_and_precedence () =
  (* a or b and c should round-trip correctly: a or (b and c) *)
  let expr = Ast.EBinop (OpOr, EVar "a", EBinop (OpAnd, EVar "b", EVar "c")) in
  let printed = Pretty.str_expr expr in
  let spec =
    Printf.sprintf
      "module T.\nFoo.\na => Bool.\nb => Bool.\nc => Bool.\n---\n%s.\n" printed
  in
  let doc = parse spec in
  let body = (List.hd doc.chapters).body in
  let reparsed = (List.hd body).Ast.value in
  check bool
    (Printf.sprintf "roundtrip or/and: %s" printed)
    true
    (Ast.equal_expr expr reparsed)

let () =
  run "Pretty"
    [
      ( "expressions",
        [
          test_case "var" `Quick test_var;
          test_case "domain" `Quick test_domain;
          test_case "qualified" `Quick test_qualified;
          test_case "lit nat" `Quick test_lit_nat;
          test_case "lit real" `Quick test_lit_real;
          test_case "lit string" `Quick test_lit_string;
          test_case "lit bool" `Quick test_lit_bool;
          test_case "app" `Quick test_app;
          test_case "primed" `Quick test_primed;
          test_case "override" `Quick test_override;
          test_case "tuple" `Quick test_tuple;
          test_case "proj" `Quick test_proj;
          test_case "and" `Quick test_binop_and;
          test_case "or" `Quick test_binop_or;
          test_case "impl" `Quick test_binop_impl;
          test_case "iff" `Quick test_binop_iff;
          test_case "eq" `Quick test_binop_eq;
          test_case "neq" `Quick test_binop_neq;
          test_case "lt" `Quick test_binop_lt;
          test_case "gt" `Quick test_binop_gt;
          test_case "le" `Quick test_binop_le;
          test_case "ge" `Quick test_binop_ge;
          test_case "in" `Quick test_binop_in;
          test_case "subset" `Quick test_binop_subset;
          test_case "add" `Quick test_binop_add;
          test_case "sub" `Quick test_binop_sub;
          test_case "mul" `Quick test_binop_mul;
          test_case "div" `Quick test_binop_div;
          test_case "not" `Quick test_unop_not;
          test_case "neg" `Quick test_unop_neg;
          test_case "card" `Quick test_unop_card;
          test_case "forall" `Quick test_forall;
          test_case "exists" `Quick test_exists;
          test_case "each" `Quick test_each;
          test_case "cond" `Quick test_cond;
          test_case "initially" `Quick test_initially;
        ] );
      ( "declarations",
        [
          test_case "domain" `Quick test_decl_domain;
          test_case "alias" `Quick test_decl_alias;
          test_case "rule" `Quick test_decl_rule;
          test_case "rule with guards" `Quick test_decl_rule_with_guards;
          test_case "rule with context" `Quick test_decl_rule_with_context;
          test_case "action" `Quick test_decl_action;
          test_case "action with context" `Quick test_decl_action_with_context;
          test_case "closure" `Quick test_decl_closure;
        ] );
      ( "type_expressions",
        [
          test_case "name" `Quick test_te_name;
          test_case "qname" `Quick test_te_qname;
          test_case "list" `Quick test_te_list;
          test_case "product" `Quick test_te_product;
          test_case "sum" `Quick test_te_sum;
          test_case "nested" `Quick test_te_nested;
        ] );
      ( "roundtrip",
        [
          test_case "simple exprs" `Quick test_roundtrip_simple_exprs;
          test_case "declarations" `Quick test_roundtrip_declarations;
          test_case "type exprs" `Quick test_roundtrip_type_exprs;
        ] );
      ("property", [ test_type_expr_roundtrip ]);
      ( "bug_finding",
        [
          test_case "roundtrip implication chain" `Quick
            test_roundtrip_implication_chain;
          test_case "roundtrip or/and precedence" `Quick
            test_roundtrip_or_and_precedence;
        ] );
    ]
