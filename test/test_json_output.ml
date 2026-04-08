(** Tests for JSON serialization *)

open Alcotest
open Pantagruel

let json_to_string j = Yojson.Basic.pretty_to_string j

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

(* --- ty_to_json tests --- *)

let test_ty_to_json_base () =
  check string "Bool" {|"Bool"|}
    (json_to_string (Json_output.ty_to_json Types.TyBool));
  check string "Nat" {|"Nat"|}
    (json_to_string (Json_output.ty_to_json Types.TyNat));
  check string "Nat0" {|"Nat0"|}
    (json_to_string (Json_output.ty_to_json Types.TyNat0));
  check string "Int" {|"Int"|}
    (json_to_string (Json_output.ty_to_json Types.TyInt));
  check string "Real" {|"Real"|}
    (json_to_string (Json_output.ty_to_json Types.TyReal));
  check string "String" {|"String"|}
    (json_to_string (Json_output.ty_to_json Types.TyString));
  check string "Nothing" {|"Nothing"|}
    (json_to_string (Json_output.ty_to_json Types.TyNothing));
  check string "Domain" {|"User"|}
    (json_to_string (Json_output.ty_to_json (Types.TyDomain "User")))

let test_ty_to_json_compound () =
  let j = Json_output.ty_to_json (Types.TyList Types.TyNat) in
  check bool "list has key" true (Yojson.Basic.Util.member "list" j <> `Null);
  let j =
    Json_output.ty_to_json (Types.TyProduct [ Types.TyNat; Types.TyBool ])
  in
  check bool "product has key" true
    (Yojson.Basic.Util.member "product" j <> `Null);
  let j =
    Json_output.ty_to_json (Types.TySum [ Types.TyNat; Types.TyNothing ])
  in
  check bool "sum has key" true (Yojson.Basic.Util.member "sum" j <> `Null);
  let j =
    Json_output.ty_to_json (Types.TyFunc ([ Types.TyNat ], Some Types.TyBool))
  in
  check bool "func has key" true (Yojson.Basic.Util.member "func" j <> `Null)

(* --- type_expr_to_json tests --- *)

let test_type_expr_to_json () =
  check string "TName" {|"Nat"|}
    (json_to_string (Json_output.type_expr_to_json (Ast.TName "Nat")));
  let j = Json_output.type_expr_to_json (Ast.TQName ("Mod", "Type")) in
  check bool "TQName" true (Yojson.Basic.Util.member "qualified" j <> `Null);
  let j = Json_output.type_expr_to_json (Ast.TList (TName "Nat")) in
  check bool "TList" true (Yojson.Basic.Util.member "list" j <> `Null);
  let j =
    Json_output.type_expr_to_json (Ast.TProduct [ TName "Nat"; TName "Bool" ])
  in
  check bool "TProduct" true (Yojson.Basic.Util.member "product" j <> `Null);
  let j =
    Json_output.type_expr_to_json (Ast.TSum [ TName "Nat"; TName "Nothing" ])
  in
  check bool "TSum" true (Yojson.Basic.Util.member "sum" j <> `Null)

(* --- binop_to_string / unop_to_string tests --- *)

let test_binop_to_string () =
  check string "and" "and" (Json_output.binop_to_string OpAnd);
  check string "or" "or" (Json_output.binop_to_string OpOr);
  check string "impl" "impl" (Json_output.binop_to_string OpImpl);
  check string "iff" "iff" (Json_output.binop_to_string OpIff);
  check string "eq" "eq" (Json_output.binop_to_string OpEq);
  check string "neq" "neq" (Json_output.binop_to_string OpNeq);
  check string "lt" "lt" (Json_output.binop_to_string OpLt);
  check string "gt" "gt" (Json_output.binop_to_string OpGt);
  check string "le" "le" (Json_output.binop_to_string OpLe);
  check string "ge" "ge" (Json_output.binop_to_string OpGe);
  check string "in" "in" (Json_output.binop_to_string OpIn);
  check string "subset" "subset" (Json_output.binop_to_string OpSubset);
  check string "add" "add" (Json_output.binop_to_string OpAdd);
  check string "sub" "sub" (Json_output.binop_to_string OpSub);
  check string "mul" "mul" (Json_output.binop_to_string OpMul);
  check string "div" "div" (Json_output.binop_to_string OpDiv)

let test_unop_to_string () =
  check string "not" "not" (Json_output.unop_to_string OpNot);
  check string "neg" "neg" (Json_output.unop_to_string OpNeg);
  check string "card" "card" (Json_output.unop_to_string OpCard)

(* --- expr_to_json tests --- *)

let has_key key j = Yojson.Basic.Util.member key j <> `Null

let test_expr_to_json_basic () =
  check bool "var" true
    (has_key "var" (Json_output.expr_to_json (Ast.EVar "x")));
  check bool "domain" true
    (has_key "domain" (Json_output.expr_to_json (Ast.EDomain "User")));
  check bool "qualified" true
    (has_key "qualified" (Json_output.expr_to_json (Ast.EQualified ("M", "n"))));
  check bool "nat" true
    (has_key "nat" (Json_output.expr_to_json (Ast.ELitNat 42)));
  check bool "real" true
    (has_key "real" (Json_output.expr_to_json (Ast.ELitReal 1.5)));
  check bool "string" true
    (has_key "string" (Json_output.expr_to_json (Ast.ELitString "hi")));
  check bool "bool" true
    (has_key "bool" (Json_output.expr_to_json (Ast.ELitBool true)));
  check bool "primed" true
    (has_key "primed" (Json_output.expr_to_json (Ast.EPrimed "f")))

let test_expr_to_json_compound () =
  check bool "app" true
    (has_key "app"
       (Json_output.expr_to_json (Ast.EApp (EVar "f", [ EVar "x" ]))));
  check bool "override" true
    (has_key "override"
       (Json_output.expr_to_json
          (Ast.EOverride ("f", [ (EVar "a", EVar "b") ]))));
  check bool "tuple" true
    (has_key "tuple"
       (Json_output.expr_to_json (Ast.ETuple [ ELitNat 1; ELitNat 2 ])));
  check bool "proj" true
    (has_key "proj" (Json_output.expr_to_json (Ast.EProj (EVar "x", 1))));
  check bool "binop" true
    (has_key "binop"
       (Json_output.expr_to_json (Ast.EBinop (OpEq, EVar "a", EVar "b"))));
  check bool "unop" true
    (has_key "unop" (Json_output.expr_to_json (Ast.EUnop (OpNot, EVar "a"))))

let test_expr_to_json_quantifiers () =
  let p = [ Ast.{ param_name = "x"; param_type = TName "Nat" } ] in
  check bool "forall" true
    (has_key "forall"
       (Json_output.expr_to_json (Ast.EForall (p, [], ELitBool true))));
  check bool "exists" true
    (has_key "exists"
       (Json_output.expr_to_json (Ast.EExists (p, [], ELitBool true))));
  check bool "each" true
    (has_key "each" (Json_output.expr_to_json (Ast.EEach (p, [], None, EVar "x"))));
  check bool "cond" true
    (has_key "cond"
       (Json_output.expr_to_json (Ast.ECond [ (ELitBool true, ELitNat 1) ])));
  check bool "initially" true
    (has_key "initially" (Json_output.expr_to_json (Ast.EInitially (EVar "x"))))

(* --- guard_to_json tests --- *)

let test_guard_to_json () =
  let j =
    Json_output.guard_to_json
      (GParam { param_name = "x"; param_type = TName "Nat" })
  in
  check bool "GParam" true (has_key "param" j);
  let j = Json_output.guard_to_json (GIn ("x", EVar "xs")) in
  check bool "GIn" true (has_key "in" j);
  let j = Json_output.guard_to_json (GExpr (ELitBool true)) in
  check bool "GExpr" true (has_key "expr" j)

(* --- doc_to_json tests --- *)

let test_doc_to_json () =
  check (list (pair string reject)) "empty" [] (Json_output.doc_to_json []);
  let result = Json_output.doc_to_json [ [ "hello" ] ] in
  check int "single group" 1 (List.length result);
  let result = Json_output.doc_to_json [ [ "a" ]; [ "b" ] ] in
  check int "multiple groups" 1 (List.length result)

(* --- decl_to_json tests --- *)

let test_decl_to_json_domain () =
  let env, doc = parse_and_collect "module T.\n\nUser.\n---\n" in
  let decl = List.hd (List.hd doc.chapters).head in
  let j = Json_output.decl_to_json env decl in
  let kind = Yojson.Basic.Util.member "kind" j |> Yojson.Basic.Util.to_string in
  check string "domain kind" "domain" kind

let test_decl_to_json_alias () =
  let env, doc = parse_and_collect "module T.\n\nPoint = Nat * Nat.\n---\n" in
  let decl = List.hd (List.hd doc.chapters).head in
  let j = Json_output.decl_to_json env decl in
  let kind = Yojson.Basic.Util.member "kind" j |> Yojson.Basic.Util.to_string in
  check string "alias kind" "alias" kind;
  check bool "has resolved" true (has_key "resolved" j)

let test_decl_to_json_rule () =
  let env, doc =
    parse_and_collect "module T.\n\nUser.\nf u: User => Bool.\n---\n"
  in
  let decl = List.nth (List.hd doc.chapters).head 1 in
  let j = Json_output.decl_to_json env decl in
  let kind = Yojson.Basic.Util.member "kind" j |> Yojson.Basic.Util.to_string in
  check string "rule kind" "rule" kind;
  check bool "has resolved" true (has_key "resolved" j)

let test_decl_to_json_action () =
  let env, doc =
    parse_and_collect "module T.\n\nUser.\n~> Do thing @ u: User.\n---\n"
  in
  let decl = List.nth (List.hd doc.chapters).head 1 in
  let j = Json_output.decl_to_json env decl in
  let kind = Yojson.Basic.Util.member "kind" j |> Yojson.Basic.Util.to_string in
  check string "action kind" "action" kind

let test_decl_to_json_closure () =
  let env, doc =
    parse_and_collect
      "module T.\n\n\
       Block.\n\
       parent b: Block => Block + Nothing.\n\
       ancestor b: Block => [Block] = closure parent.\n\
       ---\n"
  in
  let decl = List.nth (List.hd doc.chapters).head 2 in
  let j = Json_output.decl_to_json env decl in
  let kind = Yojson.Basic.Util.member "kind" j |> Yojson.Basic.Util.to_string in
  check string "closure kind" "closure" kind

(* --- document_to_json tests --- *)

let test_document_to_json () =
  let env, doc =
    parse_and_collect
      "module TEST.\n\
       context Ctx.\n\n\
       User.\n\
       {Ctx} name u: User => String.\n\
       ---\n\
       all u: User | true.\n"
  in
  let j = Json_output.document_to_json env doc in
  check bool "has module" true (has_key "module" j);
  check bool "has imports" true (has_key "imports" j);
  check bool "has contexts" true (has_key "contexts" j);
  check bool "has types" true (has_key "types" j);
  check bool "has rules" true (has_key "rules" j);
  check bool "has chapters" true (has_key "chapters" j);
  let module_name =
    Yojson.Basic.Util.member "module" j |> Yojson.Basic.Util.to_string
  in
  check string "module name" "TEST" module_name;
  let chapters =
    Yojson.Basic.Util.member "chapters" j |> Yojson.Basic.Util.to_list
  in
  check int "chapter count" 1 (List.length chapters)

let () =
  run "Json_output"
    [
      ( "ty_to_json",
        [
          test_case "base types" `Quick test_ty_to_json_base;
          test_case "compound types" `Quick test_ty_to_json_compound;
        ] );
      ( "type_expr_to_json",
        [ test_case "all variants" `Quick test_type_expr_to_json ] );
      ( "operators",
        [
          test_case "binop_to_string" `Quick test_binop_to_string;
          test_case "unop_to_string" `Quick test_unop_to_string;
        ] );
      ( "expr_to_json",
        [
          test_case "basic" `Quick test_expr_to_json_basic;
          test_case "compound" `Quick test_expr_to_json_compound;
          test_case "quantifiers" `Quick test_expr_to_json_quantifiers;
        ] );
      ("guard_to_json", [ test_case "all variants" `Quick test_guard_to_json ]);
      ( "doc_to_json",
        [ test_case "empty and non-empty" `Quick test_doc_to_json ] );
      ( "decl_to_json",
        [
          test_case "domain" `Quick test_decl_to_json_domain;
          test_case "alias" `Quick test_decl_to_json_alias;
          test_case "rule" `Quick test_decl_to_json_rule;
          test_case "action" `Quick test_decl_to_json_action;
          test_case "closure" `Quick test_decl_to_json_closure;
        ] );
      ( "document_to_json",
        [ test_case "full document" `Quick test_document_to_json ] );
    ]
