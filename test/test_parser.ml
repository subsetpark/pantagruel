(** Parser tests *)

open Alcotest
open Pantagruel

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let test_minimal () =
  let doc = parse "module EXAMPLE.\n\nFoo.\n---\n" in
  check string "module name" "EXAMPLE" doc.Ast.module_name;
  check int "chapters" 1 (List.length doc.Ast.chapters);
  check int "imports" 0 (List.length doc.Ast.imports)

let test_domain_decl () =
  let doc = parse "module TEST.\n\nUser.\nDocument.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  check int "declarations" 2 (List.length chapter.Ast.head)

let test_procedure_decl () =
  let doc = parse "module TEST.\n\nUser.\nowner d: User => User.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  check int "declarations" 2 (List.length chapter.Ast.head)

let test_void_procedure () =
  let doc = parse "module TEST.\n\nUser.\ncheck-out u: User.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 1).Ast.value with
  | Ast.DeclProc { return_type = None; _ } -> ()
  | _ -> fail "Expected Void procedure"

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

let test_forall () =
  let doc = parse "module TEST.\n\nUser.\n---\nforall u: User. true.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall _ -> ()
  | _ -> fail "Expected forall"

let test_application () =
  let doc = parse "module TEST.\n\nUser.\nf x: User => User.\n---\nf u = u.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EBinop (Ast.OpEq, Ast.EApp (_, _), _) -> ()
  | _ -> fail "Expected application in equality"

let test_type_alias () =
  let doc = parse "module TEST.\n\nPoint = Nat * Nat.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.head).Ast.value with
  | Ast.DeclAlias ("Point", Ast.TProduct [Ast.TName "Nat"; Ast.TName "Nat"]) -> ()
  | _ -> fail "Expected type alias"

let test_list_type () =
  let doc = parse "module TEST.\n\nUser.\nusers => [User].\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 1).Ast.value with
  | Ast.DeclProc { return_type = Some (Ast.TList (Ast.TName "User")); _ } -> ()
  | _ -> fail "Expected list type"

let test_sum_type () =
  let doc = parse "module TEST.\n\nResult = Nat + Nothing.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.head).Ast.value with
  | Ast.DeclAlias ("Result", Ast.TSum _) -> ()
  | _ -> fail "Expected sum type"

let () =
  run "Parser" [
    "minimal", [test_case "minimal document" `Quick test_minimal];
    "domain", [test_case "domain declaration" `Quick test_domain_decl];
    "procedure", [test_case "procedure declaration" `Quick test_procedure_decl];
    "void", [test_case "void procedure" `Quick test_void_procedure];
    "import", [test_case "import" `Quick test_import];
    "where", [test_case "where clause" `Quick test_where_clause];
    "proposition", [test_case "proposition" `Quick test_proposition];
    "forall", [test_case "forall" `Quick test_forall];
    "application", [test_case "application" `Quick test_application];
    "type_alias", [test_case "type alias" `Quick test_type_alias];
    "list_type", [test_case "list type" `Quick test_list_type];
    "sum_type", [test_case "sum type" `Quick test_sum_type];
  ]
