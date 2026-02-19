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

let test_all () =
  let doc = parse "module TEST.\n\nUser.\n---\nall u: User | true.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall _ -> ()
  | _ -> fail "Expected all"

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

let test_procedure_guard () =
  let doc = parse "module TEST.\n\nAccount.\nbalance a: Account => Nat.\nwithdraw a: Account, amount: Nat, balance a >= amount.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclProc { guards = [Ast.GExpr _]; _ } -> ()
  | _ -> fail "Expected procedure with guard"

let test_membership_binding () =
  let doc = parse "module TEST.\n\nItem.\nitems => [Item].\n---\nall i in items | true.\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.hd chapter.Ast.body).Ast.value with
  | Ast.EForall ([], [Ast.GIn ("i", _)], _) -> ()
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
  check (list string) "doc comment" ["This is a doc comment"] decl.Ast.doc

let test_multiple_guards () =
  let doc = parse "module TEST.\n\nAccount.\nbalance a: Account => Nat.\nwithdraw a: Account, amount: Nat, amount > 0, balance a >= amount.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclProc { guards = [Ast.GExpr _; Ast.GExpr _]; _ } -> ()
  | _ -> fail "Expected procedure with two guards"

let test_no_module () =
  let doc = parse "Foo.\n---\ntrue.\n" in
  check (option string) "no module" None doc.Ast.module_name;
  check int "chapters" 1 (List.length doc.Ast.chapters);
  check int "imports" 0 (List.length doc.Ast.imports)

let test_context_declaration () =
  let doc = parse "module TEST.\ncontext Banking.\n\nAccount.\n---\n" in
  check int "contexts" 1 (List.length doc.Ast.contexts);
  check string "context name" "Banking" (List.hd doc.Ast.contexts).Ast.value

let test_proc_with_context () =
  let doc = parse "module TEST.\ncontext Banking.\n\nAccount.\n{Banking} balance a: Account => Nat.\nwithdraw a: Account in Banking.\n---\n" in
  let chapter = List.hd doc.Ast.chapters in
  (match (List.nth chapter.Ast.head 1).Ast.value with
  | Ast.DeclProc { name = "balance"; contexts = ["Banking"]; _ } -> ()
  | _ -> fail "Expected proc with context footprint");
  (match (List.nth chapter.Ast.head 2).Ast.value with
  | Ast.DeclProc { name = "withdraw"; context = Some "Banking"; _ } -> ()
  | _ -> fail "Expected proc with context annotation")

let () =
  run "Parser" [
    "minimal", [test_case "minimal document" `Quick test_minimal];
    "no_module", [test_case "no module header" `Quick test_no_module];
    "domain", [test_case "domain declaration" `Quick test_domain_decl];
    "procedure", [test_case "procedure declaration" `Quick test_procedure_decl];
    "void", [test_case "void procedure" `Quick test_void_procedure];
    "import", [test_case "import" `Quick test_import];
    "where", [test_case "where clause" `Quick test_where_clause];
    "proposition", [test_case "proposition" `Quick test_proposition];
    "all", [test_case "all" `Quick test_all];
    "application", [test_case "application" `Quick test_application];
    "type_alias", [test_case "type alias" `Quick test_type_alias];
    "list_type", [test_case "list type" `Quick test_list_type];
    "sum_type", [test_case "sum type" `Quick test_sum_type];
    "guard", [test_case "procedure guard" `Quick test_procedure_guard];
    "membership", [test_case "membership binding" `Quick test_membership_binding];
    "existential", [test_case "existential" `Quick test_existential];
    "doc_comment", [test_case "doc comment" `Quick test_doc_comment];
    "multi_guard", [test_case "multiple guards" `Quick test_multiple_guards];
    "context", [test_case "context declaration" `Quick test_context_declaration];
    "proc_context", [test_case "proc with context" `Quick test_proc_with_context];
  ]
