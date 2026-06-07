(* @archlint.module test
   @archlint.domain pantagruel.lower-aliases *)

(** Identity-lowering of list-typed aliases. *)

open Alcotest
open Pantagruel

let parse_and_collect = Test_util.parse_and_collect

let kind_of name env =
  match Env.lookup_type name env with Some e -> Some e.Env.kind | None -> None

let is_domain k =
  match[@warning "-4"] k with Some Env.KDomain -> true | _ -> false

let is_alias k =
  match[@warning "-4"] k with Some (Env.KAlias _) -> true | _ -> false

type generated_decl = Domain | ListAlias | ProductAlias

let decl_names = [ "P"; "Q"; "R"; "S"; "T" ]

let mk_located value =
  { Ast.loc = Ast.dummy_loc; value; doc = []; doc_adjacent = false }

let declaration_of_generated name = function
  | Domain -> Ast.DeclDomain (Ast.Upper name)
  | ListAlias ->
      Ast.DeclAlias (Ast.Upper name, Ast.TList (Ast.TName (Ast.Upper "D")))
  | ProductAlias ->
      Ast.DeclAlias
        ( Ast.Upper name,
          Ast.TProduct [ Ast.TName (Ast.Upper "D"); Ast.TName (Ast.Upper "D") ]
        )

let doc_of_generated decls =
  let head =
    mk_located (Ast.DeclDomain (Ast.Upper "D"))
    :: List.map2
         (fun name decl -> mk_located (declaration_of_generated name decl))
         decl_names decls
  in
  {
    Ast.module_name = Some (Ast.Upper "M");
    imports = [];
    contexts = [];
    chapters = [ { head; body = []; checks = []; trailing_docs = [] } ];
  }

let gen_decl = QCheck2.Gen.oneof_list [ Domain; ListAlias; ProductAlias ]

let gen_generated_decls =
  QCheck2.Gen.list_size (QCheck2.Gen.return (List.length decl_names)) gen_decl

let print_generated_decls decls =
  let show = function
    | Domain -> "domain"
    | ListAlias -> "list-alias"
    | ProductAlias -> "product-alias"
  in
  "[" ^ String.concat "; " (List.map show decls) ^ "]"

let list_alias_names decls =
  List.filter_map
    (fun (name, decl) ->
      match[@warning "-4"] decl with ListAlias -> Some name | _ -> None)
    (List.combine decl_names decls)

(* Only list aliases ([T = [D]]) are candidates; a product alias is not. *)
let test_candidates () =
  let doc =
    Test_util.parse
      "module M.\n\
       Insight.\n\
       P = [Insight].\n\
       Pair = Insight * Insight.\n\
       f p: P => Bool.\n\
       ---\n\
       all p: P | f p.\n"
  in
  check (list string) "only the list alias" [ "P" ]
    (Lower_aliases.list_alias_candidates doc)

let test_rewrite () =
  let doc =
    Test_util.parse
      "module M.\n\
       Insight.\n\
       P = [Insight].\n\
       f p: P => Bool.\n\
       ---\n\
       all p: P | f p.\n"
  in
  let doc' = Lower_aliases.rewrite_to_domains doc [ "P" ] in
  let has_domain_p =
    List.exists
      (fun (ch : Ast.chapter) ->
        List.exists
          (fun (d : Ast.declaration Ast.located) ->
            match[@warning "-4"] d.value with
            | Ast.DeclDomain (Ast.Upper "P") -> true
            | _ -> false)
          ch.head)
      doc'.chapters
  in
  check bool "P rewritten to a domain declaration" true has_domain_p

(* An alias used only as an opaque identity is lowered to a domain. *)
let test_lower_opaque () =
  let env, doc =
    parse_and_collect
      "module M.\n\
       Insight.\n\
       P = [Insight].\n\
       f p: P => Bool.\n\
       ---\n\
       all p: P | f p.\n"
  in
  check bool "P starts as an alias" true (is_alias (kind_of "P" env));
  let env', _doc' =
    Module.lower_identity_aliases (Module.create_registry ()) env doc
  in
  check bool "P lowered to a domain" true (is_domain (kind_of "P" env'))

(* An alias destructured as a list ([i in p]) must NOT be lowered: the rewritten
   document fails to type-check, so the oracle keeps the alias. *)
let test_no_lower_destructured () =
  let env, doc =
    parse_and_collect
      "module M.\n\
       Insight.\n\
       P = [Insight].\n\
       g p: P, i: Insight => Bool.\n\
       ---\n\
       all p: P | all i: Insight | i in p -> g p i.\n"
  in
  let env', _doc' =
    Module.lower_identity_aliases (Module.create_registry ()) env doc
  in
  check bool "destructured alias is not lowered" true
    (is_alias (kind_of "P" env'))

let test_list_alias_candidates_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"list_alias_candidates returns list aliases"
       ~count:200 ~print:print_generated_decls gen_generated_decls (fun decls ->
         let doc = doc_of_generated decls in
         Lower_aliases.list_alias_candidates doc = list_alias_names decls))

let test_rewrite_to_domains_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"rewrite_to_domains rewrites selected aliases"
       ~count:200 ~print:print_generated_decls gen_generated_decls (fun decls ->
         let doc = doc_of_generated decls in
         let selected = [ "P"; "R"; "T" ] in
         let rewritten = Lower_aliases.rewrite_to_domains doc selected in
         let expected_value name decl =
           match[@warning "-4"] decl with
           | (ListAlias | ProductAlias) when List.mem name selected ->
               Ast.DeclDomain (Ast.Upper name)
           | _ -> declaration_of_generated name decl
         in
         match rewritten.chapters with
         | [ chapter ] ->
             let rewritten_values =
               List.map (fun d -> d.Ast.value) chapter.head
             in
             let expected_values =
               Ast.DeclDomain (Ast.Upper "D")
               :: List.map2 expected_value decl_names decls
             in
             List.equal Ast.equal_declaration expected_values rewritten_values
         | _ -> false))

let () =
  run "LowerAliases"
    [
      ( "lower_aliases",
        [
          test_case "candidates are list aliases" `Quick test_candidates;
          test_case "rewrite to domains" `Quick test_rewrite;
          test_case "opaque alias is lowered" `Quick test_lower_opaque;
          test_case "destructured alias is not lowered" `Quick
            test_no_lower_destructured;
        ] );
      ( "property",
        [
          test_list_alias_candidates_property; test_rewrite_to_domains_property;
        ] );
    ]
