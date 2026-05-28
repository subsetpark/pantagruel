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
    ]
