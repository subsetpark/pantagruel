(* @archlint.module test
   @archlint.domain pantagruel.normalize *)

open Alcotest
open Pantagruel

let located value =
  Ast.{ loc = dummy_loc; value; doc = []; doc_adjacent = true }

let param name ty : Ast.param = { param_name = Ast.Lower name; param_type = ty }

let rule_decl =
  Ast.DeclRule
    {
      name = Ast.Lower "active";
      params = [ param "u" (Ast.TName (Ast.Upper "User")) ];
      guards = [ Ast.GExpr (Ast.EVar (Ast.Lower "ready")) ];
      return_type = Ast.TName (Ast.Upper "Bool");
      contexts = [];
    }

let action_decl =
  Ast.DeclAction
    {
      label = "Activate";
      params = [ param "u" (Ast.TName (Ast.Upper "User")) ];
      guards = [];
      contexts = [];
    }

let action_chapter =
  {
    Ast.head = [ located action_decl ];
    body = [ located (Ast.EPrimed (Ast.Lower "active")) ];
    checks = [];
    trailing_docs = [];
  }

let decl_info name deps =
  Normalize.
    {
      name;
      is_type = true;
      is_void = false;
      decl = located (Ast.DeclDomain (Ast.Upper name));
      dependencies = StringSet.of_list deps;
      level = -1;
    }

let helper_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"public helper APIs classify syntax" ~count:100
         QCheck2.Gen.unit (fun () ->
           let expr =
             Ast.EApp
               (Ast.EVar (Ast.Lower "active"), [ Ast.EVar (Ast.Lower "u") ])
           in
           let types, terms = Normalize.symbols_in_expr expr in
           let dep_types, dep_terms =
             Normalize.symbols_in_decl_deps rule_decl
           in
           Normalize.decl_name rule_decl = "active"
           && (not (Normalize.is_type_decl rule_decl))
           && Normalize.is_action action_decl
           && Normalize.StringSet.mem "User"
                (Normalize.types_in_type_expr (Ast.TName (Ast.Upper "User")))
           && Normalize.StringSet.mem "active" terms
           && Normalize.StringSet.is_empty types
           && Normalize.StringSet.mem "User" dep_types
           && Normalize.StringSet.mem "ready" dep_terms
           && Normalize.uses_primed (Ast.EPrimed (Ast.Lower "active"))
           && Normalize.body_uses_primed action_chapter.body
           && Normalize.StringSet.mem "u"
                (Normalize.action_params action_chapter)
           && Normalize.prop_uses_params
                (Normalize.StringSet.singleton "u")
                (Ast.EVar (Ast.Lower "u"))
           && Normalize.is_tied_prop
                (Normalize.StringSet.singleton "u")
                (Ast.EVar (Ast.Lower "u"))));
  ]

let graph_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"dependency graph APIs compute closure and levels" ~count:100
         QCheck2.Gen.unit (fun () ->
           let by_name = Hashtbl.create 4 in
           Hashtbl.add by_name "A" (decl_info "A" [ "B" ]);
           Hashtbl.add by_name "B" (decl_info "B" []);
           let closure =
             Normalize.transitive_decl_deps by_name
               (Normalize.StringSet.singleton "A")
           in
           let levels = Hashtbl.create 4 in
           Hashtbl.add levels "A" 2;
           Normalize.StringSet.mem "B" closure
           && Normalize.earliest_chapter_for_prop levels
                (Ast.EVar (Ast.Lower "A"))
              = 1));
  ]

let normalize_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"normalize preserves a non-empty document"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let doc =
             Test_util.parse
               {|module N.

User.
active u: User => Bool.
---
all u: User | active u.
|}
           in
           match (Normalize.normalize doc "active").Ast.chapters with
           | [] -> false
           | _ :: _ -> true));
  ]

let () =
  run "Normalize_core"
    [
      ("helpers", helper_properties);
      ("graph", graph_properties);
      ("normalize", normalize_properties);
    ]
