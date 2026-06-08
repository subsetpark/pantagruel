(* @archlint.module test
   @archlint.domain pantagruel.normalize *)

open Alcotest
open Pantagruel

let located value =
  Ast.{ loc = dummy_loc; value; doc = []; doc_adjacent = true }

let param name ty : Ast.param = { param_name = Ast.Lower name; param_type = ty }

let gen_lower =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 10)

let gen_upper =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'A' 'Z')
    (QCheck2.Gen.int_range 1 10)

let rule_decl ~domain ~rule ~param_name ~guard_name =
  Ast.DeclRule
    {
      name = Ast.Lower rule;
      params = [ param param_name (Ast.TName (Ast.Upper domain)) ];
      guards = [ Ast.GExpr (Ast.EVar (Ast.Lower guard_name)) ];
      return_type = Ast.TName (Ast.Upper "Bool");
      contexts = [];
    }

let action_decl ~domain ~label ~param_name =
  Ast.DeclAction
    {
      label;
      params = [ param param_name (Ast.TName (Ast.Upper domain)) ];
      guards = [];
      contexts = [];
    }

let action_chapter ~domain ~rule ~label ~param_name =
  {
    Ast.head = [ located (action_decl ~domain ~label ~param_name) ];
    body = [ located (Ast.EPrimed (Ast.Lower rule)) ];
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
         (QCheck2.Gen.quad gen_upper gen_lower gen_lower gen_lower)
         (fun (domain, rule, param_name, guard_name) ->
           let decl = rule_decl ~domain ~rule ~param_name ~guard_name in
           let action =
             action_decl ~domain
               ~label:(String.capitalize_ascii rule)
               ~param_name
           in
           let chapter =
             action_chapter ~domain ~rule
               ~label:(String.capitalize_ascii rule)
               ~param_name
           in
           let expr =
             Ast.EApp
               (Ast.EVar (Ast.Lower rule), [ Ast.EVar (Ast.Lower param_name) ])
           in
           let types, terms = Normalize.symbols_in_expr expr in
           let dep_types, dep_terms = Normalize.symbols_in_decl_deps decl in
           Normalize.decl_name decl = rule
           && (not (Normalize.is_type_decl decl))
           && Normalize.is_action action
           && Normalize.StringSet.mem domain
                (Normalize.types_in_type_expr (Ast.TName (Ast.Upper domain)))
           && Normalize.StringSet.mem rule terms
           && Normalize.StringSet.is_empty types
           && Normalize.StringSet.mem domain dep_types
           && Normalize.StringSet.mem guard_name dep_terms
           && Normalize.uses_primed (Ast.EPrimed (Ast.Lower rule))
           && Normalize.body_uses_primed chapter.body
           && Normalize.StringSet.mem param_name
                (Normalize.action_params chapter)
           && Normalize.prop_uses_params
                (Normalize.StringSet.singleton param_name)
                (Ast.EVar (Ast.Lower param_name))
           && Normalize.is_tied_prop
                (Normalize.StringSet.singleton param_name)
                (Ast.EVar (Ast.Lower param_name))));
  ]

let graph_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"dependency graph APIs compute closure and levels" ~count:100
         (QCheck2.Gen.pair gen_upper gen_upper) (fun (root, dep) ->
           let dep = if String.equal root dep then dep ^ "Dep" else dep in
           let by_name = Hashtbl.create 4 in
           Hashtbl.add by_name root (decl_info root [ dep ]);
           Hashtbl.add by_name dep (decl_info dep []);
           let closure =
             Normalize.transitive_decl_deps by_name
               (Normalize.StringSet.singleton root)
           in
           let levels = Hashtbl.create 4 in
           Hashtbl.add levels root 2;
           Normalize.StringSet.mem dep closure
           && Normalize.earliest_chapter_for_prop levels
                (Ast.EVar (Ast.Lower root))
              = 1));
  ]

let normalize_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"normalize preserves a non-empty document"
         ~count:100 (QCheck2.Gen.triple gen_upper gen_upper gen_lower)
         (fun (module_name, domain, rule) ->
           let doc =
             Test_util.parse
               (Printf.sprintf
                  {|module %s.

%s.
%s u: %s => Bool.
---
all u: %s | %s u.
|}
                  module_name domain rule domain domain rule)
           in
           match (Normalize.normalize doc rule).Ast.chapters with
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
