(* @archlint.module test
   @archlint.domain pantagruel.ast *)

open Alcotest
open Pantagruel

let loc = { Ast.file = "<test>"; line = 1; col = 1 }
let lower name = Ast.Lower name
let upper name = Ast.Upper name

let param domain name =
  { Ast.param_name = lower name; param_type = Ast.TName (upper domain) }

let nonempty s = String.length s > 0

let gen_lower =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 10)

let gen_upper =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'A' 'Z')
    (QCheck2.Gen.int_range 1 10)

let ast_surface_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"ast public callable surface is coherent"
         ~count:100 (QCheck2.Gen.triple gen_upper gen_upper gen_lower)
         (fun (module_name, domain_name, var_name) ->
           let p = param domain_name var_name in
           let expr = Ast.EVar (lower var_name) in
           let guard = Ast.GExpr expr in
           let decl = Ast.DeclDomain (upper domain_name) in
           let located_decl = Ast.located loc decl in
           let located_expr = Ast.located loc expr in
           let chapter =
             {
               Ast.head = [ located_decl ];
               body = [ located_expr ];
               checks = [];
               trailing_docs = [];
             }
           in
           let document =
             {
               Ast.module_name = Some (upper module_name);
               imports = [];
               contexts = [];
               chapters = [ chapter ];
             }
           in
           let forall = Ast.make_forall [ p ] [ guard ] expr in
           let exists = Ast.make_exists [ p ] [ guard ] expr in
           let each = Ast.make_each [ p ] [ guard ] (Some Ast.CombAnd) expr in
           let mb = Ast.bind_quant [ p ] [ guard ] expr in
           let params, guards, body = Ast.unbind_quant mb [ p ] in
           let var = Ast.fresh_var var_name in
           let boxed_expr =
             Ast.box_expr [ (var_name, var) ] (Ast.EVar (lower var_name))
             |> Binder.Box.unbox
           in
           let boxed_guard =
             Ast.box_guard
               [ (var_name, var) ]
               (Ast.GExpr (Ast.EVar (lower var_name)))
             |> Binder.Box.unbox
           in
           let boxed_guards =
             Ast.box_guards
               [ (var_name, var) ]
               [ Ast.GExpr (Ast.EVar (lower var_name)) ]
             |> Binder.Box.unbox
           in
           let boxed_quant =
             Ast.box_quant [] (fun mb -> Ast.EForall (mb, [ p ])) mb [ p ]
             |> Binder.Box.unbox
           in
           Ast.upper_name (upper domain_name) = domain_name
           && Ast.lower_name (lower var_name) = var_name
           && Ast.equal_expr (Ast.mkfree_expr var) (Ast.EVar (lower var_name))
           && Ast.equal_expr boxed_expr expr
           && Ast.equal_guard boxed_guard guard
           && List.equal Ast.equal_guard boxed_guards [ guard ]
           && Ast.equal_expr boxed_quant forall
           && Ast.equal_expr forall (Ast.make_forall params guards body)
           && Ast.equal_expr exists exists
           && Ast.equal_expr each each
           && Ast.equal_params [ p ] [ p ]
           && Ast.equal_guarded_body ([ guard ], expr) ([ guard ], expr)
           && Ast.equal_declaration decl decl
           && Ast.equal_chapter chapter chapter
           && Ast.equal_document document document
           && nonempty (Format.asprintf "%a" Ast.pp_expr forall)
           && nonempty (Format.asprintf "%a" Ast.pp_guard guard)
           && nonempty (Format.asprintf "%a" Ast.pp_guards [ guard ])
           && nonempty (Format.asprintf "%a" Ast.pp_params [ p ])
           && nonempty (Format.asprintf "%a" Ast.pp_declaration decl)
           && nonempty (Format.asprintf "%a" Ast.pp_chapter chapter)
           && nonempty (Format.asprintf "%a" Ast.pp_document document)
           && nonempty (Ast.show_expr forall)
           && nonempty (Ast.show_guard guard)
           && nonempty (Ast.show_declaration decl)
           && nonempty (Ast.show_chapter chapter)
           && nonempty (Ast.show_document document)
           && Ast.equal_expr (Ast.located loc expr).Ast.value expr));
  ]

let () = run "Ast_core" [ ("ast", ast_surface_properties) ]
