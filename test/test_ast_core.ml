(* @archlint.module test
   @archlint.domain pantagruel.ast *)

open Alcotest
open Pantagruel

let loc = { Ast.file = "<test>"; line = 1; col = 1 }
let lower name = Ast.Lower name
let upper name = Ast.Upper name

let param name =
  { Ast.param_name = lower name; param_type = Ast.TName (upper "User") }

let expr = Ast.EVar (lower "x")
let guard = Ast.GExpr expr
let decl = Ast.DeclDomain (upper "User")
let located_decl = Ast.located loc decl
let located_expr = Ast.located loc expr

let chapter =
  {
    Ast.head = [ located_decl ];
    body = [ located_expr ];
    checks = [];
    trailing_docs = [];
  }

let document =
  {
    Ast.module_name = Some (upper "M");
    imports = [];
    contexts = [];
    chapters = [ chapter ];
  }

let nonempty s = String.length s > 0

let ast_surface_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"ast public callable surface is coherent"
         ~count:100 QCheck2.Gen.unit (fun () ->
           let forall = Ast.make_forall [ param "x" ] [ guard ] expr in
           let exists = Ast.make_exists [ param "x" ] [ guard ] expr in
           let each =
             Ast.make_each [ param "x" ] [ guard ] (Some Ast.CombAnd) expr
           in
           let mb = Ast.bind_quant [ param "x" ] [ guard ] expr in
           let params, guards, body = Ast.unbind_quant mb [ param "x" ] in
           let var = Ast.fresh_var "x" in
           let boxed_expr =
             Ast.box_expr [ ("x", var) ] (Ast.EVar (lower "x"))
             |> Binder.Box.unbox
           in
           let boxed_guard =
             Ast.box_guard [ ("x", var) ] (Ast.GExpr (Ast.EVar (lower "x")))
             |> Binder.Box.unbox
           in
           let boxed_guards =
             Ast.box_guards [ ("x", var) ] [ Ast.GExpr (Ast.EVar (lower "x")) ]
             |> Binder.Box.unbox
           in
           let boxed_quant =
             Ast.box_quant []
               (fun mb -> Ast.EForall (mb, [ param "x" ]))
               mb
               [ param "x" ]
             |> Binder.Box.unbox
           in
           Ast.upper_name (upper "User") = "User"
           && Ast.lower_name (lower "x") = "x"
           && Ast.equal_expr (Ast.mkfree_expr var) (Ast.EVar (lower "x"))
           && Ast.equal_expr boxed_expr expr
           && Ast.equal_guard boxed_guard guard
           && List.equal Ast.equal_guard boxed_guards [ guard ]
           && Ast.equal_expr boxed_quant forall
           && Ast.equal_expr forall (Ast.make_forall params guards body)
           && Ast.equal_expr exists exists
           && Ast.equal_expr each each
           && Ast.equal_params [ param "a" ] [ param "b" ]
           && Ast.equal_guarded_body ([ guard ], expr) ([ guard ], expr)
           && Ast.equal_declaration decl decl
           && Ast.equal_chapter chapter chapter
           && Ast.equal_document document document
           && nonempty (Format.asprintf "%a" Ast.pp_expr forall)
           && nonempty (Format.asprintf "%a" Ast.pp_guard guard)
           && nonempty (Format.asprintf "%a" Ast.pp_guards [ guard ])
           && nonempty (Format.asprintf "%a" Ast.pp_params [ param "x" ])
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
