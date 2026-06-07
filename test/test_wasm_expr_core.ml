(* @archlint.module test
   @archlint.domain pantagruel.wasm-expr *)

open Alcotest
open Pantagruel

let var name = Ast.EVar (Ast.Lower name)
let domain name = Ast.EDomain (Ast.Upper name)

let param name =
  { Ast.param_name = Ast.Lower name; param_type = Ast.TName (Ast.Upper "User") }

let quantified_x body = Ast.make_forall [ param "x" ] [] body

let wasm_expr_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"wasm expression helpers preserve binding semantics" ~count:100
         QCheck2.Gen.unit (fun () ->
           Wasm_expr.bound_names_from_guards
             [ Ast.GExpr (var "a"); Ast.GIn (Ast.Lower "item", var "xs") ]
           = [ "item" ]
           && Wasm_expr.free_vars
                (Ast.EBinop (Ast.OpAnd, var "x", quantified_x (var "x")))
              = [ "x" ]
           && Ast.equal_expr
                (Wasm_expr.rename_expr
                   [ ("UNSUPPORTED_UNKNOWN", "unsupported-unknown") ]
                   (domain "UNSUPPORTED_UNKNOWN"))
                (var "unsupported-unknown")
           && Ast.equal_expr
                (Wasm_expr.subst_var "x" (var "y") (quantified_x (var "x")))
                (quantified_x (var "x"))
           && Ast.equal_expr
                (Wasm_expr.subst_var "x" (var "z")
                   (Ast.EApp (var "f", [ var "x" ])))
                (Ast.EApp (var "f", [ var "z" ]))));
  ]

let () = run "Wasm_expr_core" [ ("wasm_expr", wasm_expr_properties) ]
