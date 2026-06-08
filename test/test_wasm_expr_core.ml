(* @archlint.module test
   @archlint.domain pantagruel.wasm-expr *)

open Alcotest
open Pantagruel

let var name = Ast.EVar (Ast.Lower name)
let domain name = Ast.EDomain (Ast.Upper name)

let param name =
  { Ast.param_name = Ast.Lower name; param_type = Ast.TName (Ast.Upper "User") }

let quantified name body = Ast.make_forall [ param name ] [] body

let gen_name =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'a' 'z')
    (QCheck2.Gen.int_range 1 10)

let gen_distinct_names =
  QCheck2.Gen.map
    (fun (a, b, c) ->
      let b = if String.equal a b then b ^ "_b" else b in
      let c = if String.equal c a || String.equal c b then c ^ "_c" else c in
      (a, b, c))
    (QCheck2.Gen.triple gen_name gen_name gen_name)

let wasm_expr_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"wasm expression helpers preserve binding semantics" ~count:100
         gen_distinct_names (fun (x, y, z) ->
           Wasm_expr.bound_names_from_guards
             [ Ast.GExpr (var y); Ast.GIn (Ast.Lower x, var z) ]
           = [ x ]
           && Wasm_expr.free_vars
                (Ast.EBinop (Ast.OpAnd, var x, quantified x (var x)))
              = [ x ]
           && Ast.equal_expr
                (Wasm_expr.rename_expr
                   [ (String.uppercase_ascii x, y) ]
                   (domain (String.uppercase_ascii x)))
                (var y)
           && Ast.equal_expr
                (Wasm_expr.subst_var x (var y) (quantified x (var x)))
                (quantified x (var x))
           && Ast.equal_expr
                (Wasm_expr.subst_var x (var z) (Ast.EApp (var y, [ var x ])))
                (Ast.EApp (var y, [ var z ]))));
  ]

let () = run "Wasm_expr_core" [ ("wasm_expr", wasm_expr_properties) ]
