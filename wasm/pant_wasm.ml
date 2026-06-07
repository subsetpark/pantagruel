(* @archlint.module exempt
   @archlint.exempt-reason effect-facade *)

open Js_of_ocaml
open Pantagruel_parser

(** Parser exports: parse, rename, and pretty-print Pantagruel expressions. *)
let () =
  Js.export "pantParser"
    (object%js
       method parseAndRename text renames_js =
         let text = Js.to_string text in
         let renames =
           Js.to_array renames_js |> Array.to_list
           |> List.map (fun pair ->
               let arr = Js.to_array pair in
               (Js.to_string (Array.get arr 0), Js.to_string (Array.get arr 1)))
         in
         match Wasm_document.parse_expr_string text with
         | Ok expr ->
             let renamed = Wasm_expr.rename_expr renames expr in
             Js.some (Js.string (Pretty.str_expr renamed))
         | Error _ -> Js.null

       method prettyPrint text =
         let text = Js.to_string text in
         match Wasm_document.parse_expr_string text with
         | Ok expr -> Js.some (Js.string (Pretty.str_expr expr))
         | Error _ -> Js.null

       method checkDocument text =
         let text = Js.to_string text in
         match Wasm_document.check_document_string text with
         | None -> Js.null
         | Some msg -> Js.some (Js.string msg)

       method checkDocumentWithDeps consumer_text deps_array =
         let consumer = Js.to_string consumer_text in
         let deps =
           Js.to_array deps_array |> Array.to_list
           |> List.map (fun pair ->
               let arr = Js.to_array pair in
               (Js.to_string (Array.get arr 0), Js.to_string (Array.get arr 1)))
         in
         match Wasm_document.check_document_with_deps consumer deps with
         | None -> Js.null
         | Some msg -> Js.some (Js.string msg)
    end)

(** AST constructor exports: build Pantagruel AST nodes from JavaScript. *)
let () =
  let js_array_to_list arr = Array.to_list (Js.to_array arr) in
  let js_array_to_pair_list arr =
    js_array_to_list arr
    |> List.map (fun pair ->
        let a = Js.to_array pair in
        (Array.get a 0, Array.get a 1))
  in
  Js.export "pantAst"
    (object%js
       method var name = Ast.EVar (Lower (Js.to_string name))
       method domain name = Ast.EDomain (Upper (Js.to_string name))
       method app fn args = Ast.EApp (fn, js_array_to_list args)
       method primed name = Ast.EPrimed (Lower (Js.to_string name))
       method binop op l r = Ast.EBinop (op, l, r)
       method unop op e = Ast.EUnop (op, e)
       method litNat n = Ast.ELitNat n
       method litBool b = Ast.ELitBool (Js.to_bool b)
       method litString s = Ast.ELitString (Js.to_string s)
       method cond arms = Ast.ECond (js_array_to_pair_list arms)
       method tuple es = Ast.ETuple (js_array_to_list es)
       method proj e n = Ast.EProj (e, n)

       method override name pairs =
         Ast.EOverride (Lower (Js.to_string name), js_array_to_pair_list pairs)

       method initially e = Ast.EInitially e

       method forall params guards body =
         Ast.make_forall (js_array_to_list params) (js_array_to_list guards)
           body

       method each params guards body =
         Ast.make_each (js_array_to_list params) (js_array_to_list guards) None
           body

       method eachComb params guards comb body =
         Ast.make_each (js_array_to_list params) (js_array_to_list guards)
           (Some comb) body

       method exists params guards body =
         Ast.make_exists (js_array_to_list params) (js_array_to_list guards)
           body

       method opAnd = Ast.OpAnd
       method opOr = Ast.OpOr
       method opImpl = Ast.OpImpl
       method opIff = Ast.OpIff
       method opEq = Ast.OpEq
       method opNeq = Ast.OpNeq
       method opLt = Ast.OpLt
       method opGt = Ast.OpGt
       method opLe = Ast.OpLe
       method opGe = Ast.OpGe
       method opIn = Ast.OpIn
       method opSubset = Ast.OpSubset
       method opAdd = Ast.OpAdd
       method opSub = Ast.OpSub
       method opMul = Ast.OpMul
       method opDiv = Ast.OpDiv
       method opNot = Ast.OpNot
       method opNeg = Ast.OpNeg
       method opCard = Ast.OpCard
       method combAdd = Ast.CombAdd
       method combMul = Ast.CombMul
       method combAnd = Ast.CombAnd
       method combOr = Ast.CombOr
       method combMin = Ast.CombMin
       method combMax = Ast.CombMax

       method param name typeExpr =
         ({ Ast.param_name = Lower (Js.to_string name); param_type = typeExpr }
           : Ast.param)

       method tName name = Ast.TName (Upper (Js.to_string name))
       method tList t = Ast.TList t
       method tProduct ts = Ast.TProduct (js_array_to_list ts)
       method tSum ts = Ast.TSum (js_array_to_list ts)
       method gExpr e = Ast.GExpr e
       method gIn name e = Ast.GIn (Lower (Js.to_string name), e)
       method gParam p = Ast.GParam p
       method declDomain name = Ast.DeclDomain (Upper (Js.to_string name))

       method declAlias name typeExpr =
         Ast.DeclAlias (Upper (Js.to_string name), typeExpr)

       method declRule name params guards returnType =
         Ast.DeclRule
           {
             name = Lower (Js.to_string name);
             params = js_array_to_list params;
             guards = js_array_to_list guards;
             return_type = returnType;
             contexts = [];
           }

       method declAction label params guards =
         Ast.DeclAction
           {
             label = Js.to_string label;
             params = js_array_to_list params;
             guards = js_array_to_list guards;
             contexts = [];
           }

       method strExpr e = Js.string (Pretty.str_expr e)
       method strTypeExpr te = Js.string (Pretty.str_type_expr te)
       method strDecl d = Js.string (Pretty.str_declaration d)

       method substituteBinder expr name replacement =
         Wasm_expr.subst_var (Js.to_string name) replacement expr
    end)
