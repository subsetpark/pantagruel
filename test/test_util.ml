(** Shared test utilities *)

open Alcotest
open Pantagruel

let parse str =
  let lexer = Lexer.create_from_string "<test>" str in
  let supplier = Lexer.menhir_token lexer in
  MenhirLib.Convert.Simplified.traditional2revised Parser.document supplier

let parse_and_collect str =
  let doc = parse str in
  match
    Collect.collect_all
      ~base_env:(Env.empty (Option.value ~default:"" doc.module_name))
      doc
  with
  | Error e -> failf "Collection error: %s" (Collect.show_collect_error e)
  | Ok env -> (
      match Check.check_document env doc with
      | Error e -> failf "Type error: %s" (Check.show_type_error e)
      | Ok _warnings -> (env, doc))

(** QCheck generator for Types.ty, depth-limited *)
let[@warning "-44"] gen_ty_at_depth =
  let open QCheck.Gen in
  let base =
    oneof
      [
        return Types.TyBool;
        return Types.TyNat;
        return Types.TyNat0;
        return Types.TyInt;
        return Types.TyReal;
        return Types.TyString;
        return Types.TyNothing;
        return (Types.TyDomain "X");
        return (Types.TyDomain "Y");
      ]
  in
  fix (fun self n ->
      if n <= 0 then base
      else
        oneof_weighted
          [
            (5, base);
            (1, map (fun t -> Types.TyList t) (self (n - 1)));
            ( 1,
              map2
                (fun a b -> Types.TyProduct [ a; b ])
                (self (n - 1))
                (self (n - 1)) );
            ( 1,
              map2
                (fun a b -> Types.TySum [ a; b ])
                (self (n - 1))
                (self (n - 1)) );
          ])

let gen_ty = gen_ty_at_depth 2
let arb_ty = QCheck.make ~print:Types.format_ty gen_ty

(** QCheck generator for Ast.type_expr, depth-limited *)
let[@warning "-44"] gen_type_expr_at_depth =
  let open QCheck.Gen in
  let names = [ "Nat"; "Bool"; "Int"; "String"; "Real"; "Nothing" ] in
  let base = oneof_list (List.map (fun n -> Ast.TName n) names) in
  fix (fun self n ->
      if n <= 0 then base
      else
        oneof_weighted
          [
            (4, base);
            (1, map (fun t -> Ast.TList t) (self (n - 1)));
            ( 1,
              map2
                (fun a b -> Ast.TProduct [ a; b ])
                (self (n - 1))
                (self (n - 1)) );
            ( 1,
              map2 (fun a b -> Ast.TSum [ a; b ]) (self (n - 1)) (self (n - 1))
            );
          ])

let gen_type_expr = gen_type_expr_at_depth 2
let arb_type_expr = QCheck.make ~print:Pretty.str_type_expr gen_type_expr
