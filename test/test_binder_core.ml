(* @archlint.module test
   @archlint.domain pantagruel.binder *)

open Alcotest
open Pantagruel

type expr = Var of expr Binder_var.t | Lit of string | Pair of expr * expr

let mkfree v = Var v

let rec equal_expr left right =
  match (left, right) with
  | Var l, Var r -> Binder_var.equal l r
  | Lit l, Lit r -> String.equal l r
  | Pair (ll, lr), Pair (rl, rr) -> equal_expr ll rl && equal_expr lr rr
  | Var _, (Lit _ | Pair _) | Lit _, (Var _ | Pair _) | Pair _, (Var _ | Lit _)
    ->
      false

let gen_name = QCheck2.Gen.map (Printf.sprintf "v_%d") QCheck2.Gen.nat_small

let binder_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"var wrapper exposes Bindlib identity" ~count:100
         gen_name (fun name ->
           let v = Binder_var.make mkfree name in
           Binder_var.name v = name
           && Binder_var.equal v v
           && Binder_var.compare v v = 0));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"box wrapper composes pure, var, list, and array"
         ~count:100 gen_name (fun name ->
           let v = Binder_var.make mkfree name in
           let boxed_pair =
             Binder_box.apply2
               (fun l r -> Pair (l, r))
               (Binder_box.var v)
               (Binder_box.pure (Lit "rhs"))
           in
           let boxed_list =
             Binder_box.list [ Binder_box.var v; Binder_box.pure (Lit "tail") ]
           in
           let boxed_array =
             Binder_box.array
               [| Binder_box.var v; Binder_box.pure (Lit "tail") |]
           in
           let boxed_mapped =
             Binder_box.apply
               (fun e -> Pair (e, Lit "mapped"))
               (Binder_box.var v)
           in
           equal_expr (Binder_box.unbox boxed_pair) (Pair (Var v, Lit "rhs"))
           && List.equal equal_expr
                (Binder_box.unbox boxed_list)
                [ Var v; Lit "tail" ]
           && Array.for_all2 equal_expr
                (Binder_box.unbox boxed_array)
                [| Var v; Lit "tail" |]
           && equal_expr
                (Binder_box.unbox boxed_mapped)
                (Pair (Var v, Lit "mapped"))));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"mbinder wrapper binds, unbinds, substitutes, and filters fvs"
         ~count:100 gen_name (fun name ->
           let v = Binder_var.make mkfree name in
           let mb =
             Binder_mbinder.bind [| v |] (Binder_box.var v) |> Binder_box.unbox
           in
           let vars, body = Binder_mbinder.unbind mb in
           let subst = Binder_mbinder.subst mb [| Lit "replacement" |] in
           let free =
             Binder_mbinder.free_vars
               (fun e -> match e with Var v -> [ v ] | Lit _ | Pair _ -> [])
               mb
           in
           Array.length vars = 1
           && Binder_var.name vars.(0) = name
           && equal_expr body (Var vars.(0))
           && equal_expr subst (Lit "replacement")
           && Binder_mbinder.arity mb = 1
           && Binder_mbinder.names mb = [| name |]
           && Binder_mbinder.equal equal_expr mb mb
           && free = []));
  ]

let () = run "Binder_core" [ ("binder", binder_properties) ]
