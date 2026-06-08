(* @archlint.module core
   @archlint.domain pantagruel.binder *)

type ('a, 'b) t = ('a, 'b) Bindlib.mbinder

let bind vars body = Bindlib.bind_mvar vars body
let unbind binder = Bindlib.unmbind binder
let subst binder values = Bindlib.msubst binder values
let arity binder = Bindlib.mbinder_arity binder
let names binder = Bindlib.mbinder_names binder
let equal equal_body left right = Bindlib.eq_mbinder equal_body left right

let free_vars body_fvs m =
  let bound, body = Bindlib.unmbind m in
  let body_fvs = body_fvs body in
  List.filter
    (fun v -> not (Array.exists (fun b -> Bindlib.eq_vars v b) bound))
    body_fvs
