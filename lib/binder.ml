module Var = struct
  type 'a t = 'a Bindlib.var

  let make = Bindlib.new_var
  let name = Bindlib.name_of
  let equal = Bindlib.eq_vars
  let compare = Bindlib.compare_vars
end

module Box = struct
  type 'a t = 'a Bindlib.box

  let pure = Bindlib.box
  let var = Bindlib.box_var
  let apply = Bindlib.box_apply
  let apply2 = Bindlib.box_apply2
  let list = Bindlib.box_list
  let array = Bindlib.box_array
  let unbox = Bindlib.unbox
end

module Mbinder = struct
  type ('a, 'b) t = ('a, 'b) Bindlib.mbinder

  let bind = Bindlib.bind_mvar
  let unbind = Bindlib.unmbind
  let subst = Bindlib.msubst
  let arity = Bindlib.mbinder_arity
  let names = Bindlib.mbinder_names
  let equal = Bindlib.eq_mbinder

  let free_vars body_fvs m =
    let bound, body = Bindlib.unmbind m in
    let body_fvs = body_fvs body in
    List.filter
      (fun v -> not (Array.exists (fun b -> Bindlib.eq_vars v b) bound))
      body_fvs
end
