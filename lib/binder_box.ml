(* @archlint.module core
   @archlint.domain pantagruel.binder *)

type 'a t = 'a Bindlib.box

let pure value = Bindlib.box value
let var v = Bindlib.box_var v
let apply f value = Bindlib.box_apply f value
let apply2 f left right = Bindlib.box_apply2 f left right
let list values = Bindlib.box_list values
let array values = Bindlib.box_array values
let unbox value = Bindlib.unbox value
