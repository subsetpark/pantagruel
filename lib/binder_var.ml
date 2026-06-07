(* @archlint.module core
   @archlint.domain pantagruel.binder *)

type 'a t = 'a Bindlib.var

let make mkfree name = Bindlib.new_var mkfree name
let name v = Bindlib.name_of v
let equal left right = Bindlib.eq_vars left right
let compare left right = Bindlib.compare_vars left right
