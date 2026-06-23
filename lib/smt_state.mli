(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** Stateful SMT auxiliary symbol allocation. *)

val reset_cond_aux : unit -> unit
val fresh_cond_default : string -> string
val drain_cond_aux_decls : unit -> string
val insert_cond_aux_decls : string -> string
val reset_fallbacks : unit -> unit
val fresh_fallback : kind:string -> sort:string -> string
val reset_list_search_cache : unit -> unit
val intern_list_search_symbol : func_s:string -> arg_s:string -> string

val intern_list_index_symbol :
  func_s:string -> arg_s:string -> sort:string -> string

val intern_card_symbol : expr_s:string -> string
val add_fallback_assert : string -> unit
val drain_fallback_decls : unit -> string
val insert_fallback_decls : string -> string
val with_cond_aux : (unit -> Smt_types.query) -> Smt_types.query
