(* @archlint.module interface
   @archlint.domain pantagruel.contracts *)

(** JSON serialization for Pantagruel documents *)

val document_to_json : Env.t -> Ast.document -> Yojson.Basic.t
