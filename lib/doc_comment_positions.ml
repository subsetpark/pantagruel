(* @archlint.module core
   @archlint.domain pantagruel.doc-comments *)

(** Pure keys used by doc comment storage. *)

let key line col = (line, col)

let key_at_pos pos =
  key pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
