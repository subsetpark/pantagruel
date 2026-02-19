(** Doc comment storage - separate module to avoid dependency cycles *)

(** Map (line, col) -> (doc comment groups, adjacent to next token) *)
let doc_map : (int * int, string list list * bool) Hashtbl.t = Hashtbl.create 16

(** Clear the doc map (call before parsing a new file) *)
let clear () = Hashtbl.clear doc_map

(** Add docs at a position. [adjacent] is true when the last doc line is on
    the line directly above the token (no blank line between them). *)
let add line col docs adjacent =
  if docs <> [] then Hashtbl.add doc_map (line, col) (docs, adjacent)

(** Look up docs at a position. Returns (groups, adjacent). *)
let get line col =
  match Hashtbl.find_opt doc_map (line, col) with
  | Some result -> result
  | None -> ([], true)

(** Look up docs by Lexing.position *)
let get_at_pos pos =
  get pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
