(** Doc comment storage - separate module to avoid dependency cycles *)

(** Map (line, col) -> doc comments *)
let doc_map : (int * int, string list) Hashtbl.t = Hashtbl.create 16

(** Clear the doc map (call before parsing a new file) *)
let clear () = Hashtbl.clear doc_map

(** Add docs at a position *)
let add line col docs =
  if docs <> [] then
    Hashtbl.add doc_map (line, col) docs

(** Look up docs at a position *)
let get line col =
  match Hashtbl.find_opt doc_map (line, col) with
  | Some docs -> docs
  | None -> []

(** Look up docs by Lexing.position *)
let get_at_pos pos =
  get pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
