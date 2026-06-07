(* @archlint.module core
   @archlint.domain pantagruel.html-title *)

(** Pure title extraction for generated HTML documents. *)

let module_name_from_first_line line =
  let line = String.trim line in
  if
    String.length line > 8
    && String.sub line 0 7 = "module "
    && line.[String.length line - 1] = '.'
  then Some (String.sub line 7 (String.length line - 8))
  else None
