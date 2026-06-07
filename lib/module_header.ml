(* @archlint.module core
   @archlint.domain pantagruel.module-header *)

(** Pure recognition of Pantagruel module headers. *)

let parse_module_header_tokens next_token =
  let rec find_module () =
    match[@warning "-4"] next_token () with
    | Parser.MODULE -> (
        match[@warning "-4"] next_token () with
        | Parser.UPPER_IDENT name -> (
            match[@warning "-4"] next_token () with
            | Parser.DOT -> Ok name
            | _ -> Error "Expected '.' after module name")
        | _ -> Error "Expected module name")
    | Parser.EOF -> Error "No module declaration found"
    | _ -> find_module ()
  in
  find_module ()
