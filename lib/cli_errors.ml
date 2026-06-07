(* @archlint.module core
   @archlint.domain pantagruel.cli *)

(** Pure formatting for CLI-facing module errors. *)

let format_module_error = function
  | Module.ModuleNotFound name ->
      Printf.sprintf "error: Module '%s' not found" name
  | Module.CyclicImport modules ->
      Printf.sprintf "error: Cyclic import: %s" (String.concat " -> " modules)
  | Module.ParseError (_, msg) -> msg
