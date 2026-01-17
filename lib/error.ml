(** Error types and reporting *)

open Ast

type error =
  | LexError of loc * string
  | ParseError of loc * string
  | TypeError of loc * string
  | ModuleError of string
  | CollectError of loc * string
[@@deriving show]

let format_loc loc =
  Printf.sprintf "%s:%d:%d" loc.file loc.line loc.col

let format_error = function
  | LexError (loc, msg) ->
      Printf.sprintf "%s: Lexer error: %s" (format_loc loc) msg
  | ParseError (loc, msg) ->
      Printf.sprintf "%s: Parse error: %s" (format_loc loc) msg
  | TypeError (loc, msg) ->
      Printf.sprintf "%s: Type error: %s" (format_loc loc) msg
  | ModuleError msg ->
      Printf.sprintf "Module error: %s" msg
  | CollectError (loc, msg) ->
      Printf.sprintf "%s: %s" (format_loc loc) msg

let report_error err =
  prerr_endline (format_error err)

let report_errors errs =
  List.iter report_error errs
