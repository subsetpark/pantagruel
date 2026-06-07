(* @archlint.module test
   @archlint.domain pantagruel.module-header *)

open Alcotest
open Pantagruel

let supplier tokens =
  let remaining = ref tokens in
  fun () ->
    match !remaining with
    | tok :: rest ->
        remaining := rest;
        tok
    | [] -> Parser.EOF

let module_header_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"module header tokens recognize valid headers"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Module_header.parse_module_header_tokens
             (supplier [ Parser.MODULE; Parser.UPPER_IDENT "DEMO"; Parser.DOT ])
           = Ok "DEMO"
           && Module_header.parse_module_header_tokens
                (supplier
                   [
                     Parser.UPPER_IDENT "Skip";
                     Parser.DOT;
                     Parser.MODULE;
                     Parser.UPPER_IDENT "M";
                     Parser.DOT;
                   ])
              = Ok "M"
           && Result.is_error
                (Module_header.parse_module_header_tokens
                   (supplier
                      [ Parser.MODULE; Parser.UPPER_IDENT "BROKEN"; Parser.EOF ]))));
  ]

let () =
  run "Module_header_core" [ ("module_header", module_header_properties) ]
