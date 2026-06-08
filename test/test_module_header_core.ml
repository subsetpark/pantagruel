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

let gen_upper =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'A' 'Z')
    (QCheck2.Gen.int_range 1 10)

let module_header_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"module header tokens recognize valid headers"
         ~count:100 (QCheck2.Gen.pair gen_upper gen_upper)
         (fun (first, second) ->
           Module_header.parse_module_header_tokens
             (supplier [ Parser.MODULE; Parser.UPPER_IDENT first; Parser.DOT ])
           = Ok first
           && Module_header.parse_module_header_tokens
                (supplier
                   [
                     Parser.UPPER_IDENT "Skip";
                     Parser.DOT;
                     Parser.MODULE;
                     Parser.UPPER_IDENT second;
                     Parser.DOT;
                   ])
              = Ok second
           && Result.is_error
                (Module_header.parse_module_header_tokens
                   (supplier
                      [ Parser.MODULE; Parser.UPPER_IDENT first; Parser.EOF ]))));
  ]

let () =
  run "Module_header_core" [ ("module_header", module_header_properties) ]
