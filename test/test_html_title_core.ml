(* @archlint.module test
   @archlint.domain pantagruel.html-title *)

open Alcotest
open Pantagruel

let module_name_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"module_name_from_first_line accepts headers"
         ~count:100 QCheck2.Gen.unit (fun () ->
           Html_title.module_name_from_first_line "module DEMO." = Some "DEMO"
           && Html_title.module_name_from_first_line "  module DEMO.  "
              = Some "DEMO"
           && Html_title.module_name_from_first_line "module DEMO" = None
           && Html_title.module_name_from_first_line "type DEMO." = None));
  ]

let () = run "Html_title_core" [ ("module_name", module_name_properties) ]
