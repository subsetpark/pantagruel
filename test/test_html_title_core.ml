(* @archlint.module test
   @archlint.domain pantagruel.html-title *)

open Alcotest
open Pantagruel

let module_name_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"module_name_from_first_line accepts headers"
         ~count:100
         (QCheck2.Gen.string_size
            ~gen:QCheck2.Gen.(char_range 'A' 'Z')
            (QCheck2.Gen.int_range 1 12))
         (fun name ->
           let line = "module " ^ name ^ "." in
           Html_title.module_name_from_first_line line = Some name
           && Html_title.module_name_from_first_line ("  " ^ line ^ "  ")
              = Some name
           && Html_title.module_name_from_first_line ("module " ^ name) = None
           && Html_title.module_name_from_first_line ("type " ^ name ^ ".")
              = None));
  ]

let () = run "Html_title_core" [ ("module_name", module_name_properties) ]
