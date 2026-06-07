(* @archlint.module stateTest
   @archlint.domain pantagruel.cli *)

open Alcotest
open Pantagruel

let cli_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"cli error formatting remains stable under mutable sequencing"
         ~count:100 (QCheck2.Gen.list QCheck2.Gen.string_printable)
         (fun names ->
           let seen = ref [] in
           List.iter
             (fun name ->
               let msg =
                 Cli_errors.format_module_error (Module.ModuleNotFound name)
               in
               seen := msg :: !seen)
             names;
           List.length !seen = List.length names
           && Cli_errors.format_module_error
                (Module.CyclicImport [ "A"; "B"; "A" ])
              = "error: Cyclic import: A -> B -> A"
           && Cli_errors.format_module_error (Module.ParseError ("x", "bad"))
              = "bad"));
  ]

let () = run "Cli_state" [ ("cli", cli_properties) ]
