(* @archlint.module test
   @archlint.domain pantagruel.wasm-document *)

open Alcotest
open Pantagruel

let valid_doc = "module MAIN.\n\nUser.\nok => Bool.\n---\nok.\n"
let dep_doc = "module LIB.\n\nUser.\nok => Bool.\n---\nok.\n"
let consumer_doc = "module MAIN.\n\nimport LIB.\n\nUser.\n---\nLIB::ok.\n"

let wasm_document_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"wasm document services parse and typecheck strings" ~count:100
         QCheck2.Gen.unit (fun () ->
           (match Wasm_document.parse_expr_string "true." with
           | Ok _ -> ()
           | Error msg -> QCheck2.Test.fail_report msg);
           (match Wasm_document.parse_document_string "<test>" valid_doc with
           | Ok _ -> ()
           | Error msg -> QCheck2.Test.fail_report msg);
           (match Wasm_document.check_document_string valid_doc with
           | None -> ()
           | Some msg -> QCheck2.Test.fail_report msg);
           if
             Wasm_document.format_module_error (Module.ModuleNotFound "Missing")
             <> "error: module not found: Missing"
           then QCheck2.Test.fail_report "unexpected module error formatting";
           match
             Wasm_document.check_document_with_deps consumer_doc
               [ ("LIB", dep_doc) ]
           with
           | None -> true
           | Some msg -> QCheck2.Test.fail_report msg));
  ]

let () =
  run "Wasm_document_core" [ ("wasm_document", wasm_document_properties) ]
