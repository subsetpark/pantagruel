(* @archlint.module test
   @archlint.domain pantagruel.wasm-document *)

open Alcotest
open Pantagruel

let gen_upper =
  QCheck2.Gen.string_size
    ~gen:QCheck2.Gen.(char_range 'A' 'Z')
    (QCheck2.Gen.int_range 1 10)

let gen_lower =
  QCheck2.Gen.map
    (fun s -> "x" ^ s)
    (QCheck2.Gen.string_size
       ~gen:QCheck2.Gen.(char_range 'a' 'z')
       (QCheck2.Gen.int_range 1 10))

let wasm_document_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make
         ~name:"wasm document services parse and typecheck strings" ~count:100
         (QCheck2.Gen.triple gen_upper gen_upper gen_lower)
         (fun (main_mod, dep_mod, rule_name) ->
           let dep_mod =
             if String.equal main_mod dep_mod then dep_mod ^ "DEP" else dep_mod
           in
           let valid_doc =
             Printf.sprintf "module %s.\n\nUser.\n%s => Bool.\n---\n%s.\n"
               main_mod rule_name rule_name
           in
           let dep_doc =
             Printf.sprintf "module %s.\n\nUser.\n%s => Bool.\n---\n%s.\n"
               dep_mod rule_name rule_name
           in
           let consumer_doc =
             Printf.sprintf "module %s.\n\nimport %s.\n\nUser.\n---\n%s::%s.\n"
               main_mod dep_mod dep_mod rule_name
           in
           (match Wasm_document.parse_expr_string (rule_name ^ ".") with
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
               [ (dep_mod, dep_doc) ]
           with
           | None -> true
           | Some msg -> QCheck2.Test.fail_report msg));
  ]

let () =
  run "Wasm_document_core" [ ("wasm_document", wasm_document_properties) ]
