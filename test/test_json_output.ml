(* @archlint.module test
   @archlint.domain pantagruel.json-output *)

(** Tests for JSON serialization *)

open Alcotest
open Pantagruel

let parse_and_collect = Test_util.parse_and_collect
let has_key key j = Yojson.Basic.Util.member key j <> `Null

let json_for source =
  let env, doc = parse_and_collect source in
  Json_output.document_to_json env doc

let test_document_to_json_top_level_shape () =
  let j =
    json_for
      "module TEST.\n\
       context Ctx.\n\n\
       User.\n\
       {Ctx} name u: User => String.\n\
       ---\n\
       all u: User | true.\n"
  in
  check bool "has module" true (has_key "module" j);
  check bool "has imports" true (has_key "imports" j);
  check bool "has contexts" true (has_key "contexts" j);
  check bool "has types" true (has_key "types" j);
  check bool "has rules" true (has_key "rules" j);
  check bool "has chapters" true (has_key "chapters" j);
  check string "module name" "TEST"
    (Yojson.Basic.Util.member "module" j |> Yojson.Basic.Util.to_string);
  check int "chapter count" 1
    (Yojson.Basic.Util.member "chapters" j
    |> Yojson.Basic.Util.to_list |> List.length)

let test_document_to_json_declaration_kinds () =
  let j =
    json_for
      "module T.\n\n\
       User.\n\
       Point = Nat * Nat.\n\
       f u: User => Bool.\n\
       ~> Do thing @ u: User.\n\
       ---\n"
  in
  let chapters =
    Yojson.Basic.Util.member "chapters" j |> Yojson.Basic.Util.to_list
  in
  let head =
    List.hd chapters
    |> Yojson.Basic.Util.member "head"
    |> Yojson.Basic.Util.to_list
  in
  let kinds =
    List.map
      (fun decl ->
        Yojson.Basic.Util.member "kind" decl |> Yojson.Basic.Util.to_string)
      head
  in
  check (list string) "declaration kinds"
    [ "domain"; "alias"; "rule"; "action" ]
    kinds

let test_document_to_json_expression_shapes () =
  let j =
    json_for
      "module T.\n\n\
       User.\n\
       owns u: User => Bool.\n\
       ---\n\
       all u: User | owns u.\n\
       check\n\
       all u: User | owns u.\n"
  in
  let chapters =
    Yojson.Basic.Util.member "chapters" j |> Yojson.Basic.Util.to_list
  in
  let chapter = List.hd chapters in
  check bool "body present" true (has_key "body" chapter);
  check bool "checks present" true (has_key "checks" chapter)

let generated_sources =
  [
    "module G.\n\nA.\n---\n";
    "module G.\n\nA.\nf a: A => Bool.\n---\nall a: A | f a.\n";
    "module G.\n\nA.\nPair = A * A.\n---\n";
    "module G.\n\ncontext C.\nA.\n{C} f a: A => Bool.\n---\n";
  ]

let test_document_to_json_property =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"document_to_json emits required top-level keys"
       ~count:100 ~print:Fun.id (QCheck2.Gen.oneof_list generated_sources)
       (fun source ->
         let j = json_for source in
         List.for_all
           (fun key -> has_key key j)
           [ "module"; "imports"; "contexts"; "types"; "rules"; "chapters" ]))

let () =
  run "Json_output"
    [
      ( "document_to_json",
        [
          test_case "top-level shape" `Quick
            test_document_to_json_top_level_shape;
          test_case "declaration kinds" `Quick
            test_document_to_json_declaration_kinds;
          test_case "expression shapes" `Quick
            test_document_to_json_expression_shapes;
        ] );
      ("property", [ test_document_to_json_property ]);
    ]
