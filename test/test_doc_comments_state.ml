(* @archlint.module stateTest
   @archlint.domain pantagruel.doc-comments *)

open Alcotest
open Pantagruel

let position ~line ~bol ~cnum =
  Lexing.{ pos_fname = ""; pos_lnum = line; pos_bol = bol; pos_cnum = cnum }

let position_properties =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"position keys use line and visual column"
         ~count:1000
         (QCheck2.Gen.pair QCheck2.Gen.nat_small QCheck2.Gen.nat_small)
         (fun (line, col) ->
           let pos = position ~line ~bol:7 ~cnum:(7 + col) in
           Doc_comment_positions.key line col
           = Doc_comment_positions.key_at_pos pos));
  ]

let state_interleavings =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~name:"doc comment state keeps latest positions"
         ~count:1000
         (QCheck2.Gen.list
            (QCheck2.Gen.pair QCheck2.Gen.nat_small QCheck2.Gen.nat_small))
         (fun coords ->
           Doc_comments.clear ();
           let docs = [ [ "doc" ] ] in
           List.iter
             (fun (line, col) ->
               let line, col = Doc_comment_positions.key line col in
               Doc_comments.add line col docs false)
             coords;
           let all_present =
             List.for_all
               (fun (line, col) ->
                 let line, col = Doc_comment_positions.key line col in
                 Doc_comments.get line col = (docs, false))
               coords
           in
           Doc_comments.clear ();
           all_present
           && List.for_all
                (fun (line, col) ->
                  let line, col = Doc_comment_positions.key line col in
                  Doc_comments.get line col = ([], true))
                coords));
  ]

let () =
  run "Doc_comments_state"
    [ ("positions", position_properties); ("state", state_interleavings) ]
