(* @archlint.module test
   @archlint.domain pantagruel.util *)

open Alcotest
open Pantagruel

let print_int_list xs =
  "[" ^ String.concat "; " (List.map string_of_int xs) ^ "]"

let print_result_value = function
  | Ok x -> "Ok " ^ string_of_int x
  | Error e -> "Error " ^ string_of_int e

let gen_int = QCheck2.Gen.int_range (-20) 20
let gen_int_list = QCheck2.Gen.list_small gen_int

let gen_result_list =
  QCheck2.Gen.list_small
    (QCheck2.Gen.oneof
       [
         QCheck2.Gen.map (fun x -> Ok x) gen_int;
         QCheck2.Gen.map (fun e -> Error e) gen_int;
       ])

let first_error xs =
  List.find_map (function Ok _ -> None | Error e -> Some e) xs

let test_sequence_results_matches_list_projection =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"sequence_results preserves order or first error"
       ~count:500
       ~print:(fun xs ->
         "[" ^ String.concat "; " (List.map print_result_value xs) ^ "]")
       gen_result_list
       (fun xs ->
         match[@warning "-4"] (Util.sequence_results xs, first_error xs) with
         | Ok values, None -> values = List.filter_map Result.to_option xs
         | Error e, Some first -> e = first
         | _ -> false))

let test_map_result_matches_map_then_sequence =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"map_result matches map then sequence" ~count:500
       ~print:print_int_list gen_int_list (fun xs ->
         let f x = if x mod 5 = 0 then Error x else Ok (x + 1) in
         Util.map_result f xs = Util.sequence_results (List.map f xs)))

let test_fold_result_matches_left_fold =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~name:"fold_result short-circuits like result fold"
       ~count:500 ~print:print_int_list gen_int_list (fun xs ->
         let f acc x = if x < 0 then Error x else Ok (acc + x) in
         let expected =
           List.fold_left
             (fun acc x ->
               match acc with Error e -> Error e | Ok total -> f total x)
             (Ok 0) xs
         in
         Util.fold_result f 0 xs = expected))

let () =
  run "Util"
    [
      ( "property",
        [
          test_sequence_results_matches_list_projection;
          test_map_result_matches_map_then_sequence;
          test_fold_result_matches_left_fold;
        ] );
    ]
