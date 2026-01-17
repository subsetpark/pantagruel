(** Utility functions *)

(** Sequence results: turn a list of results into a result of list *)
let rec sequence_results = function
  | [] -> Ok []
  | Ok x :: rest ->
      (match sequence_results rest with
       | Ok xs -> Ok (x :: xs)
       | Error e -> Error e)
  | Error e :: _ -> Error e

(** Map with result *)
let map_result f xs =
  sequence_results (List.map f xs)

(** Fold with result *)
let fold_result f init xs =
  List.fold_left (fun acc x ->
    match acc with
    | Error e -> Error e
    | Ok a -> f a x)
    (Ok init) xs

(** Option result bind *)
let ( let* ) = Result.bind
