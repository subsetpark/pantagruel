(** Utility functions *)

(** Sequence results: turn a list of results into a result of list *)
let sequence_results xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | Ok x :: rest -> go (x :: acc) rest
    | Error e :: _ -> Error e
  in
  go [] xs

(** Map with result, short-circuiting on first error *)
let map_result f xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest ->
        match f x with
        | Ok y -> go (y :: acc) rest
        | Error e -> Error e
  in
  go [] xs

(** Fold with result *)
let fold_result f init xs =
  List.fold_left (fun acc x ->
    match acc with
    | Error e -> Error e
    | Ok a -> f a x)
    (Ok init) xs

(** Option result bind *)
let ( let* ) = Result.bind
