(* @archlint.module core
   @archlint.domain pantagruel.lower-aliases *)

(** Identity-lowering of list-typed aliases.

    A type alias [T = [D]] is normally expanded to the SMT sort [Array D Bool].
    When the spec never destructures a value of type [T] as a list, [T] is used
    purely as an opaque identity, and lowering it to a domain sort (a) is
    faithful to that usage and (b) keeps the SMT encoding out of the
    arrays-indexed-by-arrays regime, which the solver cannot decide (a
    list-of-[T] would otherwise become [Array (Array D Bool) Bool]).

    Detection is delegated to the type checker rather than a bespoke syntactic
    analysis: tentatively rewriting [T] to a domain and re-checking the document
    is a sound oracle — any list operation on a now-opaque [T] (membership,
    cardinality, comprehension source) becomes a type error. See
    [Module.lower_identity_aliases] for the oracle loop. *)

open Ast

(** Names of aliases whose definition is a list type ([T = [D]]) — the
    candidates for identity lowering. *)
let list_alias_candidates (doc : document) : string list =
  List.concat_map
    (fun (ch : chapter) ->
      List.filter_map
        (fun (d : declaration located) ->
          match[@warning "-4"] d.value with
          | DeclAlias (Upper name, TList _) -> Some name
          | _ -> None)
        ch.head)
    doc.chapters

(** Rewrite each [DeclAlias (T, _)] with [T] in [names] to [DeclDomain T],
    leaving everything else untouched. *)
let rewrite_to_domains (doc : document) (names : string list) : document =
  let lower (d : declaration located) =
    match[@warning "-4"] d.value with
    | DeclAlias (Upper name, _) when List.mem name names ->
        { d with value = DeclDomain (Upper name) }
    | _ -> d
  in
  let chapters =
    List.map
      (fun (ch : chapter) -> { ch with head = List.map lower ch.head })
      doc.chapters
  in
  { doc with chapters }
