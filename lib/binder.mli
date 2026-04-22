(** Binder adapter for Pantagruel.

    This module is the single seam between Pantagruel and its underlying binder
    library. The Bindlib backend is re-exported under project-local names; no
    raw [Bindlib] types leak through this interface.

    The adapter API is deliberately restricted to primitives that both Bindlib
    and a hand-rolled locally-nameless representation can support:
    - [Var.make] / [Var.name] / [Var.equal] / [Var.compare]
    - [Mbinder.bind] / [unbind] / [subst] / [arity] / [names]
    - alpha-aware [Mbinder.equal]
    - [Mbinder.free_vars] parameterised over a body free-variable function

    The [Box] type is needed to construct binders under the Bindlib backend. For
    a locally-nameless backend, [Box] would collapse to the identity and its
    operations would be trivial wrappers. *)

(** {2 Variables} *)

module Var : sig
  type 'a t
  (** Type of a free variable carrying a value of type ['a]. *)

  val make : ('a t -> 'a) -> string -> 'a t
  (** [make mkfree name] creates a fresh variable. The [mkfree] function injects
      a variable into the carrier type ['a] (typically a constructor of the AST
      type). *)

  val name : 'a t -> string
  (** [name v] returns the preferred name of [v]. This name is not guaranteed to
      be collision-free; use [Mbinder.unbind] inside a renaming context when
      converting binders to text. *)

  val equal : 'a t -> 'a t -> bool
  (** [equal v1 v2] compares variables by unique identity. *)

  val compare : 'a t -> 'a t -> int
  (** [compare v1 v2] gives a total order on variables by unique identity. *)
end

(** {2 Boxed terms}

    Terms with free variables that are to be bound must first be placed in the
    [Box.t] type. This reifies the free-variable structure so the library can
    weave it through [Mbinder.bind]. *)

module Box : sig
  type 'a t
  (** Type of a term of type ['a] under construction. *)

  val pure : 'a -> 'a t
  (** [pure x] lifts a closed value into the box. *)

  val var : 'a Var.t -> 'a t
  (** [var v] lifts a variable into the box. *)

  val apply : ('a -> 'b) -> 'a t -> 'b t
  (** [apply f b] applies a function to a boxed value. *)

  val apply2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [apply2 f b1 b2] applies a binary function to two boxed values. *)

  val list : 'a t list -> 'a list t
  (** [list bs] lifts a list of boxed values to a boxed list. *)

  val array : 'a t array -> 'a array t
  (** [array bs] lifts an array of boxed values to a boxed array. *)

  val unbox : 'a t -> 'a
  (** [unbox b] closes a boxed term. All intended bindings must already have
      been introduced; any remaining free variables become meaningful. *)
end

(** {2 Multi-variable binders} *)

module Mbinder : sig
  type ('a, 'b) t
  (** A binder for an array of ['a]-valued variables in a body of type ['b]. *)

  val bind : 'a Var.t array -> 'b Box.t -> ('a, 'b) t Box.t
  (** [bind vs body] binds every variable in [vs] in [body], returning a boxed
      [mbinder]. Call [Box.unbox] once all desired bindings have been
      introduced. *)

  val unbind : ('a, 'b) t -> 'a Var.t array * 'b
  (** [unbind b] opens [b] with a fresh array of variables. *)

  val subst : ('a, 'b) t -> 'a array -> 'b
  (** [subst b vs] substitutes the bound variables with the values [vs]. The
      length of [vs] must equal [arity b]. *)

  val arity : ('a, 'b) t -> int
  (** [arity b] is the number of variables bound by [b]. *)

  val names : ('a, 'b) t -> string array
  (** [names b] returns the preferred names of the variables bound by [b]. As
      with [Var.name], these are not guaranteed to be collision-free. *)

  val equal : ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
  (** [equal eq b1 b2] tests alpha-equivalence of [b1] and [b2]. The binders
      must have the same arity; their bodies are opened with the same fresh
      variables and compared with [eq]. *)

  val free_vars : ('b -> 'a Var.t list) -> ('a, 'b) t -> 'a Var.t list
  (** [free_vars body_fvs b] returns the free variables of [b]'s body, excluding
      those bound by [b]. The [body_fvs] function must extract the free
      variables of a body of type ['b]. *)
end
