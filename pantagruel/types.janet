## Type logic.
##
## All logic concerned with type algebra and resolution: determining the type
## of all valid typed expressions, and the entire algebra available for
## manipulating types.
##
## The main entry point is `resolve-type`, which will take an AST element and
## return its type in the context of a fully-populated environment. However,
## the evaluation engine, responsible for the population of that environment,
## also calls into this logic for types that are fully resolvable without an
## environment.

(import /pantagruel/stdlib :prefix "")

(def ResolutionError @{})

(defn- throw
  ```
  Handle type errors encountered during type resolution.

  This doesn't include errors or gaps in type resolution logic, which will be
  raised immediately.
  ```
  [t &opt vars]
  (default vars @{})
  (error (table/setproto (merge vars @{:type t}) ResolutionError)))

(defn- is-in-hierarchy?
  ```
  Determine if type `haystack` is a descendent of type `needle`.
  ```
  [needle haystack]
  (cond
    (not (table? haystack)) false
    (= needle haystack) true

    (match (table/getproto haystack)
      'nil false
      'needle true
      proto (is-in-hierarchy? needle proto))))

(defn- gcd-type
  ```
  Type unification logic.

  For any two types, find the "greatest common denominator", that is, the
  narrowest type, if any, that is common to the hierarchy of both.

  This explicity excludes Any, which is present in the hierarchy of all types,
  unless one of the two types *is* itself Any. That is: Any is the greatest
  common denominator of itself and any other type, but it's not the greatest
  common denominator of two non-Any types.

  User-defined types always behave as though they are directly descended from
  Any. Built-in types have a more elaborate hierarchy, allowing for meaningful
  interactions between, for instance, different members of the numeric tower.
  ```
  [left right]

  (defn find-gcd-
    ```
    Basic type unification. Recursively seek the "shallowest" type present in
    the hierarchies of both `t` and `t2`, where `n` is the depth of the search.
    ```
    [t t2 n]
    (or (and (= t t2) [n t])
        (let [left (and (table? t2)
                        (find-gcd- t (table/getproto t2) (inc n)))
              right (and (table? t)
                         (find-gcd- (table/getproto t) t2 (inc n)))]
          (extreme (fn [x y]
                     (cond
                       (and x (not y)) true
                       (and y (not x)) false
                       (and x y) (<= (x 0) (y 0))))
                   [left right]))))

  (defn find-gcd
    ```
    Type unification logic wrapping `find-gcd-` with special handling of `Any`.
    ```
    [t t2]
    (cond
      (= t Any) Any
      (= t2 Any) Any
      (if-let [[_n gcd] (find-gcd- t t2 0)]
        (if (and gcd (not= gcd Any))
          gcd))))

  (let [gcd (match [left right]
              # Handle any 1-tuples we've received, eg, from type summing.
              ([wrapped-left _]
                (tuple? wrapped-left)
                (= 1 (length wrapped-left)))
              (gcd-type (wrapped-left 0) right)

              ([_ wrapped-right]
                (tuple? wrapped-right)
                (= 1 (length wrapped-right)))
              (gcd-type left (wrapped-right 0))

              [{:sum ts} {:sum ts}] {:sum ts}
              # TODO: This handles identical sum types; handle the other cases.
              [{:sum _} {:sum _}] (errorf "Couldn't unify non-identical sum types: %q %q" left right)
              [{:sum ts} t2] (any? (map |(find-gcd $ t2) ts))
              [t {:sum t2s}] (any? (map |(find-gcd t $) t2s))

              [{:list-of t} {:set-of t2}]
              (if-let [gcd (find-gcd t t2)] {:set-of gcd})

              [{:set-of t} {:list-of t2}]
              (if-let [gcd (find-gcd t t2)] {:set-of gcd})

              [{:list-of t} {:list-of t2}]
              (if-let [gcd (find-gcd t t2)] {:list-of gcd})

              [{:set-of t} {:set-of t2}]
              (if-let [gcd (find-gcd t t2)] {:set-of gcd})

              [t t2] (find-gcd t t2))]
    (if gcd
      gcd
      (throw :gcd {:left left :right right}))))

(defn- collapse-application-args
  ```
  Procedure application is represented in the syntax tree as a series of
  single-argument applications. Given some application node, recurse through
  it, collapsing all applications into a single array, until we find some other
  node.
  ```
  [x]
  (match x
    {:kind :application
     :f arg1
     :x arg2}
    [arg1 ;(collapse-application-args arg2)]

    [x]))

(defn- is-domain-reference?
  [thunk]
  # We need to determine that we've encountered a bare symbol, in the text,
  # that is the name of a domain (including complex ones or aliases.)
  # In other words, we need to differentiate between references to domains and
  # references to bound variables that are in domains.
  (case (thunk :kind)
    # If we encounter a symbol-reference to a concrete type, or a type alias,
    # then the type of that reference is Domain (not the value it's
    # referencing).
    :domain true

    false))

(defn- fully-resolve-type
  ```
  Fully resolve the type of any expression that couldn't be resolved at
  evaluation time, because it might have referred to a symbol that was bound
  later on in the document.
  ```
  [entry env]
  (match entry
    #
    # Environment lookup cases.
    #

    # Handle symbols referred to literally in the document. This includes
    # differentiating between *references* to domains (which have the type of
    # Domain) and *instances* of domains, whose type *is* that domain.
    {:symbol-reference s}
    # Trim the string; variables like "foo'" derive their type from the
    # variables they are iterations on.
    (let [base-name (string/trim s "'")
          looked-up (env base-name)]
      (if (is-domain-reference? looked-up)
        Domain
        (fully-resolve-type looked-up env)))

    # An explicit :thunk is like a bare string - a reference to some other
    # piece of program text - but was interned during the evaluation stage.
    {:thunk {:text thunk}}
    (fully-resolve-type (env thunk) env)

    #
    # The three types of values that can be found in the environment: domains,
    # procedures, and bound variables.
    #

    {:kind :domain
     :type t}
    (fully-resolve-type t env)

    # Special-case procedures with no arguments: treat them as singletons.
    # TODO: Is this right at all?
    ({:kind :procedure
      :type {:args args
             :yields yields}}
      (= 0 (length args)) (not= yields Void))
    (fully-resolve-type yields env)

    # Main procedure case.
    {:kind :procedure
     :type {:args args
            :yields yields}}
    {:args (map |(fully-resolve-type $ env) args)
     :yields (fully-resolve-type yields env)}

    {:kind :bound
     :type t}
    (fully-resolve-type t env)

    {:value _
     :type t}
    (fully-resolve-type t env)

    #
    # Type components: recursive cases.
    #

    {:list-of inner}
    {:list-of (fully-resolve-type inner env)}

    {:set-of inner}
    {:set-of (fully-resolve-type inner env)}

    {:sum ts}
    {:sum (map |(fully-resolve-type $ env) ts)}

    {:product ts}
    {:product (map |(fully-resolve-type $ env) ts)}

    #
    # Base case: we've fully resolved any environment references.
    #

    {:concrete _}
    entry

    (errorf "Couldn't fully resolve type:\n%q\nin\n%q"
            entry
            (dyn :current-expression))))

(defn- application-type
  ```
  Resolve the type of a function application expression. Possible inferences are:

  - Normal procedure application
  - List application:
    - Application of a list to the type of its elements => the type of an index
      into the list
    - Application of a list to the type of an index => the type of its elements
  ```
  [f x]
  (match [f x]
    [{:args f-args :yields yields} arg-ts]
    (do
      (unless (= (length f-args) (length arg-ts))
        (throw :arg-length {:f-args f-args :args arg-ts}))
      (if (every? (map |(gcd-type $0 $1) f-args arg-ts))
        yields))

    ([{:list-of t1} ts] (= 1 (length ts)) (= t1 (ts 0)))
    Nat0

    ([{:list-of t1} ts] (= 1 (length ts)) (is-in-hierarchy? Int (ts 0)))
    t1

    ([{:list-of t1} ts] (< 1 (length ts)))
    (throw :list-application-multiple-args {:xs ts})

    [{:list-of t1} ts]
    (throw :list-application-bad-arg {:f t1 :x (ts 0)})

    # For now, Strings are special-cased here.
    ([(@ String) [t1]] (index-of t1 [Char String]))
    Nat0

    ([(@ String) ts] (= 1 (length ts)) (is-in-hierarchy? Int (ts 0)))
    Char

    (throw :application {:f f :x x})))


(defn- inner-type
  ```
  Resolve the type of a membership operation, ie, get the type of a container's
  elements.
  ```
  [t]
  (match t
    {:set-of inner-t}
    inner-t

    {:list-of inner-t}
    inner-t

    (@ String)
    Char

    (throw :container {:t t})))

(defn number-type
  ```
  Given a number, resolve the narrowest element of the number tower it is a
  member of.
  ```
  [n]
  (cond
    (and (nat? n) (> n 0)) Nat
    (nat? n) Nat0
    (int? n) Int

    Real))

(defn sum-type
  ```
    Handle sum type syntax, either:
    - Foo + Bar
    - {value1, value2}
    ```
  [left-t right-t]
  # If either side is itself a sum type, unpack it; in other words,
  # (X + Y) + Z = {X, Y, Z}.
  (let [t1 (or (left-t :sum) [left-t])
        t2 (or (right-t :sum) [right-t])
        gcd (protect (gcd-type t1 t2))]
    (cond
      # If the two types are unifiable, return the common denominator.
      (gcd 0) (gcd 1)
      # If both sides are equivalent, we don't need a sum. In other words,
      # X + X = X.
      (and (= t1 t2) (= 1 (length t1))) (t1 0)

      {:sum (distinct [;t1 ;t2])})))

(defn resolve-type
  ```
  Get the type of some AST expression when it is fully evaluated (ie, reduced).
  ```
  [expr env]
  (defn resolve-type-inner
    [expr]
    (match expr
      {:kind :application
       :f f
       :x x}
      (let [args (collapse-application-args x)]
        (application-type (resolve-type f env)
                          (map |(resolve-type $ env) args)))

      {:kind :quantification
       :expr expr}
      (resolve-type expr env)

      ({:operator boolop
        :left left} (index-of boolop boolean-operators))
      (let [right (expr :right)]
        (resolve-type left env)
        (if right (resolve-type right env))
        Bool)

      ({:operator compop
        :left left
        :right right} (index-of compop comparison-operators))
      (let [t (resolve-type left env)
            t2 (resolve-type right env)]
        (if (gcd-type t t2)
          Bool))

      {:operator "in"
       :left left
       :right right}
      (let [element-t (resolve-type left env)
            inner-t (inner-type (resolve-type right env))]
        (if (gcd-type element-t inner-t)
          Bool))

      {:operator "#"
       :left left}
      (if (inner-type (resolve-type left env))
        Nat0)

      ({:operator arithop
        :left left
        :right right} (index-of arithop arithmetic-operators))
      # TODO: Determine possible types of subtraction, etc, between different types
      (gcd-type (resolve-type left env) (resolve-type right env))

      {:mapping {:mapping-clauses mapping}}
      (do
        # If the `:case` is populated, then attempt to unify its type with the
        # types of all branch patterns.
        (when-let [test (expr :case)
                   test-type (resolve-type test env)
                   case-types (map |(resolve-type ($ :left) env) mapping)]
          (reduce2 gcd-type [test-type ;case-types]))
        # In all cases, attempt to unify the types of all branch expressions.
        (let [all-exprs (map |(resolve-type ($ :right) env) mapping)]
          (reduce2 gcd-type all-exprs)))

      {:kind :string}
      String

      {:container container
       :inner inner}
      (do
        (when (> (length inner) 1)
          # TODO: Handle this case. Is it possible?
          (errorf "Encountered parens with more than one element: %q" inner))
        (let [inner-t (resolve-type (inner 0) env)]
          (case container
            :parens inner-t
            :square {:list-of inner-t}
            :braces {:set-of inner-t})))

      {:kind :num
       :text n}
      (number-type n)

      {:kind :sym
       :text s}
      {:symbol-reference s}

      (errorf "Couldn't determine type of expression\n%q\nin\n%q"
              expr
              (dyn :current-expression))))

  (-> expr
      (resolve-type-inner)
      (fully-resolve-type env)))
