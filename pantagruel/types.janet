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
              ([lt _] (tuple? lt) (one? (length lt)))
              (gcd-type (lt 0) right)

              ([_ rt] (tuple? rt) (one? (length rt)))
              (gcd-type left (rt 0))

              [{:kind :sum
                :inner ts}
               {:kind :sum
                :inner ts2}]
              (let [gcds @{}]
                (each t ts
                  (each t2 ts2
                    (try
                      (let [success-type (gcd-type t t2)]
                        (put gcds success-type true))
                      ([err] :ok))))
                (when (not (empty? gcds))
                  {:kind :sum
                   :inner (keys gcds)}))

              [{:kind :sum :inner ts} t2]
              (do
                (var gcd nil)
                (each t ts
                  (try
                    (let [success-type (gcd-type t t2)]
                      (set gcd success-type)
                      (break))
                    ([err] :ok)))
                gcd)

              [t {:kind :sum :inner t2s}]
              (do
                (var gcd nil)
                (each t2 t2s
                  (try
                    (let [success-type (gcd-type t t2)]
                      (set gcd success-type)
                      (break))
                    ([err] :ok)))
                gcd)

              [{:list-of t}
               {:container :set
                :inner t2}]
              {:container :set
               :inner (gcd-type t t2)}

              [{:container :set
                :inner t}
               {:list-of t2}]
              {:container :set
               :inner (gcd-type t t2)}

              [{:list-of t} {:list-of t2}]
              {:list-of (gcd-type t t2)}

              [{:container :set
                :inner t}
               {:container :set
                :inner t2}]
              {:container :set
               :inner (gcd-type t t2)}

              [{:tuple-of ts} {:tuple-of ts2}]
              (when (= (length ts) (length ts2))
                {:tuple-of (map gcd-type ts ts2)})

              [{:args args-t :yields yields-t} {:args args-t2 :yields yields-t2}]
              (let [args-gcd (gcd-type args-t args-t2)
                    yield-gcd (gcd-type yields-t yields-t2)]
                {:args args-gcd :yields yield-gcd})

              [t t2]
              (find-gcd t t2))]
    (if gcd
      gcd
      (throw :inner-gcd {:left left :right right}))))

(defn- gcd-outer
  ```
  Entrypoint for finding unification types. 

  Unification logic is recursive, so maintain this point in order to return the
  outer types that couldn't be unified.
  ```
  [left right]
  (try
    (gcd-type left right)
    ([err fib]
      (if (and (table? err) (= (err :type) :inner-gcd))
        (throw :gcd {:left left :right right})
        (propagate err fib)))))

(defn- inner-type
  ```
  Resolve the type of a membership operation, ie, get the type of a container's
  elements.
  ```
  [t]
  (match t
    {:container :set
     :inner inner-t}
    inner-t

    {:list-of inner-t}
    inner-t

    (@ String)
    Char

    (throw :container {:t t})))

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
    [{:args {:tuple-of f-args} :yields yields} arg-ts]
    (do
      (unless (= (length f-args) (length arg-ts))
        (throw :arg-length {:f-args f-args :args arg-ts}))
      (if (every? (map |(gcd-outer $0 $1) f-args arg-ts))
        yields))

    ([{:list-of t1} ts] (one? (length ts)) (= t1 (ts 0)))
    Nat0

    ([{:list-of t1} ts] (one? (length ts)) (is-in-hierarchy? Int (ts 0)))
    t1

    ([{:list-of t1} ts] (> (length ts) 1))
    (throw :list-application-multiple-args {:xs ts})

    [{:list-of t1} ts]
    (throw :list-application-bad-arg {:f t1 :x (ts 0)})

    # For now, Strings are special-cased here.
    ([(@ String) [t1]] (index-of t1 [Char String]))
    Nat0

    ([(@ String) ts] (one? (length ts)) (is-in-hierarchy? Int (ts 0)))
    Char

    (throw :application {:f f :x x})))

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
        gcd (protect (gcd-outer t1 t2))]
    (cond
      # If the two types are unifiable, return the common denominator.
      (gcd 0) (gcd 1)
      # If both sides are equivalent, we don't need a sum. In other words,
      # X + X = X.
      (and (= t1 t2) (one? (length t1))) (t1 0)

      {:kind :sum
       :inner (distinct [;t1 ;t2])})))

(defn- look-up-base-string
  [s env]
  (let [base-name (string/trim s "'")]
    (env base-name)))


(defn resolve-type
  ```
  Get the type of some AST expression when it is fully evaluated (ie, reduced).
  ```
  [expr env]

  (match expr

    # When we encounter a deferred reference to a symbol, fully resolve it (if
    # it's a reference to a domain, it's been stored to be fully evaluated and
    # then assigned as the type of a static value).
    {:thunk {:kind :sym
             :text s}}
    (let [looked-up (tracev (look-up-base-string s env))]
      (resolve-type looked-up env))

    # When we encounter a bare symbol, and it's a reference to a domain, it's a
    # direct reference to that domain and therefore has the type of Domain (not
    # of the domain its referencing).
    {:kind :sym
     :text s}
    (let [looked-up (look-up-base-string s env)]
      (if (is-domain-reference? looked-up)
        Domain
        (resolve-type looked-up env)))

    # Any other deferred references should be unwrapped and continued to be evaluated.
    {:thunk thunk}
    (resolve-type thunk env)

    {:kind :application
     :f f
     :x x}
    (let [args (collapse-application-args x)]
      (application-type (resolve-type f env)
                        (map |(resolve-type $ env) args)))

    {:kind :quantification
     :bindings {:seq bindings}
     :expr expr}
    (do
      (each binding-or-guard bindings
        # Type-check any guard expressions for side-effects.
        (unless (= (binding-or-guard :kind) :binding)
          (resolve-type binding-or-guard env)))
      (resolve-type expr env))

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
      (if (gcd-outer t t2)
        Bool))

    {:operator "in"
     :left left
     :right right}
    (let [element-t (resolve-type left env)
          inner-t (inner-type (resolve-type right env))]
      (if (gcd-outer element-t inner-t)
        Bool))

    {:operator "#"
     :left left}
    (if (inner-type (resolve-type left env))
      Nat0)

    ({:operator arithop
      :left left
      :right right} (index-of arithop arithmetic-operators))
    (gcd-outer (resolve-type left env) (resolve-type right env))

    {:kind :case
     :mapping {:seq mapping}}
    (do
      # If the `:case` is populated, then attempt to unify its type with the
      # types of all branch patterns.
      (when-let [test (expr :case)
                 test-type (resolve-type test env)
                 case-types (map |(resolve-type ($ :left) env) mapping)]
        (reduce2 gcd-outer [test-type ;case-types]))
      # In all cases, attempt to unify the types of all branch expressions.
      (let [all-exprs (map |(resolve-type ($ :right) env) mapping)]
        (reduce2 gcd-outer all-exprs)))

    {:kind :update
     :mapping {:seq mapping}
     :case test}
    (let [test-type (resolve-type test env)
          case-types (map |(resolve-type ($ :left) env) mapping)
          expr-types (map |(resolve-type ($ :right) env) mapping)]
      (match test-type
        # The update case is a procedure mapping args to yields. In updating,
        # type the left sides against the arguments and the right sides
        # against the yields. 
        {:args args-type :yields yield-type}
        (do
          (reduce2 gcd-outer [args-type ;case-types])
          (reduce2 gcd-outer [yield-type ;expr-types])
          test-type)

        # TODO: Handle updates on other data types
        (errorf "Couldn't type update of type: %q" test-type)))

    {:container :parens
     :inner inner}
    (let [inner-ts (map |(resolve-type $ env) inner)]
      (if (one? (length inner-ts))
        (inner-ts 0)
        {:tuple-of inner-ts}))

    {:container :value-set
     :inner inner}
    (let [ts (map |(resolve-type $ env) inner)]
      (or (reduce2 sum-type ts)
          {:container :set
           :inner []}))

    {:container :value-list
     :inner inner}
    {:list-of (resolve-type inner env)}

    {:kind :string}
    String

    {:kind :num
     :text n}
    (number-type n)

    {:kind :member
     :type t}
    (inner-type (resolve-type t env))

    # Special-case procedures with no arguments: treat them as singletons (that
    # is, to mention a procedure with no arguments is the same as applying it).
    ({:kind :procedure
      :type {:args {:tuple-of args}
             :yields yields}}
      (empty? args) (not= yields Void))
    (resolve-type yields env)

    # Main procedure case.
    {:kind :procedure
     :type {:args args
            :yields yields}}
    {:args (resolve-type args env)
     :yields (resolve-type yields env)}

    {:list-of inner}
    {:list-of (resolve-type inner env)}

    ({:container :set
      :inner inner} (empty? inner))
    expr

    {:container :set
     :inner inner}
    {:container :set
     :inner (resolve-type inner env)}

    {:tuple-of inner}
    {:tuple-of (tuple/slice (map |(resolve-type $ env) inner))}

    {:kind :sum :inner ts}
    {:kind :sum :inner (map |(resolve-type $ env) ts)}

    {:kind :bound
     :type t}
    (resolve-type t env)

    {:kind :domain
     :type t}
    (resolve-type t env)

    {:concrete _}
    expr

    {:value _
     :type t}
    (resolve-type t env)
    (errorf "Couldn't determine type of expression\n%q" expr)))
