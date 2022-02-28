(import /pantagruel/stdlib :prefix "")

(def ResolutionError @{})

(defn- throw
  ```
  Handle type errors encountered while trying to perform type resolution.

  This doesn't include errors or gaps in type resolution logic, which will be
  raised immediately.
  ```
  [t &opt vars]
  (default vars @{})
  (error (table/setproto (merge vars @{:type t}) ResolutionError)))


(defn- gcd-type
  [left right]

  (defn find-gcd-
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
    [t t2]
    (if-let [[_n gcd] (find-gcd- t t2 0)]
      (if (and gcd (not= gcd Any))
        gcd)))

  (let [gcd (match [left right]
              # TODO: This handles identical sum types; handle the other cases.
              [{:sum ts} {:sum ts}] {:sum ts}
              [{:sum _} {:sum _}] nil
              [{:sum ts} t2] (any? (map |(find-gcd $ t2) ts))
              [t {:sum t2s}] (any? (map |(find-gcd t $) t2s))
              [{:list-of t} {:set-of t2}] {:set-of (find-gcd t t2)}
              [{:set-of t} {:list-of t2}] {:set-of (find-gcd t t2)}
              [{:list-of t} {:list-of t2}] {:list-of (find-gcd t t2)}
              [{:set-of t} {:set-of t2}] {:set-of (find-gcd t t2)}
              [t t2] (find-gcd t t2))]
    (if gcd
      gcd
      (throw :gcd {:left left :right right}))))

(defn is-in-hierarchy?
  [needle haystack]
  (cond
    (not (table? haystack)) false
    (= needle haystack) true

    (match (table/getproto haystack)
      'nil false
      'needle true
      proto (is-in-hierarchy? needle proto))))

(defn collapse-application-args
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

(defn- handle-thunk
  ```
  Fully resolve the type of any expression that couldn't be resolved at
  evaluation time, because it might have referred to a symbol that was bound
  later on in the document.
  ```
  [partial-t env]
  (match partial-t
    {:list-of inner}
    {:list-of (handle-thunk inner env)}

    {:set-of inner}
    {:set-of (handle-thunk inner env)}

    # Special-case procedures with no arguments: treat them as singletons.
    # TODO: Is this right at all?
    ({:args args
      :yields yields}
      (= 0 (length args)) (not= yields Void))
    (handle-thunk yields env)

    {:args args
     :yields yields}
    {:args (map |(handle-thunk $ env) args)
     :yields (handle-thunk yields env)}

    {:thunk thunk}
    (handle-thunk (env thunk) env)

    {:type t}
    (handle-thunk t env)

    {:concrete _}
    partial-t

    {:sum _}
    partial-t

    (s (string? s))
    (let [base-name (string/trim s "'")]
      # Variables like "foo'" derive their type from the variables they are
      # iterations on.
      (handle-thunk (env base-name) env))

    (errorf "Couldn't handle thunk:\n%q\nin\n%q"
            partial-t
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

    (throw :application {:f f :x x})))


(defn inner-type
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

    (throw :container {:t t})))

(defn- number-type
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
        :left left
        :right right} (index-of boolop boolean-operators))
      (if (and (resolve-type left env)
               (resolve-type right env))
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

      {:kind :case
       :mapping mapping}
      (reduce2 gcd-type (map |(resolve-type ($ :right) env) mapping))

      {:container :parens
       :inner inner}
      (do
        (when (> (length inner) 1)
          # TODO: Handle this case. Is it possible?
          (errorf "Encountered parens with more than one element: %q" inner))
        (resolve-type (inner 0) env))

      {:kind :string}
      String

      (n (number? n))
      (number-type n)

      (s (string? s))
      s

      (errorf "Couldn't determine type of expression\n%q\nin\n%q"
              expr
              (dyn :current-expression))))

  (-> expr
      (resolve-type-inner)
      (handle-thunk env)))
