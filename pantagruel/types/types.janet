## All logic concerned with type algebra and resolution: determining the type
## of all valid typed expressions, and the entire algebra available for
## manipulating types.
##
## The main entry point is `resolve-type`, which will take an AST element and
## return its type in the context of a fully-populated environment. However,
## the evaluation engine, responsible for the population of that environment,
## also calls into this logic for types that are fully resolvable without an
## environment.

(import /pantagruel/types/gcd)
(import /pantagruel/types/utils)
(import /pantagruel/types/literals)
(import /pantagruel/types/errors)
(import /pantagruel/stdlib :prefix "")

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

(defn- member-type
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

    {:tuple-of inner-ts}
    (reduce2 utils/sum-type inner-ts)

    (@ String)
    Char

    (errors/throw :container {:t t})))

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
        (errors/throw :arg-length {:f-args f-args :args arg-ts}))
      (if (every? (map |(gcd/gcd-type $0 $1) f-args arg-ts))
        yields))

    ([{:list-of t1} ts] (one? (length ts)) (= t1 (ts 0)))
    Nat0

    ([{:list-of t1} ts] (one? (length ts)) (is-in-hierarchy? Int (ts 0)))
    t1

    ([{:list-of t1} ts] (> (length ts) 1))
    (errors/throw :list-application-multiple-args {:xs ts})

    [{:list-of t1} ts]
    (errors/throw :list-application-bad-arg {:f t1 :x (ts 0)})

    ([{:tuple-of tuple-ts} ts] (one? (length ts)) (is-in-hierarchy? Int (ts 0)))
    (reduce2 utils/sum-type tuple-ts)

    # For now, Strings are special-cased here.
    ([(@ String) [t1]] (index-of t1 [Char String]))
    Nat0

    ([(@ String) ts] (one? (length ts)) (is-in-hierarchy? Int (ts 0)))
    Char

    (errors/throw :application {:f f :x x})))

(defn resolve-type
  ```
  Get the type of some AST expression when it is fully evaluated (ie, reduced).
  ```
  [expr env]

  (match expr

    {:kind :sym
     :text s}
    (let [looked-up (env s)]
      (unless looked-up
        (errors/throw :unknown-symbol {:sym s}))
      (case (looked-up :kind)
        # When we encounter a bare symbol, and it's a reference to a domain, it's a
        # direct reference to that domain and therefore has the type of Domain (not
        # of the domain its referencing).
        :domain Domain
        :concrete Domain
        # If it's not a reference to a domain, it has the type of the domain of
        # the value it refers to.
        (resolve-type looked-up env)))

    # When we encounter a deferred reference to a symbol, fully resolve it (if
    # it's a reference to a domain, it's been stored to be fully evaluated and
    # then assigned as the type of a static value). Go straight to type
    # resolution, skipping the `Domain` check above.
    {:thunk {:kind :sym
             :text s}}
    (let [looked-up (env s)]
      (if (and (= :domain (looked-up :kind))
               (= :meta-domain (get-in looked-up [:type :kind])))
        # If the stored type is `Domain` (why doesn't `= Domain` work here?) then
        # `s` is a symbol that was declared as a concrete domain, not an alias
        # to an existing domain.
        {:kind :concrete :name s :type Domain}
        (resolve-type looked-up env)))

    # Any other deferred references should be unwrapped and continued to be
    # evaluated.
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
     :expr quant-expr}
    # Look up the extended environment for just this quantification form (which
    # contains any symbols bound by the quantification) inside the overall
    # environment.
    (let [closed-env (get-in expr [:scope 0])]
      (each binding-or-guard bindings
        # Type-check any guard expressions for side-effects.
        (unless (= (binding-or-guard :kind) :binding)
          (resolve-type binding-or-guard closed-env)))
      (resolve-type quant-expr closed-env))

    ({:operator boolop
      :left left} (index-of boolop boolean-operators))
    (let [right (expr :right)]
      (resolve-type left env)
      (if right (resolve-type right env))
      Bool)

    ({:operator compop
      :left left
      :right right} (index-of compop comparison-operators))
    (let [t (literals/widen (resolve-type left env))
          t2 (literals/widen (resolve-type right env))]
      (if (gcd/gcd-type t t2)
        Bool))

    ({:operator arithop
      :left left
      :right right} (index-of arithop arithmetic-operators))
    (gcd/gcd-type (literals/widen (resolve-type left env))
                  (literals/widen (resolve-type right env)))

    {:operator "in"
     :left left
     :right right}
    (let [element-t (resolve-type left env)
          inner-t (member-type (resolve-type right env))]
      (if (gcd/gcd-type element-t inner-t)
        Bool))

    {:operator "#"
     :left left}
    (if (member-type (resolve-type left env))
      Nat0)

    {:kind :case
     :mapping {:seq mapping}}
    (do
      # If the `:case` is populated, then attempt to unify its type with the
      # types of all branch patterns.
      (when-let [test (expr :case)
                 test-type (resolve-type test env)
                 case-types (map |(resolve-type ($ :left) env) mapping)]
        (each case-type case-types
          (gcd/gcd-type test-type case-type)))
      # In all cases, attempt to unify the types of all branch expressions.
      (let [all-exprs (map |(resolve-type ($ :right) env) mapping)]
        (reduce2 gcd/gcd-type all-exprs)))

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
          (reduce2 gcd/gcd-type [args-type ;case-types])
          (reduce2 gcd/gcd-type [yield-type ;expr-types])
          test-type)

        # TODO: Handle updates on other data types
        (errorf "Couldn't type update of type: %q" test-type)))

    {:container :parens
     :inner inner}
    (let [inner-ts (map |(resolve-type $ env) inner)]
      (if (one? (length inner-ts))
        (inner-ts 0)
        {:tuple-of inner-ts}))

    {:container :set-comprehension
     :inner inner}
    {:container :set
     :inner (resolve-type inner env)}

    {:container :list-comprehension
     :inner inner}
    {:list-of (resolve-type inner env)}

    {:kind :string :text text}
    (literals/intern String text)

    {:kind :num
     :text n}
    (literals/intern (utils/number-type n) n)

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

    {:kind :sum
     :inner ts}
    (reduce2 utils/sum-type (map |(resolve-type $ env) ts))

    {:kind :bound
     :type t}
    (resolve-type t env)

    {:kind :domain
     :type t}
    (resolve-type t env)

    {:value _
     :type t}
    (resolve-type t env)

    # The base case: a type defined in the base environment.
    {:kind :concrete}
    expr

    (errorf "Couldn't determine type of expression\n%q" expr)))
