(import /pantagruel/stdlib :prefix "")

(defn- handle-thunk
  [from-env env]
  (match from-env
    {:list-of inner}
    {:list-of (handle-thunk inner env)}

    {:set-of inner}
    {:set-of (handle-thunk inner env)}

    # Special-case procedures with no arguments: treat them as singletons.
    # TODO: Is this right at all?
    ({:args args
      :yields yields} (= 0 (length args)))
    (handle-thunk yields env)

    {:args args
     :yields yields}
    {:args (map |(handle-thunk $ env) args)
     :yields (handle-thunk yields env)}

    {:thunk thunk}
    (handle-thunk (env thunk) env)

    {:type type}
    (handle-thunk type env)

    {:concrete _}
    from-env

    {:sum _}
    from-env

    (s (string? s))
    (handle-thunk (env s) env)

    (errorf "Don't know how to concretize maybe-thunk: %q in %q" from-env (dyn :current-expression))))

(defn- application-type
  [f x env]
  (match [f x]
    [{:yields yields} _]
    yields
    # When a list is applied to an element, the result is the index of that element.
    ([{:list-of t1} [ t2]] (= t1 t2))
    Nat0
    # When a list is applied to an index, the result is the inner type of the list.
    [{:list-of t1} [Nat0]]
    t1

    (errorf "Couldn't determine type of application %q of %q in %q" f x (dyn :current-expression))))


(defn inner-type
  [t]
  (match t
    {:set-of inner-t}
    inner-t

    {:list-of inner-t}
    inner-t

    (errorf "Can't get inner type of %q\n   in\n%q" t (dyn :current-expression))))

(defn- number-type
  [n]
  # TODO: Parse ints, nats, etc
  (cond
    (nat? n) (base-env "Nat0")

    (base-env "Real")))

(defn collapse-application-args
  [x]
  (match x
    {:kind :application
     :f arg1
     :x arg2}
    [arg1 ;(collapse-application-args arg2)]

    [x]))

(defn resolve-type
  [expr env]
  (let [from-env
        (match expr
          {:kind :application
           :f f
           :x x}
          (application-type (resolve-type f env)
                            (map |(resolve-type $ env) (collapse-application-args x)) env)

          {:kind :quantification
           :expr expr}
          (resolve-type expr env)

          {:operator "and"}
          (base-env "Bool")

          {:operator "="}
          (base-env "Bool")

          {:operator "in"}
          (base-env "Bool")

          {:operator "#"}
          (base-env "Nat0")

          ({:operator arithop
            :left t} (index-of arithop ["-" "+" "*" "/"]))
          # TODO: Determine possible types of subtraction, etc, between different types
          (resolve-type t env)

          {:kind :case
           :mapping mapping}
          {:sum (map |(resolve-type ($ :right) env) mapping)}

          {:container :parens
           :inner inner}
          (do
            (when (> (length inner) 1)
              # TODO: Handle this case. Is it possible?
              (errorf "Encountered parens with more than one element: %q" inner))
            (resolve-type (inner 0) env))

          {:kind :string}
          (base-env "String")

          (s (string? s))
          (env expr)

          (n (number? n))
          (number-type n)

          (errorf "Couldn't determine type of expression: %q" expr))]
    (handle-thunk from-env env)))
