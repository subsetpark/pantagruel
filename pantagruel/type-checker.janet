(import /pantagruel/stdlib :prefix "")

(defn handle-thunk
  [from-env env]
  (match from-env
    {:list-of inner}
    {:list-of (handle-thunk inner env)}

    {:set-of inner}
    {:set-of (handle-thunk inner env)}

    # Special-case procedures with no arguments: treat them as singletons.
    # TODO: Is this right at all?
    ( {:args args
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

    (errorf "Don't know how to concretize maybe-thunk: %q" from-env))
 )

# Type resolution

(defn application-type
  [f x env]
  (match {:f f :x x} 
    {:f  {:yields yields}}
    yields
    # When a list is applied to an element, the result is the index of that element.
    ({:f {:list-of t1} :x t2} (= t1 t2))
    (root-env "Nat0")
    # When a list is applied to an index, the result is the inner type of the list.
    {:f {:list-of t1} :x {:concrete "Nat0"}} 
    t1
    # TODO: Determine type for application of non-procedure
    (errorf "Can't get the type of the application of a non-procedure:\nf: %q\nx: %q" f x)))

(defn inner-type
  [t]
  (match t 
    {:set-of inner-t}
    inner-t

    {:list-of inner-t}
    inner-t

  (errorf "Can't get inner type of %q\nin\n%q" t (dyn :current-expression))))

(defn resolve-type
  [expr env]
  (let [from-env (match expr
                   {:kind :application
                    :f f
                    :x x}
                   (application-type (resolve-type f env) (resolve-type x env) env)

                   {:kind :quantification
                    :expr expr}
                   (resolve-type expr env)

                   {:operator "and"}
                   (root-env "Bool")

                   {:operator "="}
                   (root-env "Bool")

                   {:operator "in"}
                   (root-env "Bool")

                   {:container :parens
                    :inner inner}
                   (do
                     (when (> (length inner) 1)
                       # TODO: Handle this case. Is it possible?
                       (errorf "Encountered parens with more than one element: %q" inner))
                     (resolve-type (inner 0) env))

                   (s (string? s))
                   (env expr)

                   (errorf "Couldn't determine type of expression: %q" expr))]
    (handle-thunk from-env env)))

# Assertions

(defn check-match
  [left right]
  (unless (= left right)
    (errorf "Type match failure: %q %q\nin\n%q" left right (dyn :current-expression))))

(defn check-arg-types
  [f-type & args]
  (let [f-args (f-type :args)]
    (unless (= (length f-args) (length args))
      (errorf "Type check failure: expected %n arguments, got %n\nin\n%q" 
              f-args 
              args 
              (dyn :current-expression)))
    (map |(check-match $0 $1) f-args args))
)

(defn check-is-truthy
  [t]
  (match t 
    {:concrete "Bool"}
    :ok
    
    (errorf "Type check failure: %q is not truthy\nin\n%q" t (dyn :current-expression)))
)

(defn check-expr
  [expr env]
  (setdyn :current-expression expr)
  (match expr

    {:container :parens
     :inner inner}
    (do
      (when (> (length inner) 1)
        # TODO: Handle this case. Is it possible?
        (errorf "Encountered parens with more than one element: %q" inner))
      (check-expr (inner 0) env))

    ({:operator compop
      :left left
      :right right} (index-of compop ["="]))
    (check-match
      (resolve-type (check-expr left env) env)
      (resolve-type (check-expr right env) env))

    ({:operator binop
      :left left
      :right right} (index-of binop ["and" "or"]))
    (do (check-is-truthy (resolve-type (check-expr left env) env))
         (check-is-truthy (resolve-type (check-expr right env) env)))

    ({:operator inop
      :left left
      :right right} (index-of inop ["in"]))
    (check-match
      (resolve-type left env)
      (inner-type (resolve-type right env)))

    {:kind :application
     :f f
     :x x}
    (check-arg-types (resolve-type f env) (resolve-type x env))

    {:kind :quantification
     :expr expr}
    (check-expr expr env)

    (errorf "Don't know how to type-check expression: %q" expr))
  expr)

(defn type-check
  [tree env]
  (let [body-exprs (mapcat |($0 :body) (tree :chapters))]
    (each body-expr body-exprs (check-expr body-expr env)))

  true)
