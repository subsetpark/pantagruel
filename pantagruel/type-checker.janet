(defn handle-thunks
  [from-env env]
  #TODO: Concretize any thunks by looking up their referents in the environment.
  from-env)

# Type resolution

(defn application-type
  [f env]
  (match f 
    {:kind :procedure
     :type {:yields yields}}
    yields
    
    # TODO: Determine type for application of non-procedure
    (errorf "Can't get the type of the application of a non-procedure: %q" f)))

(defn inner-type
  [t]
  # TODO: Get element type of a container.
  (errorf "Can't get inner type of %q" t))

(defn resolve-type
  [expr env]
  (let [from-env (match expr
                   {:kind :application
                    :f f}
                   (application-type (resolve-type f env) env)

                   {:kind :quantification
                    :expr expr}
                   (resolve-type expr env)

                   {:operator "and"}
                   "Bool"

                   {:operator "="}
                   "Bool"

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
    (handle-thunks from-env env)))

# Assertions

(defn check-match
  [left right]
  (unless (= left right)
    (errorf "Type match failure: %q %q" left right)))

(defn check-arg-types
  [f-type & args]
  #TODO: Check correct number of type of args for f-type
)

(defn check-is-truthy
  [type]
  # TODO: Check that it's sensible to be in a boolean position
)

(defn check-expr
  [expr env]
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
    (and (check-is-truthy (resolve-type (check-expr left env) env))
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
    (if-let [f-type (env f)]
      (check-arg-types f-type x)
      # TODO: Check application of non-procedure
      (errorf "Couldn't check application of non-procedure: %q" f))

    {:kind :quantification
     :expr expr}
    (check-expr expr env)

    (errorf "Don't know how to type-check expression: %q" expr))
  expr)

(defn type-check
  [tree env]
  (pp env)
  (let [body-exprs (mapcat |($0 :body) (tree :chapters))]
    (each body-expr body-exprs (check-expr body-expr env)))

  true)
