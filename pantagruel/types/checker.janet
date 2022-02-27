(import /pantagruel/stdlib)
(import /pantagruel/types/resolution)

(def- CheckError @{})

(defn- throw
  ```
  Handle type errors encountered during type checking.

  This doesn't include errors or gaps in type checking logic, which will be
  raised immediately.
  ```
  [type left right]
  (error (table/setproto @{:type type :left left :right right} CheckError)))

(defn- try-type-eq
  [left right]
  (unless (= left right)
    (throw :eq left right)))

(defn- try-type-lte
  [left right]
  (match [left right]
    [{:list-of t} {:list-of t2}]
    (try-type-lte t t2)

    [{:set-of t} {:set-of t2}]
    (try-type-lte t t2)

    [{:sum t} {:sum t2}]
    (try-type-eq t t2)

    [{:sum t} t2]
    (each child t
      (try-type-lte child t2))

    [t {:sum t2}]
    (do
      (var found-match false)
      (each child t2
        (let [[res _] (protect (try-type-lte child t))]
          (set found-match (or found-match res))))
      (unless found-match (throw :lte left right)))

    [t t2]
    (unless (resolution/is-in-hierarchy? t2 t)
      (throw :lte t t2))))

(defn check-arg-types
  [f-type args]
  (when-let [f-args (f-type :args)]
    (unless (= (length f-args) (length args))
      (throw :arg-length f-type args))
    (map |(try-type-lte $0 $1) f-args args)))

(defn check-is-truthy
  [t]
  (match t
    stdlib/Bool true

    {:list-of _} true

    {:set-of _} true

    (errorf "Type check failure: %q is not truthy\n   in\n%q" t (dyn :current-expression))))

(defn check-arith
  [left right]
  (unless (and (index-of left stdlib/numeric-types)
               (index-of right stdlib/numeric-types))
    (throw :arithmetic left right)))

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

    {:operator "#"
     :left left}
    (let [t (resolution/resolve-type (check-expr left env) env)]
      # TODO: Do this right!
      (check-arith t stdlib/Int))

    ({:operator compop
      :left left
      :right right} (index-of compop stdlib/comparison-operators))
    (try-type-lte
      (resolution/resolve-type (check-expr left env) env)
      (resolution/resolve-type (check-expr right env) env))

    ({:operator arithop
      :left left
      :right right} (index-of arithop stdlib/arithmetic-operators))
    (let [t1 (resolution/resolve-type (check-expr left env) env)
          t2 (resolution/resolve-type (check-expr right env) env)]
      (check-arith t1 t2)
      (try-type-lte t1 t2))

    ({:operator boolop
      :left left
      :right right} (index-of boolop stdlib/boolean-operators))
    (do (check-is-truthy (resolution/resolve-type (check-expr left env) env))
      (check-is-truthy (resolution/resolve-type (check-expr right env) env)))

    ({:operator inop
      :left left
      :right right} (index-of inop ["in"]))
    (try-type-lte
      (resolution/resolve-type left env)
      (resolution/inner-type (resolution/resolve-type right env)))

    {:kind :application
     :f f
     :x x}
    (check-arg-types (resolution/resolve-type f env)
                     (map |(resolution/resolve-type $ env)
                          (map |(check-expr $ env) (resolution/collapse-application-args x))))

    {:kind :quantification
     :expr expr}
    (check-expr expr env)

    {:kind :case
     :mapping mapping}
    (each {:left left :right right} mapping
      (check-expr left env)
      (check-expr right env))

    (s (string? s))
    :ok

    (n (number? n))
    :ok

    {:kind :string}
    :ok

    (errorf "Don't know how to type-check expression: %q\n   in\n%q\n   with\n%q"
            expr
            (dyn :current-line) env))
  expr)

(defn- print-types
  [str & args]
  (defn- render-type
    [t]

    (defn- join
      [ts]
      (string/format "[%s]" (-> (map render-type ts) (string/join ", "))))

    (match t
      (ts (indexed? ts)) (join ts)
      {:concrete t-name} t-name
      {:set-of t} (string/format "{%s}" (render-type t))
      {:list-of t} (string/format "[%s]" (render-type t))
      {:args args :yields yields} (string/format "(%s => %s)" (join args) (render-type yields))
      t (string/format "%q" t)))

  (printf str ;(map render-type args)))

(defn- handle-resolution-error
  [err]
  (case (err :type)
    :list-application-multiple-args
    (print-types "Attempted to apply a list type to multiple arguments:\n%s"
                 (err :xs))

    :list-application-bad-arg
    (print-types "Attempted to apply a list of type:\n%s\nto an argument of type:\n%s"
                 (err :f)
                 (err :x))

    :application
    (print-types "Attempted to apply non-procedure/list type:\n%s"
                 (err :f))

    :container
    (print-types "Attempted to check for membership in non-container type:\n%s"
                 (err :t))

    (print-types "Unknown type resolution error: %s" type)))

(defn- handle-check-error
  [err]
  (case (err :type)
    :eq
    (print-types "Domains not equal:\n%s, %s"
                 (err :left)
                 (err :right))

    :lte
    (print-types "Domain:\n%s\nis not less than or equal to domain:\n%s"
                 (err :left)
                 (err :right))

    :arithmetic
    (print-types "Can't apply arithmetic operators to domains:\n%s, %s"
                 (err :left)
                 (err :right))

    :arg-length
    (print-types "Invalid arguments:\n%s\nto procedure type:\n%s"
                 (err :right)
                 (err :left))))

(defn type-check
  [tree env]
  (let [body-exprs (mapcat |($0 :body) (tree :chapters))]
    (each body-expr body-exprs
      (try
        (check-expr body-expr env)
        ([err]
          (print "Type error.")
          (if (table? err)
            (cond
              (= resolution/ResolutionError (table/getproto err)) (handle-resolution-error err)
              (= CheckError (table/getproto err)) (handle-check-error err))
            (print err))
          (printf "In expression:\n%q" body-expr)
          (os/exit 1)))))

  true)
