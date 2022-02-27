(import /pantagruel/stdlib :prefix "")
(import /pantagruel/types/resolution)

(defn check-match
  [left right]
  (match {:left left :right right}
    {:left {:sum t} :right {:sum t2}}
    (check-match t t2)

    {:left {:sum t} :right t2}
    (each child t
      (check-match child t2))

    {:left t :right {:sum t2}}
    (each child t2
      (check-match child t))

    {:left t :right t2}
    (unless (= t t2)
      (errorf "Type match failure: %q %q\n   in\n%q\n   in\n%q"
              left
              right
              (dyn :current-expression)
              (dyn :current-line)))))

(defn check-arg-types
  [f-type args]
  (when-let [f-args (f-type :args)]
    (unless (= (length f-args) (length args))
      (errorf "Type check failure: expected %n arguments, got %n\n   in\n%q"
              (length f-args)
              (length args)
              (dyn :current-expression)))
    (map |(check-match $0 $1) f-args args)))

(defn check-is-truthy
  [t]
  (match t
    {:concrete "Bool"}
    :ok

    (errorf "Type check failure: %q is not truthy\n   in\n%q" t (dyn :current-expression))))

(defn check-arith
  [t]
  (unless (index-of t [(base-env "Nat") (base-env "Nat0") (base-env "Int") (base-env "Rat") (base-env "Real")])
    (errorf "Can't perform arithmetic:\n%q\n   in\n%q" t (dyn :current-expression))))

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
      (check-arith t))

    ({:operator compop
      :left left
      :right right} (index-of compop ["=" "<" ">" "<=" ">="]))
    (check-match
      (resolution/resolve-type (check-expr left env) env)
      (resolution/resolve-type (check-expr right env) env))

    ({:operator arithop
      :left left
      :right right} (index-of arithop ["-" "+" "*" "/"]))
    (let [t1 (resolution/resolve-type (check-expr left env) env)
          t2 (resolution/resolve-type (check-expr right env) env)]
      (check-arith t1)
      (check-arith t2)
      (check-match t1 t2))

    ({:operator logop
      :left left
      :right right} (index-of logop ["and" "or"]))
    (do (check-is-truthy (resolution/resolve-type (check-expr left env) env))
      (check-is-truthy (resolution/resolve-type (check-expr right env) env)))

    ({:operator inop
      :left left
      :right right} (index-of inop ["in"]))
    (check-match
      (resolution/resolve-type left env)
      (resolution/inner-type (resolution/resolve-type right env)))

    {:kind :application
     :f f
     :x x}
    (check-arg-types (resolution/resolve-type f env)
                     (map |(resolution/resolve-type $ env) (resolution/collapse-application-args x)))

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

    (errorf "Don't know how to type-check expression: %q\n   in\n%q\n   with\n%q" expr (dyn :current-line) env))
  expr)

(defn type-check
  [tree env]
  (let [body-exprs (mapcat |($0 :body) (tree :chapters))]
    (each body-expr body-exprs
      (setdyn :current-line body-expr)
      (check-expr body-expr env)))

  true)
