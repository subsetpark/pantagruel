(import /pantagruel/stdlib)

(defn distribute-bindings-types
  [bindings]

  (defn distribute-binding-type
    [binding]
    (match binding
      {:kind :binding
       :name {:container :parens
              :inner inner}
       :expr expr}
      (map (fn [_] expr) inner)

      {:expr expr}
      [expr]))

  (mapcat distribute-binding-type bindings))

(defn get-type
  [form]
  (match form
    {:container :square
     :inner inner}
    {:list-of (get-type inner)}

    {:container :braces
     :inner inner}
    {:set-of (get-type inner)}

    {:container :parens
     :inner inner}
    (get-type inner)

    {:kind :declaration
     :yields yields
     :bindings bindings}
    {:yields (get-type yields)
     :args (map get-type (distribute-bindings-types bindings))}

    {:kind :declaration :name name}
    (if (= (name 0) ((string/ascii-upper name) 0))
      {:concrete name}
      {:args [] :yields (stdlib/base-env "Void")})

    {:operator "+"
     :left left
     :right right}
    {:sum (flatten [(let [left-type (get-type left)]
                      (if (left-type :sum)
                        (left-type :sum)
                        left-type))
                    (let [right-type (get-type right)]
                      (if (right-type :sum)
                        (right-type :sum)
                        right-type))])}

    # Thunks
    # References to expressions which will have to be looked up in
    # the environment when the whole document has been bound.
    {:kind :application}
    @{:thunk form}

    (s (string? s))
    @{:thunk form}

    # Recursive cases
    (wrapped (tuple? wrapped))
    (if (> (length wrapped) 1)
      # TODO: Handle [Foo, Bar] types. If those exist.
      (errorf "Encountered unknown type: %q" wrapped)
      (get-type (wrapped 0)))

    (errorf "Encountered unknown type: %q" form)))

(defn introduce-bindings
  [form env symbol-references]

  (defn bind [name expr] (put env name {:kind :bound
                                        :type (get-type expr)}))
  (defn alias [name expr] (put env name {:kind :alias
                                         :type (get-type expr)}))

  (match form

    {:kind :decl-alias
     :name name
     :alias expr}
    (do
      (match name
        {:container _ :inner names} (each name names (alias name expr))
        (alias name expr))
      (introduce-bindings expr env symbol-references))

    {:kind :declaration
     :name name
     :bindings bindings}
    (let [yields (form :yields)]
      (let [kind (if (and (nil? yields) (empty? bindings))
                   :domain
                   :procedure)
            type (get-type form)]
        (put env name {:kind kind :type type}))
      (introduce-bindings yields env symbol-references)
      (each binding bindings
        (introduce-bindings binding env symbol-references)))

    {:kind :binding
     :name name
     :expr expr}
    (do
      (match name
        {:container _ :inner names} (each name names (bind name expr))
        (bind name expr))
      (introduce-bindings expr env symbol-references))

    {:kind :case
     :mapping mapping-form}
    (do
      (introduce-bindings (form :case) env symbol-references)
      (each {:left left :right right} mapping-form
        (introduce-bindings left env symbol-references)
        (introduce-bindings right env symbol-references)))

    {:kind :quantification
     :bindings bindings
     :expr expr}
    (do
      (each binding bindings
        (introduce-bindings binding env symbol-references))
      (introduce-bindings expr env symbol-references))

    {:kind :binary-operation
     :left left
     :right right}
    (do
      (introduce-bindings left env symbol-references)
      (introduce-bindings right env symbol-references))

    {:kind :unary-operation
     :left left}
    (introduce-bindings left env symbol-references)

    {:kind :application
     :f f
     :x x}
    (do
      (introduce-bindings f env symbol-references)
      (introduce-bindings x env symbol-references))

    {:container _ :inner exprs}
    (each expr exprs
      (introduce-bindings expr env symbol-references))

    (sym (string? sym)) (put symbol-references sym true)
    (num (number? num)) :ok
    {:kind :string} :ok

    (@ 'nil) :ok

    (printf "Handling unknown binding form: %q" form)))

(defn eval-head
  [head env symbol-references]
  (each declaration head
    (introduce-bindings declaration env symbol-references))
  [env symbol-references])

(defn eval-body
  [body env symbol-references]
  (each statement body
    (introduce-bindings statement env symbol-references))
  [env symbol-references])

(defn- normalize
  [reference]
  (string/trim reference "'"))

(defn- resolve-references
  [env references &opt suffix]
  (default suffix "")

  (each reference (keys references)
    (when (env (normalize reference))
      (put references reference nil)))

  (if (not (empty? references))
    (errorf `
            Unglossed symbols%s:

            %s
            `
            suffix
            (string/join (keys references) ", ")))

  [env references])

(defn eval-chapter
  [{:head head :body body} env prev-references]
  (let [head-references @{}
        body-references @{}]

    (eval-head head env head-references)
    (resolve-references env head-references " in chapter head")

    (eval-body body env body-references)
    (resolve-references env prev-references)
    [env body-references]))

(defn eval
  [{:chapters chapters}]
  (let [[env references]
        (resolve-references ;(reduce (fn [[env references] chapter]
                                       (eval-chapter chapter env references))
                                     [stdlib/base-env @{}]
                                     chapters))]
    env))
