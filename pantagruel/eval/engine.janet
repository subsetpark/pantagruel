## Evaluation of a Pantagruel document.
##
## Recursively evaluate the AST of an entire document, resulting in a binding
## context mapping all symbols in the documents to the available type
## information.
##
## If any symbol has been introduced but not successfully bound into the
## environment according to the binding rules of the language, will throw an
## Evaluation Error.

(import /pantagruel/stdlib)
(import /pantagruel/types)

(def EvaluationError @{})

(defn- throw
  ```
  Handle violations of the symbol binding rules.

  This doesn't include errors or gaps in evaluation logic, which will be
  raised immediately.
  ```
  [symbols locale &opt vars]
  (default vars @{})
  (error (table/setproto (merge vars @{:symbols symbols
                                       :locale locale})
                         EvaluationError)))


(defn- distribute-bindings-types
  ```
  Handle binding form (x, y, z):T, shorthand for x:T, y:T, z:T.
  ```
  [bindings]

  (defn distribute-binding-type
    [binding]
    (match binding
      {:kind :binding
       :name {:container :parens
              :inner {:syms inner}}
       :expr expr}
      (map (fn [_] expr) inner)

      {:kind :binding
       :expr expr}
      [expr]

      # Bindings lists can have arbitrary expressions as guards; those don't
      # assign any types to any variables.
      {}
      []

      (errorf "Attempted to distribute binding type; got binding %q" binding)))

  (mapcat distribute-binding-type bindings))

(defn- type-of-form
  ```
  All forms that syntactically establish some type.
  ```
  [form]

  (defn err [] (errorf "Encountered unrecognized type syntax:\n%q" form))

  (defn unwrap
    [wrapped]
    (if (> (length wrapped) 1)
      # TODO: Handle [Foo, Bar] types. If those exist.
      (err)
      (type-of-form (wrapped 0))))

  (match form
    {:container :square
     :inner inner}
    {:list-of (type-of-form inner)}

    # Treat comma-separated values inside of braces as a sum of the types of
    # the values.
    # TODO: This just assumes these are values. Maybe we just accept {Int,
    # String} as a synonym for Int + String?
    ({:container :braces
      :inner inner} (tuple? inner) (> (length inner) 1))
    (reduce2 types/sum-type (map type-of-form inner))

    {:container :braces
     :inner inner}
    {:set-of (type-of-form inner)}

    {:container :parens
     :inner inner}
    (type-of-form inner)

    {:kind :declaration
     :yields yields
     :bindings {:bindings-exprs bindings}}
    {:yields (type-of-form yields)
     :args (map type-of-form (distribute-bindings-types bindings))}

    ({:kind :declaration
      :name {:text name}
      :bindings {:bindings-exprs bindings}} (= 0 (length bindings)))
    (if (= (name 0) ((string/ascii-upper name) 0))
      {:concrete name}
      {:args [] :yields stdlib/Void})

    {:kind :declaration
     :name {:text name}
     :bindings {:bindings-exprs bindings}}
    {:args (map type-of-form (distribute-bindings-types bindings))
     :yields stdlib/Void}

    {:operator "+"
     :left left
     :right right}
    (types/sum-type (type-of-form right) (type-of-form right))

    {:operator "*"
     :left left
     :right right}
    # TODO: Do we need any more type math in the case of other algebraic types?
    {:product [left right]}

    {:kind :string}
    stdlib/String

    {:kind :num
     :text n}
    (types/number-type n)

    # Thunks
    # References to expressions which will have to be looked up in
    # the environment when the whole document has been bound.
    {:kind :application}
    {:thunk form}

    {:kind :sym}
    {:thunk form}

    # Recursive cases
    ({:syms wrapped} (tuple? wrapped))
    (unwrap wrapped)

    (wrapped (tuple? wrapped))
    (unwrap wrapped)

    (err)))

(defn introduce-bindings
  [form env symbol-references]

  (defn introduce
    [sym t]
    (match sym
      {:kind :sym
       :text text}
      (put env text t)
      (errorf "Don't know how to introduce symbol: %q" sym)))

  (defn bind
    [name expr]
    (introduce name {:kind :bound
                     :type (type-of-form expr)}))
  (defn alias
    [name expr]
    (printf "In alias, expr %q" expr)
    (introduce name {:kind :domain
                     :type (type-of-form expr)}))

  (match form
    {:kind :decl-alias
     :name name
     :alias expr}
    (do
      (match name
        {:container _ :inner {:syms names}} (each name names (alias name expr))
        (alias name expr))
      (introduce-bindings expr env symbol-references))

    {:kind :declaration
     :name name
     :bindings {:bindings-exprs bindings}}
    (let [yields (form :yields)]
      (let [kind (if (and (nil? yields) (empty? bindings))
                   :domain
                   :procedure)
            t (type-of-form form)]
        (introduce name {:kind kind :type t}))

      (introduce-bindings yields env symbol-references)

      (each binding bindings
        (introduce-bindings binding env symbol-references)))

    {:kind :binding
     :name name
     :expr expr}
    (do
      (match name
        {:container _ :inner {:syms names}} (each name names (bind name expr))
        (bind name expr))
      (introduce-bindings expr env symbol-references))

    {:mapping {:mapping-clauses mapping-form}}
    (do
      (introduce-bindings (form :case) env symbol-references)
      (each {:left left :right right} mapping-form
        (introduce-bindings left env symbol-references)
        (introduce-bindings right env symbol-references)))

    {:kind :quantification
     :bindings {:bindings-exprs bindings}
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

    {:kind :sym
     :text sym}
    (put symbol-references sym true)

    {:kind :num} :ok

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
  [env references locale]

  (each reference (keys references)
    (when (env (normalize reference))
      (put references reference nil)))

  (if (not (empty? references))
    (throw references locale))

  [env references])

(defn eval-chapter
  [[env prev-references] {:head head :body body}]
  (let [head-references @{}
        body-references @{}]

    (eval-head head env head-references)
    (resolve-references env head-references :chapter)

    (eval-body body env body-references)
    (resolve-references env prev-references :body)
    [env body-references]))

(defn handle-evaluation-error
  [err]
  (printf "Unglossed symbols%s:\n\n%s"
          (case (err :locale)
            :body ""
            :chapter " in chapter head")
          (string/join (keys (err :symbols)) ", "))
  (os/exit 1))

(defmacro eval-or-throw
  [body]
  ~(try
     ,body
     ([err]
       (if (table? err)
         (handle-evaluation-error err)
         (error err)))))

(defn eval
  [{:chapters chapters}]

  (let [acc [stdlib/base-env @{}]
        document-result (eval-or-throw (reduce eval-chapter acc chapters))
        [env references] (eval-or-throw (resolve-references ;document-result :body))]
    env))
