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

(defn introduce-bindings-and-references
  ```
  Handle any given AST form for environment bindings.

  If it's a binding form, introduce it into the execution environment,
  associated with the syntactically derived type information (or a thunk, if
  type information is not available syntactically and needs resolution after
  the environment has been fully populated).
  ```
  [form env symbol-references]

  (defn introduce
    [sym t]
    (match sym
      {:kind :sym
       :text text
       :span span}
      (put env text t)
      (errorf "Don't know how to introduce symbol: %q" sym)))

  (defn bind
    [name expr]
    (introduce name {:kind :bound
                     :type (types/type-of-form expr)}))
  (defn bind-member
    ```
    Bind a symbol to the inner type of `expr`.
    ```
    [name expr]
    (introduce name {:kind :member
                     :type (types/type-of-form expr)}))
  (defn alias
    [name expr]
    (introduce name {:kind :domain
                     :type (types/type-of-form expr)}))

  (match form
    {:kind :decl-alias
     :name name
     :alias expr}
    (do
      (match name
        {:container _ :inner {:seq names}} (each name names (alias name expr))
        (alias name expr))
      (introduce-bindings-and-references expr env symbol-references))

    {:kind :declaration
     :name name
     :bindings {:seq bindings}}
    (let [yields (form :yields)]
      (let [kind (if (and (nil? yields) (empty? bindings))
                   :domain
                   :procedure)
            t (types/type-of-form form)]
        (introduce name {:kind kind :type t}))

      (introduce-bindings-and-references yields env symbol-references)

      (each binding bindings
        (introduce-bindings-and-references binding env symbol-references)))

    {:kind :binding
     :binding-type binding-type
     :name name
     :expr expr}
    (let [f (case binding-type
              :: bind
              :from bind-member)]
      (match name
        {:container _ :inner {:seq names}}
        (each name names (f name expr))

        (f name expr))
      (introduce-bindings-and-references expr env symbol-references))

    {:kind :case
     :mapping {:seq mapping-form}}
    (do
      (introduce-bindings-and-references (form :case) env symbol-references)
      (each {:left left :right right} mapping-form
        (introduce-bindings-and-references left env symbol-references)
        (introduce-bindings-and-references right env symbol-references)))

    {:kind :update
     :mapping {:seq mapping-form}}
    (do
      (introduce-bindings-and-references (form :case) env symbol-references)
      (each {:left left :right right} mapping-form
        (introduce-bindings-and-references left env symbol-references)
        (introduce-bindings-and-references right env symbol-references)))

    {:kind :quantification
     :bindings {:seq bindings}
     :expr expr}
    (do
      (each binding bindings
        (introduce-bindings-and-references binding env symbol-references))
      (introduce-bindings-and-references expr env symbol-references))

    {:kind :binary-operation
     :left left
     :right right}
    (do
      (introduce-bindings-and-references left env symbol-references)
      (introduce-bindings-and-references right env symbol-references))

    {:kind :unary-operation
     :left left}
    (introduce-bindings-and-references left env symbol-references)

    {:kind :application
     :f f
     :x x}
    (do
      (introduce-bindings-and-references f env symbol-references)
      (introduce-bindings-and-references x env symbol-references))

    {:container _ :inner {:seq exprs}}
    (each expr exprs
      (introduce-bindings-and-references expr env symbol-references))

    ({:container _ :inner exprs} (tuple? exprs))
    (each expr exprs
      (introduce-bindings-and-references expr env symbol-references))

    {:container _ :inner expr}
    (introduce-bindings-and-references expr env symbol-references)

    {:kind :domain-sum
     :inner inner}
    (each domain inner
      (introduce-bindings-and-references domain env symbol-references))

    {:kind :sym
     :text sym}
    (put symbol-references form true)

    # Domains built out of sets of literal values, hence, guaranteed not to
    # contain symbol references or bindings
    {:kind :domain-set}
    :ok

    {:kind :num} :ok

    {:kind :string} :ok

    (@ 'nil) :ok

    (printf "Unknown form in engine: %q" form)))

(defn eval-head
  ```
  Evaluate a chapter head for any environment bindings.
  ```
  [head env symbol-references]
  (each declaration head
    (introduce-bindings-and-references declaration env symbol-references))
  [env symbol-references])

(defn eval-body
  ```
  Evaluate a chapter body for any environment bindings.
  ```
  [body env symbol-references]
  (each statement body
    (introduce-bindings-and-references statement env symbol-references))
  [env symbol-references])

(defn- normalize
  [{:text reference}]
  (string/trim reference "'"))

(defn- resolve-references
  ```
  Given an environment and a set of symbol references, eliminate all references
  that are bound with respect to that environment and throw if any remain.
  ```
  [env references locale]

  (each reference (keys references)
    (when (env (normalize reference))
      (put references reference nil)))

  (if (not (empty? references))
    (throw references locale))

  [env references])

(defn eval-chapter
  ```
  Handle a single chapter, binding any introduced symbols into the environment
  and resolving any references that are due.
  ```
  [[env prev-references] {:head head :body body}]
  (let [head-references @{}
        body-references @{}]

    (eval-head head env head-references)
    (resolve-references env head-references :chapter)

    (eval-body body env body-references)
    (resolve-references env prev-references :body)
    [env body-references]))

(defn eval
  [{:chapters chapters}]

  (let [acc [(table/setproto @{} stdlib/base-env) @{}]
        document-result (reduce eval-chapter acc chapters)
        [env references] (resolve-references ;document-result :body)]
    env))
