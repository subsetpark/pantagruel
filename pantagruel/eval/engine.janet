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

(def currently-importing-modules @[])

(defn currently-importing?
  [module]
  (index-of module currently-importing-modules))

(defn currently-importing
  [module]
  (array/push currently-importing-modules module))

(defn- normalize-thunk
  [obj]
  (match obj
    @[:span _] []

    {:kind :bound :type thunk}
    (normalize-thunk thunk)

    {:kind :procedure :type {:args {:tuple-of @[]} :yields yields}}
    (normalize-thunk yields)

    (o (dictionary? o))
    (struct ;(mapcat normalize-thunk (pairs o)))

    (i (indexed? i))
    (map normalize-thunk i)

    obj))

(defn- resolve-references
  ```
  Given an environment and a set of symbol references, eliminate all references
  that are bound with respect to that environment.
  ```
  [env references]

  (each reference (keys references)
    (when (env (reference :text))
      (put references reference nil))))

(def Evaluator-
  @{:throw
    (fn throw
      [self type args]
      (error (merge {:err type :evaluator self} args)))

    :check-references
    (fn check-references
      # Throw if any references haven't been resolved.
      [self env references locale]
      (if (not (empty? references))
        (:throw self :evaluation {:symbols references :locale locale})))

    :introduce-bindings-and-references
    (fn introduce-bindings-and-references
      # Handle any given AST form for environment bindings.

      # If it's a binding form, introduce it into the execution environment,
      # associated with the syntactically derived type information (or a thunk, if
      # type information is not available syntactically and needs resolution after
      # the environment has been fully populated).
      [self form env symbol-references &opt include-prime]

      (defn introduce
        [sym t]
        (match sym
          {:kind :sym
           :text text
           :span span}
          (do
            (when-let [already (env text)]
              (if (not= (normalize-thunk already)
                        (normalize-thunk t))
                (:throw self :single-binding {:sym sym :already already :t t})))
            (put env text t)
            (if include-prime (put env (string text "'") t)))

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
          (:introduce-bindings-and-references self expr env symbol-references))

        {:kind :declaration
         :name name
         :bindings {:seq bindings}}
        (let [yields (form :yields)]
          (let [kind (if (and (nil? yields) (empty? bindings))
                       :domain
                       :procedure)
                t (types/type-of-form form)]
            (introduce name {:kind kind :type t}))

          (:introduce-bindings-and-references self yields env symbol-references)

          (each binding bindings
            (:introduce-bindings-and-references self binding env symbol-references (nil? yields))))

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
          (:introduce-bindings-and-references self expr env symbol-references))

        {:kind :case
         :mapping {:seq mapping-form}}
        (do
          (:introduce-bindings-and-references self (form :case) env symbol-references)
          (each {:left left :right right} mapping-form
            (:introduce-bindings-and-references self left env symbol-references)
            (:introduce-bindings-and-references self right env symbol-references)))

        {:kind :update
         :mapping {:seq mapping-form}}
        (do
          (:introduce-bindings-and-references self (form :case) env symbol-references)
          (each {:left left :right right} mapping-form
            (:introduce-bindings-and-references self left env symbol-references)
            (:introduce-bindings-and-references self right env symbol-references)))

        {:kind :quantification
         :bindings {:seq bindings}
         :expr expr}
        (do
          # Close around the current, extended environment, linking it back to the
          # AST form that it's associated with.
          (put-in form [:scope 0] env)
          (each binding bindings
            (:introduce-bindings-and-references self binding env symbol-references))
          (:introduce-bindings-and-references self expr env symbol-references))

        {:kind :binary-operation
         :left left
         :right right}
        (do
          (:introduce-bindings-and-references self left env symbol-references)
          (:introduce-bindings-and-references self right env symbol-references))

        {:kind :unary-operation
         :left left}
        (:introduce-bindings-and-references self left env symbol-references)

        {:kind :application
         :f f
         :x x}
        (do
          (:introduce-bindings-and-references self f env symbol-references)
          (:introduce-bindings-and-references self x env symbol-references))

        {:container _ :inner {:seq exprs}}
        (each expr exprs
          (:introduce-bindings-and-references self expr env symbol-references))

        ({:container _ :inner exprs} (tuple? exprs))
        (each expr exprs
          (:introduce-bindings-and-references self expr env symbol-references))

        {:container _ :inner expr}
        (:introduce-bindings-and-references self expr env symbol-references)

        {:kind :domain-sum
         :inner inner}
        (each domain inner
          (:introduce-bindings-and-references self domain env symbol-references))

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

    :eval-subsection
    (fn eval-subsection
      # Evaluate a subsection for any environment bindings.
      [self subsection env symbol-references]
      (each statement subsection
        (:introduce-bindings-and-references self statement env symbol-references)))

    :eval
    (fn eval
      # Main evaluation logic. Optionally, accepts a map of modules to import
      # and a callback to call in order to import them. If not passed in,
      # the import pass is skipped.
      [self {:directives directives :chapters chapters}
       &opt
       available-modules evaluator-callback]

      # Environment setup. Create a root environment and then merge in any
      # imported modules.

      (def env (table/setproto @{} stdlib/base-env))

      (when (and available-modules evaluator-callback)
        (each directive directives
          (match directive
            {:statement "module" :args sym}
            (let [current-module (sym :text)]
              (unless (currently-importing? current-module)
                (currently-importing current-module)))

            {:statement "import" :args sym}
            (if-let [to-import (sym :text)
                     path (available-modules to-import)]
              (let [err? (currently-importing? to-import)
                    [tree evaluator] (evaluator-callback path)]
                (currently-importing to-import)
                (if err?
                  (:throw self :import-cycle {:currently-importing-modules currently-importing-modules
                                              :to-import sym}))
                (merge-into env (->
                                  evaluator
                                  (:eval tree available-modules evaluator-callback)
                                  # Convert the imported environment into a struct in order to
                                  # drop the prototype.
                                  (table/to-struct))))
              (:throw self :import {:available-modules available-modules
                                    :to-import sym})))))

      # Evaluation pass on this document.

      (defn eval-chapter
        # Handle a single chapter, binding any introduced symbols into the
        # environment and resolving any references that are due.
        [prev-references {:head head :body body}]
        (let [head-references @{}]
          # Populate environment and references with bindings and references from
          # chapter head.
          (:eval-subsection self head env head-references)

          # Clear outstanding references in previous chapter that were bound in head.
          (resolve-references env prev-references)
          # Check that references in previous chapter have all been cleared.
          (:check-references self env prev-references :body)

          # Clear outstanding references in head that were bound in head.
          (resolve-references env head-references)
          # Check that references in head have all been cleared.
          (:check-references self env head-references :chapter)

          # We've validated all references up until this point; begin capturing
          # references for validation in the *next* chapter.
          (let [body-references @{}
                # Push a new scope on the stack for body-declared bindings.
                body-env (table/setproto @{} env)]
            # Populate body references, and new scope with body bindings.
            (:eval-subsection self body body-env body-references)
            # Clear outstanding references in this body using the bindings in the
            # body (as well as existing env).
            (resolve-references body-env body-references)
            # Pass on any outstanding references; they will have to be resolved
            # using document-level bindings.
            body-references)))

      (let [remaining-references (reduce eval-chapter @{} chapters)]
        # Check for any body references that were made in the last chapter but
        # never bound.
        (:check-references self env remaining-references :body)
        env))})

(defn Evaluator
  ```
  Document evaluation object. Tracks the file being evaluated, in order to be
  able to bubble up evaluation errors from imported documents correctly.
  ```
  [file src]
  (table/setproto
    @{:file file
      :src src}
    Evaluator-))
