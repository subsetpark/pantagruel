(import /pantagruel/stdlib)

(defn introduce-bindings
  [form env symbol-references]

  (defn bind [name] (put env name {:kind :bound}))
  (defn alias [name] (put env name {:kind :alias}))

  (match form

    {:kind :decl-alias
     :name name
     :alias expr}
    (do
      (match name
        {:container _ :inner names} (each name names (alias name))
        (alias name))
      (introduce-bindings expr env symbol-references))

    {:kind :declaration
     :name name
     :bindings bindings}
    (let [yields (form :yields)]
      (put env name {:kind (if (and (nil? yields) (empty? bindings))
                             :domain
                             :procedure)})
      (introduce-bindings yields env symbol-references)
      (each binding bindings
        (introduce-bindings binding env symbol-references)))

    {:kind :binding
     :name name
     :expr expression}
    (do
      (match name
        {:container _ :inner names} (each name names (bind name))
        (bind name))
      (introduce-bindings expression env symbol-references))

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
  (resolve-references ;(reduce (fn [[env references] chapter]
                                 (eval-chapter chapter env references))
                               [stdlib/root-env @{}]
                               chapters)))
