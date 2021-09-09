(defn introduce-bindings
  [form env symbol-references]
  (match form

    {:kind :decl-alias
     :name name
     :alias expr}
    (do
      (put env name {:kind :alias})
      (introduce-bindings expr env symbol-references))

    {:kind :declaration
     :name name
     :bindings bindings}
    (do
      (put env name {:kind :procedure})
      (introduce-bindings (form :yields) env symbol-references)
      (each binding bindings
        (introduce-bindings binding env symbol-references)))

    {:kind :binding
     :name name
     :expr expression}
    (do
      (put env name {:kind :bound})
      (introduce-bindings expression env symbol-references))

    {:container _ :inner expr} (introduce-bindings expr env symbol-references)

    (sym (string? sym)) (put symbol-references sym true)

    (@ nil) :ok

    form
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

(defn- resolve-references
  [env references]
  (each reference (keys references)
    (unless (env reference)
      (errorf "Unknown symbol: %q" reference))))

(defn eval-chapter
  [{:head head :body body} env prev-references]

  (let [head-references @{}
        body-references @{}]
    (eval-head head env head-references)
    (resolve-references env head-references)

    (eval-body body env body-references)
    (resolve-references env prev-references)
    [env body-references]))

(defn eval
  [{:chapters chapters}]
  (reduce (fn [[env references] chapter]
            (eval-chapter chapter env references))
          [@{} @{}]
          chapters))
