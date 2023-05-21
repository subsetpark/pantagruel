## Logic to derive type information from purely syntactic forms.

(import /pantagruel/types/utils)
(import /pantagruel/types/literals)
(import /pantagruel/stdlib :prefix "")

(defn- distribute-bindings-types
  ```
  Handle binding form (x, y, z):T, shorthand for x:T, y:T, z:T.
  ```
  [bindings]

  (defn distribute-binding-type
    [binding]
    (match binding
      {:kind :binding
       :name {:container :parens :inner {:seq inner}}
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

(defn type-of-form
  ```
  All forms that syntactically establish some type.
  ```
  [form]

  (defn unwrap
    [wrapped]
    (if (one? (length wrapped))
      (type-of-form (wrapped 0))
      (map type-of-form wrapped)))

  (match form
    {:container :list-of
     :inner inner}
    {:list-of (type-of-form inner)}

    {:container :set-of
     :inner inner}
    {:set-of (type-of-form inner)}

    {:kind :domain-sum
     :inner inner}
    (or
      (reduce2 utils/sum-type (map type-of-form inner))
      {:set-of []})

    {:kind :domain-set
     :inner {:seq inner}}
    (or
      (reduce2 utils/sum-type (map type-of-form inner))
      {:set-of []})

    {:container :parens
     :inner inner}
    (let [inner-t (type-of-form inner)]
      (if (array? inner-t)
        {:tuple-of inner-t}
        inner-t))

    {:kind :declaration
     :name {:text name}
     :yields yields
     :bindings {:seq bindings}}
    {:decl-name name
     :yields (type-of-form yields)
     :args {:tuple-of (map type-of-form (distribute-bindings-types bindings))}}

    ({:kind :declaration
      :name {:text name}
      :bindings {:seq bindings}} (empty? bindings))
    (if (= (name 0) ((string/ascii-upper name) 0))
      Domain
      {:decl-name name
       :args {:tuple-of []}
       :yields Void})

    {:kind :declaration
     :name {:text name}
     :bindings {:seq bindings}}
    {:decl-name name
     :args {:tuple-of (map type-of-form (distribute-bindings-types bindings))}
     :yields Void}

    {:kind :string :text text}
    (literals/intern String text)

    {:kind :num
     :text n}
    (literals/intern (utils/number-type n) n)

    # Recursive cases
    ({:seq wrapped} (tuple? wrapped))
    (unwrap wrapped)

    (wrapped (tuple? wrapped))
    (unwrap wrapped)

    # Fall-through case: if we can't tell the type now, defer it for later.
    {:thunk form}))

