## Parser generator grammar.

(import yacc)

(defn span
  [left right]
  (cond
    (nil? left) (right :span)
    (nil? right) (left :span)
    (nil? (left :span)) (right :span)
    (nil? (right :span)) (left :span)

    [((left :span) 0) ((right :span) 0)]))

(defn wrap
  [container]
  (fn [left inner right]
    {:container container
     :inner inner
     :span (span left right)}))

(def grammar
  ~(yacc
     (%left :logical-operator)
     (%left :boolean-operator)
     (%left :arithmetic-operator2)
     (%left :arithmetic-operator1)
     (%left :funcapp)
     (%left :unary-operator)

     (program (directives chapters) ,|{:directives $0
                                       :chapters $1})

     (directives () ,tuple
                 (directive directives) ,|(tuple $0 ;$1))

     (directive
       (:directive sym :.) ,(fn [directive sym dot]
                              {:kind :directive
                               :statement (directive :text)
                               :args sym
                               :span (span directive dot)}))

     (chapters () ,tuple
               (chapter) ,tuple
               (chapter :where chapters) ,|(tuple $0 ;$2))
     (chapter (head body) ,|{:kind :chapter
                             :head $0
                             :body $1
                             :span (span ($0 0) (last $1))})

     (head (:line) ,(fn [_] [])
           (head-line :. head) ,|(tuple $0 ;$2))

     (head-line
       (sym bindings-exprs)
       ,|{:kind :declaration
          :name $0
          :bindings $1
          :span (span $0 $1)}

       (sym bindings-exprs :yields container-name)
       ,|{:kind :declaration
          :name $0
          :bindings $1
          :yields $3
          :span (span $0 $3)}

       (container-name :reverse-yields expr)
       ,|{:kind :decl-alias
          :name $0
          :alias $2
          :span (span $0 $2)})

     (body () ,tuple
           (body-line) ,tuple
           (body-line body) ,|(tuple $0 ;$1))

     (body-line
       (expr :.) ,|(struct ;(kvs $0) :span (span $0 $1)))

     (syms (sym) ,|{:kind :syms
                    :syms [$0]
                    :span ($0 :span)}
           (sym :comma syms) ,|{:kind :syms
                                :syms [$0 ;($2 :syms)]
                                :span (span $0 $2)})

     (bindings-exprs () ,|{:kind :bindings-exprs
                           :bindings-exprs []}
                     (binding-expr) ,|{:kind :bindings-exprs
                                       :bindings-exprs [$0]
                                       :span ($0 span)}
                     (binding-expr :comma bindings-exprs) ,|{:kind :bindings-exprs
                                                             :bindings-exprs [$0 ;($2 :bindings-exprs)]
                                                             :span (span $0 $2)})
     (binding-expr (binding) ,identity
                   (expr) ,identity)
     (binding (container-name :: expr) ,|{:kind :binding
                                          :name $0
                                          :expr $2})

     (mapping-word (:update) ,identity
                   (:case) ,identity)
     (mapping-form (:... mapping-clauses) ,|(struct ;(kvs $1) :span (span $0 $1)))
     (mapping-clauses (mapping-clause) ,|{:kind :mapping-clauses
                                          :mapping-clauses [$0]
                                          :span ($0 :span)}
                      (mapping-clause :comma mapping-clauses) ,|{:kind :mapping-clauses
                                                                 :mapping-clauses [$0 ;($2 :mapping-clauses)]
                                                                 :span (span $0 $2)})
     (mapping-clause (expr :yields expr) ,|{:kind :map
                                            :left $0
                                            :right $2
                                            :span (span $0 $2)})

     (quantification-word (:all) ,identity
                          (:some) ,identity
                          (:some1) ,identity)

     (container-name
       (sym) ,identity
       (container-names) ,identity)

     (container-names-or-syms
       (container-names) ,identity
       (syms) ,identity)

     (container-names
       (:lparen container-names-or-syms :rparen) ,(wrap :parens)
       (:lsquare container-names-or-syms :rsquare) ,(wrap :square)
       (:lbrace container-names-or-syms :rbrace) ,(wrap :braces))

     (maybe-expr () ,nil
                 (expr) ,identity)

     (exprs
       () ,tuple
       (expr) ,tuple
       (expr :comma exprs) ,|(tuple $0 ;$2))

     (expr
       (mapping-word maybe-expr mapping-form) ,|{:kind $0
                                                 :case $1
                                                 :mapping $2
                                                 :span (span $0 $2)}
       (expr :logical-operator expr) ,|{:kind :binary-operation
                                        :left $0
                                        :right $2
                                        :operator ($1 :text)
                                        :span (span $0 $2)}
       (expr :boolean-operator expr) ,|{:kind :binary-operation
                                        :left $0
                                        :right $2
                                        :operator ($1 :text)
                                        :span (span $0 $2)}
       (expr :arithmetic-operator1 expr) ,|{:kind :binary-operation
                                            :left $0
                                            :right $2
                                            :operator ($1 :text)
                                            :span (span $0 $2)}
       (expr :arithmetic-operator2 expr) ,|{:kind :binary-operation
                                            :left $0
                                            :right $2
                                            :operator ($1 :text)
                                            :span (span $0 $2)}
       (:unary-operator expr) ,|{:kind :unary-operation
                                 :left $1
                                 :operator ($0 :text)
                                 :span (span $0 $1)}
       (quantification-word bindings-exprs :yields expr) ,|{:kind :quantification
                                                            :quantifier $0
                                                            :bindings $1
                                                            :expr $3
                                                            :span (span $0 $3)}
       (expr expr %prec :funcapp) ,|{:kind :application
                                     :f $0
                                     :x $1
                                     :span (span $0 $1)}

       (:lparen exprs :rparen) ,(wrap :parens)
       (:lsquare exprs :rsquare) ,(wrap :square)
       (:lbrace exprs :rbrace) ,(wrap :braces)

       (string) ,identity
       (sym) ,identity
       (num) ,identity)

     (binary-operator
       (:logical-operator) ,identity
       (:boolean-operator) ,identity
       (:arithmetic-operator1) ,identity
       (:arithmetic-operator2) ,identity)

     (string (:string) ,identity)
     (sym (:sym) ,identity)
     (num (:num) ,|(struct ;(kvs $0) :text (scan-number ($0 :text))))))

(def parser-tables (yacc/compile grammar))

(defn- in-bounds
  [n mx]
  (if (< n 0)
    (max n (- mx))
    (min n mx)))

(defn- err-msg
  [form src]
  (default form {})
  (let [from (if-let [from (get-in form [:span 0])]
               (- from 10)
               -20)
        to (if-let [to (get-in form [:span 1])]
             (+ to 10)
             -1)
        prefix (if (or (= from (- (length src))) (= from 0)) "" "…")
        suffix (if (or (= to (length src)) (= to -1)) "" "…")]

    (if (os/getenv "PANT_DEBUG") (print (dyn :yydebug)))

    (errorf
      `Syntax Error:

      Token type %q
      Text %q

      in

      %s%s%s
      `
      (form :kind)
      (form :text)
      prefix
      (string/slice src (in-bounds from (length src)) (in-bounds to (length src)))
      suffix)))

(defn parse
  ```
  Generate an AST from a sequence of tokens.
  ```
  [tokens src]
  (if (os/getenv "PANT_DEBUG") (setdyn :yydebug @""))

  (-> (yacc/parse parser-tables tokens)
      (match
        [:ok tree] tree
        [:syntax-error err] (errorf "Syntax error: %q" (err-msg err src)))))
