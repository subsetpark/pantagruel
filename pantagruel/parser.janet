(import yacc)

(defn fst [& args] (first args))
(defn snd [& args] (args 1))
(defn kind [$0] ($0 :kind))

(defn wrap
  [container]
  (fn [_ inner _]
    {:container container :inner inner}))

(def grammar
  ~(yacc
     (%left :logical-operator)
     (%left :boolean-operator)
     (%left :arithmetic-operator2)
     (%left :arithmetic-operator1)
     (%left :funcapp)

     (program (chapters) ,|{:chapters $0})

     (chapters () ,tuple
               (chapter) ,tuple
               (chapter :where chapters) ,|(tuple $0 ;$2))
     (chapter (head body) ,|{:kind :chapter
                             :head $0
                             :body $1})

     (head (:line) ,(fn [_] [])
           (head-line :. head) ,|(tuple $0 ;$2))
     (head-line (sym bindings-exprs)
                ,|{:kind :declaration
                   :name $0
                   :bindings $1}

                (sym bindings-exprs :yields container-name)
                ,|{:kind :declaration
                   :name $0
                   :bindings $1
                   :yields $3}

                (sym :reverse-yields container-name)
                ,|{:kind :decl-alias
                   :name $0
                   :alias $2})

     (body () ,tuple
           (body-line) ,tuple
           (body-line body) ,|(tuple $0 ;$1))

     (body-line
       (expr :.) ,fst)

     (bindings-exprs () ,tuple
                     (binding-expr) ,tuple
                     (binding-expr :comma bindings-exprs) ,|(tuple $0 ;$2))
     (binding-expr (binding) ,identity
                   (expr) ,identity)
     (binding (sym :: expr) ,|{:kind :binding
                               :name $0
                               :expr $2})

     (mapping-word (:update) ,kind
                   (:case) ,kind)
     (mapping-form (:... mapping-clauses) ,snd)
     (mapping-clauses () ,tuple
                      (mapping-clause) ,tuple
                      (mapping-clause :comma mapping-clauses) ,|(tuple $0 ;$2))
     (mapping-clause (expr :yields expr) ,|{:kind :map
                                            :left $0
                                            :right $2})

     (quantification-word (:all) ,kind
                          (:some) ,kind)

     (container-name (sym) ,identity
                     (:lparen container-name :rparen) ,(wrap :parens)
                     (:lsquare container-name :rsquare) ,(wrap :square)
                     (:lbrace container-name :rbrace) ,(wrap :braces))

     (maybe-expr () ,nil
                 (expr) ,identity)
     (expr
       (mapping-word maybe-expr mapping-form) ,|{:kind $0
                                                 :case $1
                                                 :mapping $2}
       (expr :logical-operator expr) ,|{:kind :binary-operation
                                        :left $0
                                        :right $2
                                        :operator ($1 :text)}
       (expr :boolean-operator expr) ,|{:kind :binary-operation
                                        :left $0
                                        :right $2
                                        :operator ($1 :text)}
       (expr :arithmetic-operator1 expr) ,|{:kind :binary-operation
                                            :left $0
                                            :right $2
                                            :operator ($1 :text)}
       (expr :arithmetic-operator2 expr) ,|{:kind :binary-operation
                                            :left $0
                                            :right $2
                                            :operator ($1 :text)}
       (:unary-operator expr) ,|{:kind :unary-operation
                                 :left $1
                                 :operator ($0 :text)}
       (quantification-word bindings-exprs :yields expr) ,|{:kind $0
                                                            :bindings $1
                                                            :expr $3}

       (expr expr %prec :funcapp) ,|{:kind :application
                                     :f $0
                                     :x $1}

       (:lparen expr :rparen) ,(wrap :parens)
       (:lsquare expr :rsquare) ,(wrap :square)
       (:lbrace expr :rbrace) ,(wrap :braces)

       (sym) ,identity
       (num) ,identity)

     (binary-operator
       (:logical-operator) ,identity
       (:boolean-operator) ,identity
       (:arithmetic-operator1) ,identity
       (:arithmetic-operator2) ,identity)

     (sym (:sym) ,|($0 :text))
     (num (:num) ,|(scan-number ($0 :text)))))

(def parser-tables (yacc/compile grammar))

(defn- err-msg
  [{:kind kind :span (from to) :text text} src]
  (errorf
    `Syntax Error:

     Token type %q
     Text %q

     in

     ...%q...
    `
    kind text (string/slice src (- from 5) (+ to 5))))

(defn parse
  [tokens src]
  (-> (yacc/parse parser-tables tokens)
      (match
        [:ok tree] tree
        [:syntax-error err] (errorf "Syntax error: %q" (err-msg err src)))))
