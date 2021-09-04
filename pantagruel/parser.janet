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
     (program (chapters) ,|{:chapters $0})

     (chapters () ,tuple
               (chapter) ,tuple
               (chapter :where chapters) ,|(tuple $0 ;$2))
     (chapter (head body) ,|{:kind :chapter
                             :head $0
                             :body $1})

     (head (:line) ,tuple
           (head-line head) ,|(tuple $0 ;$1))
     (head-line (sym bindings-exprs :.)
                ,(fn [sym bindings-exprs _dot]
                   {:kind :declaration
                    :name sym
                    :bindings bindings-exprs})

                (sym bindings-exprs :yields container-name)
                ,(fn [sym bindings-exprs _ cont-name]
                   {:kind :declaration
                    :name sym
                    :bindings bindings-exprs
                    :yields cont-name})

                (sym :reverse-yields container-name)
                ,(fn [sym _ cont-name]
                   {:kind :decl-alias
                    :name sym
                    :alias cont-name}))

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
     (mapping-clause (expr :.. expr) ,|{:kind :map
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
       (expr :binary-operator expr) ,|{:kind :binary-operation
                                       :left $0
                                       :right $2
                                       :operator ($1 :text)}
       (:unary-operator expr) ,|{:kind :unary-operation
                                 :left $1
                                 :operator ($0 :text)}
       (quantification-word bindings-exprs :.. expr) ,|{:kind $0
                                                        :bindings $1
                                                        :expr $3}

       (:lparen expr :rparen) ,(wrap :parens)
       (:lsquare expr :rsquare) ,(wrap :square)
       (:lbrace expr :rbrace) ,(wrap :braces)

       (sym) ,identity
       (num) ,identity)

     (sym (:sym) ,|($0 :text))
     (num (:num) ,|(int/s64 ($0 :text)))))

(setdyn :yydebug @"")
(def parser-tables (yacc/compile grammar))

(defn parse
  [tokens]
  (yacc/parse parser-tables tokens))
