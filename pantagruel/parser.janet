## Parser generator grammar.

(import yacc)

(def SyntaxError @{})

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

(defn new-seq
  [&opt expr]
  {:kind :seq
   :seq (if expr [expr] [])
   :span (if expr (expr :span) nil)})

(defn cons-seq
  [expr _ rest]
  {:kind :seq
   :seq [expr ;(rest :seq)]
   :span (span expr rest)})

(def grammar
  ~(yacc

     (%left :logical-operator)
     (%left :boolean-operator)
     (%left :=)
     (%left :+)
     (%left :arithmetic-operator2)
     (%left :arithmetic-operator1)
     (%left :funcapp)
     (%left :unary-operator)

     (program (directives chapters) ,|{:directives $0
                                       :chapters $1})

     ## Directives
     # Module management statements for the checker.

     (directives () ,tuple
                 (directive directives) ,|(tuple $0 ;$1))

     (directive
       (:directive sym :.) ,(fn [directive sym dot]
                              {:kind :directive
                               :statement (directive :text)
                               :args sym
                               :span (span directive dot)}))

     ## Chapters
     # The main body of a document.

     (chapters () ,tuple
               (chapter) ,tuple
               (chapter :where chapters) ,|(tuple $0 ;$2))

     (chapter (head body) ,|{:kind :chapter
                             :head $0
                             :body $1
                             :span (span ($0 0) (last $1))})

     ### Head
     # The text above the line in a chapter.

     (head (:line) ,(fn [_] [])
           (head-line :. head) ,|(tuple $0 ;$2))

     (head-line
       (sym domain-bindings-exprs)
       ,|{:kind :declaration
          :name $0
          :bindings $1
          :span (span $0 $1)}

       (sym domain-bindings-exprs :yields domain)
       ,|{:kind :declaration
          :name $0
          :bindings $1
          :yields $3
          :span (span $0 $3)}

       (sym-or-tuple-of-syms := domain)
       ,|{:kind :decl-alias
          :name $0
          :alias $2
          :span (span $0 $2)})

     ### Body
     # The text below the line in a chapter.

     (body () ,tuple
           (body-line) ,tuple
           (body-line body) ,|(tuple $0 ;$1))

     (body-line
       (expr :.) ,|(struct ;(kvs $0) :span (span $0 $1)))

     ### Domains

     (domain
       (:lparen domains :rparen) ,(wrap :parens)
       (:lsquare domain :rsquare) ,(wrap :list-of)
       (:lbrace domain :rbrace) ,(wrap :set-of)
       (:lbrace literals :rbrace) ,|{:kind :domain-set
                                     :span (span $0 $2)
                                     :inner $1}
       (domain :+ domain) ,(fn
                             [left _ right]
                             (let [inner (if (left :inner)
                                           [;(left :inner) right]
                                           [left right])]
                               {:kind :domain-sum
                                :span (span left right)
                                :inner inner}))
       (sym) ,identity)

     (domains
       (domain) ,new-seq
       (domain :comma domains) ,cons-seq)

     ### Special forms

     #### Binding forms: x:X, y<:z

     ##### Binding forms that admit only domains.

     (domain-bindings-exprs () ,new-seq
                            (domain-binding-expr) ,new-seq
                            (domain-binding-expr :comma domain-bindings-exprs) ,cons-seq)

     (domain-binding-expr (domain-binding) ,identity
                          (expr) ,identity)

     (domain-binding
       (sym-or-tuple-of-syms :: domain) ,|{:kind :binding
                                           :binding-type ::
                                           :name $0
                                           :expr $2})

     ##### Binding forms that admit domains or iteration over values.

     (bindings-exprs () ,new-seq
                     (binding-expr) ,new-seq
                     (binding-expr :comma bindings-exprs) ,cons-seq)

     (binding-expr (binding) ,identity
                   (expr) ,identity)

     (binding
       (domain-binding) ,identity
       (sym-or-tuple-of-syms :from expr) ,|{:kind :binding
                                            :binding-type :from
                                            :name $0
                                            :expr $2})

     #### Mapping forms: case, update

     (mapping-word (:update) ,identity
                   (:case) ,identity)

     (mapping-clauses (mapping-clause) ,new-seq
                      (mapping-clause :comma mapping-clauses) ,cons-seq)

     (mapping-clause (expr :yields expr) ,|{:kind :map
                                            :left $0
                                            :right $2
                                            :span (span $0 $2)})

     #### Quantification: some, all, some1

     (quantification-word (:all) ,identity
                          (:some) ,identity
                          (:some1) ,identity)

     ### Expressions

     (expr
       (mapping-word maybe-expr :... mapping-clauses) ,|{:kind ($0 :kind)
                                                         :case $1
                                                         :mapping $3
                                                         :span (span $0 $3)}

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

       # = and + are special cased because they are normal binary operators,
       # but also special syntax (= is used for domain aliasing, + is used for
       # sum typing). So here we detect them specifically and roll them into
       # the binary-operation node type.
       (expr := expr) ,|{:kind :binary-operation
                         :left $0
                         :right $2
                         :operator ($1 :text)
                         :span (span $0 $2)}

       (expr :+ expr) ,|{:kind :binary-operation
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

       # Parens as value: tuple of values.
       (:lparen exprs :rparen) ,(wrap :parens)
       # Curly braces as value: set of values.
       (:lbrace exprs :rbrace) ,(wrap :value-set)

       (string) ,identity
       (sym) ,identity
       (num) ,identity)

     (maybe-expr () ,nil
                 (expr) ,identity)

     (exprs
       () ,tuple
       (expr) ,tuple
       (expr :comma exprs) ,|(tuple $0 ;$2))

     (literals
       (literal) ,new-seq
       (literal :comma literals) ,cons-seq)

     (literal
       (:num) ,identity
       (:string) ,identity)

     (binary-operator
       (:logical-operator) ,identity
       (:boolean-operator) ,identity
       (:=) ,identity
       (:+) ,identity
       (:arithmetic-operator1) ,identity
       (:arithmetic-operator2) ,identity)

     (sym-or-tuple-of-syms
       (:lparen syms :rparen) ,(wrap :parens)
       (sym) ,identity)

     (syms (sym) ,new-seq
           (sym :comma syms) ,cons-seq)

     (string (:string) ,identity)
     (sym (:sym) ,identity)
     (num (:num) ,identity)))

(def parser-tables (yacc/compile grammar))

(defn parse
  ```
  Generate an AST from a sequence of tokens.
  ```
  [tokens]
  (if (os/getenv "PANT_DEBUG") (setdyn :yydebug @""))

  (-> parser-tables
      (yacc/parse tokens)
      (match
        [:ok tree] tree
        [:syntax-error form] (error (table/setproto @{:form form} SyntaxError)))))
