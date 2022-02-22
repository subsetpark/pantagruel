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

     (program (directives chapters) ,|{:directives $0
                                       :chapters $1})

     (directives () ,tuple
                 (directive directives) ,|(tuple $0 ;$1))

     (directive
       (:directive sym :.) ,(fn [directive sym _] {:kind :directive
                                                   :statement (directive :text)
                                                   :args sym}))

     (chapters () ,tuple
               (chapter) ,tuple
               (chapter :where chapters) ,|(tuple $0 ;$2))
     (chapter (head body) ,|{:kind :chapter
                             :head $0
                             :body $1})

     (head (:line) ,(fn [_] [])
           (head-line :. head) ,|(tuple $0 ;$2))
     (head-line
       (sym bindings-exprs)
       ,|{:kind :declaration
          :name $0
          :bindings $1}

       (sym bindings-exprs :yields container-name)
       ,|{:kind :declaration
          :name $0
          :bindings $1
          :yields $3}

       (container-name :reverse-yields expr)
       ,|{:kind :decl-alias
          :name $0
          :alias $2})

     (body () ,tuple
           (body-line) ,tuple
           (body-line body) ,|(tuple $0 ;$1))

     (body-line
       (expr :.) ,fst)

     (syms (sym) ,tuple
           (sym :comma syms) ,|(tuple $0 ;$2))

     (bindings-exprs () ,tuple
                     (binding-expr) ,tuple
                     (binding-expr :comma bindings-exprs) ,|(tuple $0 ;$2))
     (binding-expr (binding) ,identity
                   (expr) ,identity)
     (binding (container-name :: expr) ,|{:kind :binding
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
                          (:some) ,kind
                          (:some1) ,kind)

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
       (quantification-word bindings-exprs :yields expr) ,|{:kind :quantification
                                                            :quantifier $0
                                                            :bindings $1
                                                            :expr $3}

       (expr expr %prec :funcapp) ,|{:kind :application
                                     :f $0
                                     :x $1}

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
     (sym (:sym) ,|($0 :text))
     (num (:num) ,|(scan-number ($0 :text)))))

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
  [tokens src]
  (if (os/getenv "PANT_DEBUG") (setdyn :yydebug @""))

  (-> (yacc/parse parser-tables tokens)
      (match
        [:ok tree] tree
        [:syntax-error err] (errorf "Syntax error: %q" (err-msg err src)))))
