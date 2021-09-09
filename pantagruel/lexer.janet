(defn- wrap-rule
  [[kw peg]]
  [kw ~(cmt (* ($) (constant ,kw) (<- ,peg) ($)) ,|{:kind $1 :text $2 :span [$0 $3]})])

(defn- rules-to-peg
  [rules]
  (def rules (map wrap-rule rules))
  (def grammar @{})
  (each [kw p] rules
    (put grammar kw p))
  (merge-into
    grammar
    ~{:main (* (any :tok) (choice (not 1) :lex-error))
      :tok ,(tuple 'choice ;(map first rules))
      :lex-error (* (cmt ($) ,|{:kind :lex-error :span [$ $]}) 1)})
  (table/to-struct grammar))

(def- lexer-grammar
  ~[[:comment (* "//" (thru "\n"))]
    [:string (* `"` (thru `"`))]
    [:ws :s+]
    [:where ";"]
    [:line "---"]
    [:... "..."]
    [:. "."]
    [:: ":"]
    [:yields "=>"]
    [:reverse-yields "<="]
    [:comma ","]
    [:update "update"]
    [:case "case"]
    [:all "all"]
    [:some "some"]
    [:lparen "("]
    [:rparen ")"]
    [:lsquare "["]
    [:rsquare "]"]
    [:lbrace "{"]
    [:rbrace "}"]
    [:logical-operator (+ "<->" "->")]
    [:boolean-operator (+ "=" ">" "<" "=<" ">=" "!="
                          # TODO: This is a total hack!
                          (* "in" (not :w))
                          (* "or" (not :w))
                          (* "and" (not :w)))]
    [:arithmetic-operator1 (+ "*" "/" "^")]
    [:arithmetic-operator2 (+ "+" "-")]
    [:unary-operator (+ "~" "#")]
    # TODO: Make proper float/int support.
    [:num (* (? "-") :d (any (+ :d "_")))]
    # TODO: Add unicode support
    [:sym (* :a (any (+ :w "'" "_" "?" "!")))]])

(def- lexer (-> lexer-grammar (rules-to-peg) (peg/compile)))

(defn lex
  [text]
  (def tokens (peg/match lexer text))
  (filter |(not (index-of ($ :kind) [:ws :comment])) tokens))
