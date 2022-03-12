## PEG-based grammar and lexer.

(defn- wrap-rule
  [[kw peg]]
  [kw ~(cmt
         (* ($) (constant ,kw) (<- ,peg) ($))
         ,(fn
            [left kind text right-or-num &opt maybe-right]
            # Special handling of numbers:
            # We use cmt with scan-number to parse numbers, meaning that in a
            # number match there'll be an extra argument. If there are 5
            # arguments, treat the last argument as the right span and then
            # second-to-last as the original input to the cmt form.
            (let [right (if maybe-right maybe-right right-or-num)]
              {:kind kind :text text :span [left right]})))])

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
  # Symbol grammar cribbed from Janet spec.
  (let [digits-peg '(some (+ (range "09" "AZ" "az") (set "_")))
        sym-peg '(some (+ (range "09" "AZ" "az" "\x80\xFF") (set "'!$%&*+-/<?=>@^_")))]
    ~[[:comment (* "//" (thru "\n"))]
      [:string (* `"` (thru `"`))]
      [:ws :s+]
      [:directive (+ "module" "import")]
      [:where ";"]
      [:line "---"]
      [:... "..."]
      [:. "."]
      [:: ":"]
      [:yields "=>"]
      [:= "="]
      [:comma ","]
      [:update "update"]
      [:case "case"]
      [:all "all"]
      [:some1 "some1"]
      [:some "some"]
      [:lparen "("]
      [:rparen ")"]
      [:lsquare "["]
      [:rsquare "]"]
      [:lbrace "{"]
      [:rbrace "}"]
      [:logical-operator (+ "<->" "->")]
      [:boolean-operator (+ ">" "<" "=<" ">=" "!="
                            # TODO: This is a total hack!
                            (* "in" (not :w))
                            (* "or" (not :w))
                            (* "and" (not :w))
                            (* "xor" (not :w)))]
      [:arithmetic-operator1 (+ "*" "/" "^"
                                (* "mod" (not :w)))]
      [:arithmetic-operator2 (+ "+" "-" "|" "&")]
      [:unary-operator (+ "~" "#")]
      [:num (cmt (<- (+
                       (* ,digits-peg "." ,digits-peg)
                       (* "." ,digits-peg)
                       ,digits-peg))
                 ,scan-number)]
      [:sym ,sym-peg]]))

(def- lexer (-> lexer-grammar (rules-to-peg) (peg/compile)))

(defn lex
  ```
  Generate an array of tokens from document text.
  ```
  [text]
  (def tokens (peg/match lexer text))
  (filter |(not (index-of ($ :kind) [:ws :comment])) tokens))
