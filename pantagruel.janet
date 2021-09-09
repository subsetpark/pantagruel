(import /pantagruel/lexer)
(import /pantagruel/parser)


(defn main
  [_ file]
  (let [src (slurp file)
        lexed (lexer/lex src)
        parsed (parser/parse lexed src)]
    (pp parsed)))
