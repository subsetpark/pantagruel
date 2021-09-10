(import /pantagruel/lexer)
(import /pantagruel/parser)
(import /pantagruel/engine)


(defn main
  [_ file]
  (let [src (case file "--"
              (file/read stdin :all)
              (slurp file))
        lexed (lexer/lex src)
        tree (parser/parse lexed src)]
    (engine/eval tree)))
