(use spork/argparse)

(import /pantagruel/lexer)
(import /pantagruel/parser)
(import /pantagruel/engine)

(def params
  [```
   A program specification notation.

   # USAGE

   pant <file> | evaluate Pantagruel text <file>
   pant        | read Pantagruel from stdin
   ```
   :default {:kind :accumulate}])

(defn main
  [& _args]
  (let [args (argparse ;params)
        src (match (args :default)
              (@ 'nil) (file/read stdin :all)
              [file] (slurp file))
        lexed (lexer/lex src)
        tree (parser/parse lexed src)]
    (engine/eval tree)))
