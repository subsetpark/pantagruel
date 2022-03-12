(use spork/argparse)

(import /pantagruel/lexer)
(import /pantagruel/parser)
(import /pantagruel/eval/engine)
(import /pantagruel/eval/type-checking)

(def version "0.5.0")

(def params
  [```
   A program specification notation.

   # USAGE

   pant <file> | evaluate Pantagruel text <file>
   pant        | read Pantagruel from stdin
   ```
   "version" {:kind :flag
              :short "v"
              :help "Show version and exit"}
   :default {:kind :accumulate}])

(defn handle-evaluation-error
  [err]
  (printf "Unglossed symbols%s:\n\n%s"
          (case (err :locale)
            :body ""
            :chapter " in chapter head")
          (string/join (keys (err :symbols)) ", "))
  (os/exit 1))

(defn main
  ```
  Main application logic.

  Load a file or read a document from stdin.

  Evaluate it and check; if all checks pass, return 0.
  ```
  [& _args]
  (let [args (argparse ;params)]
    (when (args "version")
      (print (string "Pantagruel " version))
      (os/exit))
    (let [src (match (args :default)
                (@ 'nil) (file/read stdin :all)
                [file] (slurp file))
          lexed (lexer/lex src)
          tree (parser/parse lexed src)
          env (try (engine/eval tree)
                ([err fib] (if (table? err)
                             (handle-evaluation-error err)
                             (propagate err fib))))]
      (type-checking/type-check tree env src))))
