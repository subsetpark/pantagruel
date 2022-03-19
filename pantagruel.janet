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

(defn handle-syntax-error
  [file {:form form} src]

  (defn- in-bounds
    [n mx]
    (if (< n 0)
      (max n (- mx))
      (min n mx)))

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

(defn handle-evaluation-error
  [file err]
  (printf "In file: %s" file)
  (printf "Unglossed symbols%s:\n\n%s"
          (case (err :locale)
            :body ""
            :chapter " in chapter head")
          (string/join (keys (err :symbols)) ", "))
  (os/exit 1))

(defn handle-version
  []
  (print (string "Pantagruel " version))
  (os/exit))

(defn handle-src
  [file src]
  (let [lexed (lexer/lex src)
        tree (try (parser/parse-tokens lexed)
               ([err fib] (if (table? err)
                            (handle-syntax-error file err src)
                            (propagate err fib))))
        env (try (engine/eval (tracev tree))
              ([err fib] (if (table? err)
                           (handle-evaluation-error file err)
                           (propagate err fib))))]
    (type-checking/type-check tree env src)))

(defn main
  ```
  Main application logic.

  Load a file or read a document from stdin.

  Evaluate it and check; if all checks pass, return 0.
  ```
  [& _args]
  (let [args (argparse ;params)]
    (when (args "version") (handle-version))
    (let [[file src] (match (args :default)
                       (@ 'nil) ["<stdin>" (file/read stdin :all)]
                       [file] [file (slurp file)])]
      (handle-src file src))))
