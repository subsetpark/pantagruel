(use spork/argparse)

(import spork/path)

(import /pantagruel/lexer)
(import /pantagruel/parser)
(import /pantagruel/eval/engine)
(import /pantagruel/eval/type-checking)
(import /pantagruel/print-src)

(def version "0.5.2")

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
  [file form src]

  (defn- in-bounds
    [n mx]
    (if (< n 0)
      (max n (- mx))
      (min n mx)))

  (let [from (if-let [from (get-in form [:span 0])]
               (- from 10)
               -20)
        to (if-let [to (get-in form [:span 1])]
             (+ to 10)
             -1)
        prefix (if (or (= from (- (length src))) (= from 0)) "" "…")
        suffix (if (or (= to (length src)) (= to -1)) "" "…")]

    (if (os/getenv "PANT_DEBUG") (print (dyn :yydebug)))

    (printf
      ```
      %s:%i: syntax error: `%s` (%q)

      in

      %s%s%s
      ```
      file
      (print-src/line-no form src)
      (form :text)
      (form :kind)
      prefix
      (string/slice src (in-bounds from (length src)) (in-bounds to (length src)))
      suffix))

  (os/exit 1))

(defn default-form
  [src]
  {:span [(dec (length src)) (length src)]
   :text ""})

(defn handle-evaluation-error
  [file err src]
  (each sym (keys (err :symbols))
    (printf "%s:%i: unglossed symbol error: %s"
            (path/basename file)
            (print-src/line-no sym src)
            (sym :text)))

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
                            (let [form (or (err :form) (default-form src))]
                              (handle-syntax-error file form src))
                            (propagate err fib))))
        env (try (engine/eval tree)
              ([err fib] (if (table? err)
                           (handle-evaluation-error file err src)
                           (propagate err fib))))]
    (type-checking/type-check tree env file src)))

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
