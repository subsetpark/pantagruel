(use spork/argparse)

(import spork/path)

(import /pantagruel/lexer)
(import /pantagruel/parser)
(import /pantagruel/eval/engine)
(import /pantagruel/eval/type-checking)
(import /pantagruel/print-src)

(def version "0.8.0")

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
   "path" {:kind :option
           :short "p"
           :help "The module path"
           :default "./pantagruel"}
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
      (string (form :text))
      (form :kind)
      prefix
      (string/slice src (in-bounds from (length src)) (in-bounds to (length src)))
      suffix))

  (when (dyn :exit-on-error) (os/exit 1)))

(defn default-form
  [src]
  {:span [(dec (length src)) (length src)]
   :text ""})

(defn handle-evaluation-error
  [file err src]
  (case (table/getproto err)
    engine/EvaluationError
    (each sym (keys (err :symbols))
      (printf "%s:%i: unglossed symbol error: %s"
              (path/basename file)
              (print-src/line-no sym src)
              (sym :text)))

    engine/SingleBindingError
    (do
      (prinf "%s:%i: " (path/basename file) (print-src/line-no (err :sym) src))
      (type-checking/print-types "can't bind %s to `%s`, already bound to `%s`"
                                 (get-in err [:sym :text])
                                 (get-in err [:t :type])
                                 (get-in err [:already :type]))))

  (when (dyn :exit-on-error) (os/exit 1)))

(defn handle-version
  []
  (print (string "Pantagruel " version))
  (os/exit))

(defn lex-and-parse
  [file src]
  (let [lexed (lexer/lex src)
        tree (try (parser/parse-tokens lexed)
               ([err fib] (if (table? err)
                            (let [form (or (err :form) (default-form src))]
                              (handle-syntax-error file form src))
                            (propagate err fib))))]
    tree))

(defn handle-src
  [file src]
  (let [tree (lex-and-parse file src)
        env (try (engine/eval tree)
              ([err fib] (if (table? err)
                           (handle-evaluation-error file err src)
                           (propagate err fib))))]
    (type-checking/type-check tree env file src)))

(defn populate-available-modules
  [module-path]
  (each file module-path
    (let [file (path/join (args "path") file)
          src (slurp file)
          {:directives directives} (lex-and-parse file src)]
      (var module-name nil)
      (each directive directives
        (when (= (directive :statement) "module")
          (let [directive-name (get-in directive [:args :text])]
            (when module-name
              (prinf "%s:%i: " file (print-src/line-no directive src))
              (printf "module name `%s` already declared; found module declaration `%s`"
                      module-name
                      directive-name))
            (set module-name directive-name))))
      (if module-name (put available-modules module-name file)))))

(defn main
  ```
  Main application logic.

  Load a file or read a document from stdin.

  Evaluate it and check; if all checks pass, return 0.
  ```
  [& _args]
  (setdyn :exit-on-error true)
  (let [args (argparse ;params)]
    (when (args "version") (handle-version))
    (let [[file src] (match (args :default)
                       (@ 'nil) ["<stdin>" (file/read stdin :all)]
                       [file] [file (slurp file)])
          module-path (->> (os/dir (args "path"))
                           (filter |(= (path/ext $) ".pant")))
          available-modules (populate-available-modules module-path)]

      (handle-src file src))))
