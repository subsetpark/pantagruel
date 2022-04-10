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

   pant FLAGS <file> | evaluate Pantagruel text <file>
   pant FLAGS        | read Pantagruel from stdin

   Flags:
     --path, -p      | Specify the module path containing documents that can be
                     | imported.
                     | Default: ./pantagruel
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
  [err file src]

  (def start-line (print-src/line-starter file src))

  (defn- in-bounds
    [n mx]
    (if (< n 0)
      (max n (- mx))
      (min n mx)))

  (let [form (or (err :form)
                 {:span [(dec (length src)) (length src)]
                  :text ""})
        from (if-let [from (get-in form [:span 0])]
               (- from 10)
               -20)
        to (if-let [to (get-in form [:span 1])]
             (+ to 10)
             -1)
        prefix (if (or (= from (- (length src)))
                       (= from 0))
                 ""
                 "…")
        suffix (if (or (= to (length src))
                       (= to -1))
                 ""
                 "…")]

    (if (os/getenv "PANT_DEBUG") (print (dyn :yydebug)))

    (start-line form)
    (printf
      ```
      syntax error: `%s` (%q)

      in

      %s%s%s
      ```
      (string (form :text))
      (form :kind)
      prefix
      (string/slice src (in-bounds from (length src)) (in-bounds to (length src)))
      suffix))

  (when (dyn :exit-on-error)
    (os/exit 1)))

(defn handle-evaluation-error
  [err]
  (let [{:evaluator {:src src :file file}} err]

    (def start-line (print-src/line-starter file src))

    (case (err :err)
      :evaluation
      (each sym (keys (err :symbols))
        (start-line sym)
        (printf "unglossed symbol error: %s"
                (sym :text)))

      :single-binding
      (do
        (start-line (err :sym))
        (type-checking/print-types
          "can't bind %s to `%s`, already bound to `%s`"
          (get-in err [:sym :text])
          (get-in err [:t :type])
          (get-in err [:already :type])))

      :import
      (do
        (start-line (err :to-import))
        (printf "import error: module `%s` not found. Available modules: %q"
                (get-in err [:to-import :text])
                (keys (err :available-modules))))

      (errorf "Got unknown evaluation error: %q" err)))

  (when (dyn :exit-on-error) (os/exit 1)))

(defn handle-version
  []
  (print (string "Pantagruel " version))
  (os/exit))

(defn lex-and-parse
  [file src]

  (let [lexed (lexer/lex src)
        tree (try
               (parser/parse-tokens lexed)
               ([err fib]
                 (when (struct? err) (handle-syntax-error err file src))
                 (propagate err fib)))]
    tree))

(defn handle-src
  ```
  Given some input file, fully evaluate it as a Pantagruel document.
  ```
  [file src available-modules]

  (defn evaluator-callback
    ```
    The logic to slurp and parse a file, encapsulated and passed into the :eval
    method. This way, the evaluator logic knows how to call this callback but
    it doesn't know how to lex and parse by itself.
    ```
    [file]
    (let [src (slurp file)
          tree (lex-and-parse file src)]
      [tree (engine/Evaluator file src)]))

  (let [tree (lex-and-parse file src)
        evaluator (engine/Evaluator file src)
        env (try (:eval evaluator tree available-modules evaluator-callback)
              ([err fib] (when (table? err) (handle-evaluation-error err))
                         (propagate err fib)))]
    (type-checking/type-check tree env file src)))

(defn populate-available-modules
  ```
  Read the module path and parse all the Pantagruel files that are there. Build
  a map of module names (for any file that declares one) to file paths.
  ```
  [path]

  (def available-modules @{})

  (let [module-path (->> (os/dir path)
                         (filter |(= (path/ext $) ".pant")))]
    (each file module-path
      (let [file (path/join path file)
            src (slurp file)
            start-line (print-src/line-starter file src)
            {:directives directives} (lex-and-parse file src)]

        (var module-name nil)
        (each directive directives
          (match directive
            {:statement "module" :args {:text directive-name}}
            (do
              (when module-name
                (start-line directive)
                (printf "module name `%s` already declared; found module declaration `%s`"
                        module-name
                        directive-name)
                (when (dyn :exit-on-error) (os/exit 1))
                (error :available-modules-error))

              (set module-name directive-name))))

        (when module-name
          (put available-modules module-name file)))))

  available-modules)

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
          available-modules (populate-available-modules (args "path"))]

      (handle-src file src available-modules))))
