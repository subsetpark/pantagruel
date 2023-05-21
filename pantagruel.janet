(use spork/argparse)

(import spork/path)

(import /pantagruel/lexer)
(import /pantagruel/parser)
(import /pantagruel/eval/engine)
(import /pantagruel/types/type-checking)
(import /pantagruel/print-src)

(def version "0.9.1")
(def default-path "pantagruel")

(def params
  [```
   A program specification notation.

   Usage:
   pant <file1 file2 ...> | evaluate Pantagruel document files
   pant                   | read Pantagruel from stdin
   ```
   "version" {:kind :flag
              :short "v"
              :help "Show version and exit"}
   "path" {:kind :option
           :short "p"
           :help "The module path"}
   "config" {:kind :option
             :short "c"
             :help "The config file to consult, in JDN format"
             :default ".pantagruel.jdn"}
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
    (eprintf
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

(defn- print-types
  [str & args]

  (defn- render-type
    [t]

    (defn- join
      [ts]
      (string/format "(%s)" (-> (map render-type ts) (string/join ", "))))

    (defn- render-procedure
      [args yields]
      (string/format "%s => %s" (render-type args) (render-type yields)))

    (match t
      (ts (indexed? ts)) (join ts)
      {:literal literal} (string literal)
      {:name t-name} t-name
      # Special case the empty set.
      {:set-of ()} "{}"
      {:set-of t} (string/format "{%s}" (render-type t))
      {:list-of t} (string/format "[%s]" (render-type t))
      {:tuple-of ts} (join ts)
      {:kind :sum :inner ts} (-> (map render-type ts) (string/join " + "))
      {:decl-name name :args args :yields yields} (string/format
                                                    "%s %s"
                                                    name
                                                    (render-procedure args yields))
      {:args args :yields yields} (render-procedure args yields)
      {:thunk thunk} (render-type thunk)
      {:kind :sym :text text} text
      t (string/format "%q" t)))

  (eprintf (string "type error. " str) ;(map render-type args)))

(defn handle-evaluation-error
  [err]
  (let [{:evaluator {:src src :file file}} err]

    (def start-line (print-src/line-starter file src))

    (case (err :err)
      :evaluation
      (each sym (keys (err :symbols))
        (start-line sym)
        (eprintf "unglossed symbol error: %s"
                 (sym :text)))

      :single-binding
      (do
        (start-line (err :sym))
        (print-types
          "can't bind %s to `%s`, already bound to `%s`"
          (get-in err [:sym :text])
          (get-in err [:t :type])
          (get-in err [:already :type])))

      :import
      (do
        (start-line (err :to-import))
        (eprintf "import error: module `%s` not found. Available modules: %s"
                 (get-in err [:to-import :text])
                 (-> (err :available-modules) (keys) (string/join ", "))))

      :import-cycle
      (do
        (start-line (err :to-import))
        (eprintf "import cycle error: encountered cycle: %s"
                 (string/join (err :currently-importing-modules) " -> ")))

      (errorf "Got unknown evaluation error: %q" err)))

  (when (dyn :exit-on-error) (os/exit 1)))

(defn handle-version
  []
  (print (string "Pantagruel " version)))

(defn lex-and-parse
  [file src]

  (let [lexed (lexer/lex src)
        tree (try
               (parser/parse-tokens lexed)
               ([err fib]
                 (when (struct? err) (handle-syntax-error err file src))
                 (propagate err fib)))]
    tree))

(defn- handle-resolution-error
  [err]
  (case (err :type)
    :list-application-multiple-args
    (print-types "attempted to apply to multiple arguments: `%s`"
                 (err :xs))

    :list-application-bad-arg
    (print-types "attempted to apply type: %s to an argument of type: %s"
                 (err :f)
                 (err :x))

    :application
    (print-types "attempted to apply type: %s to type: %s"
                 (err :f)
                 (err :x))

    :container
    (print-types "attempted to check for membership or cardinality in non-container type: %s"
                 (err :t))

    :arg-length
    (print-types "received invalid arguments: %s to procedure expecting: %s"
                 (err :args)
                 (err :f-args))

    :gcd-app
    (print-types "couldn't bind value of type %s to argument of type %s in procedure: %s" (err :right) (err :left) (get-in err [:extra :f]))

    :gcd-comp
    (print-types "couldn't unify types for comparison: %s and %s" (err :left) (err :right))

    :gcd-arith
    (print-types "couldn't unify types for arithmetic: %s and %s" (err :left) (err :right))

    :gcd-in
    (print-types "couldn't unify set element type %s when checking membership of %s" (err :left) (err :right))

    :gcd-case-test
    (print-types "couldn't unify test expression of `case` %s with branch %s" (err :left) (err :right))

    :gcd-case-branches
    (print-types "couldn't unify branch expressions of `case` %s and %s" (err :left) (err :right))

    :gcd-update-procedure-args
    (print-types "couldn't unify procedure argument type of `update` %s with left side of mapping %s" (err :left) (err :right))

    :gcd-update-procedure-yields
    (print-types "couldn't unify procedure yields type of `update` %s with right side of mapping %s" (err :left) (err :right))

    :gcd-set-update
    (print-types "couldn't unify set of %s when updating with element type %s" (err :left) (err :right))

    :gcd-list-update
    (print-types "couldn't unify list of %s when updating with element type %s" (err :left) (err :right))

    :gcd-set-extension
    (print-types "couldn't unify set of %s when extending with element type %s" (err :left) (err :right))

    :gcd-list-extension
    (print-types "couldn't unify list of %s when extending with element type %s" (err :left) (err :right))

    :gcd
    (print-types "couldn't unify types: %s and %s" (err :left) (err :right))

    (print-types "unknown type resolution error: %s" err)))

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

  (array/clear engine/currently-importing-modules)

  (let [start-line (print-src/line-starter file src)
        tree (lex-and-parse file src)
        evaluator (engine/Evaluator file src)
        env (try (:eval evaluator tree available-modules evaluator-callback)
              ([err fib] (when (table? err) (handle-evaluation-error err))
                         (propagate err fib)))
        type-errors (type-checking/get-type-errors tree env file src)]

    (each [body-expr type-error] type-errors
      (start-line body-expr)
      (handle-resolution-error type-error)
      (eprintf "\nin expression:\n\n%s\n" (print-src/print-src body-expr src)))

    (when (and (not (empty? type-errors))
               (dyn :exit-on-error))
      (os/exit 1))

    type-errors))

(defn- maybe-read
  [filename]
  (when (os/stat filename) (slurp filename)))

(defn populate-available-modules
  ```
  Read the module path and parse all the Pantagruel files that are there. Build
  a map of module names (for any file that declares one) to file paths.
  ```
  [args]

  (defn pantagruel-files
    [path]
    (if (os/stat path)
      (->> (os/dir path)
           (filter |(= (path/ext $) ".pant"))
           (map |(path/join path $)))
      @[]))

  (def available-modules @{})

  (let [config (or (-?> (args "config") (maybe-read) (parse)) {})
        path (-> (or (args "path") (config "path") default-path))
        cur-path (pantagruel-files ".")
        module-path (pantagruel-files path)]
    (each file (array ;module-path ;cur-path)
      (let [src (slurp file)
            start-line (print-src/line-starter file src)
            {:directives directives} (lex-and-parse file src)]

        (var module-name nil)
        (each directive directives
          (match directive
            {:statement "module" :args {:text directive-name}}
            (do
              (when module-name
                (start-line directive)
                (eprintf "module name `%s` already declared; found module declaration `%s`"
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

  Load file(s) or read a document from stdin.

  Evaluate it and check; if all checks pass, return 0.
  ```
  [& _args]

  (def args (argparse ;params))
  (unless args (os/exit 1))
  (setdyn :exit-on-error true)

  (cond
    (args "version") (handle-version)
    (let [available-modules (populate-available-modules args)]

      (if-let [filenames (args :default)]
        (each file filenames
          (let [src (slurp file)]
            (handle-src file src available-modules)))

        (let [src (file/read stdin :all)]

          (handle-src "<stdin>" src available-modules))))))
