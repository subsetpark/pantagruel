## Type checking.
##
## Responsible for using the type resolution logic to check the types of a
## document and report errors if found.
(import spork/path)

(import /pantagruel/stdlib)
(import /pantagruel/types/types)
(import /pantagruel/print-src)

(defn print-types
  [str & args]

  (defn- render-type
    [t]

    (defn- join
      [ts]
      (string/format "(%s)" (-> (map render-type ts) (string/join ", "))))

    (match t
      (ts (indexed? ts)) (join ts)
      {:literal literal} (string literal)
      {:name t-name} t-name
      # Special case the empty set.
      {:container :set :inner ()} "{}"
      {:container :set :inner t} (string/format "{%s}" (render-type t))
      {:list-of t} (string/format "[%s]" (render-type t))
      {:tuple-of ts} (join ts)
      {:kind :sum :inner ts} (-> (map render-type ts) (string/join " + "))
      {:args args :yields yields} (string/format "%s => %s" (render-type args) (render-type yields))
      {:thunk thunk} (render-type thunk)
      {:kind :sym :text text} text
      t (string/format "%q" t)))

  (printf (string "type error. " str) ;(map render-type args)))

(defn- handle-resolution-error
  [err]
  (case (err :type)
    :list-application-multiple-args
    (print-types "attempted to apply to multiple arguments: `%s`"
                 (err :xs))

    :list-application-bad-arg
    (print-types "attempted to apply type: `%s` to an argument of type: `%s`"
                 (err :f)
                 (err :x))

    :application
    (print-types "attempted to apply type: `%s` to type: `%s`"
                 (err :f)
                 (err :x))

    :container
    (print-types "attempted to check for membership or cardinality in non-container type: `%s`"
                 (err :t))

    :arg-length
    (print-types "received invalid arguments: `%s` to procedure expecting: `%s`"
                 (err :args)
                 (err :f-args))

    :gcd
    (print-types "couldn't unify types: `%s` and `%s`" (err :left) (err :right))

    (print-types "unknown type resolution error: `%s`" err)))

(defn type-check
  ```
  Attempt full type resolution, sequentially, of each body expression in a
  document. Output error messages for each type error found and, if any were
  found, exit with a non-zero status code.
  ```
  [tree env file src]
  (def start-line (print-src/line-starter file src))

  (let [body-exprs (mapcat |($0 :body) (tree :chapters))]
    (var type-error false)
    (each body-expr body-exprs
      (try
        (types/resolve-type body-expr env)
        ([err fib]
          (if (table? err)
            (do
              (start-line body-expr)
              (handle-resolution-error err))
            (propagate err fib))
          (printf "\nin expression:\n\n%s\n" (print-src/print-src body-expr src))
          (set type-error true))))

    (when (and type-error (dyn :exit-on-error)) (os/exit 1))))