## Type checking.
##
## Responsible for using the type resolution logic to check the types of a
## document and report errors if found.

(import /pantagruel/stdlib)
(import /pantagruel/types)
(import /pantagruel/print-src)

(defn- print-types
  [str & args]

  (defn- render-type
    [t]

    (defn- join
      [ts]
      (string/format "(%s)" (-> (map render-type ts) (string/join ", "))))

    (match t
      (ts (indexed? ts)) (join ts)
      {:concrete t-name} t-name
      {:set-of t} (string/format "{%s}" (render-type t))
      {:list-of t} (string/format "[%s]" (render-type t))
      {:tuple-of ts} (join ts)
      {:sum ts} (-> (map render-type ts) (string/join " + "))
      {:product ts} (-> (map render-type ts) (string/join " * "))
      {:args args :yields yields} (string/format "(%s => %s)" (render-type args) (render-type yields))
      t (string/format "%q" t)))

  (printf (string "Type error: " str) ;(map render-type args)))

(defn- handle-resolution-error
  [err]
  (case (err :type)
    :list-application-multiple-args
    (print-types "Attempted to apply a list type to multiple arguments:\n\n%s"
                 (err :xs))

    :list-application-bad-arg
    (print-types "Attempted to apply a list of type:\n\n%s\n\nto an argument of type:\n\n%s"
                 (err :f)
                 (err :x))

    :application
    (print-types "Attempted to apply non-procedure/list type:\n\n%s"
                 (err :f))

    :container
    (print-types "Attempted to check for membership or cardinality in non-container type:\n\n%s"
                 (err :t))

    :arg-length
    (print-types "Invalid arguments:\n\n%s\n\nto procedure expecting:\n\n%s"
                 (err :args)
                 (err :f-args))

    :gcd
    (print-types "Couldn't unify types:\n\n%s, %s" (err :left) (err :right))

    (print-types "Unknown type resolution error: %s" err)))

(defn type-check
  ```
  Attempt full type resolution, sequentially, of each body expression in a
  document. Output error messages for each type error found and, if any were
  found, exit with a non-zero status code.
  ```
  [tree env src]
  (let [body-exprs (mapcat |($0 :body) (tree :chapters))]
    (var type-error false)
    (each body-expr body-exprs
      (try
        (let [expr-t (types/resolve-type body-expr env)]
          (when (nil? expr-t)
            (errorf "Type was nil")))
        ([err fib]
          (if (table? err)
            (handle-resolution-error err)
            (propagate err fib))
          (printf "\nIn expression:\n\n%s\n" (print-src/print-src body-expr src))
          (set type-error true))))
    (if type-error (os/exit 1))))
