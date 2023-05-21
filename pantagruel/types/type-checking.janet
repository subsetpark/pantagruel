## Type checking.
##
## Responsible for using the type resolution logic to check the types of a
## document and report errors if found.
(import spork/path)

(import /pantagruel/stdlib)
(import /pantagruel/types/types)

(defn get-type-errors
  ```
  Attempt full type resolution, sequentially, of each body expression in a
  document. Output error messages for each type error found and, if any were
  found, exit with a non-zero status code.
  ```
  [tree env file src]

  (let [type-errors @[]
        body-exprs (mapcat |($0 :body) (tree :chapters))]
    (var type-error false)
    (each body-expr body-exprs
      (try
        (types/resolve-type body-expr env)
        ([err fib]
          (if (table? err)
            (array/push type-errors [body-expr err])
            (propagate err fib)))))

    type-errors))

