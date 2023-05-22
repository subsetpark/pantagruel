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

  (let [type-errors @[]]

    (defn check-expr
      [e]
      (try
        (types/resolve-type e env)
        ([err fib]
          (if (table? err)
            (array/push type-errors [e err])
            (propagate err fib)))))

    (var type-error false)
    (each {:head head :body body} (tree :chapters)
      (each head-expr head
        (when (head-expr :bindings)
          (check-expr head-expr)))
      (each body-expr body
        (check-expr body-expr)))

    type-errors))
