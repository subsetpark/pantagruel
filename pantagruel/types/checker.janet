(import /pantagruel/stdlib)
(import /pantagruel/types/resolution)


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
      {:args args :yields yields} (string/format "(%s => %s)" (join args) (render-type yields))
      t (string/format "%q" t)))

  (printf str ;(map render-type args)))

(defn- handle-resolution-error
  [err]
  (case (err :type)
    :list-application-multiple-args
    (print-types "Attempted to apply a list type to multiple arguments:\n%s"
                 (err :xs))

    :list-application-bad-arg
    (print-types "Attempted to apply a list of type:\n%s\nto an argument of type:\n%s"
                 (err :f)
                 (err :x))

    :application
    (print-types "Attempted to apply non-procedure/list type:\n%s"
                 (err :f))

    :container
    (print-types "Attempted to check for membership in non-container type:\n%s"
                 (err :t))

    :arg-length
    (print-types "Invalid arguments:\n%s\nto procedure expecting:\n%s"
                 (err :args)
                 (err :f-args))

    :gcd
    (print-types "Couldn't unify types:\n%q, %q" (err :left) (err :right))

    (print-types "Unknown type resolution error: %s" err)))

(defn type-check
  [tree env]
  (let [body-exprs (mapcat |($0 :body) (tree :chapters))]
    (each body-expr body-exprs
      (try
        (let [expr-t (resolution/resolve-type body-expr env)]
          (when (nil? expr-t)
            (errorf "Type was nil")))
        ([err]
          (print "Type error.")
          (if (table? err)
            (handle-resolution-error err)
            (error err))
          (printf "In expression:\n%q" body-expr)
          (os/exit 1)))))

  true)
