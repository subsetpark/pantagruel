(def- ResolutionError @{})

(defn throw
  ```
  Handle type errors encountered during type resolution.

  This doesn't include errors or gaps in type resolution logic, which will be
  raised immediately.
  ```
  [t &opt vars]
  (default vars @{})
  (error (table/setproto (merge vars @{:type t}) ResolutionError)))

