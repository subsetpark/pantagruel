(defn print-src
  ```
  Given a span, return the slice of the source document that corresponds to it.
  ```
  [{:span span} src]
  (string/slice src ;span))
