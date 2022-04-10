(import spork/path)

(defn print-src
  ```
  Given a span, return the slice of the source document that corresponds to it.
  ```
  [{:span span} src]
  (string/slice src ;span))

(defn- line-no
  ```
  Find the line number of an index into `src`.
  ```
  [{:span [n]} src]
  (var line 1)
  (for i 0 n
    (case (src i)
      (chr "\n") (+= line 1)))
  line)

(defn line-starter
  [file src]
  (fn [sym]
    (prinf "%s:%i: "
           (path/basename file)
           (line-no sym src))))
