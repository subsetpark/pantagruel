(def- literals @{})

(defn intern
  ```
  Get or create an object around a specific literal value.
  ```
  [proto value]
  (if-let [interned (literals value)]
    interned
    (let [literal (table/setproto @{:literal value} proto)]
      (put literals value literal)
      literal)))

(defn widen
  ```
  Given a literal type, widen it to its prototype.
  ```
  [t]
  (match t
    {:literal _ }
    (table/getproto t)

    t))

