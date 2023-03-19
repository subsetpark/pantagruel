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

