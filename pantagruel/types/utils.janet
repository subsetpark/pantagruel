(import /pantagruel/types/gcd)
(import /pantagruel/stdlib :prefix "")

(defn number-type
  ```
  Given a number, resolve the narrowest element of the number tower it is a
  member of.
  ```
  [n]
  (cond
    (and (nat? n) (> n 0)) Nat
    (nat? n) Nat0
    (int? n) Int

    Real))

(defn sum-type
  ```
    Handle sum type syntax, either:
    - Foo + Bar
    - {value1, value2}
    ```
  [left-t right-t]
  # If either side is itself a sum type, unpack it; in other words,
  # (X + Y) + Z = {X, Y, Z}.
  (let [t1 (or (left-t :sum) [left-t])
        t2 (or (right-t :sum) [right-t])]
    (cond
      (and (all |($ :literal) t1)
           (all |($ :literal) t2))
      {:kind :sum :inner (distinct [;t1 ;t2])}

      (let [gcd (protect (gcd/gcd-type t1 t2))]
        (cond
          # If the two types are unifiable, return the common denominator.
          (gcd 0) (gcd 1)
          # If both sides are equivalent, we don't need a sum. In other words,
          # X + X = X.
          (and (= t1 t2) (one? (length t1))) (t1 0)

          {:kind :sum
           :inner (distinct [;t1 ;t2])})))))
