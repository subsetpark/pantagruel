(import /pantagruel/types/errors)
(import /pantagruel/stdlib :prefix "")

(defn gcd-type
  ```
  Type unification logic.

  For any two types, find the "greatest common denominator", that is, the
  narrowest type, if any, that is common to the hierarchy of both.

  This explicity excludes Any, which is present in the hierarchy of all types,
  unless one of the two types *is* itself Any. That is: Any is the greatest
  common denominator of itself and any other type, but it's not the greatest
  common denominator of two non-Any types.

  User-defined types always behave as though they are directly descended from
  Any. Built-in types have a more elaborate hierarchy, allowing for meaningful
  interactions between, for instance, different members of the numeric tower.

  Accepts the "original arguments" in the case that this is a recursive call.
  If we reach the bottom of our recursion and have to throw a type error, they
  will be the ones we throw with.
  ```
  [left right &opt original-left original-right]
  (default original-left left)
  (default original-right right)

  (defn recurse
    [t t2]
    ```
    Recurse over two new arguments, preserving the initial arguments.
    ```
    (gcd-type t t2 original-left original-right))

  (defn find-gcd-
    ```
    Basic type unification. Recursively seek the "shallowest" type present in
    the hierarchies of both `t` and `t2`, where `n` is the depth of the search.
    ```
    [t t2 n]
    (or (and (= t t2) [n t])
        (let [t-proto (and (table? t2)
                           (find-gcd- t (table/getproto t2) (inc n)))
              t2-proto (and (table? t)
                            (find-gcd- (table/getproto t) t2 (inc n)))]
          (extreme (fn [x y]
                     (cond
                       (and x (not y)) true
                       (and y (not x)) false
                       (and x y) (<= (x 0) (y 0))))
                   [t-proto t2-proto]))))

  (defn find-gcd
    ```
    Type unification logic wrapping `find-gcd-` with special handling of `Any`.
    ```
    [t t2]
    (cond
      (= t Any) Any
      (= t2 Any) Any
      (if-let [[_n gcd] (find-gcd- t t2 0)]
        (if (and gcd (not= gcd Any))
          gcd))))

  (if-let [gcd (match [left right]
                 # Handle any 1-tuples we've received, eg, from type summing.
                 ([lt _] (tuple? lt) (one? (length lt)))
                 (recurse (lt 0) right)

                 ([_ rt] (tuple? rt) (one? (length rt)))
                 (recurse left (rt 0))

                 [{:kind :sum
                   :inner ts}
                  {:kind :sum
                   :inner ts2}]
                 (let [gcds @{}]
                   (each t ts
                     (each t2 ts2
                       (try
                         (let [success-type (recurse t t2)]
                           (put gcds success-type true))
                         ([err] :ok))))
                   (when (not (empty? gcds))
                     {:kind :sum
                      :inner (keys gcds)}))

                 [{:kind :sum :inner ts} t2]
                 (do
                   (var gcd nil)
                   (each t ts
                     (try
                       (let [success-type (recurse t t2)]
                         (set gcd success-type)
                         (break))
                       ([err] :ok)))
                   gcd)

                 [t {:kind :sum :inner t2s}]
                 (do
                   (var gcd nil)
                   (each t2 t2s
                     (try
                       (let [success-type (recurse t t2)]
                         (set gcd success-type)
                         (break))
                       ([err] :ok)))
                   gcd)

                 [{:list-of t} {:set-of t2}]
                 {:set-of (recurse t t2)}

                 [{:set-of t} {:list-of t2}]
                 {:set-of (recurse t t2)}

                 [{:list-of t} {:list-of t2}]
                 {:list-of (recurse t t2)}

                 [{:set-of t} {:set-of t2}]
                 {:set-of (recurse t t2)}

                 [{:tuple-of ts} {:tuple-of ts2}]
                 (when (= (length ts) (length ts2))
                   {:tuple-of (map recurse ts ts2)})

                 [{:args args-t :yields yields-t} {:args args-t2 :yields yields-t2}]
                 (let [args-gcd (recurse args-t args-t2)
                       yield-gcd (recurse yields-t yields-t2)]
                   {:args args-gcd :yields yield-gcd})

                 # When unifying two literal values, compare them directly;
                 # don't try to find a GCD.
                 [{:literal lit-left} {:literal lit-right}]
                 (if (= lit-left lit-right)
                   left
                   nil)

                 [t t2]
                 (find-gcd t t2))]
    gcd
    (errors/throw :gcd {:left original-left :right original-right})))
