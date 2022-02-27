(def base-env @{})

(defmacro- deftype
  [name parent]
  ~(upscope
     (def ,name (table/setproto @{:concrete ,(string name)} ,parent))
     (put base-env ,(string name) ,name)))

(defmacro- defvalue
  [name parent]
  (let [sym (symbol "pant-" name)]
    ~(upscope
       (def ,sym (table/setproto @{:value ,(string name)} ,parent))
       (put base-env ,(string name) ,sym))))

(def Any @{:concrete "Any"})

(deftype Real Any)
(deftype Rat Real)
(deftype Int Rat)
(deftype Nat0 Int)
(deftype Nat Nat0)
(deftype Bool Nat0)

(def numeric-types [Nat Nat0 Int Rat Real])

(deftype String Any)
(deftype Date Any)
(deftype Void Any)

(defvalue "true" Bool)
(defvalue "false" Bool)
(defvalue "null" Any)

(def arithmetic-operators ["-" "+" "*" "/"])

(def comparison-operators ["=" "<" ">" "<=" ">="])

(def boolean-operators ["and" "or"])
