(def base-env @{})

(defmacro- deftype
  [name parent]
  ~(upscope
     (def ,name (table/setproto @{:kind :domain
                                  :concrete ,(string name)} ,parent))
     (put base-env ,(string name) ,name)))

(defmacro- defvalue
  [name parent]
  (let [sym (symbol "pant-" name)]
    ~(upscope
       (def ,sym @{:value ,(string name) :type ,parent})
       (put base-env ,(string name) ,sym))))

(def Any @{:kind :domain
           :concrete "Any"})
(put base-env "Any" Any)

(deftype Domain Any)

(deftype Real Any)
(deftype Rat Real)
(deftype Int Rat)
(deftype Nat0 Int)
(deftype Bool Nat0)
(deftype Nat Nat0)
(deftype Char Nat)

(deftype String Any)
(deftype Date Any)
(deftype Void Any)

(defvalue "true" Bool)
(defvalue "false" Bool)
(defvalue "null" Any)

(def arithmetic-operators ["-" "+" "*" "/" "mod"])

(def comparison-operators ["=" "!=" "<" ">" "=<`" ">="])

(def boolean-operators ["~" "and" "or" "->" "<->"])
