(def base-env @{})

(def Domain @{:kind :domain
              :name "Domain"})
(put base-env "Domain" Domain)

(defmacro- deftype
  [name parent]
  ~(upscope
     (def ,name (table/setproto @{:kind :domain
                                  :name ,(string name)
                                  :type {:concrete Domain}}
                                ,parent))
     (put base-env ,(string name) ,name)))

(defmacro- defvalue
  [name parent]
  (let [sym (symbol "pant-" name)]
    ~(upscope
       (def ,sym @{:value ,(string name) :type ,parent})
       (put base-env ,(string name) ,sym))))

(def Any @{:kind :domain
           :name "Any"
           :type {:concrete Domain}})
(put base-env "Any" Any)

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
