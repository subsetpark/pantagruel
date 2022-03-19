(def . {:kind :.})
(def bind {:kind ::})
(def --- {:kind :line})
(def => {:kind :yields})
(def = {:kind := :text "="})
(def + {:kind :arithmetic-operator2 :text "+"})
(def lp {:kind :lparen})
(def rp {:kind :rparen})
(def card {:kind :unary-operator :text "#"})
(def comma {:kind :comma :text ","})

(def head-placeholder [{:kind :sym :text "f"} . ---])

(defn sym [text] {:kind :sym :text text})
(defn num [text] {:kind :num :text text})

