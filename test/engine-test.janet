(use testament)

(import /pantagruel/engine)

(defn is-eval
  [res tree &opt env references]

  (default env @{})
  (default references @{})

  (is (== res
          (engine/eval-chapter tree env references))))

(defn is-head
  [res tree]
  (is (== res (engine/eval-head tree @{} @{}))))

(defn is-body
  [res tree]
  (is (== res (engine/eval-body tree @{} @{}))))

(deftest eval-single-declaration
  (is-head [{"f" {:kind :procedure}} {}]
           [{:kind :declaration
             :name "f"
             :bindings []}]))

(deftest eval-declaration-with-binding
  (is-head [{"f" {:kind :procedure}
             "x" {:kind :bound}}
            {"X" true}]
           [{:kind :declaration
             :name "f"
             :bindings [{:kind :binding
                         :expr "X"
                         :name "x"}]}]))

(deftest eval-declaration-with-yields
  (is-head [{"f" {:kind :procedure}}
            {"F" true}]
           [{:kind :declaration
             :name "f"
             :bindings []
             :yields "F"}]))

(deftest eval-alias-declaration
  (is-head [{"f" {:kind :alias}}
            {"F" true}]
           [{:kind :decl-alias
             :name "f"
             :alias "F"}]))

(deftest eval-alias-declaration-container
  (is-head [{"f" {:kind :alias}}
            {"F" true}]
           [{:kind :decl-alias
             :name "f"
             :alias {:container :square
                     :inner "F"}}]))

(deftest eval-body
  (is-body [{} {"g" true}] ["g"]))

(deftest eval-chapter
  (is-eval [@{"X" :inject
              "f" {:kind :procedure}
              "x" {:kind :bound}}
            @{"y" true}]

           {:kind :chapter
            :head [{:kind :declaration
                    :name "f"
                    :bindings [{:kind :binding
                                :expr "X"
                                :name "x"}]}]
            :body ["y"]}
           @{"X" :inject}))

(run-tests!)
