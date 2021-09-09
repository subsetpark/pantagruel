(use testament)

(import /pantagruel/engine)
(import /pantagruel/stdlib)

(defn is-eval
  [res tree]
  (is (== res (engine/eval tree))))

(defn is-chapter
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
  (is-head [{"f" {:kind :set}} {}]
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
                     :inner ["F"]}}]))

(deftest eval-body
  (is-body [{} {"g" true}] ["g"]))

(deftest eval-qualification
  (is-body [{"x" {:kind :bound}}
            {"Nat" true "x" true}]
           [{:bindings
             [{:expr "Nat"
               :kind :binding
               :name "x"}
              {:kind :binary-operation
               :left "x"
               :operator ">"
               :right 1}]
             :expr {:kind :binary-operation
                    :left "x"
                    :operator "<"
                    :right 10}
             :quantifier :some
             :kind :quantification}]))

(deftest eval-quantification-with-container
  (is-body
    [{"a" {:kind :bound}
      "b" {:kind :bound}}
     {"A" true "a" true "b" true}]
    [{:bindings
      [{:expr "A"
        :kind :binding
        :name {:container :parens
               :inner ["a" "b"]}}]
      :expr {:kind :binary-operation
             :left "a"
             :operator "+"
             :right "b"}
      :kind :quantification
      :quantifier :some}]))

(deftest eval-chapter
  (is-chapter [@{"X" :inject
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

(deftest eval-fib
  (is-eval [(merge stdlib/root-env
                   @{"fib" {:kind :procedure}
                     "x" {:kind :bound}})
            @{"fib" true
              "x" true}]

           {:chapters [{:body
                        [{:kind :binary-operation
                          :operator "="
                          :left {:f "fib" :kind :application :x "x"}
                          :right {:kind :case
                                  :mapping [{:kind :map
                                             :left {:kind :binary-operation
                                                    :operator ">"
                                                    :left "x"
                                                    :right 2}
                                             :right {:kind :binary-operation
                                                     :operator "+"
                                                     :left {:kind :application
                                                            :f "fib"
                                                            :x {:container :parens
                                                                :inner [{:kind :binary-operation
                                                                         :operator "-"
                                                                         :left "x"
                                                                         :right 1}]}}
                                                     :right {:kind :application
                                                             :f "fib"
                                                             :x {:container :parens
                                                                 :inner [{:kind :binary-operation
                                                                          :operator "-"
                                                                          :left "x"
                                                                          :right 2}]}}}}
                                            {:kind :map
                                             :left {:kind :binary-operation
                                                    :operator "="
                                                    :left "x"
                                                    :right 1}
                                             :right 1}
                                            {:kind :map
                                             :left {:kind :binary-operation
                                                    :operator "="
                                                    :left "x"
                                                    :right 2}
                                             :right 1}]}}]
                        :head [{:bindings
                                [{:expr "Nat"
                                  :kind :binding
                                  :name "x"}]
                                :kind :declaration
                                :name "fib"
                                :yields "Nat"}]
                        :kind :chapter}]}))

(run-tests!)
