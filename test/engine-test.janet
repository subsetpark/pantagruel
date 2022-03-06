(use testament)

(import /pantagruel/eval/engine)
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
  (is-head [@{"f" {:kind :domain
                   :type {:args ()
                          :yields @{:kind :domain
                                    :concrete "Void"}}}}
            @{}]
           [{:kind :declaration
             :name "f"
             :bindings []}]))

(deftest eval-declaration-with-binding
  (is-head [@{"f" {:kind :procedure
                   :type {:args @[{:thunk "X"}]
                          :yields @{:kind :domain
                                    :concrete "Void"}}}
              "x" {:kind :bound
                   :type {:thunk "X"}}}
            @{"X" true}]
           [{:kind :declaration
             :name "f"
             :bindings [{:kind :binding
                         :expr "X"
                         :name "x"}]}]))

(deftest eval-declaration-with-yields
  (is-head [@{"f" {:kind :procedure
                   :type {:args @[]
                          :yields {:thunk "F"}}}}
            @{"F" true}]
           [{:kind :declaration
             :name "f"
             :bindings []
             :yields "F"}]))

(deftest eval-alias-declaration
  (is-head [@{"f" {:kind :domain
                   :type {:thunk "F"}}}
            @{"F" true}]
           [{:kind :decl-alias
             :name "f"
             :alias "F"}]))

(deftest eval-alias-declaration-container
  (is-head [@{"f" {:kind :domain
                   :type {:list-of {:thunk "F"}}}}
            @{"F" true}]
           [{:kind :decl-alias
             :name "f"
             :alias {:container :square
                     :inner ["F"]}}]))

(deftest eval-body
  (is-body [{} {"g" true}] ["g"]))

(deftest eval-qualification
  (is-body [@{"x" {:kind :bound
                   :type {:thunk "Nat"}}}
            @{"Nat" true
              "x" true}]
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
    [@{"a" {:kind :bound
            :type {:thunk "A"}}
       "b" {:kind :bound
            :type {:thunk "A"}}}
     @{"A" true "a" true "b" true}]
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
                 "f" {:kind :procedure
                      :type {:args @[{:thunk "X"}]
                             :yields @{:concrete "Void"
                                       :kind :domain}}}
                 "x" {:kind :bound
                      :type {:thunk "X"}}}
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
  (is-eval (merge stdlib/base-env
                  {"fib" {:kind :procedure
                          :type {:args [{:thunk "Nat"}]
                                 :yields {:thunk "Nat"}}}
                   "x" {:kind :bound
                        :type {:thunk "Nat"}}})
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
