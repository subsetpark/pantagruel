(use testament)

(import yacc)
(import /pantagruel/parser)

(defn- is-parse
  [tokens ast &opt dbg]
  (var parsed nil)

  (defn- parse [] (set parsed (yacc/parse parser/parser-tables tokens)))

  (if dbg
    (with-dyns [:yydebug @""]
      (parse)
      (print (dyn :yydebug)))
    (parse))

  (is (== ast parsed)))

(deftest empty-program
  (is-parse
    []
    [:ok {:chapters []}]))

(deftest base-chapter
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}]
    [:ok {:chapters [{:kind :chapter
                      :head [{:kind :declaration
                              :name "f"
                              :bindings []}]
                      :body []}]}]))

(deftest base-head
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}
     {:kind :sym :text "g"} {:kind :.}]
    [:ok {:chapters [{:kind :chapter
                      :head [{:kind :declaration
                              :name "f"
                              :bindings []}]
                      :body ["g"]}]}]))

(deftest multiple-chapters
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}
     {:kind :sym :text "g"} {:kind :.}
     {:kind :where}
     {:kind :sym :text "h"} {:kind :.}
     {:kind :line}]
    [:ok {:chapters [{:kind :chapter
                      :head [{:kind :declaration
                              :name "f"
                              :bindings []}]
                      :body ["g"]}
                     {:kind :chapter
                      :head [{:kind :declaration
                              :name "h"
                              :bindings []}]
                      :body []}]}]))

(deftest precedence
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}
     {:kind :sym :text "x"} {:kind :arithmetic-operator2 :text "-"} {:kind :num :text "1"}
     {:kind :boolean-operator :text "="}
     {:kind :sym :text "y"} {:kind :arithmetic-operator2 :text "+"} {:kind :num :text "2"}
     {:kind :.}]
    [:ok {:chapters [{:body [{:kind :binary-operation
                              :left {:kind :binary-operation :left "x"
                                     :operator "-"
                                     :right 1}
                              :operator "="
                              :right {:kind :binary-operation
                                      :left "y"
                                      :operator "+"
                                      :right 2}}]
                      :head [{:bindings ()
                              :kind :declaration :name "f"}]
                      :kind :chapter}]}]))

(deftest fib
  (is-parse
    [{:kind :sym :text "fib"}
     {:kind :sym :text "x1"} {:kind :: :text ":"} {:kind :sym :text "Nat"}
     {:kind :yields :text "=>"} {:kind :sym :text "Nat"} {:kind :. :text "."}
     {:kind :line :text "---"}
     {:kind :sym :text "fib"} {:kind :sym :text "x2"} {:kind :boolean-operator :text "="}
     {:kind :case :text "case"} {:kind :... :text "..."}
     {:kind :sym :text "x3"} {:kind :boolean-operator :text ">"} {:kind :num :text "2"}
     {:kind :yields :text "=>"}
     {:kind :sym :text "fib"}
     {:kind :lparen}
     {:kind :sym :text "x4"} {:kind :arithmetic-operator2 :text "-"} {:kind :num :text "1"}
     {:kind :rparen}
     {:kind :arithmetic-operator2 :text "+"}
     {:kind :sym :text "fib"}
     {:kind :lparen}
     {:kind :sym :text "x5"} {:kind :arithmetic-operator2 :text "-"} {:kind :num :text "2"}
     {:kind :rparen}
     {:kind :comma :text ","}
     {:kind :sym :text "x6"} {:kind :boolean-operator :text "="} {:kind :num :text "1"}
     {:kind :yields :text "=>"}
     {:kind :num :text "1"} {:kind :comma :text ","}
     {:kind :sym :text "x7"} {:kind :boolean-operator :text "="} {:kind :num :text "2"}
     {:kind :yields :text "=>"} {:kind :num :text "1"} {:kind :. :text "."}]
    [:ok {:chapters [{:body
                      [{:kind :binary-operation
                        :operator "="
                        :left {:f "fib" :kind :application :x "x2"}
                        :right {:kind :case
                                :mapping [{:kind :map
                                           :left {:kind :binary-operation
                                                  :operator ">"
                                                  :left "x3"
                                                  :right 2}
                                           :right {:kind :binary-operation
                                                   :operator "+"
                                                   :left {:kind :application
                                                          :f "fib"
                                                          :x {:container :parens
                                                              :inner {:kind :binary-operation
                                                                      :operator "-"
                                                                      :left "x4"
                                                                      :right 1}}}
                                                   :right {:kind :application
                                                           :f "fib"
                                                           :x {:container :parens
                                                               :inner {:kind :binary-operation
                                                                       :operator "-"
                                                                       :left "x5"
                                                                       :right 2}}}}}
                                          {:kind :map
                                           :left {:kind :binary-operation
                                                  :operator "="
                                                  :left "x6"
                                                  :right 1}
                                           :right 1}
                                          {:kind :map
                                           :left {:kind :binary-operation
                                                  :operator "="
                                                  :left "x7"
                                                  :right 2}
                                           :right 1}]}}]
                      :head [{:bindings
                              [{:expr "Nat"
                                :kind :binding
                                :name "x1"}]
                              :kind :declaration
                              :name "fib"
                              :yields "Nat"}]
                      :kind :chapter}]}]))

(run-tests!)
