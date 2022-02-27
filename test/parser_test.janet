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
    [:ok {:directives [] :chapters []}]))

(deftest base-chapter
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings []}]
                                     :body []}]}]))

(deftest base-head
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}
     {:kind :sym :text "g"} {:kind :.}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings []}]
                                     :body ["g"]}]}]))

(deftest head-with-binding
  (is-parse
    [{:kind :sym :text "f"} {:kind :sym :text "x"} {:kind ::} {:kind :sym :text "X"} {:kind :.}
     {:kind :line}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings [{:kind :binding
                                                         :expr "X"
                                                         :name "x"}]}]
                                     :body []}]}]))

(deftest head-with-yields
  (is-parse
    [{:kind :sym :text "f"} {:kind :yields} {:kind :sym :text "F"} {:kind :.}
     {:kind :line}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings []
                                             :yields "F"}]
                                     :body []}]}]))

(deftest head-with-reverse-yields
  (is-parse
    [{:kind :sym :text "f"} {:kind :reverse-yields} {:kind :sym :text "F"} {:kind :.}
     {:kind :line}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name "f"
                                             :alias "F"}]
                                     :body []}]}]))

(deftest head-with-reverse-yields-container
  (is-parse
    [{:kind :sym :text "f"} {:kind :reverse-yields} {:kind :lsquare} {:kind :sym :text "F"} {:kind :rsquare} {:kind :.}
     {:kind :line}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name "f"
                                             :alias {:container :square
                                                     :inner ["F"]}}]
                                     :body []}]}]))

(deftest head-with-reverse-yields-container-comma
  (is-parse
    [{:kind :lparen}
     {:kind :sym :text "f"}
     {:kind :comma}
     {:kind :sym :text "g"}
     {:kind :rparen}
     {:kind :reverse-yields}
     {:kind :lsquare}
     {:kind :sym :text "F"}
     {:kind :comma}
     {:kind :sym :text "G"}
     {:kind :rsquare} {:kind :.}
     {:kind :line}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name {:container :parens
                                                    :inner ["f" "g"]}
                                             :alias {:container :square
                                                     :inner ["F" "G"]}}]
                                     :body []}]}]))
(deftest multiple-chapters
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}
     {:kind :sym :text "g"} {:kind :.}
     {:kind :where}
     {:kind :sym :text "h"} {:kind :.}
     {:kind :line}]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings []}]
                                     :body ["g"]}
                                    {:kind :chapter
                                     :head [{:kind :declaration
                                             :name "h"
                                             :bindings []}]
                                     :body []}]}]))

(deftest qualification
  (is-parse
    [{:kind :sym :text "x"} {:kind :.}
     {:kind :line}
     {:kind :some :text "some"}
     {:kind :sym :text "x"} {:kind :: :text ":"} {:kind :sym :text "Nat"}
     {:kind :comma :text ","}
     {:kind :sym :text "x"} {:kind :boolean-operator :text ">"} {:kind :num :text "1"}
     {:kind :yields :text "=>"}
     {:kind :sym :text "x"} {:kind :boolean-operator :text "<"} {:kind :num :text "10"}
     {:kind :.}]
    [:ok {:directives [] :chapters [{:body [{:bindings
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
                                             :kind :quantification
                                             :quantifier :some}]
                                     :head [{:bindings ()
                                             :kind :declaration
                                             :name "x"}]
                                     :kind :chapter}]}]))

(deftest quantification-with-container
  (is-parse [{:kind :sym :text "A"} {:kind :.}
             {:kind :line}
             {:kind :some :text "some"}
             {:kind :lparen :text "("}
             {:kind :sym :text "a"} {:kind :comma :text ","}
             {:kind :sym :text "b"}
             {:kind :rparen :text ")"}
             {:kind :: :text ":"}
             {:kind :sym :text "A"}
             {:kind :yields :text "=>"}
             {:kind :sym :text "a"} {:kind :arithmetic-operator2 :text "+"} {:kind :sym :text "b"}
             {:kind :.}]
            [:ok {:directives [] :chapters [{:body [{:bindings
                                                     [{:expr "A"
                                                       :kind :binding
                                                       :name {:container :parens
                                                              :inner ["a" "b"]}}]
                                                     :expr {:kind :binary-operation
                                                            :left "a"
                                                            :operator "+"
                                                            :right "b"}
                                                     :kind :quantification
                                                     :quantifier :some}]
                                             :head [{:bindings () :kind :declaration :name "A"}]
                                             :kind :chapter}]}]))

(deftest precedence
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}
     {:kind :sym :text "x"} {:kind :arithmetic-operator2 :text "-"} {:kind :num :text "1"}
     {:kind :boolean-operator :text "="}
     {:kind :sym :text "y"} {:kind :arithmetic-operator2 :text "+"} {:kind :num :text "2"}
     {:kind :.}]
    [:ok {:directives [] :chapters [{:body [{:kind :binary-operation
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

(deftest multiple-application
  (is-parse
    [{:kind :sym :text "f"} {:kind :.}
     {:kind :line}
     {:kind :sym :text "x"} {:kind :sym :text "y"} {:kind :num :text "1"}
     {:kind :.}]
    [:ok {:chapters [{:body [{:f "x"
                              :kind :application
                              :x {:f "y" :kind :application :x 1}}]
                      :head [{:bindings ()
                              :kind :declaration
                              :name "f"}]
                      :kind :chapter}]
          :directives []}]))

(deftest unary-application
  (is-parse
    [{:kind :sym :text "f"}
     {:kind :. :text "."}
     {:kind :line :text "---"}
     {:kind :unary-operator :text "#"}
     {:kind :sym :text "x"}
     {:kind :boolean-operator :text "="}
     {:kind :unary-operator :text "#"}
     {:kind :sym :text "s"}
     {:kind :. :text "."}]
    [:ok {:chapters [{:body [{:kind :binary-operation
                              :left {:kind :unary-operation
                                     :operator "#"
                                     :left "x"}
                              :operator "="
                              :right {:kind :unary-operation
                                      :left "s"
                                      :operator "#"}}]
                      :head [{:bindings ()
                              :kind :declaration
                              :name "f"}]
                      :kind :chapter}]
          :directives ()}]))

(deftest directives
  (is-parse
    [{:kind :directive :text "module"} {:kind :sym :text "FIB"} {:kind :.}
     {:kind :sym :text "f"} {:kind :.}
     {:kind :line}]
    [:ok {:chapters [{:body [] :head [{:bindings () :kind :declaration :name "f"}]
                      :kind :chapter}]
          :directives [{:args "FIB"
                        :kind :directive
                        :statement "module"}]}]))

(deftest fib
  (is-parse
    [{:kind :sym :text "fib"}
     {:kind :sym :text "x"} {:kind :: :text ":"} {:kind :sym :text "Nat"}
     {:kind :yields :text "=>"}
     {:kind :sym :text "Nat"} {:kind :. :text "."}
     {:kind :line :text "---"}
     {:kind :sym :text "fib"} {:kind :sym :text "x"} {:kind :boolean-operator :text "="}
     {:kind :case :text "case"} {:kind :... :text "..."}
     {:kind :sym :text "x"} {:kind :boolean-operator :text ">"}
     {:kind :num :text "2"} {:kind :yields :text "=>"}
     {:kind :sym :text "fib"}
     {:kind :lparen :text "("}
     {:kind :sym :text "x"} {:kind :arithmetic-operator2 :text "-"} {:kind :num :text "1"}
     {:kind :rparen :text ")"}
     {:kind :arithmetic-operator2 :text "+"}
     {:kind :sym :text "fib"} {:kind :lparen :text "("}
     {:kind :sym :text "x"} {:kind :arithmetic-operator2 :text "-"} {:kind :num :text "2"}
     {:kind :rparen :text ")"} {:kind :comma :text ","}
     {:kind :sym :text "x"} {:kind :boolean-operator :text "="} {:kind :num :text "1"}
     {:kind :yields :text "=>"} {:kind :num :text "1"} {:kind :comma :text ","}
     {:kind :sym :text "x"} {:kind :boolean-operator :text "="} {:kind :num :text "2"}
     {:kind :yields :text "=>"} {:kind :num :text "1"} {:kind :. :text "."}]
    [:ok {:directives [] :chapters [{:body
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
                                     :kind :chapter}]}]))

(run-tests!)
