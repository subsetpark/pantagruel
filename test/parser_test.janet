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

(def . {:kind :.})
(def bind {:kind ::})
(def --- {:kind :line})
(def => {:kind :yields})
(def <= {:kind :reverse-yields})
(def lp {:kind :lparen})
(def rp {:kind :rparen})
(def card {:kind :unary-operator :text "#"})
(def head-placeholder [{:kind :sym :text "f"} . ---])
(defn sym [text] {:kind :sym :text text})
(defn num [text] {:kind :num :text (string text)})

(deftest empty-program
  (is-parse
    []
    [:ok {:directives [] :chapters []}]))

(deftest base-head
  (is-parse
    head-placeholder
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings []}]
                                     :body []}]}]))

(deftest base-chapter
  (is-parse
    [;head-placeholder
     (sym "g") .]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings []}]
                                     :body ["g"]}]}]))

(deftest head-with-binding
  (is-parse
    [(sym "f") (sym "x") bind (sym "X") .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings [{:kind :binding
                                                         :expr "X"
                                                         :name "x"}]}]
                                     :body []}]}]))

(deftest head-with-yields
  (is-parse
    [(sym "f") => (sym "F") .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name "f"
                                             :bindings []
                                             :yields "F"}]
                                     :body []}]}]))

(deftest head-with-reverse-yields
  (is-parse
    [(sym "f") <= (sym "F") .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name "f"
                                             :alias "F"}]
                                     :body []}]}]))

(deftest head-with-reverse-yields-container
  (is-parse
    [(sym "f") <= {:kind :lsquare} (sym "F") {:kind :rsquare} .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name "f"
                                             :alias {:container :square
                                                     :inner ["F"]}}]
                                     :body []}]}]))

(deftest head-with-reverse-yields-container-comma
  (is-parse
    [lp
     (sym "f")
     {:kind :comma}
     (sym "g")
     rp
     <=
     {:kind :lsquare}
     (sym "F")
     {:kind :comma}
     (sym "G")
     {:kind :rsquare} .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name {:container :parens
                                                    :inner ["f" "g"]}
                                             :alias {:container :square
                                                     :inner ["F" "G"]}}]
                                     :body []}]}]))
(deftest multiple-chapters
  (is-parse
    [;head-placeholder
     (sym "g")
     .
     {:kind :where}
     (sym "h")
     .
     ---]
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
    [(sym "x")
     .
     ---
     {:kind :some}
     (sym "x") bind (sym "Nat")
     {:kind :comma}
     (sym "x") {:kind :boolean-operator :text ">"} (num 1)
     =>
     (sym "x") {:kind :boolean-operator :text "<"} (num 10)
     .]
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
  (is-parse [(sym "A") .
             ---
             {:kind :some :text "some"}
             lp
             {:kind :sym :text "a"} {:kind :comma}
             (sym "b")
             rp
             bind
             (sym "A")
             =>
             (sym "a") {:kind :arithmetic-operator2 :text "+"} (sym "b")
             .]
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
    [;head-placeholder
     (sym "x") {:kind :arithmetic-operator2 :text "-"} (num 1)
     {:kind :boolean-operator :text "="}
     (sym "y") {:kind :arithmetic-operator2 :text "+"} (num 2) .]
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
    [;head-placeholder
     (sym "x") (sym "y") (num 1) .]
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
    [;head-placeholder
     card (sym "x")
     {:kind :boolean-operator :text "="}
     card (sym "s") .]
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
          :directives ()}])

  (is-parse
    [;head-placeholder
     card (sym "x") {:kind :boolean-operator :text "-"} (num 1) .]
    [:ok {:chapters [{:body [{:kind :binary-operation
                              :left {:kind :unary-operation
                                     :operator "#"
                                     :left "x"}
                              :operator "-"
                              :right 1}]
                      :head [{:bindings ()
                              :kind :declaration
                              :name "f"}]
                      :kind :chapter}]
          :directives ()}])

  (is-parse
    [;head-placeholder
     card
     (sym "x") {:kind :boolean-operator :text "-"} (num 1) .]
    [:ok {:chapters [{:body [{:kind :binary-operation
                              :left {:kind :unary-operation
                                     :operator "#"
                                     :left "x"}
                              :operator "-"
                              :right 1}]
                      :head [{:bindings ()
                              :kind :declaration
                              :name "f"}]
                      :kind :chapter}]
          :directives ()}]))

(deftest directives
  (is-parse
    [{:kind :directive :text "module"}
     (sym "FIB") .
     ;head-placeholder]
    [:ok {:chapters [{:body [] :head [{:bindings () :kind :declaration :name "f"}]
                      :kind :chapter}]
          :directives [{:args "FIB"
                        :kind :directive
                        :statement "module"}]}]))

(deftest fib
  (is-parse
    [(sym "fib") (sym "x") bind (sym "Nat") => (sym "Nat") .
     ---
     (sym "fib") (sym "x") {:kind :boolean-operator :text "="}
     {:kind :case :text "case"} {:kind :... :text "..."}
     (sym "x") {:kind :boolean-operator :text ">"} (num 2) =>
     (sym "fib")
     lp (sym "x") {:kind :arithmetic-operator2 :text "-"} (num 1) rp
     {:kind :arithmetic-operator2 :text "+"} (sym "fib")
     lp (sym "x") {:kind :arithmetic-operator2 :text "-"} (num 2) rp
     {:kind :comma :text ","}
     (sym "x") {:kind :boolean-operator :text "="} (num 1)
     => (num 1) {:kind :comma :text ","}
     (sym "x") {:kind :boolean-operator :text "="} (num 2)
     => (num 1) .]
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
