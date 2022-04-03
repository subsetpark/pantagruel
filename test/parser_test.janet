(use testament)

(import yacc)
(import /pantagruel/parser)
(import /test/util :prefix "")

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

(deftest base-head
  (is-parse
    head-placeholder
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name {:kind :sym :text "f"}
                                             :bindings {:kind :seq
                                                        :seq []}}]
                                     :body []}]}]))

(deftest base-chapter
  (is-parse
    [;head-placeholder
     (sym "g") .]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name (sym "f")
                                             :bindings {:kind :seq
                                                        :seq []}}]
                                     :body [(sym "g")]}]}]))

(deftest head-with-binding
  (is-parse
    [(sym "f") (sym "x") bind (sym "X") .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name (sym "f")
                                             :bindings {:kind :seq
                                                        :seq [{:kind :binding
                                                               :binding-type ::
                                                               :expr (sym "X")
                                                               :name (sym "x")}]}}]
                                     :body []}]}]))

(deftest head-with-binding-expr-equals
  (is-parse
    [(sym "X") = (sym "Nat") .
     (sym "f") (sym "x") bind (sym "X") comma (sym "x") = (num 1) .
     ---]
    [:ok {:chapters [{:body ()
                      :head [{:alias {:kind :sym :text "Nat"}
                              :kind :decl-alias
                              :name {:kind :sym :text "X"}}
                             {:bindings {:kind :seq
                                         :seq [{:expr {:kind :sym :text "X"}
                                                :kind :binding
                                                :binding-type ::
                                                :name {:kind :sym :text "x"}}
                                               {:kind :binary-operation
                                                :left {:kind :sym :text "x"}
                                                :operator "="
                                                :right (num 1)}]}
                              :kind :declaration
                              :name {:kind :sym :text "f"}}]
                      :kind :chapter}]
          :directives ()}]))

(deftest head-with-yields
  (is-parse
    [(sym "f") => (sym "F") .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :declaration
                                             :name (sym "f")
                                             :bindings {:kind :seq :seq []}
                                             :yields (sym "F")}]
                                     :body []}]}]))

(deftest head-with-alias
  (is-parse
    [(sym "f") = (sym "F") .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name (sym "f")
                                             :alias (sym "F")}]
                                     :body []}]}]))

(deftest head-with-tuple-alias
  (is-parse
    [lp (sym "f") comma (sym "g") rp = (sym "F") .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name {:container :parens
                                                    :inner {:kind :seq
                                                            :seq [{:kind :sym :text "f"}
                                                                  {:kind :sym :text "g"}]}}
                                             :alias (sym "F")}]
                                     :body []}]}]))
(deftest head-with-alias-container
  (is-parse
    [(sym "f") = {:kind :lsquare} (sym "F") {:kind :rsquare} .
     ---]
    [:ok {:directives [] :chapters [{:kind :chapter
                                     :head [{:kind :decl-alias
                                             :name (sym "f")
                                             :alias {:container :list-of
                                                     :inner (sym "F")}}]
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
                                             :name (sym "f")
                                             :bindings {:kind :seq :seq []}}]
                                     :body [(sym "g")]}
                                    {:kind :chapter
                                     :head [{:kind :declaration
                                             :name (sym "h")
                                             :bindings {:kind :seq :seq []}}]
                                     :body []}]}]))

(deftest qualification
  (with-dyns [:normalize-syms true]
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
                                               {:kind :seq
                                                :seq [{:expr (sym "Nat")
                                                       :kind :binding
                                                       :binding-type ::
                                                       :name (sym "x")}
                                                      {:kind :binary-operation
                                                       :left (sym "x")
                                                       :operator ">"
                                                       :right (num 1)}]}
                                               :expr {:kind :binary-operation
                                                      :left (sym "x")
                                                      :operator "<"
                                                      :right (num 10)}
                                               :kind :quantification
                                               :ref :ref
                                               :quantifier {:kind :some}}]
                                       :head [{:bindings {:kind :seq :seq []}
                                               :kind :declaration
                                               :name (sym "x")}]
                                       :kind :chapter}]}])))

(deftest quantification-with-container
  (with-dyns [:normalize-syms true]
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
               (sym "a") + (sym "b")
               .]
              [:ok {:directives []
                    :chapters [{:body [{:bindings
                                        {:kind :seq
                                         :seq [{:expr (sym "A")
                                                :kind :binding
                                                :binding-type ::
                                                :name {:container :parens
                                                       :inner {:kind :seq
                                                               :seq [(sym "a") (sym "b")]}}}]}
                                        :expr {:kind :binary-operation
                                               :left (sym "a")
                                               :operator "+"
                                               :right (sym "b")}
                                        :kind :quantification
                                        :ref :ref
                                        :quantifier {:kind :some :text "some"}}]
                                :head [{:bindings {:kind :seq :seq []}
                                        :kind :declaration
                                        :name (sym "A")}]
                                :kind :chapter}]}])))

(deftest precedence
  (is-parse
    [;head-placeholder
     (sym "x") {:kind :arithmetic-operator2 :text "-"} (num 1)
     =
     (sym "y") + (num 2) .]
    [:ok {:directives [] :chapters [{:body [{:kind :binary-operation
                                             :left {:kind :binary-operation :left (sym "x")
                                                    :operator "-"
                                                    :right (num 1)}
                                             :operator "="
                                             :right {:kind :binary-operation
                                                     :left (sym "y")
                                                     :operator "+"
                                                     :right (num 2)}}]
                                     :head [{:bindings {:kind :seq :seq []}
                                             :kind :declaration :name (sym "f")}]
                                     :kind :chapter}]}]))

(deftest multiple-application
  (is-parse
    [;head-placeholder
     (sym "x") (sym "y") (num 1) .]
    [:ok {:chapters [{:body [{:f (sym "x")
                              :kind :application
                              :x {:f (sym "y")
                                  :kind :application
                                  :x (num 1)}}]
                      :head [{:bindings {:kind :seq :seq []}
                              :kind :declaration
                              :name (sym "f")}]
                      :kind :chapter}]
          :directives []}]))

(deftest unary-application
  (is-parse
    [;head-placeholder
     card (sym "x")
     =
     card (sym "s") .]
    [:ok {:chapters [{:body [{:kind :binary-operation
                              :left {:kind :unary-operation
                                     :operator "#"
                                     :left (sym "x")}
                              :operator "="
                              :right {:kind :unary-operation
                                      :left (sym "s")
                                      :operator "#"}}]
                      :head [{:bindings {:kind :seq :seq []}
                              :kind :declaration
                              :name (sym "f")}]
                      :kind :chapter}]
          :directives ()}])

  (is-parse
    [;head-placeholder
     card (sym "x") {:kind :boolean-operator :text "-"} (num 1) .]
    [:ok {:chapters [{:body [{:kind :binary-operation
                              :left {:kind :unary-operation
                                     :operator "#"
                                     :left (sym "x")}
                              :operator "-"
                              :right (num 1)}]
                      :head [{:bindings {:kind :seq :seq []}
                              :kind :declaration
                              :name (sym "f")}]
                      :kind :chapter}]
          :directives ()}])

  (is-parse
    [;head-placeholder
     card lp (sym "concat") (sym "s") (sym "r") rp
     =
     card (sym "s") + card (sym "r") .]
    [:ok {:chapters [{:body [{:kind :binary-operation
                              :operator "="
                              :left {:kind :unary-operation
                                     :operator "#"
                                     :left {:container :parens
                                            :inner [{:f (sym "concat")
                                                     :kind :application
                                                     :x {:f (sym "s")
                                                         :kind :application
                                                         :x (sym "r")}}]}}
                              :right {:kind :binary-operation
                                      :operator "+"
                                      :left {:kind :unary-operation
                                             :left (sym "s")
                                             :operator "#"}
                                      :right {:kind :unary-operation
                                              :left (sym "r")
                                              :operator "#"}}}]
                      :head [{:bindings {:kind :seq :seq []}
                              :kind :declaration
                              :name (sym "f")}]
                      :kind :chapter}]
          :directives ()}]))

(deftest directives
  (is-parse
    [{:kind :directive :text "module"}
     (sym "FIB") .
     ;head-placeholder]
    [:ok {:chapters [{:body [] :head [{:bindings {:kind :seq :seq []} :kind :declaration :name (sym "f")}]
                      :kind :chapter}]
          :directives [{:args (sym "FIB")
                        :kind :directive
                        :statement "module"}]}]))

(deftest fib
  (is-parse
    [(sym "fib") (sym "x") bind (sym "Nat") => (sym "Nat") .
     ---
     (sym "fib") (sym "x") =
     {:kind :case :text "case"} {:kind :... :text "..."}
     (sym "x") {:kind :boolean-operator :text ">"} (num 2) =>
     (sym "fib")
     lp (sym "x") {:kind :arithmetic-operator2 :text "-"} (num 1) rp
     + (sym "fib")
     lp (sym "x") {:kind :arithmetic-operator2 :text "-"} (num 2) rp
     comma
     (sym "x") = (num 1)
     => (num 1) comma
     (sym "x") = (num 2)
     => (num 1) .]
    [:ok {:directives []
          :chapters [{:body
                      [{:kind :binary-operation
                        :operator "="
                        :left {:f (sym "fib") :kind :application :x (sym "x")}
                        :right {:kind :case
                                :mapping {:kind :seq
                                          :seq [{:kind :map
                                                 :left {:kind :binary-operation
                                                        :operator ">"
                                                        :left (sym "x")
                                                        :right (num 2)}
                                                 :right {:kind :binary-operation
                                                         :operator "+"
                                                         :left {:kind :application
                                                                :f (sym "fib")
                                                                :x {:container :parens
                                                                    :inner [{:kind :binary-operation
                                                                             :operator "-"
                                                                             :left (sym "x")
                                                                             :right (num 1)}]}}
                                                         :right {:kind :application
                                                                 :f (sym "fib")
                                                                 :x {:container :parens
                                                                     :inner [{:kind :binary-operation
                                                                              :operator "-"
                                                                              :left (sym "x")
                                                                              :right (num 2)}]}}}}
                                                {:kind :map
                                                 :left {:kind :binary-operation
                                                        :operator "="
                                                        :left (sym "x")
                                                        :right (num 1)}
                                                 :right (num 1)}
                                                {:kind :map
                                                 :left {:kind :binary-operation
                                                        :operator "="
                                                        :left (sym "x")
                                                        :right (num 2)}
                                                 :right (num 1)}]}}}]
                      :head [{:bindings
                              {:kind :seq
                               :seq [{:expr (sym "Nat")
                                      :kind :binding
                                      :binding-type ::
                                      :name (sym "x")}]}
                              :kind :declaration
                              :name (sym "fib")
                              :yields (sym "Nat")}]
                      :kind :chapter}]}]))

(run-tests!)
