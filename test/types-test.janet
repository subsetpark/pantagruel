(use testament)

(import /pantagruel/stdlib)
(import /pantagruel/types)
(import /pantagruel/eval/type-checking)

(import /test/util :prefix "")

(defn is-type
  [t form env]
  (let [env (table/setproto (table ;(kvs env)) stdlib/base-env)
        [success resolved] (protect (types/resolve-type form env))]
    (if success
      (is (== t resolved) (string/format "Type of %q" form))
      (is false (string/format "Type of: %q\ncaught: %q" form resolved)))))

(deftest singleton-test
  (is-type
    stdlib/Nat
    {:kind :sym :text "n"}
    {"n" {:kind :procedure
          :type {:args {:tuple-of ()}
                 :yields {:thunk {:kind :sym :text "Nat"}}}}}))

(deftest domain-mention-test
  (is-type
    stdlib/Domain
    {:kind :sym :text "Nat"}
    {}))

(deftest domain-alias-test
  (let [env @{"P" {:kind :domain
                   :type {:list-of {:thunk {:kind :sym :text "String"}}}}
              "p" {:kind :procedure
                   :type {:args {:tuple-of @[]} :yields {:thunk {:kind :sym :text "P"}}}}}]
    (is-type
      {:list-of stdlib/String}
      {:kind :sym :text "p"}
      env)

    (is-type
      stdlib/Domain
      {:kind :sym :text "P"}
      env)))

(deftest application-test
  (let [env @{"f" {:kind :procedure
                   :type {:args {:tuple-of @[{:thunk {:kind :sym
                                                      :text "Nat"}}]}
                          :yields {:thunk {:kind :sym
                                           :text "Real"}}}}
              "x" {:kind :bound
                   :type {:thunk {:kind :sym
                                  :text "Nat"}}}}]
    (is-type
      stdlib/Nat
      {:kind :sym :text "x"}
      env)

    (is-type
      {:args {:tuple-of [stdlib/Nat]} :yields stdlib/Real}
      {:kind :sym :text "f"}
      env)

    (is-type
      stdlib/Real
      {:kind :application
       :f {:kind :sym :text "f"}
       :x {:kind :num :text 1}}
      env)))

(deftest binding-regression
  (let [env {"Alias" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "String"}}}}
             "Body" {:kind :domain :type {:list-of {:inner @[{:thunk {:kind :sym :text "Comment"}}
                                                             {:thunk {:kind :sym :text "Expression"}}]
                                                    :kind :sum}}}
             "Comment" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "String"}}}}
             "Declaration" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "String"}}}}
             "Expression" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "String"}}}}
             "Head" {:kind :domain
                     :type {:list-of {:inner @[{:inner @[{:thunk {:kind :sym :text "Comment"}}
                                                         {:thunk {:kind :sym :text "Declaration"}}]
                                                :kind :sum}
                                               {:thunk {:kind :sym :text "Alias"}}] :kind :sum}}}
             "Program" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "Section"}}}}
             "Scope" {:kind :domain :type {:container :set :inner {:thunk {:kind :sym :text "String"}}}}
             "Section" {:kind :domain :type @{:kind :meta-domain :name "Domain"}}
             "b" {:kind :member
                  :type {:thunk {:f {:kind :sym :text "body"}
                                 :kind :application
                                 :x {:container :parens
                                     :inner [{:f {:kind :sym :text "p"}
                                              :kind :application
                                              :x {:container :parens
                                                  :inner [{:kind :binary-operation
                                                           :left {:f {:kind :sym :text "p"}
                                                                  :kind :application
                                                                  :x {:kind :sym :text "sect"}}
                                                           :operator "-"
                                                           :right {:kind :num :text 1}}]}}]}}}}
             "body" {:kind :bound :type {:thunk {:kind :sym :text "Body"}}}
             "env" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Program"}}]}
                                            :yields {:list-of {:thunk {:kind :sym :text "Scope"}}}}}
             "eval" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Program"}}]}
                                             :yields {:thunk {:kind :sym :text "Bool"}}}}
             "h" {:kind :member :type {:thunk {:f {:kind :sym :text "head"}
                                               :kind :application
                                               :x {:kind :sym :text "sect"}}}}
             "head" {:kind :bound :type {:thunk {:kind :sym :text "Head"}}}
             "init_scope" {:kind :procedure :type {:args {:tuple-of @[]}
                                                   :yields {:thunk {:kind :sym :text "Scope"}}}}
             "is_all_bound?" {:kind :procedure
                              :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Section"}}]}
                                     :yields {:thunk {:kind :sym :text "Bool"}}}}
             "is_bound?" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "String"}}]}
                                                  :yields {:thunk {:kind :sym :text "Bool"}}}}
             "p" {:kind :bound :type {:thunk {:kind :sym :text "Program"}}}
             "sect" {:kind :bound :type {:thunk {:kind :sym :text "Section"}}}
             "section" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Head"}}
                                                                   {:thunk {:kind :sym :text "Body"}}]}
                                                :yields {:thunk {:kind :sym :text "Section"}}}}
             "sym" {:kind :bound :type {:thunk {:kind :sym :text "String"}}}}]

    # A compound alias to built-in types.
    (is-type
      stdlib/Domain
      {:kind :sym :text "Body"}
      env)
    # A compound alias to a user-defined concrete type.
    (is-type
      stdlib/Domain
      {:kind :sym :text "Program"}
      env)
    # A user-defined concrete type.
    (is-type
      stdlib/Domain
      {:kind :sym :text "Section"}
      env)

    (is-type
      stdlib/Bool
      {:f {:kind :sym
           :text "eval"}
       :kind :application
       :x {:kind :sym
           :text "p"}}
      env)

    (is-type
      stdlib/Bool
      {:bindings {:kind :seq
                  :seq [{:binding-type :from
                         :expr {:kind :sym
                                :text "p"}
                         :kind :binding
                         :name {:kind :sym
                                :text "sect"}}]}
       :expr {:f {:kind :sym
                  :text "is_all_bound?"} :kind :application
              :x {:kind :sym
                  :text "sect"}}
       :kind :quantification
       :quantifier {:kind :all
                    :text "all"}}

      env)))

(run-tests!)