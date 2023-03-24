(use testament)

(import /pantagruel/stdlib)
(import /pantagruel/types/types)
(import /pantagruel/types/type-checking)

(import /test/util :prefix "")

(defn is-type
  [t form env]
  (let [env (table/setproto env stdlib/base-env)
        [success resolved] (protect (types/resolve-type form env))]
    (if success
      (is (== t resolved) (string/format "Type of %q" form))
      (is false (string/format "Type of: %q\ncaught: %q" form resolved)))))

(deftest singleton-test
  (is-type
    stdlib/Nat
    {:kind :sym :text "n"}
    @{"n" {:kind :procedure
           :type {:args {:tuple-of ()}
                  :yields {:thunk {:kind :sym :text "Nat"}}}}}))

(deftest domain-mention-test
  (is-type
    stdlib/Domain
    {:kind :sym :text "Nat"}
    @{}))

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

(deftest nested-sums
  (let [env @{"Alias" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "String"}}}}
              "Body" {:kind :domain
                      :type {:list-of {:inner @[{:thunk {:kind :sym
                                                         :text "Comment"}}
                                                {:thunk {:kind :sym
                                                         :text "Expression"}}]
                                       :kind :sum}}}
              "Comment" {:kind :domain :type {:list-of {:thunk {:kind :sym
                                                                :text "String"}}}}
              "Expression" {:kind :domain :type {:list-of {:thunk {:kind :sym
                                                                   :text "String"}}}}
              "b" {:kind :procedure :type {:args {:tuple-of @[]}
                                           :yields {:thunk {:kind :sym
                                                            :text "Body"}}}}}]
    (is-type
      {:list-of {:list-of @{:kind :concrete :name "String"
                            :type @{:kind :meta-domain :name "Domain"}}}}

      {:kind :sym :text "b"}
      env)))

(deftest binding-regression
  (let [env @{"Alias" {:kind :domain
                       :type {:list-of {:thunk {:kind :sym
                                                :text "String"}}}}
              "Body" {:kind :domain
                      :type {:list-of {:inner @[{:thunk {:kind :sym
                                                         :text "Comment"}}
                                                {:thunk {:kind :sym
                                                         :text "Expression"}}]
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
              "Scope" {:kind :domain :type {:set-of {:thunk {:kind :sym :text "String"}}}}
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

              "section" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Head"}}
                                                                    {:thunk {:kind :sym :text "Body"}}]}
                                                 :yields {:thunk {:kind :sym :text "Section"}}}}
              "sym" {:kind :bound :type {:thunk {:kind :sym :text "String"}}}}
        closure (table/setproto @{"sect" {:kind :bound :type {:thunk {:kind :sym :text "Section"}}}} env)]
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
      env)))

(deftest zk-regression-test
  (let [env @{"Index" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "Line"}}}}
              "Line" {:kind :domain :type {:thunk {:kind :sym :text "String"}}}
              "Note" {:kind :domain :type {:list-of {:thunk {:kind :sym :text "Line"}}}}
              "Reference" {:kind :domain :type {:thunk {:kind :sym :text "Line"}}}
              "backlinks" {:kind :procedure
                           :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Note"}}]}
                                  :yields {:set-of {:thunk {:kind :sym :text "Reference"}}}}}
              "body" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Note"}}]}
                                              :yields {:list-of {:thunk {:kind :sym :text "Line"}}}}}
              "bracketed" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "String"}}]}
                                                   :yields {:thunk {:kind :sym :text "String"}}}}
              "created_at" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Note"}}]}
                                                    :yields {:thunk {:kind :sym :text "Date"}}}}
              "escape" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "String"}}]}
                                                :yields {:thunk {:kind :sym :text "String"}}}}
              "i" {:kind :bound :type {:thunk {:kind :sym :text "Index"}}}
              "index" {:kind :procedure
                       :type {:args {:tuple-of @[{:list-of {:thunk {:kind :sym :text "Note"}}}]}
                              :yields @{:kind :concrete :name "Void"
                                        :type @{:kind :meta-domain :name "Domain"}}}}
              "line" {:kind :member :type {:thunk {:kind :sym :text "n"}}}
              "n" {:kind :bound :type {:thunk {:kind :sym :text "Note"}}}
              "name" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Note"}}]}
                                              :yields {:thunk {:kind :sym :text "String"}}}}
              "notes" {:kind :bound :type {:list-of {:thunk {:kind :sym :text "Note"}}}}
              "r" {:kind :member :type {:thunk {:f {:kind :sym :text "references"}
                                                :kind :application
                                                :x {:kind :sym :text "n"}}}}
              "ref" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "String"}}]}
                                             :yields {:thunk {:kind :sym :text "Reference"}}}}
              "ref_note" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Note"}}]}
                                                  :yields {:thunk {:kind :sym :text "Reference"}}}}
              "references" {:kind :procedure :type {:args {:tuple-of @[{:thunk {:kind :sym :text "Note"}}]}
                                                    :yields {:list-of {:thunk {:kind :sym :text "Reference"}}}}}
              "s" {:kind :bound :type {:thunk {:kind :sym :text "String"}}}}
        closure (table/setproto @{"m" {:kind :bound
                                       :type {:thunk {:kind :sym :text "Note"}}}} env)]
    (is-type
      {:set-of stdlib/String}
      {:container :set-comprehension
       :inner {:bindings {:kind :seq
                          :seq [{:binding-type ::
                                 :expr {:kind :sym :text "Note"}
                                 :kind :binding
                                 :name {:kind :sym :text "m"}}
                                {:kind :binary-operation
                                 :left {:container :parens
                                        :inner [{:f {:kind :sym :text "bracketed"}
                                                 :kind :application
                                                 :x {:container :parens
                                                     :inner [{:f {:kind :sym :text "name"}
                                                              :kind :application
                                                              :x {:kind :sym :text "n"}}]}}]}
                                 :operator "in"
                                 :right {:kind :sym :text "m"}}]}
               :expr {:f {:kind :sym :text "ref_note"}
                      :kind :application
                      :x {:kind :sym :text "m"}}
               :kind :quantification
               :scope @[closure]
               :quantifier {:kind :all :text "all"}}}

      env)))

(run-tests!)
