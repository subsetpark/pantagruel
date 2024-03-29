(use testament)

(import /pantagruel/eval/engine)
(import /pantagruel/stdlib)
(import /pantagruel/lexer)
(import /pantagruel/parser)

(defn do-parse
  [src]
  (-> src
      (lexer/lex)
      (parser/parse-tokens)))

(defn is-eval
  [expected-env src]
  (let [evaluator (engine/Evaluator "unit test" src)
        tree (do-parse src)
        [success? res] (protect (:eval evaluator tree))]
    (is success? (string/format "eval failure:\n\n%s\n\nFails with:\n%s"
                                (string src)
                                (if (table? res) (string/format "%q" res) (string res))))
    (if success?
      (is (== expected-env (table/to-struct res))))))

(deftest eval-single-declaration
  (is-eval
    {"f" {:kind :domain
          :type {:decl-name "f"
                 :args {:tuple-of ()}
                 :yields stdlib/Void}}}
    `f.
     ---
     `))

(deftest eval-declaration-with-binding
  (is-eval
    {"X" {
          :kind :domain
          :type stdlib/Domain}
     "f" {
          :kind :procedure
          :type {:decl-name "f"
                 :args {:tuple-of @[{:thunk {:kind :sym :span [7 8] :text "X"}}]}
                 :yields stdlib/Void}}
     "x" {:kind :bound :type {:thunk {:kind :sym :span [7 8] :text "X"}}}
     "x'" {:kind :bound :type {:thunk {:kind :sym :span [7 8] :text "X"}}}}
    `
    X.
    f x:X.
     ---
     `))

(deftest eval-declaration-with-yields
  (is-eval
    {"F" {:kind :domain
          :type stdlib/Domain}
     "f" {:kind :procedure
          :type {:decl-name "f"
                 :args {:tuple-of @[]}
                 :yields {:thunk {:kind :sym :span [8 9] :text "F"}}}}}
    `
    F.
    f => F.
     ---
     `))

(deftest eval-alias-declaration
  (is-eval
    {"F" {:kind :domain
          :type stdlib/Domain}
     "f" {:kind :domain
          :type {:thunk {:kind :sym :span [7 8] :text "F"}}}}
    `
    F.
    f = F.
     ---
     `))

(deftest eval-alias-declaration-container
  (is-eval
    {"F" {:kind :domain
          :type stdlib/Domain}
     "f" {:kind :domain
          :type {:list-of {:thunk {:kind :sym :span [8 9] :text "F"}}}}}
    `
    F.
    f = [F].
     ---
     `))

(deftest eval-body
  (is-eval
    {"g" {:kind :domain
          :type {:decl-name "g"
                 :args {:tuple-of ()}
                 :yields stdlib/Void}}}
    `g.
     ---
     g.
     `))

(deftest eval-qualification
  (is-eval
    {"f" {:kind :domain
          :type {:decl-name "f"
                 :args {:tuple-of ()}
                 :yields stdlib/Void}}}

    `f.
     ---
     some x:Nat, x > 1 ... x < 10.
     `))

(deftest eval-quantification-with-container
  (is-eval
    {"A" {:kind :domain
          :type stdlib/Domain}}
    `A.
     ---
     some (a, b):A ... a + b.
     `))

(deftest eval-chapter
  (is-eval
    {"X" {:kind :domain
          :type stdlib/Domain}
     "f" {:kind :procedure
          :type {:decl-name "f"
                 :args {:tuple-of @[{:thunk {:kind :sym :span [10 11] :text "X"}}]}
                 :yields stdlib/Void}}
     "x" {:kind :bound
          :type {:thunk {:kind :sym :span [10 11] :text "X"}}}
     "x'" {:kind :bound
           :type {:thunk {:kind :sym :span [10 11] :text "X"}}}
     "y" {:kind :domain
          :type {:decl-name "y"
                 :args {:tuple-of ()}
                 :yields stdlib/Void}}}
    `
    X.
    y.
    f x:X.
     ---
     y.
     `))

(deftest eval-fib
  (is-eval
    {"fib" {:kind :procedure
            :type {:decl-name "fib"
                   :args {:tuple-of @[{:thunk {:kind :sym :span [8 11] :text "Nat"}}]}
                   :yields {:thunk {:kind :sym :span [15 18] :text "Nat"}}}}
     "x" {:kind :bound
          :type {:thunk {:kind :sym :span [8 11] :text "Nat"}}}}

    (slurp "priv/fib.pant")))

(run-tests!)
