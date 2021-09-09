(use testament)

(import /pantagruel/lexer)

(defn- is-lex
  [src tokens]
  (is (== tokens
          (map |{:kind ($ :kind) :text ($ :text)} (lexer/lex src)))))

(deftest basic-declaration
  (is-lex
    "f."
    [{:kind :sym :text "f"} {:kind :. :text "."}]))

(deftest declaration-yields
  (is-lex
    "f => g."
    [{:kind :sym :text "f"} {:kind :yields :text "=>"} {:kind :sym :text "g"} {:kind :. :text "."}]))

(deftest declaration-binding-yields
  (is-lex
    "f x:X => g."
    [{:kind :sym :text "f"}
     {:kind :sym :text "x"}
     {:kind :: :text ":"}
     {:kind :sym :text "X"}
     {:kind :yields :text "=>"} {:kind :sym :text "g"} {:kind :. :text "."}]))

(deftest declaration-bindings-yields
  (is-lex
    "f x:X, y:Y => g."
    [{:kind :sym :text "f"}
     {:kind :sym :text "x"}
     {:kind :: :text ":"}
     {:kind :sym :text "X"}
     {:kind :comma :text ","}
     {:kind :sym :text "y"}
     {:kind :: :text ":"}
     {:kind :sym :text "Y"}
     {:kind :yields :text "=>"} {:kind :sym :text "g"} {:kind :. :text "."}]))

(deftest basic-head
  (is-lex
    `
    f.
    ---
    `
    [{:kind :sym :text "f"} {:kind :. :text "."} {:kind :line :text "---"}]))

(deftest basic-chapter
  (is-lex
    `
    f.
    ---
    g.
    `
    [{:kind :sym :text "f"} {:kind :. :text "."}
     {:kind :line :text "---"}
     {:kind :sym :text "g"} {:kind :. :text "."}]))

(deftest multiple-chapters
  (is-lex
    `
    f.
    ---
    g.

    ;

    h.
    `
    [{:kind :sym :text "f"} {:kind :. :text "."}
     {:kind :line :text "---"}
     {:kind :sym :text "g"} {:kind :. :text "."}
     {:kind :where :text ";"}
     {:kind :sym :text "h"} {:kind :. :text "."}]))

(deftest quantifier
  (is-lex "some (a,b):A => a + b"
          [{:kind :some :text "some"}
           {:kind :lparen :text "("}
           {:kind :sym :text "a"} {:kind :comma :text ","}
           {:kind :sym :text "b"}
           {:kind :rparen :text ")"}
           {:kind :: :text ":"}
           {:kind :sym :text "A"}
           {:kind :yields :text "=>"}
           {:kind :sym :text "a"} {:kind :arithmetic-operator2 :text "+"} {:kind :sym :text "b"}]))

(deftest mapping-form
  (is-lex
    `
    fib x = case x ...
      a => b,
      y => z.
    `
    [{:kind :sym :text "fib"} {:kind :sym :text "x"} {:kind :boolean-operator :text "="}
     {:kind :case :text "case"} {:kind :sym :text "x"} {:kind :... :text "..."}
     {:kind :sym :text "a"} {:kind :yields :text "=>"}
     {:kind :sym :text "b"} {:kind :comma :text ","}
     {:kind :sym :text "y"} {:kind :yields :text "=>"}
     {:kind :sym :text "z"} {:kind :. :text "."}]))

(deftest fibonacci
  (is-lex
    `
    fib x : Nat => Nat.
    ---
    fib x = case ...
      x > 2 => fib (x - 1) + fib (x - 2),
      x = 1 => 1,
      x = 2 => 1.
    `
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
     {:kind :yields :text "=>"} {:kind :num :text "1"} {:kind :. :text "."}]))

(deftest comment-test
  (is-lex
    `
    f.
    // single-line comment
    g.
    `
    [{:kind :sym :text "f"} {:kind :. :text "."}
     {:kind :sym :text "g"} {:kind :. :text "."}])

  (is-lex
    `
    f. // partial comment
    g.
    `
    [{:kind :sym :text "f"} {:kind :. :text "."}
     {:kind :sym :text "g"} {:kind :. :text "."}]))

(deftest symbols
  (is-lex "f" [{:kind :sym :text "f"}])
  (is-lex "f2" [{:kind :sym :text "f2"}])
  (is-lex "f'" [{:kind :sym :text "f'"}])
  (is-lex "f_g" [{:kind :sym :text "f_g"}]))

(deftest strings
  (is-lex `"foo" bar` [{:kind :string :text `"foo"`}
                       {:kind :sym :text "bar"}]))

(deftest qualification
  (is-lex "some x:Nat, x > 1 => x < 10"
          [{:kind :some :text "some"}
           {:kind :sym :text "x"} {:kind :: :text ":"} {:kind :sym :text "Nat"}
           {:kind :comma :text ","}
           {:kind :sym :text "x"} {:kind :boolean-operator :text ">"} {:kind :num :text "1"}
           {:kind :yields :text "=>"}
           {:kind :sym :text "x"} {:kind :boolean-operator :text "<"} {:kind :num :text "10"}]))

(run-tests!)
