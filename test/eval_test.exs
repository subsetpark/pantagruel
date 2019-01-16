defmodule EvalTest do
  use ExUnit.Case
  alias Pantagruel.Values.{Variable, Lambda, Domain}
  alias Pantagruel.Eval

  defp scan_and_parse(text) when is_binary(text) do
    text
    |> Pantagruel.Scan.scan()
    |> scan_and_parse
  end

  defp scan_and_parse(text) do
    {:ok, parsed} =
      :pant_lexer.string(text)
      |> Pantagruel.Parse.handle_lex()

    parsed
  end

  defp eval(parsed) do
    {:ok, scope} = Eval.eval(parsed, [])
    scope
  end

  describe "program evaluation" do
    test "eval happy path" do
      parsed = "f x:Nat, x > 1  :: Real" |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: {:symbol, 'Real'},
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: '::'
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               }
             ] == eval(parsed)
    end

    test "eval unbound" do
      parsed = "f x:X  :: Real" |> scan_and_parse

      {:error, {:unbound_variables, unbounds, _}} = Eval.eval(parsed, [])

      assert MapSet.new([{:symbol, 'X'}]) == unbounds
    end

    test "eval late binding" do
      parsed =
        """
        f x:X, y:Y :: Real
        make_y => Y
        make_x => X
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'X'} => %Domain{
                   name: {:symbol, 'X'},
                   ref: {:symbol, 'X'}
                 },
                 {:symbol, 'Y'} => %Domain{
                   name: {:symbol, 'Y'},
                   ref: {:symbol, 'Y'}
                 },
                 {:symbol, 'f'} => %Lambda{
                   codomain: {:symbol, 'Real'},
                   domain: [symbol: 'X', symbol: 'Y'],
                   name: {:symbol, 'f'},
                   type: '::'
                 },
                 {:symbol, 'make_x'} => %Lambda{
                   codomain: {:symbol, 'X'},
                   domain: [],
                   name: {:symbol, 'make_x'},
                   type: '=>'
                 },
                 {:symbol, 'make_y'} => %Lambda{
                   codomain: {:symbol, 'Y'},
                   domain: [],
                   name: {:symbol, 'make_y'},
                   type: '=>'
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'X'},
                   name: {:symbol, 'x'}
                 },
                 {:symbol, 'y'} => %Variable{
                   domain: {:symbol, 'Y'},
                   name: {:symbol, 'y'}
                 }
               }
             ] == eval(parsed)
    end

    test "eval whole section" do
      parsed =
        """
        f x:Nat
        ---
        f x > 0
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               }
             ] == eval(parsed)
    end

    test "eval two sections" do
      parsed =
        """
        f x:Nat
        ;
        g :: Nat
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               },
               %{
                 {:symbol, 'g'} => %Lambda{
                   codomain: {:symbol, 'Nat'},
                   domain: [],
                   name: {:symbol, 'g'},
                   type: '::'
                 }
               }
             ] == eval(parsed)
    end

    test "unbound variable in body" do
      parsed =
        """
        f x:Nat
        ---
        f x = g x
        """
        |> scan_and_parse

      assert {:error,
              {:unbound_variables,
               [
                 bin_appl: [
                   :=,
                   {:f_appl, [symbol: 'f', symbol: 'x']},
                   {:f_appl, [symbol: 'g', symbol: 'x']}
                 ]
               ]
               |> MapSet.new(),
               [
                 %{
                   {:symbol, 'f'} => %Pantagruel.Values.Lambda{
                     codomain: nil,
                     domain: [symbol: 'Nat'],
                     name: {:symbol, 'f'},
                     type: nil
                   },
                   {:symbol, 'x'} => %Pantagruel.Values.Variable{
                     domain: {:symbol, 'Nat'},
                     name: {:symbol, 'x'}
                   }
                 }
               ]}} == Eval.eval(parsed, [])
    end

    test "bind variables in the next section" do
      parsed =
        """
        f x:Nat
        ---
        f x <- g x
        ;
        g y:Nat ::Bool
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               },
               %{
                 {:symbol, 'g'} => %Lambda{
                   codomain: {:symbol, 'Bool'},
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'g'},
                   type: '::'
                 },
                 {:symbol, 'y'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'y'}
                 }
               }
             ] == eval(parsed)
    end

    test "variable is bound too late" do
      parsed =
        """
        f x:Nat
        ---
        f x = g x
        ;
        b => Bool
        ;
        g y:Nat  :: Bool
        """
        |> scan_and_parse

      {:error, {:unbound_variables, _, _}} = Eval.eval(parsed, [])
    end

    test "lambda binding failure" do
      parsed =
        """
        f x:Nat
        ---
        f x in fn z:D::D
        """
        |> scan_and_parse

      {:error, {:unbound_variables, _, _}} = Eval.eval(parsed, [])
    end

    test "lambda binding" do
      parsed =
        """
        f x:Nat
        ---
        f x in fn z:D ::D
        ;
        d => D
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               },
               %{
                 {:symbol, 'D'} => %Domain{
                   name: {:symbol, 'D'},
                   ref: {:symbol, 'D'}
                 },
                 {:symbol, 'd'} => %Lambda{
                   codomain: {:symbol, 'D'},
                   domain: [],
                   name: {:symbol, 'd'},
                   type: '=>'
                 }
               }
             ] == eval(parsed)
    end

    test "lambdas introduce temporary bindings" do
      parsed =
        """
        f x:Nat
        ---
        f x in fn z:Nat, z > 100
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               }
             ] == eval(parsed)
    end

    test "refinement with 0" do
      parsed =
        """
        f
        ---
        f <- 0
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Pantagruel.Values.Lambda{
                   codomain: nil,
                   domain: [],
                   name: {:symbol, 'f'},
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "alls introduce temporary bindings" do
      parsed =
        """
        f x:Nat
        con=> X
        ---
        all y : X \\ y < 10
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'X'} => %Domain{
                   name: {:symbol, 'X'},
                   ref: {:symbol, 'X'}
                 },
                 {:symbol, 'con'} => %Lambda{
                   codomain: {:symbol, 'X'},
                   domain: [],
                   name: {:symbol, 'con'},
                   type: '=>'
                 },
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               }
             ] == eval(parsed)
    end

    test "temporary bindings are temporary" do
      parsed =
        """
        f x:Nat
        con=> X
        ---
        all y : X \\ y < 10
        y > 1
        """
        |> scan_and_parse

      {:error, {:unbound_variables, unbounds, _}} = Eval.eval(parsed, [])

      assert MapSet.new(bin_appl: [:>, {:symbol, 'y'}, 1]) == unbounds
    end

    test "binding without bunching evals as two malformed bindings" do
      parsed =
        """
        w
        ---
        all j, k : X \\ j > k
        """
        |> scan_and_parse

      {:error, {:unbound_variables, unbounds, _}} = Eval.eval(parsed, [])

      assert [
               quantification: [
                 :all,
                 [guard: {:symbol, 'j'}, binding: [symbol: 'k', symbol: 'X']],
                 {:bin_appl, [:>, {:symbol, 'j'}, {:symbol, 'k'}]}
               ]
             ]
             |> MapSet.new() == unbounds
    end

    test "binding with bunching evals as two bindings" do
      parsed =
        """
        X <= 1
        ---
        all (j, k) : X \\ j > k
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'X'} => %Domain{name: {:symbol, 'X'}, ref: 1}
               }
             ] == eval(parsed)
    end

    test "exists binds" do
      parsed =
        """
        f x:Nat
        ---
        exists y : Nat \\ f y > 10
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 },
                 {:symbol, 'y'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'y'}
                 }
               }
             ] == eval(parsed)
    end

    test "binding rules regression" do
      parsed =
        """
        f
        ---
        f <- (all z : 1.f \\ f)
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [],
                   name: {:symbol, 'f'},
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "look in earlier scope for variable" do
      parsed =
        """
        f
        ;
        x
        ;
        y
        ---
        y x = f
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [],
                   name: {:symbol, 'f'},
                   type: nil
                 }
               },
               %{
                 {:symbol, 'x'} => %Lambda{
                   codomain: nil,
                   domain: [],
                   name: {:symbol, 'x'},
                   type: nil
                 }
               },
               %{
                 {:symbol, 'y'} => %Lambda{
                   codomain: nil,
                   domain: [],
                   name: {:symbol, 'y'},
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "comprehensions do not bind their variables to external scope" do
      parsed =
        """
        f x:Nat
        ---
        [y : Nat \\ f x > y]
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 }
               }
             ] == eval(parsed)
    end

    test "generics are bound into scope" do
      parsed =
        """
        f x:_A
        ---
        [y : _A \\ f x > y]
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, '_A'} => %Domain{
                   name: {:symbol, '_A'},
                   ref: {:symbol, '_A'}
                 },
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: '_A'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, '_A'},
                   name: {:symbol, 'x'}
                 }
               }
             ] == eval(parsed)
    end

    test "aliasing" do
      parsed =
        """
        Status <= {`ok}
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'Status'} => %Pantagruel.Values.Domain{
                   name: {:symbol, 'Status'},
                   ref: {:cont, [:set, [literal: 'ok']]}
                 }
               }
             ] == eval(parsed)
    end

    test "multiple aliasing" do
      parsed =
        """
        Status, State <= {`ok}
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'State'} => %Pantagruel.Values.Domain{
                   name: {:symbol, 'State'},
                   ref: {:cont, [:set, [literal: 'ok']]}
                 },
                 {:symbol, 'Status'} => %Pantagruel.Values.Domain{
                   name: {:symbol, 'Status'},
                   ref: {:cont, [:set, [literal: 'ok']]}
                 }
               }
             ] == eval(parsed)
    end

    test "object access" do
      parsed =
        """
        f (x, y):Nat
        ---
        f.x
        f.y
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat', symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 },
                 {:symbol, 'y'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'y'}
                 }
               }
             ] == eval(parsed)
    end

    test "object access on expression" do
      parsed =
        """
        f (x, y):Nat
        ---
        (f 1).x
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [symbol: 'Nat', symbol: 'Nat'],
                   name: {:symbol, 'f'},
                   type: nil
                 },
                 {:symbol, 'x'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'x'}
                 },
                 {:symbol, 'y'} => %Variable{
                   domain: {:symbol, 'Nat'},
                   name: {:symbol, 'y'}
                 }
               }
             ] == eval(parsed)
    end

    test "comprehension aliasing" do
      parsed = "Day <= {n : Nat, n =< 30 \\ n}" |> scan_and_parse

      assert [
               %{
                 {:symbol, 'Day'} => %Pantagruel.Values.Domain{
                   name: {:symbol, 'Day'},
                   ref:
                     {:cont,
                      [
                        :set,
                        {:comprehension,
                         [
                           [
                             binding: [symbol: 'n', symbol: 'Nat'],
                             guard: {:bin_appl, [:"=<", {:symbol, 'n'}, 30]}
                           ],
                           {:symbol, 'n'}
                         ]}
                      ]}
                 }
               }
             ] == eval(parsed)
    end

    test "nested quantifiers" do
      parsed =
        """
        f
        ---
        (all x : Nat \\ all y : x \\ y > 0)
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Lambda{
                   codomain: nil,
                   domain: [],
                   name: {:symbol, 'f'},
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "refinements are evaluated in context of guard" do
      parsed =
        """
        f
        ---
        f <- exists n : Nat \\ n > 1 \\ n
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'f'} => %Pantagruel.Values.Lambda{
                   codomain: nil,
                   domain: [],
                   name: {:symbol, 'f'},
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "sort" do
      # Note: this program is incorrect!
      parsed =
        """
        sort xs : [X]  :: [X]
        x => X
        ---
        all (x,y) : xs' \\ x =< y <-> ind xs' x < ind xs' y

        ;

        ind xs : [X], x : X :: Nat0
        """
        |> scan_and_parse

      assert [
               %{
                 {:symbol, 'X'} => %Pantagruel.Values.Domain{
                   name: {:symbol, 'X'},
                   ref: {:symbol, 'X'}
                 },
                 {:symbol, 'x'} => %Pantagruel.Values.Lambda{
                   codomain: {:symbol, 'X'},
                   domain: [],
                   name: {:symbol, 'x'},
                   type: '=>'
                 },
                 {:symbol, 'sort'} => %Pantagruel.Values.Lambda{
                   name: {:symbol, 'sort'},
                   type: '::',
                   codomain: {:cont, [:list, [symbol: 'X']]},
                   domain: [cont: [:list, [symbol: 'X']]]
                 },
                 {:symbol, 'xs'} => %Pantagruel.Values.Variable{
                   name: {:symbol, 'xs'},
                   domain: {:cont, [:list, [symbol: 'X']]}
                 }
               },
               %{
                 {:symbol, 'x'} => %Pantagruel.Values.Variable{
                   domain: {:symbol, 'X'},
                   name: {:symbol, 'x'}
                 },
                 {:symbol, 'ind'} => %Pantagruel.Values.Lambda{
                   codomain: {:symbol, 'Nat0'},
                   name: {:symbol, 'ind'},
                   type: '::',
                   domain: [cont: [:list, [symbol: 'X']], symbol: 'X']
                 },
                 {:symbol, 'xs'} => %Pantagruel.Values.Variable{
                   name: {:symbol, 'xs'},
                   domain: {:cont, [:list, [symbol: 'X']]}
                 }
               }
             ] == eval(parsed)
    end
  end
end
