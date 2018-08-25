defmodule EvalTest do
  use ExUnit.Case
  alias Pantagruel.Eval.State.UnboundVariablesError
  alias Pantagruel.Eval.{Variable, Lambda, Scope}

  defp scan_and_parse(text) do
    {:ok, parsed, "", %{}, _, _} =
      text
      |> Pantagruel.Scan.scan()
      |> Pantagruel.Parse.program()

    parsed
  end

  describe "program evaluation" do
    test "eval happy path" do
      parsed = "f|x:Nat and x > 1| :: Real" |> scan_and_parse

      assert {%Scope{
                bindings: %{
                  "x" => %Variable{name: "x", domain: "Nat"},
                  "f" => %Lambda{name: "f", domain: ["Nat"], codomain: "Real", type: :function}
                },
                parent: nil
              }, MapSet.new(), MapSet.new()} == Pantagruel.Eval.eval(parsed)
    end

    test "eval unbound" do
      parsed = "f|x:X| :: Real" |> scan_and_parse

      exc = assert_raise UnboundVariablesError, fn -> Pantagruel.Eval.eval(parsed) end

      assert exc.unbound == MapSet.new(["X"])
    end

    test "eval late binding" do
      parsed =
        """
        f|x, y:X, Y| :: Real
        make_y|| => Y
        make_x|| => X
        """
        |> scan_and_parse

      assert {%{
                "f" => %Lambda{name: "f", domain: ["X", "Y"], codomain: "Real", type: :function},
                "x" => %Variable{name: "x", domain: "X"},
                "y" => %Variable{name: "y", domain: "Y"},
                "X" => %Variable{name: "X", domain: "X"},
                "Y" => %Variable{name: "Y", domain: "Y"},
                "make_x" => %Lambda{name: "make_x", domain: [], codomain: "X", type: :constructor},
                "make_y" => %Lambda{name: "make_y", domain: [], codomain: "Y", type: :constructor}
              } == Pantagruel.Eval.eval(parsed), MapSet.new()}
    end

    test "eval whole section" do
      parsed = "f|x:Nat|\nf x > 0" |> scan_and_parse

      assert {%{
                "x" => %Variable{name: "x", domain: "Nat"},
                "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function}
              } == Pantagruel.Eval.eval(parsed), MapSet.new()}
    end

    test "eval two sections" do
      parsed = "f|x:Nat|\n;;\ng|| :: Nat" |> scan_and_parse

      assert {%{
                "x" => %Variable{name: "x", domain: "Nat"},
                "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                "g" => %Lambda{name: "g", domain: [], codomain: "Nat", type: :function}
              } == Pantagruel.Eval.eval(parsed), MapSet.new()}
    end

    test "unbound variable in body" do
      parsed = "f|x:Nat|\nf x = g x" |> scan_and_parse

      assert_raise UnboundVariablesError, fn -> Pantagruel.Eval.eval(parsed) end
    end

    test "bind variables in the next section" do
      parsed =
        """
        f|x:Nat|
        f x = g x
        ;;
        g|y:Nat|::Bool
        """
        |> scan_and_parse

      assert {
               %Scope{
                 bindings: %{
                   "g" => %Lambda{name: "g", domain: ["Nat"], codomain: "Bool", type: :function},
                   "y" => %Variable{name: "y", domain: "Nat"}
                 },
                 parent: %Scope{
                   bindings: %{
                     "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                     "x" => %Variable{name: "x", domain: "Nat"}
                   },
                   parent: nil
                 }
               },
               MapSet.new(),
               MapSet.new()
             } == Pantagruel.Eval.eval(parsed)
    end

    test "variable is bound too late" do
      parsed =
        """
        f|x:Nat|
        f x = g x
        ;;
        b|| => Bool
        ;;
        g|y:Nat| :: Bool
        """
        |> scan_and_parse

      assert_raise UnboundVariablesError, fn -> Pantagruel.Eval.eval(parsed) end
    end

    test "lambda binding failure" do
      parsed =
        """
        f|x:Nat|
        f x : |z:D|::D
        """
        |> scan_and_parse

      assert_raise UnboundVariablesError, fn -> Pantagruel.Eval.eval(parsed) end
    end

    test "lambda binding" do
      parsed =
        """
        f|x:Nat|
        f x : |z:D|::D
        ;;
        d|| => D
        """
        |> scan_and_parse

      assert {
               %Scope{
                 bindings: %{
                   "d" => %Lambda{name: "d", domain: [], codomain: "D", type: :constructor},
                   "D" => %Variable{name: "D", domain: "D"}
                 },
                 parent: %Scope{
                   bindings: %{
                     "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                     "x" => %Variable{name: "x", domain: "Nat"}
                   },
                   parent: nil
                 }
               },
               MapSet.new(),
               MapSet.new()
             } == Pantagruel.Eval.eval(parsed)
    end

    test "lambdas introduce temporary bindings" do
      parsed =
        """
        f|x:Nat|
        f x : |z:Nat and z > 100|
        """
        |> scan_and_parse

      assert {
               %Scope{
                 bindings: %{
                   "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                   "x" => %Variable{name: "x", domain: "Nat"}
                   # Notice `z` is not here. It was introduced so that
                   # `z > 100` checked, but it's not in the resulting scope.
                 },
                 parent: nil
               },
               MapSet.new(),
               MapSet.new()
             } == Pantagruel.Eval.eval(parsed)
    end

    test "alls introduce temporary bindings" do
      parsed =
        """
        f|x:Nat|
        con||=> X
        all y : X y < 10
        """
        |> scan_and_parse

      assert {
               %Scope{
                 bindings: %{
                   "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                   "con" => %Lambda{name: "con", domain: [], codomain: "X", type: :constructor},
                   "X" => %Variable{name: "X", domain: "X"},
                   "x" => %Variable{name: "x", domain: "Nat"}
                 },
                 parent: nil
               },
               MapSet.new(),
               MapSet.new()
             } == Pantagruel.Eval.eval(parsed)
    end

    test "temporary bindings are temporary" do
      parsed =
        """
        f|x:Nat|
        con||=> X
        all y : X y < 10
        y > 1
        """
        |> scan_and_parse

      exc = assert_raise UnboundVariablesError, fn -> Pantagruel.Eval.eval(parsed) end
      assert exc.unbound == MapSet.new(["y"])
    end

    test "exists binds" do
      parsed =
        """
        f|x:Nat|
        exists y : Nat f y > 10
        """
        |> scan_and_parse

      assert {
               %Scope{
                 bindings: %{
                   "f" => %Lambda{
                     name: "f",
                     domain: ["Nat"],
                     codomain: nil,
                     type: :function
                   },
                   "x" => %Variable{name: "x", domain: "Nat"},
                   "y" => %Variable{name: "y", domain: "Nat"}
                 },
                 parent: nil
               },
               MapSet.new(),
               MapSet.new()
             } == Pantagruel.Eval.eval(parsed)
    end
  end
end
