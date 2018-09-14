defmodule EvalTest do
  use ExUnit.Case
  alias Pantagruel.Env.UnboundVariablesError
  alias Pantagruel.Eval.{Variable, Lambda, Domain}

  defp scan_and_parse(text) do
    {:ok, parsed, "", %{}, _, _} =
      text
      |> Pantagruel.Scan.scan()
      |> Pantagruel.Parse.program()

    parsed
  end

  describe "program evaluation" do
    test "eval happy path" do
      parsed = "f|x:Nat . x > 1| :: Real" |> scan_and_parse

      assert [
               %{
                 "x" => %Variable{name: "x", domain: "ℕ"},
                 "f" => %Lambda{name: "f", domain: ["ℕ"], codomain: "ℝ", type: :function}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "eval unbound" do
      parsed = "f|x:X| :: Real" |> scan_and_parse

      exc =
        assert_raise UnboundVariablesError, fn ->
          Pantagruel.Eval.eval(parsed)
        end

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

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["X", "Y"], codomain: "ℝ", type: :function},
                 "x" => %Variable{name: "x", domain: "X"},
                 "y" => %Variable{name: "y", domain: "Y"},
                 "X" => %Domain{name: "X", alias: "X"},
                 "Y" => %Domain{name: "Y", alias: "Y"},
                 "make_x" => %Lambda{
                   name: "make_x",
                   domain: [],
                   codomain: "X",
                   type: :constructor
                 },
                 "make_y" => %Lambda{
                   name: "make_y",
                   domain: [],
                   codomain: "Y",
                   type: :constructor
                 }
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "eval whole section" do
      parsed = "f|x:Nat|\nf x > 0" |> scan_and_parse

      assert [
               %{
                 "x" => %Variable{name: "x", domain: "ℕ"},
                 "f" => %Lambda{name: "f", domain: ["ℕ"], codomain: nil, type: nil}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "eval two sections" do
      parsed = "f|x:Nat|\n;;\ng|| :: Nat" |> scan_and_parse

      assert [
               %{
                 "x" => %Variable{name: "x", domain: "ℕ"},
                 "f" => %Lambda{
                   name: "f",
                   domain: ["ℕ"],
                   codomain: nil,
                   type: nil
                 }
               },
               %{
                 "g" => %Lambda{
                   name: "g",
                   domain: [],
                   codomain: "ℕ",
                   type: :function
                 }
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "unbound variable in body" do
      parsed = "f|x:Nat|\nf x = g x" |> scan_and_parse

      assert_raise UnboundVariablesError, fn ->
        Pantagruel.Eval.eval(parsed)
      end
    end

    test "bind variables in the next section" do
      parsed =
        """
        f|x:Nat|
        f x <- g x
        ;;
        g|y:Nat|::Bool
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["ℕ"], codomain: nil, type: nil},
                 "x" => %Variable{name: "x", domain: "ℕ"}
               },
               %{
                 "g" => %Lambda{name: "g", domain: ["ℕ"], codomain: "𝔹", type: :function},
                 "y" => %Variable{name: "y", domain: "ℕ"}
               }
             ] == Pantagruel.Eval.eval(parsed)
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

      assert_raise UnboundVariablesError, fn ->
        Pantagruel.Eval.eval(parsed)
      end
    end

    test "lambda binding failure" do
      parsed =
        """
        f|x:Nat|
        f x : |z:D|::D
        """
        |> scan_and_parse

      assert_raise UnboundVariablesError, fn ->
        Pantagruel.Eval.eval(parsed)
      end
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

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["ℕ"], codomain: nil, type: nil},
                 "x" => %Variable{name: "x", domain: "ℕ"}
               },
               %{
                 "d" => %Lambda{name: "d", domain: [], codomain: "D", type: :constructor},
                 "D" => %Domain{name: "D", alias: "D"}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "lambdas introduce temporary bindings" do
      parsed =
        """
        f|x:Nat|
        f x : |z:Nat . z > 100|
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["ℕ"], codomain: nil, type: nil},
                 "x" => %Variable{name: "x", domain: "ℕ"}
                 # Notice `z` is not here. It was introduced so that
                 # `z > 100` checked, but it's not in the resulting scope.
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "alls introduce temporary bindings" do
      parsed =
        """
        f|x:Nat|
        con||=> X
        all y : X . y < 10
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["ℕ"], codomain: nil, type: nil},
                 "con" => %Lambda{name: "con", domain: [], codomain: "X", type: :constructor},
                 "X" => %Domain{name: "X", alias: "X"},
                 "x" => %Variable{name: "x", domain: "ℕ"}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "temporary bindings are temporary" do
      parsed =
        """
        f|x:Nat|
        con||=> X
        all y : X . y < 10
        y > 1
        """
        |> scan_and_parse

      exc =
        assert_raise UnboundVariablesError, fn ->
          Pantagruel.Eval.eval(parsed)
        end

      assert exc.unbound == MapSet.new([appl: [operator: :gt, x: "y", y: 1]])
    end

    test "too many domains" do
      parsed =
        """
        f|x:Nat,Real|
        """
        |> scan_and_parse

      assert_raise RuntimeError, fn ->
        Pantagruel.Eval.eval(parsed)
      end
    end

    test "exists binds" do
      parsed =
        """
        f|x:Nat|
        exists y : Nat . f y > 10
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   name: "f",
                   domain: ["ℕ"],
                   codomain: nil,
                   type: nil
                 },
                 "x" => %Variable{name: "x", domain: "ℕ"},
                 "y" => %Variable{name: "y", domain: "ℕ"}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "look in earlier scope for variable" do
      parsed =
        """
        f||
        ;;
        x||
        ;;
        y||
        y x = f
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Pantagruel.Eval.Lambda{
                   name: "f",
                   domain: [],
                   codomain: nil,
                   type: nil
                 }
               },
               %{
                 "x" => %Pantagruel.Eval.Lambda{
                   name: "x",
                   domain: [],
                   codomain: nil,
                   type: nil
                 }
               },
               %{
                 "y" => %Pantagruel.Eval.Lambda{
                   name: "y",
                   domain: [],
                   codomain: nil,
                   type: nil
                 }
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "comprehensions do not bind their variables to external scope" do
      parsed =
        """
        f|x:Nat|
        [y from Nat . f x > y]
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Pantagruel.Eval.Lambda{
                   codomain: nil,
                   domain: ["ℕ"],
                   name: "f",
                   type: nil
                 },
                 "x" => %Pantagruel.Eval.Variable{domain: "ℕ", name: "x"}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "generics are bound into scope" do
      parsed =
        """
        f|x:_A|
        [y from _A . f x > y]
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Pantagruel.Eval.Lambda{
                   codomain: nil,
                   domain: ["_A"],
                   name: "f",
                   type: nil
                 },
                 "_A" => %Pantagruel.Eval.Domain{name: "_A", alias: "_A"},
                 "x" => %Pantagruel.Eval.Variable{domain: "_A", name: "x"}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "sort" do
      # Note: this program is incorrect!
      parsed =
        """
        sort|xs : [X]| :: [X]
        x|| => X

        all (x,y) from xs'⸳x <= y = ind xs' x < ind xs' y

        ;;

        ind|xs, x : [X], X| :: Nat0
        """
        |> scan_and_parse

      assert [
               %{
                 "X" => %Domain{name: "X", alias: "X"},
                 "sort" => %Lambda{
                   domain: [list: ["X"]],
                   codomain: {:list, ["X"]},
                   name: "sort",
                   type: :function
                 },
                 "x" => %Lambda{codomain: "X", domain: [], name: "x", type: :constructor},
                 "xs" => %Variable{domain: {:list, ["X"]}, name: "xs"}
               },
               %{
                 "x" => %Variable{name: "x", domain: "X"},
                 "ind" => %Lambda{
                   domain: [{:list, ["X"]}, "X"],
                   codomain: "ℕ0",
                   name: "ind",
                   type: :function
                 },
                 "xs" => %Variable{domain: {:list, ["X"]}, name: "xs"}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end
  end
end
