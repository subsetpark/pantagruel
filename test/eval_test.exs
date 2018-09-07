defmodule EvalTest do
  use ExUnit.Case
  alias Pantagruel.Eval.BindingChecks.UnboundVariablesError
  alias Pantagruel.Eval.{Variable, Lambda}

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
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"},
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: "Real", type: :function}
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
                 "f" => %Lambda{name: "f", domain: ["X", "Y"], codomain: "Real", type: :function},
                 "x" => %Variable{name: "x", domain: "X"},
                 "x'" => %Variable{name: "x'", domain: "X"},
                 "y" => %Variable{name: "y", domain: "Y"},
                 "y'" => %Variable{name: "y'", domain: "Y"},
                 "X" => %Variable{name: "X", domain: "X"},
                 "Y" => %Variable{name: "Y", domain: "Y"},
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
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"},
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end

    test "eval two sections" do
      parsed = "f|x:Nat|\n;;\ng|| :: Nat" |> scan_and_parse

      assert [
               %{
                 "g" => %Lambda{
                   name: "g",
                   domain: [],
                   codomain: "Nat",
                   type: :function
                 }
               },
               %{
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"},
                 "f" => %Lambda{
                   name: "f",
                   domain: ["Nat"],
                   codomain: nil,
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
                 "g" => %Lambda{name: "g", domain: ["Nat"], codomain: "Bool", type: :function},
                 "y" => %Variable{name: "y", domain: "Nat"},
                 "y'" => %Variable{name: "y'", domain: "Nat"}
               },
               %{
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"}
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
                 "d" => %Lambda{name: "d", domain: [], codomain: "D", type: :constructor},
                 "D" => %Variable{name: "D", domain: "D"}
               },
               %{
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"}
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
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"}
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
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: :function},
                 "con" => %Lambda{name: "con", domain: [], codomain: "X", type: :constructor},
                 "X" => %Variable{name: "X", domain: "X"},
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"}
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

      assert exc.unbound == MapSet.new(["y"])
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
                   domain: ["Nat"],
                   codomain: nil,
                   type: :function
                 },
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "x'" => %Variable{name: "x'", domain: "Nat"},
                 "y" => %Variable{name: "y", domain: "Nat"}
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
                 "y" => %Pantagruel.Eval.Lambda{
                   name: "y",
                   domain: [],
                   codomain: nil,
                   type: :function
                 }
               },
               %{
                 "x" => %Pantagruel.Eval.Lambda{
                   name: "x",
                   domain: [],
                   codomain: nil,
                   type: :function
                 }
               },
               %{
                 "f" => %Pantagruel.Eval.Lambda{
                   name: "f",
                   domain: [],
                   codomain: nil,
                   type: :function
                 }
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
                 "x" => %Variable{name: "x", domain: "X"},
                 "x'" => %Variable{name: "x'", domain: "X"},
                 "ind" => %Lambda{
                   domain: [{:list, ["X"]}, "X"],
                   codomain: "Nat0",
                   name: "ind",
                   type: :function
                 },
                 "xs" => %Variable{domain: {:list, ["X"]}, name: "xs"},
                 "xs'" => %Variable{domain: {:list, ["X"]}, name: "xs'"}
               },
               %{
                 "X" => %Variable{domain: "X", name: "X"},
                 "sort" => %Lambda{
                   domain: [list: ["X"]],
                   codomain: {:list, ["X"]},
                   name: "sort",
                   type: :function
                 },
                 "x" => %Lambda{codomain: "X", domain: [], name: "x", type: :constructor},
                 "xs" => %Variable{domain: {:list, ["X"]}, name: "xs"},
                 "xs'" => %Variable{domain: {:list, ["X"]}, name: "xs'"}
               }
             ] == Pantagruel.Eval.eval(parsed)
    end
  end
end
