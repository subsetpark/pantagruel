defmodule EvalTest do
  use ExUnit.Case
  alias Pantagruel.Values.{Variable, Lambda, Domain}
  alias Pantagruel.Eval

  defp scan_and_parse(text) when is_binary(text) do
    text
    |> Kernel.<>("\n")
    |> String.to_charlist()
    |> scan_and_parse
  end

  defp scan_and_parse(text) do
    with {:ok, tokens, _} <- :lexer.string(text ++ '\n') do
      {:ok, parsed} = Pantagruel.Parse.program(tokens)
      parsed
    end
  end

  defp eval(parsed) do
    {:ok, scope} = Eval.eval(parsed, [])
    scope
  end

  describe "program evaluation" do
    test "eval happy path" do
      parsed = "f(x:Nat . x > 1) :: Real" |> scan_and_parse

      assert [
               %{
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: "Real", type: :function}
               }
             ] == eval(parsed)
    end

    test "eval unbound" do
      parsed = "f(x:X) :: Real" |> scan_and_parse

      {:error, {:unbound_variables, e}} = Eval.eval(parsed, [])

      assert e.unbound == MapSet.new(["X"])
    end

    test "eval late binding" do
      parsed =
        """
        f(x, y:X, Y) :: Real
        make_y() => Y
        make_x() => X
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["X", "Y"], codomain: "Real", type: :function},
                 "x" => %Variable{name: "x", domain: "X"},
                 "y" => %Variable{name: "y", domain: "Y"},
                 "X" => %Domain{name: "X", ref: "X"},
                 "Y" => %Domain{name: "Y", ref: "Y"},
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
             ] == eval(parsed)
    end

    test "eval whole section" do
      parsed =
        """
        f(x:Nat)
        ---
        f x > 0
        """
        |> scan_and_parse

      assert [
               %{
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: nil}
               }
             ] == eval(parsed)
    end

    test "eval two sections" do
      parsed =
        """
        f(x:Nat)
        ;
        g() :: Nat
        """
        |> scan_and_parse

      assert [
               %{
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "f" => %Lambda{
                   name: "f",
                   domain: ["Nat"],
                   codomain: nil,
                   type: nil
                 }
               },
               %{
                 "g" => %Lambda{
                   name: "g",
                   domain: [],
                   codomain: "Nat",
                   type: :function
                 }
               }
             ] == eval(parsed)
    end

    test "unbound variable in body" do
      parsed =
        """
        f(x:Nat)
        ---
        f x = g x
        """
        |> scan_and_parse

      {:error, {:unbound_variables, _}} = Eval.eval(parsed, [])
    end

    test "bind variables in the next section" do
      parsed =
        """
        f(x:Nat)
        ---
        f x <- g x
        ;
        g(y:Nat)::Bool
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: nil},
                 "x" => %Variable{name: "x", domain: "Nat"}
               },
               %{
                 "g" => %Lambda{name: "g", domain: ["Nat"], codomain: "Bool", type: :function},
                 "y" => %Variable{name: "y", domain: "Nat"}
               }
             ] == eval(parsed)
    end

    test "variable is bound too late" do
      parsed =
        """
        f(x:Nat)
        ---
        f x = g x
        ;
        b() => Bool
        ;
        g(y:Nat) :: Bool
        """
        |> scan_and_parse

      {:error, {:unbound_variables, _}} = Eval.eval(parsed, [])
    end

    test "lambda binding failure" do
      parsed =
        """
        f(x:Nat)
        ---
        f x : fn (z:D)::D
        """
        |> scan_and_parse

      {:error, {:unbound_variables, _}} = Eval.eval(parsed, [])
    end

    test "lambda binding" do
      parsed =
        """
        f(x:Nat)
        ---
        f x : fn(z:D)::D
        ;
        d() => D
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: nil},
                 "x" => %Variable{name: "x", domain: "Nat"}
               },
               %{
                 "d" => %Lambda{name: "d", domain: [], codomain: "D", type: :constructor},
                 "D" => %Domain{name: "D", ref: "D"}
               }
             ] == eval(parsed)
    end

    test "lambdas introduce temporary bindings" do
      parsed =
        """
        f(x:Nat)
        ---
        f x : (z:Nat . z > 100)
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: nil},
                 "x" => %Variable{name: "x", domain: "Nat"}
                 # Notice `z` is not here. It was introduced so that
                 # `z > 100` checked, but it's not in the resulting scope.
               }
             ] == eval(parsed)
    end

    test "alls introduce temporary bindings" do
      parsed =
        """
        f(x:Nat)
        con()=> X
        ---
        all y : X . y < 10
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{name: "f", domain: ["Nat"], codomain: nil, type: nil},
                 "con" => %Lambda{name: "con", domain: [], codomain: "X", type: :constructor},
                 "X" => %Domain{name: "X", ref: "X"},
                 "x" => %Variable{name: "x", domain: "Nat"}
               }
             ] == eval(parsed)
    end

    test "temporary bindings are temporary" do
      parsed =
        """
        f(x:Nat)
        con()=> X
        ---
        all y : X . y < 10
        y > 1
        """
        |> scan_and_parse

      {:error, {:unbound_variables, e}} = Eval.eval(parsed, [])

      assert e.unbound == MapSet.new(appl: [operator: :gt, x: "y", y: 1])
    end

    test "too many domains" do
      parsed =
        """
        f(x:Nat,Real)
        """
        |> scan_and_parse

      {:error, {:domain_mismatch, e}} = Eval.eval(parsed, [])

      assert e.args == ["x"]
      assert e.doms == ["Nat", "Real"]
    end

    test "binding without bunching evals as two malformed bindings" do
      parsed =
        """
        w()
        ---
        all j, k : X . j > k
        """
        |> scan_and_parse

      {:error, {:unbound_variables, e}} = Eval.eval(parsed, [])

      assert [
               quantification: [
                 quantifier: :forall,
                 bindings: [
                   guard: "j",
                   binding: [
                     bind_symbol: "k",
                     bind_op: :in,
                     bind_domain: "X"
                   ]
                 ],
                 expr:
                   {:appl,
                    [
                      operator: :gt,
                      x: "j",
                      y: "k"
                    ]}
               ]
             ]
             |> MapSet.new() == e.unbound
    end

    test "binding with bunching evals as two bindings" do
      parsed =
        """
        X <= 1
        ---
        all (j, k) : X . j > k
        """
        |> scan_and_parse

      assert [
               %{
                 "X" => %Domain{name: "X", ref: 1}
               }
             ] == eval(parsed)
    end

    test "exists binds" do
      parsed =
        """
        f(x:Nat)
        ---
        exists y : Nat . f y > 10
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   name: "f",
                   domain: ["Nat"],
                   codomain: nil,
                   type: nil
                 },
                 "x" => %Variable{name: "x", domain: "Nat"},
                 "y" => %Variable{name: "y", domain: "Nat"}
               }
             ] == eval(parsed)
    end

    test "binding rules regression" do
      parsed =
        """
        f()
        ---
        f <- (all z from 1.f . f)
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   codomain: nil,
                   domain: [],
                   name: "f",
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "look in earlier scope for variable" do
      parsed =
        """
        f()
        ;
        x()
        ;
        y()
        ---
        y x = f
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   name: "f",
                   domain: [],
                   codomain: nil,
                   type: nil
                 }
               },
               %{
                 "x" => %Lambda{
                   name: "x",
                   domain: [],
                   codomain: nil,
                   type: nil
                 }
               },
               %{
                 "y" => %Lambda{
                   name: "y",
                   domain: [],
                   codomain: nil,
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "comprehensions do not bind their variables to external scope" do
      parsed =
        """
        f(x:Nat)
        ---
        [y from Nat . f x > y]
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   codomain: nil,
                   domain: ["Nat"],
                   name: "f",
                   type: nil
                 },
                 "x" => %Variable{domain: "Nat", name: "x"}
               }
             ] == eval(parsed)
    end

    test "generics are bound into scope" do
      parsed =
        """
        f(x:_A)
        ---
        [y from _A . f x > y]
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   codomain: nil,
                   domain: ["_A"],
                   name: "f",
                   type: nil
                 },
                 "_A" => %Domain{name: "_A", ref: "_A"},
                 "x" => %Variable{domain: "_A", name: "x"}
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
                 "Status" => %Domain{
                   ref: {:set, [literal: "ok"]},
                   name: "Status"
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
                 "Status" => %Domain{
                   ref: {:set, [literal: "ok"]},
                   name: "Status"
                 },
                 "State" => %Domain{
                   ref: {:set, [literal: "ok"]},
                   name: "State"
                 }
               }
             ] == eval(parsed)
    end

    test "object access" do
      parsed =
        """
        f(x, y:Nat)
        ---
        f.x
        f.y
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   codomain: nil,
                   domain: ["Nat"],
                   name: "f",
                   type: nil
                 },
                 "x" => %Variable{domain: "Nat", name: "x"},
                 "y" => %Variable{domain: "Nat", name: "y"}
               }
             ] == eval(parsed)
    end

    test "object access on expression" do
      parsed =
        """
        f(x, y:Nat)
        ---
        (f 1).x
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   codomain: nil,
                   domain: ["Nat"],
                   name: "f",
                   type: nil
                 },
                 "x" => %Variable{domain: "Nat", name: "x"},
                 "y" => %Variable{domain: "Nat", name: "y"}
               }
             ] == eval(parsed)
    end

    test "comprehension aliasing" do
      parsed = "Day <= {n : Nat, n <= 30 . n}" |> scan_and_parse

      assert [
               %{
                 "Day" => %Domain{
                   name: "Day",
                   ref:
                     {:comprehension,
                      [
                        set: [
                          {:comp_bindings,
                           [
                             binding: [
                               bind_symbol: "n",
                               bind_op: :in,
                               bind_domain: "Nat"
                             ],
                             guard:
                               {:appl,
                                [
                                  operator: :lte,
                                  x: "n",
                                  y: 30
                                ]}
                           ]},
                          {:comp_expression, "n"}
                        ]
                      ]}
                 }
               }
             ] == eval(parsed)
    end

    test "nested quantifiers" do
      parsed =
        """
        f()
        ---
        (all x from Nat . all y from x . y > 0)
        """
        |> scan_and_parse

      assert [
               %{
                 "f" => %Lambda{
                   codomain: nil,
                   domain: [],
                   name: "f",
                   type: nil
                 }
               }
             ] == eval(parsed)
    end

    test "sort" do
      # Note: this program is incorrect!
      parsed =
        """
        sort(xs : [X]) :: [X]
        x() => X
        ---
        all (x,y) from xs'⸳x <= y = ind xs' x < ind xs' y

        ;

        ind(xs, x : [X], X) :: Nat0
        """
        |> scan_and_parse

      assert [
               %{
                 "X" => %Domain{name: "X", ref: "X"},
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
                   codomain: "Nat0",
                   name: "ind",
                   type: :function
                 },
                 "xs" => %Variable{domain: {:list, ["X"]}, name: "xs"}
               }
             ] == eval(parsed)
    end
  end
end
