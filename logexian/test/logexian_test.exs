defmodule LogexianTest do
  use ExUnit.Case
  doctest Logexian

  defp do_decl_asserts(basic_decl) do
    {:ok, [sect: [decl: [ident, vars, doms, yield_type, yield_domain]]], "", _, _, _} =
      Logexian.Parse.program(basic_decl)

    assert ident == {:decl_ident, "f"}
    assert vars == {:decl_args, ["a", "b"]}
    assert doms == {:decl_doms, ["B", "C"]}
    assert yield_type == {:yield_type, "::"}
    assert yield_domain == {:yield_domain, "D"}
  end

  describe "subexpression parsing" do
    test "parse symbol sequence" do
      text = "x y z"
      {:ok, subexp, "", %{}, _, _} = Logexian.Parse.subexpression(text)
      assert ["x", "y", "z"] == subexp
    end

    test "parse bunch symbol sequence" do
      text = "(x y z)"
      {:ok, subexp, "", %{}, _, _} = Logexian.Parse.subexpression(text)
      assert [bunch: ["x", "y", "z"]] == subexp
    end

    test "parse set symbol sequence" do
      text = "{x y z}"
      {:ok, subexp, "", %{}, _, _} = Logexian.Parse.subexpression(text)
      assert [set: ["x", "y", "z"]] == subexp
    end

    test "parse bunched set symbol sequence" do
      text = "(x {y z})"
      {:ok, subexp, "", %{}, _, _} = Logexian.Parse.subexpression(text)
      assert [bunch: ["x", set: ["y", "z"]]] == subexp
    end
  end

  describe "expression parsing" do
    test "parse two expressions" do
      text = "f||\nx != y\ny > 1"
      {:ok, prog, "", %{}, _, _} = Logexian.Parse.program(text)

      assert [
               sect: [
                 decl: [
                   decl_ident: "f"
                 ],
                 expr: [right: ["x", "!=", "y"]],
                 expr: [right: ["y", ">", 1]]
               ]
             ] == prog
    end

    test "parse two expressions with connecting operator" do
      text = "f||\nx != y\nor y > 1"
      {:ok, prog, "", %{}, _, _} = Logexian.Parse.program(text)

      assert [
               sect: [
                 decl: [
                   decl_ident: "f"
                 ],
                 expr: [right: ["x", "!=", "y"]],
                 expr: [intro_op: "or", right: ["y", ">", 1]]
               ]
             ] == prog
    end

    test "parse expression with domain" do
      text = "f|x:Y and a : Y|"
      {:ok, prog, "", %{}, _, _} = Logexian.Parse.program(text)

      assert [
               sect: [
                 decl: [
                   decl_ident: "f",
                   decl_args: ["x"],
                   decl_doms: ["Y"],
                   expr: [right: ["a", ":", "Y"]]
                 ]
               ]
             ] == prog
    end

    test "parse expression with relation in it" do
      text = "f||\nx=y and y > 1"
      {:ok, prog, "", %{}, _, _} = Logexian.Parse.program(text)

      assert [
               sect: [
                 decl: [
                   decl_ident: "f"
                 ],
                 expr: [
                   left: ["x"],
                   op: "=",
                   right: ["y", "and", "y", ">", 1]
                 ]
               ]
             ] == prog
    end

    test "parse expression with multiple elements on the left" do
      text = "f||\nf x=y"
      {:ok, prog, "", %{}, _, _} = Logexian.Parse.program(text)

      assert [
               sect: [
                 decl: [
                   decl_ident: "f"
                 ],
                 expr: [
                   left: ["f", "x"],
                   op: "=",
                   right: ["y"]
                 ]
               ]
             ] == prog
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = """
      f || => D
      """

      {:ok,
       [
         sect: [
           decl: [
             decl_ident: "f",
             yield_type: "=>",
             yield_domain: "D"
           ]
         ]
       ], "", %{}, _, _} = Logexian.Parse.program(text)
    end

    test "basic heading parsing" do
      text = """
      f|a, b:B, C| :: D
      """

      do_decl_asserts(text)
    end

    test "heading with multiple clauses" do
      text = """
      f|x: Y and x != 1|
      """

      {:ok, prog, "", %{}, _, _} = Logexian.Parse.program(text)

      assert [
               sect: [
                 decl: [
                   decl_ident: "f",
                   decl_args: ["x"],
                   decl_doms: ["Y"],
                   expr: [right: ["x", "!=", 1]]
                 ]
               ]
             ] == prog
    end
  end

  describe "program structure" do
    test "two sections" do
      text = "f|x: Y|\n;;\ny|| => Y"

      {:ok, [sect: [decl: decl], sect: [decl: decl2]] = prog, "", %{}, _, _} =
        Logexian.Parse.program(text)

      assert [
               decl_ident: "f",
               decl_args: ["x"],
               decl_doms: ["Y"]
             ] == decl

      assert [
               decl_ident: "y",
               yield_type: "=>",
               yield_domain: "Y"
             ] == decl2

      text2 = "f|x: Y|\n\n;;\n\ny|| => Y"

      {:ok, ^prog, "", %{}, _, _} = Logexian.Parse.program(text2)
    end
  end
end
