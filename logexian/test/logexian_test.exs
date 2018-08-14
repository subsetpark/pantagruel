defmodule LogexianTest do
  use ExUnit.Case
  doctest Logexian

  defp do_decl_asserts(basic_decl) do
    {:ok, [decl: [ident, vars, doms, yield_type, yield_domain]], "", _, _, _} =
      Logexian.program(basic_decl)

    assert ident == {:decl_ident, "f"}
    assert vars == {:decl_args, ["a", "b"]}
    assert doms == {:decl_doms, ["B", "C"]}
    assert yield_type == {:yield_type, "::"}
    assert yield_domain == {:yield_domain, "D"}
  end

  describe "relation parsing" do
    test "parse inequality" do
      {:ok, [left: "x", operator: "!=", right: "y"], "", %{}, _, _} = Logexian.relation("x != y")
    end
  end

  describe "declaration parsing" do
    test "empty heading parsing" do
      text = """
      f <> => D 
      """

      {:ok,
       [
         decl: [
           decl_ident: "f",
           yield_type: "=>",
           yield_domain: "D"
         ]
       ], "", %{}, _, _} = Logexian.program(text)
    end

    test "basic heading parsing" do
      text = """
      f<a, b:B, C> :: D 
      """

      do_decl_asserts(text)
    end

    test "heading with multiple clauses" do
      text = """
      f<x: Y and x != 1>
      """

      :ok = Logexian.program(text)
    end
  end

  describe "program structure" do
    test "two declarations" do
      text = "f<x: Y>\n;;\ny<> => Y"
      {:ok, [decl: decl, decl: decl2], "", %{}, _, _} = Logexian.program(text)
      assert [decl_ident: "f", decl_args: ["x"], decl_doms: ["Y"]] == decl

      assert [
               decl_ident: "y",
               yield_type: "=>",
               yield_domain: "Y"
             ] == decl2
    end
  end
end
