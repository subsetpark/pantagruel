defmodule LogexianTest do
  use ExUnit.Case
  doctest Logexian

  test "greets the world" do
    {:ok, [vars, doms], "", _, _, _} = Logexian.program(";;\n<a, b:B, C>")
    assert vars == ["a", "b"]
    assert doms == ["B", "C"]
  end
end
