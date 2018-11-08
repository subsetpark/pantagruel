defmodule LexerTest do
  use ExUnit.Case

  describe "integers" do
    test "integers" do
      {:ok, tokens, 1} = :lexer.string('45')
      assert [{:int, 1, 45}] == tokens
    end

    test "integers with underscores" do
      {:ok, tokens, 1} = :lexer.string('45_000')
      assert [{:int, 1, 45000}] == tokens
    end
  end

  describe "floats" do
    test "float" do
      {:ok, tokens, 1} = :lexer.string('1.0')
      assert [{:float, 1, 1.0}] == tokens
    end

    test "negative float" do
      {:ok, tokens, 1} = :lexer.string('-1.0')
      assert [{:float, 1, -1.0}] == tokens
    end
  end

  describe "literals" do
    test "literals" do
      {:ok, tokens, 1} = :lexer.string('`ok')
      assert [{:literal, 1, 'ok'}] == tokens
    end

    test 'literals with spaces' do
      {:ok, tokens, 1} = :lexer.string('`ok ok`')
      assert [{:literal, 1, 'ok ok'}] == tokens
    end

    test 'backtick literals' do
      {:ok, tokens, 1} = :lexer.string('``ok')
      assert [{:literal, 1, '`ok'}] == tokens
    end
  end

  describe "symbols" do
    test "symbols" do
      {:ok, tokens, 1} = :lexer.string('x')
      assert [{:symbol, 1, 'x'}] == tokens
    end

    test "list" do
      {:ok, tokens, 1} = :lexer.string('[x]')
      assert [{:leftbracket, 1}, {:symbol, 1, 'x'}, {:rightbracket, 1}] == tokens
    end

    test "list with commas" do
      {:ok, tokens, 1} = :lexer.string('[x, y]')

      assert [
               {:leftbracket, 1},
               {:symbol, 1, 'x'},
               {:comma, 1},
               {:symbol, 1, 'y'},
               {:rightbracket, 1}
             ] == tokens
    end
  end

  describe "comments" do
    test "comments" do
      {:ok, tokens, 2} = :lexer.string('"ok\n')
      assert [{:comment, 1, 'ok'}] == tokens
    end

    test "comment after token" do
      {:ok, tokens, 2} = :lexer.string('10 "ok\n')
      assert [{:int, 1, 10}, {:comment, 1, 'ok'}] == tokens
    end
  end
end
