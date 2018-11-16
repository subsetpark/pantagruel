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

  describe "operators" do
    test "operators" do
      {:ok, tokens, 1} = :lexer.string('#')
      assert [{:unary_operator, 1, '#'}] == tokens
    end

    test "prefix operators" do
      {:ok, tokens, 1} = :lexer.string('#x')
      assert [{:unary_operator, 1, '#'}, {:symbol, 1, 'x'}] == tokens
    end

    test "infix operators" do
      {:ok, tokens, 1} = :lexer.string('x>y')
      assert [{:symbol, 1, 'x'}, {:binary_operator, 1, '>'}, {:symbol, 1, 'y'}] == tokens
    end

    test "infix implies" do
      {:ok, tokens, 1} = :lexer.string('x->y')
      assert [{:symbol, 1, 'x'}, {:binary_operator, 1, '->'}, {:symbol, 1, 'y'}] == tokens
    end

    test "infix dot" do
      {:ok, tokens, 1} = :lexer.string('x.y')
      assert [{:symbol, 1, 'x'}, {:'.', 1, '.'}, {:symbol, 1, 'y'}] == tokens
    end
  end

  describe "symbols" do
    test "symbols" do
      {:ok, tokens, 1} = :lexer.string('x')
      assert [{:symbol, 1, 'x'}] == tokens
    end

    test "unicode symbols" do
      {:ok, tokens, 1} = :lexer.string('δδδ')
      assert [{:symbol, 1, 'δδδ'}] == tokens
    end

    test "symbols beginning with numbers" do
      {:ok, tokens, 1} = :lexer.string('1o')
      assert [{:symbol, 1, '1o'}] == tokens
    end

    test "list" do
      {:ok, tokens, 1} = :lexer.string('[x]')
      assert [{:'[', 1, '['}, {:symbol, 1, 'x'}, {:']', 1, ']'}] == tokens
    end

    test "list with commas" do
      {:ok, tokens, 1} = :lexer.string('[x, y]')

      assert [
               {:'[', 1, '['},
               {:symbol, 1, 'x'},
               {:',', 1, ','},
               {:symbol, 1, 'y'},
               {:']', 1, ']'}
             ] == tokens
    end
  end

  describe "comments" do
    test "comments" do
      {:ok, tokens, 2} = :lexer.string('"ok')
      assert [{:comment, 1, 'ok'}] == tokens
    end

    test "comment after token" do
      {:ok, tokens, 2} = :lexer.string('10 "ok')
      assert [{:int, 1, 10}, {:comment, 1, 'ok'}] == tokens
    end
  end

  describe "declarations" do
    test "basic declaration" do
      {:ok, tokens, 1} = :lexer.string('f()')

      assert [
               {:symbol, 1, 'f'},
               {:'(', 1, '('},
               {:')', 1, ')'}
             ] == tokens
    end
  end
end
