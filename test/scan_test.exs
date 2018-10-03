defmodule ScanLest do
  use ExUnit.Case

  describe "Scanner test" do
    test "scan comments" do
      text = """
      line
      line2 ; comment
      line3
      """

      scanned = Pantagruel.Scan.scan(text)
      assert "line\nline2\n; comment\nline3" == scanned
    end

    test "scan spaces" do
      text = """
      first half
      second  half
      third
      """

      scanned = Pantagruel.Scan.scan(text)
      assert "first half\nsecond half\nthird" == scanned
    end

    test "scan right arrow" do
      text = """
      x -> y
      """

      scanned = Pantagruel.Scan.scan(text)
      assert "x→y" == scanned
    end

    test "space/newline consolidation" do
      text = "first line  \n\n  second line"
      text2 = "first line  \n  \n  second line"

      for t <- [text, text2] do
        scanned = Pantagruel.Scan.scan(t)
        assert "first line\nsecond line" == scanned
      end
    end

    test "newline consolidation in where" do
      text = """
      foo

      ;;

      where
      """

      scanned = Pantagruel.Scan.scan(text)
      assert "foo\n;;\nwhere" == scanned
    end

    test "two replaced characters in a row" do
      text = "(all h from sect.head . all sym from h . is_bound? sym)"
      scanned = Pantagruel.Scan.scan(text)
      assert "(∀h∈sect.head⸳∀sym∈h⸳is_bound? sym)" == scanned
    end

    test "or at the beginning?" do
      text = """
      [x from X, y from Z, z from Y . x * y ^ z]; Now includes semicolons; much better.
      or [x from String . x + `ok]
      """

      scanned = Pantagruel.Scan.scan(text)
      assert "[x∈X,y∈Z,z∈Y⸳x*y^z]\n; Now includes semicolons; much better.\n∨[x∈String⸳x+`ok]" == scanned
    end
  end
end
