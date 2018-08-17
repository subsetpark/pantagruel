defmodule ScanLest do
  use ExUnit.Case

  describe "Scanner test" do
    test "scan comments" do
      text = """
      line
      line2 ; comment
      line3
      """

      scanned = Logexian.Scan.scan(text)
      assert "line\nline2 \nline3\n" == scanned
    end

    test "scan spaces" do
      text = """
      first half
      second  half
      third
      """

      scanned = Logexian.Scan.scan(text)
      assert "first half\nsecond half\nthird\n" == scanned
    end

    test "scan right arrow" do
      text = """
      x -> y
      """

      scanned = Logexian.Scan.scan(text)
      assert "x → y\n" == scanned
    end

    test "space/newline consolidation" do
      text = "first line \n second line"

      scanned = Logexian.Scan.scan(text)
      assert "first line\nsecond line" == scanned
    end
  end
end
