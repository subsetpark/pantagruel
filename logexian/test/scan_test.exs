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
  end
end
