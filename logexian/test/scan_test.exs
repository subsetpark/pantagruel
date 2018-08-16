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
  end
end
