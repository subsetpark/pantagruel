class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.22.2"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.22.2/pant-macos-arm64.tar.gz"
    sha256 "2651a180b9ea1cf068bdae4a92792c5ec3f03ba9255918f67ad055f7953e33dd"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.22.2/pant-linux-x86_64.tar.gz"
    sha256 "7593838d3a3535d0d8fda66fb6ed3e416e7dfc22c9680983d380e319b0ed8f3d"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
