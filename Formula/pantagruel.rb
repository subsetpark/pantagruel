class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.22.0"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.22.0/pant-macos-arm64.tar.gz"
    sha256 "71387673bd63dfc5478ddd59fbff9f1d896dba37fb8dcb80d34bd2654b7adce7"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.22.0/pant-linux-x86_64.tar.gz"
    sha256 "2827b4ace0f2f8b33c4396776077763778a07f4f1257927ad388f661dc58fc71"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
