class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.23.0"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.23.0/pant-macos-arm64.tar.gz"
    sha256 "f6e7e8fd04e4fba128958b4f72eae2f6a8c31ca45ea95fb279259332195ac921"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.23.0/pant-linux-x86_64.tar.gz"
    sha256 "a371ca4bcba8f7d3c16a86acfd3fb7e3b605e0b7c0dc69b7d832d3545fc54485"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
