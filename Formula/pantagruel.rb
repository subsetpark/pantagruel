class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.24.2"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.24.2/pant-macos-arm64.tar.gz"
    sha256 "ca1e4c11d8d22bbc4e6ad011323a9a79c1a938f1120a1d78d235d26cef20437e"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.24.2/pant-linux-x86_64.tar.gz"
    sha256 "d04cdba7556db64e8aa69210bfa91183fdaec26599088f475afc5d2b95270c89"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
