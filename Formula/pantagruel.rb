class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.24.1"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.24.1/pant-macos-arm64.tar.gz"
    sha256 "4d3d87ac3730daf4064091e37b2a02ba95a0d64952d6653be002d51b7486b7bf"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.24.1/pant-linux-x86_64.tar.gz"
    sha256 "36fbf510b3eef39fd026d5e1b7fcc47233608cf3adbd8f6e753ea83cb4d807e1"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
