class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.25.0"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.25.0/pant-macos-arm64.tar.gz"
    sha256 "3468773b4eda6790a451b44f70be94f9427ecc1569292adaa1dfc02058b3dcd7"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.25.0/pant-linux-x86_64.tar.gz"
    sha256 "5a77c75a85061355a2fcd47b178e2414afc72766cbe3284d24f20e329898212a"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
