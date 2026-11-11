class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.25.1"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.25.1/pant-macos-arm64.tar.gz"
    sha256 "92e2458b6a2bd176a8a9c1130fcd6b8fe9d5aa6fb38c3961565d2aaf823dfc3c"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.25.1/pant-linux-x86_64.tar.gz"
    sha256 "d02a64b2acf05072ff40216c75be6ef5a0e7bc50246a958ced4defd3b9a90a46"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
