class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.21.1"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/subsetpark/pantagruel/releases/download/v#{version}/pant-macos-arm64.tar.gz"
      sha256 "PLACEHOLDER"
    else
      url "https://github.com/subsetpark/pantagruel/releases/download/v#{version}/pant-macos-x86_64.tar.gz"
      sha256 "PLACEHOLDER"
    end
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v#{version}/pant-linux-x86_64.tar.gz"
    sha256 "PLACEHOLDER"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
