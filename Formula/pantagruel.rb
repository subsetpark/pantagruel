class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  license "BSD-3-Clause"
  version "0.24.0"

  on_macos do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.24.0/pant-macos-arm64.tar.gz"
    sha256 "2e580cda715334604e3b071c2be38e6bbbf70ab704a94a2a597f734f32fab5f7"
  end

  on_linux do
    url "https://github.com/subsetpark/pantagruel/releases/download/v0.24.0/pant-linux-x86_64.tar.gz"
    sha256 "2ef13d67a597fd62f165a332e0dea37fe9ae206ec43f248af8c24501a0167221"
  end

  def install
    bin.install "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
