class Pantagruel < Formula
  desc "A specification language checker"
  homepage "https://github.com/subsetpark/pantagruel"
  url "https://github.com/subsetpark/pantagruel/archive/refs/tags/v0.20.1.tar.gz"
  sha256 "c7dc5af09f3c4eb2d0c4c1c405e487304c425545ed04eb1420215fc628914154"
  license "BSD-3-Clause"

  depends_on "ocaml" => :build
  depends_on "opam" => :build

  def install
    # Initialize opam if needed (no-setup avoids shell config changes)
    system "opam", "init", "--no-setup", "--disable-sandboxing", "--bare" unless Dir.exist?(File.join(Dir.home, ".opam"))
    # Create a local switch to isolate dependencies
    system "opam", "switch", "create", ".", "ocaml-base-compiler.5.2.1", "--no-install", "--yes" unless File.exist?("_opam")
    # Install dependencies
    system "opam", "exec", "--", "opam", "install", ".", "--deps-only", "--yes"
    # Build
    system "opam", "exec", "--", "dune", "build", "-p", "pantagruel"
    bin.install "_build/default/bin/main.exe" => "pant"
  end

  test do
    (testpath/"test.pant").write("module Test.\n\nFoo.\n")
    system bin/"pant", testpath/"test.pant"
  end
end
