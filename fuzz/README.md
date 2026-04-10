# Fuzzing Targets

Coverage-guided fuzzing for the Pantagruel pipeline using [Crowbar](https://github.com/stedolan/crowbar) and [AFL](https://lcamtuf.coredump.cx/afl/).

## Targets

- **fuzz_lexer.ml**: Feeds arbitrary bytes to the lexer. Only `Lexer_error` and `Sedlexing.MalFormed` are acceptable.
- **fuzz_parser.ml**: Feeds arbitrary bytes through lexer + parser. Any parse error is acceptable; only unhandled crashes indicate bugs.
- **fuzz_pipeline.ml**: Full pipeline: lexer -> parser -> collect -> check. Exercises type resolution and checking on malformed but parseable inputs.

## QuickCheck Mode (no setup required)

Without AFL instrumentation, Crowbar runs in QuickCheck mode — generates random inputs and checks for crashes. This runs in CI automatically.

```bash
opam install crowbar
opam exec -- dune exec fuzz/fuzz_lexer.exe
opam exec -- dune exec fuzz/fuzz_parser.exe
opam exec -- dune exec fuzz/fuzz_pipeline.exe
```

## AFL Mode (coverage-guided, recommended for thorough testing)

AFL instruments the binary to track code coverage and mutates inputs to explore new paths. This finds edge cases that random testing misses.

### 1. Create an AFL-instrumented switch

```bash
opam switch create 5.3.0+afl ocaml-variants.5.3.0+options ocaml-option-afl
eval $(opam env)
opam install . --deps-only
opam install crowbar
```

### 2. Install AFL

```bash
# macOS
brew install afl-fuzz

# Ubuntu/Debian
sudo apt install afl++
```

### 3. Build and run

```bash
# Seed the corpus with sample files
mkdir -p corpus findings
cp samples/*.pant corpus/

# Build with AFL instrumentation (automatic in the +afl switch)
opam exec -- dune build fuzz/fuzz_lexer.exe

# Run AFL (Ctrl-C to stop; runs indefinitely)
afl-fuzz -i corpus -o findings -- ./_build/default/fuzz/fuzz_lexer.exe @@
```

Replace `fuzz_lexer` with `fuzz_parser` or `fuzz_pipeline` to fuzz other targets.

### 4. Triage crashes

AFL saves crashing inputs in `findings/crashes/`. To reproduce:

```bash
opam exec -- dune exec fuzz/fuzz_lexer.exe < findings/crashes/id:000000,*
```

Any crashes found should be added as regression tests in the appropriate test file.

## Tips

- Run AFL for at least 10-30 minutes per target for meaningful coverage.
- The pipeline fuzzer is the highest-value target — it exercises the most code.
- Multiple AFL instances can run in parallel with `-M`/`-S` flags.
