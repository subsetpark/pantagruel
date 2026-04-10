# Fuzzing Targets

AFL-based fuzzing for the Pantagruel pipeline. Requires `crowbar` and `afl-persistent`.

## Setup

```bash
opam install crowbar afl-persistent
```

Then uncomment the executables stanza in `fuzz/dune`:

```
(executables
 (names fuzz_lexer fuzz_parser fuzz_pipeline)
 (libraries pantagruel crowbar)
 (preprocess (pps sedlex.ppx))
 (ocamlopt_flags (:standard -afl-instrument)))
```

## Running

```bash
mkdir -p corpus findings
cp samples/*.pant corpus/
dune build fuzz/fuzz_lexer.exe
afl-fuzz -i corpus -o findings -- ./_build/default/fuzz/fuzz_lexer.exe @@
```

## Targets

- **fuzz_lexer.ml**: Feeds arbitrary bytes to the lexer. Only `Lexer_error` and `Sedlexing.MalFormed` are acceptable.
- **fuzz_parser.ml**: Feeds arbitrary bytes through lexer + parser.
- **fuzz_pipeline.ml**: Full pipeline: lexer -> parser -> collect -> check.

Any crashes found should be added as regression tests in the appropriate test file.
