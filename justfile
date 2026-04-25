# Pantagruel — multi-language workspace task runner.
#
# Layout:
#   - OCaml core: lib/, bin/, test/ — built with dune
#   - WASM bridge: wasm/ — built with dune + wasm_of_ocaml, gated by BUILD_WASM
#   - ts2pant: tools/ts2pant/ — TypeScript translator, embeds the WASM bridge
#
# Why just?
# Cross-language dependency ordering used to be implicit: `tests/helpers.mts`
# would lazily call `dune build` from inside every Node test worker, and a
# SIGKILLed worker would poison `_build/.lock` for every subsequent run. The
# fix is to make the dep graph explicit and bounded — `ts2pant-test-integration`
# declaring a `build-pant` dependency means the pant binary is always built
# once, sequentially, before any test worker spawns. Lock-management and
# orchestration live in this justfile, not in any per-language script: the JS
# side just builds and tests, dune just compiles, and just chains them.
#
# Install: `brew install just` (macOS) or `cargo install just` (cross-platform).
# CI installs it via `extractions/setup-just` — see .github/workflows/ci.yml.

# Show available recipes
default:
    @just --list

# Full OCaml build (everything dune knows about, modulo BUILD_WASM)
build:
    eval $(opam env) && dune build

# Build just the pant binary — the only OCaml artifact ts2pant integration tests need
build-pant:
    #!/usr/bin/env bash
    set -euo pipefail
    # Stale-lock recovery: dune holds an OS-level flock on `_build/.lock`
    # while building. If a prior build was SIGKILLed, the file may persist
    # with no live holder and every subsequent `dune build` blocks on
    # `flock()` indefinitely — exactly the failure mode that hung the
    # pre-commit hook for 15+ hours. Heuristic: if the file exists, no
    # `dune` process is running, and the file is older than 1 minute
    # (so it isn't a freshly-released lock from a successful sibling
    # build), treat it as orphaned and remove it. Dune will recreate
    # the lock cleanly on the next build.
    LOCK="_build/.lock"
    if [[ -f "$LOCK" ]] && ! pgrep -x dune > /dev/null && [[ -n "$(find "$LOCK" -mmin +1 2>/dev/null)" ]]; then
        echo "build-pant: removing stale $LOCK (no live dune; older than 1 min)" >&2
        rm -f "$LOCK"
    fi
    eval $(opam env)
    dune build bin/main.exe

# Build the WASM bridge target via dune (BUILD_WASM-gated)
build-wasm:
    eval $(opam env) && BUILD_WASM=true dune build wasm/

# Run OCaml tests (alcotest)
test-ocaml:
    eval $(opam env) && dune test

# Install ts2pant deps from the lockfile (CI-shaped)
ts2pant-install:
    cd tools/ts2pant && npm ci

# Patch + copy the WASM bridge into tools/ts2pant/src/wasm/
ts2pant-build-wasm:
    cd tools/ts2pant && npm run build:wasm

# Compile ts2pant TypeScript (depends on the WASM bridge being in place)
ts2pant-build: ts2pant-build-wasm
    cd tools/ts2pant && npx tsc

# Lint ts2pant (auto-fix-friendly)
ts2pant-lint:
    cd tools/ts2pant && npx biome check src

# Lint ts2pant in CI mode (exits non-zero on findings, no auto-fix)
ts2pant-lint-ci:
    cd tools/ts2pant && npx biome ci src

# Type-check ts2pant without emitting
ts2pant-typecheck:
    cd tools/ts2pant && npx tsc --noEmit

# Run ts2pant unit tests (no pant binary needed)
ts2pant-test-unit:
    cd tools/ts2pant && npm run test:unit

# Run ts2pant integration tests (e2e + dogfood); builds pant first
ts2pant-test-integration: build-pant
    cd tools/ts2pant && npm run test:integration

# Run all ts2pant tests (unit then integration)
ts2pant-test: ts2pant-test-unit ts2pant-test-integration

# Update snapshot expectations for ts2pant tests; rebuilds pant first
ts2pant-test-update-snapshots: build-pant
    cd tools/ts2pant && npm run test:update-snapshots

# Run what the pre-commit hook runs for ts2pant
ts2pant-precommit: ts2pant-lint-ci ts2pant-typecheck ts2pant-test

# Run every test in the workspace
test: test-ocaml ts2pant-test

# Remove every build artifact (dune _build, ts2pant dist + WASM bridge)
clean:
    eval $(opam env) && dune clean
    cd tools/ts2pant && rm -rf dist src/wasm
