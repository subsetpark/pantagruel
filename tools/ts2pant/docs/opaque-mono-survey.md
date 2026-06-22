# Opaque Monomorphization Survey

**Workstream:** ts2pant `any`/`unknown` handling via Opaque sort — Milestone 5 (`ts2pant-opaque-mono-survey`). **Date:** 2026-06-22. **Method:** read-only whole-program pass over ts2pant `src/` (its own corpus); no translation output is modified.

## Summary

- Files surveyed: **42**
- Top-level function declarations: **770**
- Monomorphization candidates (>=1 top-level `any`/`unknown` parameter): **1**
- Functions with a top-level `any`/`unknown` return (context): **1**

### Verdict distribution

| Verdict | Count |
| --- | --- |
| all-agree | 0 |
| disagree-but-bounded | 0 |
| disagree-unbounded | 1 |
| recursive | 0 |
| oversized | 0 |
| no-visible-call-sites | 0 |

### M6 decision metric

- all-agree fraction (of all candidates): **0.0%**
- all-agree fraction (of candidates with >=1 visible call site): **0.0%**
- Threshold to proceed to M6 (`ts2pant-opaque-mono`): **>= 25.0% all-agree**.

### Verdict

**Do not proceed to M6 on this corpus.** 0.0% of candidates are all-agree, below the 25.0% threshold.

- The dogfood corpus is a strict, self-hosted TypeScript codebase that avoids top-level `any`/`unknown` by construction, so it is **not representative** of the `any`/`unknown`-heavy user code M6 targets (third-party APIs, JSON, gradual code). A near-zero candidate count here reflects the corpus, not the value of monomorphization for real inputs.
- Treat this run as evidence that the dogfood corpus alone cannot justify M6. A go decision needs a re-survey over representative `any`/`unknown`-heavy user TypeScript; absent that, the workstream legitimately closes at M5 (the M5 Definition of Done permits this).

## Per-function detail

| Function | Dynamic params | Call sites | Verdict | Exported | Body LOC |
| --- | --- | --- | --- | --- | --- |
| `emit.ts:isExecError` (L274) | err | 1 | disagree-unbounded | no | 3 |

- `emit.ts:isExecError` — observed: err: {unknown}
