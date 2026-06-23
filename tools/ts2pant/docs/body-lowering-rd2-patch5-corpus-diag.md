# Body Lowering RD2 Patch 5 Corpus Diagnostic

Generated on 2026-06-23 with:

```sh
cd tools/ts2pant && npx tsx scripts/corpus-diag.mts --summary-only
```

Prompt baseline:

- `functions: 806`
- `clean: 163`
- record-return-with-branches bucket: `8`

Patch 5 result:

- `functions: 823`
- `clean: 163`
- `unsupported: 655`
- `errors: 5`

Relevant bucket movement:

- `record return combined with early-return arms or if/else branches is not yet supported`: `2`
- `record return branches must all return object literals with the same field set`: `4`
- Additional record-shape rejects remain explicit, including spread/accessor and extra-field cases.

