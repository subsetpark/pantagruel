import assert from "node:assert/strict";
import { describe, it } from "node:test";

import { createSourceFileFromSource } from "../src/extract.js";
import { buildDocumentFromSourceFile, emitAndCheck } from "./helpers.mjs";

describe("loop-break-continue", () => {
  async function translate(source: string): Promise<string> {
    const sourceFile = createSourceFileFromSource(source);
    return emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "update"),
    );
  }

  it("counter loop with break translates and bumps to fixed-point", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        for (let i = 0; i < n; i++) {
          a.balance = a.balance + 1;
          if (a.balance >= n) break;
        }
      }
    `);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("counter loop with continue translates and stays bounded-quantified", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        for (let i = 0; i < n; i++) {
          if (i === 0) continue;
          a.balance = i;
        }
      }
    `);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
    assert.doesNotMatch(output, /fn--loop/u);
  });

  it("bounded-while with break translates and bumps to fixed-point", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        let i = 0;
        while (i < n) {
          a.balance = a.balance + 1;
          if (a.balance >= n) break;
          i++;
        }
      }
    `);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("fixed-point while with break translates", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        while (a.balance < n) {
          a.balance = a.balance + 1;
          if (a.balance >= n) break;
        }
      }
    `);
    assert.match(output, /fn--loop/u);
  });

  it("fixed-point while with continue translates", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        while (a.balance < n) {
          if (n === 0) continue;
          a.balance = a.balance + 1;
        }
      }
    `);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("fixed-point while with bare return translates", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        while (a.balance < n) {
          a.balance = a.balance + 1;
          if (a.balance >= n) return;
        }
      }
    `);
    assert.doesNotMatch(output, /UNSUPPORTED/u);
  });

  it("fixed-point while with return value translates and emits return-value equation", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): number {
        while (a.balance < n) {
          a.balance = a.balance + 1;
          if (a.balance >= n) return a.balance;
        }
      }
    `);
    assert.match(output, /update'/u);
  });

  it("fixed-point while with throw translates and emits iteration precondition", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        while (a.balance < n) {
          if (n === 0) throw "bad";
          a.balance = a.balance + 1;
        }
      }
    `);
    assert.match(output, /~\(n = 0\)/u);
  });

  it("while(true)+break translates (event-loop idiom)", async () => {
    const output = await translate(`
      interface Account { balance: number; }
      function update(a: Account, n: number): void {
        while (true) {
          a.balance = a.balance + 1;
          if (a.balance >= n) break;
        }
      }
    `);
    assert.doesNotMatch(output, /literal-true guard/u);
  });

  it("labeled break rejects with M7 diagnostic", async () => {
    const sourceFile = createSourceFileFromSource(`
      interface Account {
        balance: number;
      }

      function update(a: Account): void {
        outer: for (let i = 0; i < 3; i++) {
          a.balance = i;
          break outer;
        }
      }
    `);

    const output = await emitAndCheck(
      await buildDocumentFromSourceFile(sourceFile, "update"),
    );

    assert.match(
      output,
      /labeled loop early-exit is M7 future work; remove the label or restructure/u,
    );
  });
});
