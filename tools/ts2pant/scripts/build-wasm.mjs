import { execFileSync } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(__dirname, "../../..");

execFileSync("dune", ["build", "wasm/"], {
  cwd: repoRoot,
  env: { ...process.env, BUILD_WASM: "true" },
  stdio: "inherit",
});

const outDir = path.join(repoRoot, "tools/ts2pant/src/wasm");
fs.rmSync(outDir, { recursive: true, force: true });
fs.mkdirSync(outDir, { recursive: true });

const jsSrc = path.join(repoRoot, "_build/default/wasm/pant_wasm.bc.wasm.js");
const jsDst = path.join(outDir, "pant_wasm.bc.wasm.js");

// Read from dune's read-only _build output, patch in memory, write to dest
const text = fs
  .readFileSync(jsSrc, "utf8")
  .replace(/require\.main\.filename/g, "__filename");
fs.writeFileSync(jsDst, text);

fs.cpSync(
  path.join(repoRoot, "_build/default/wasm/pant_wasm.bc.wasm.assets"),
  path.join(outDir, "pant_wasm.bc.wasm.assets"),
  { recursive: true },
);
