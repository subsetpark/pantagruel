import { execSync } from "child_process";
import { writeFileSync, unlinkSync, rmSync, mkdtempSync, existsSync } from "fs";
import { tmpdir } from "os";
import { join, resolve } from "path";
import type { PantDocument } from "./types.js";

export interface CheckResult {
  passed: boolean;
  output: string;
  /** Individual check results parsed from pant --check output. */
  checks: CheckItem[];
}

export interface CheckItem {
  passed: boolean;
  message: string;
}

export interface CheckOptions {
  /** Working directory for dune exec. Defaults to process.cwd(). */
  projectRoot?: string;
}

/**
 * Emit a PantDocument as Pantagruel source text.
 */
export function emitDocument(doc: PantDocument): string {
  const lines: string[] = [];
  lines.push(`module ${doc.moduleName}.`);
  lines.push("");

  for (const decl of doc.declarations) {
    switch (decl.kind) {
      case "domain":
        lines.push(`${decl.name}.`);
        break;
      case "alias":
        lines.push(`${decl.name} = ${decl.type}.`);
        break;
      case "rule": {
        const params = decl.params.map((p) => `${p.name}: ${p.type}`).join(", ");
        const guard = decl.guard ? `, ${decl.guard}` : "";
        lines.push(`${decl.name} ${params}${guard} => ${decl.returnType}.`);
        break;
      }
      case "action": {
        if (decl.params.length === 0) {
          lines.push(`~> ${decl.label}.`);
        } else {
          const params = decl.params.map((p) => `${p.name}: ${p.type}`).join(", ");
          const guard = decl.guard ? `, ${decl.guard}` : "";
          lines.push(`~> ${decl.label} @ ${params}${guard}.`);
        }
        break;
      }
    }
  }

  lines.push("");
  lines.push("---");
  lines.push("");

  if (doc.propositions.length === 0) {
    lines.push("true.");
  } else {
    for (const prop of doc.propositions) {
      lines.push(`${prop.text}.`);
    }
  }

  lines.push("");
  return lines.join("\n");
}

/**
 * Write a .pant source string to a temp file, invoke `pant --check`,
 * and parse the output.
 */
export function runCheck(pantSource: string, opts?: CheckOptions): CheckResult {
  const cwd = opts?.projectRoot ?? findProjectRoot();
  const dir = mkdtempSync(join(tmpdir(), "ts2pant-"));
  const filePath = join(dir, "generated.pant");

  try {
    writeFileSync(filePath, pantSource);

    const output = execSync(`dune exec pant -- --check "${filePath}"`, {
      encoding: "utf-8",
      timeout: 60_000,
      cwd,
    });

    const checks = parseCheckOutput(output);
    const passed = checks.every((c) => c.passed);

    return { passed, output, checks };
  } catch (err: unknown) {
    if (isExecError(err)) {
      const stdout = err.stdout ?? "";
      const stderr = err.stderr ?? "";
      const output = stdout + stderr;
      // Exit code 2 = solver not found
      if (err.status === 2) {
        return {
          passed: false,
          output,
          checks: [{ passed: false, message: stderr.trim() || output.trim() }],
        };
      }
      // Exit code 1 = check failures
      const checks = parseCheckOutput(stdout);
      return { passed: false, output, checks };
    }
    throw err;
  } finally {
    try { unlinkSync(filePath); } catch { /* ignore */ }
    try { rmSync(dir, { recursive: true }); } catch { /* ignore */ }
  }
}

function parseCheckOutput(output: string): CheckItem[] {
  const items: CheckItem[] = [];
  for (const line of output.split("\n")) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    if (trimmed.startsWith("OK:")) {
      items.push({ passed: true, message: trimmed });
    } else if (trimmed.startsWith("FAIL:")) {
      items.push({ passed: false, message: trimmed });
    } else if (trimmed.startsWith("WARN:")) {
      items.push({ passed: true, message: trimmed });
    }
  }
  return items;
}

function findProjectRoot(): string {
  let dir = process.cwd();
  for (let i = 0; i < 10; i++) {
    if (existsSync(join(dir, "dune-project"))) return dir;
    const parent = resolve(dir, "..");
    if (parent === dir) break;
    dir = parent;
  }
  return process.cwd();
}

function isExecError(
  err: unknown,
): err is Error & { status: number; stdout: string; stderr: string } {
  return err instanceof Error && "status" in err;
}
