import { execFileSync, execSync } from "node:child_process";
import {
  existsSync,
  mkdtempSync,
  rmSync,
  unlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";
import process from "node:process";
import { getAst } from "./pant-wasm.js";
import type { PantDocument, PropResult } from "./types.js";

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
  /**
   * Path to a prebuilt `pant` binary. When set, invoked directly — avoids
   * the ~75ms/call overhead of `dune exec`. When unset, falls back to
   * `dune exec pant --` from {@link projectRoot}.
   */
  pantBin?: string;
}

function renderPropResult(prop: PropResult): string {
  const ast = getAst();
  switch (prop.kind) {
    case "equation": {
      const eq = ast.binop(ast.opEq(), prop.lhs, prop.rhs);
      const guards = prop.guards ?? [];
      if (prop.quantifiers.length === 0 && guards.length === 0) {
        return ast.strExpr(eq);
      }
      return ast.strExpr(ast.forall(prop.quantifiers, guards, eq));
    }
    case "assertion": {
      const guards = prop.guards ?? [];
      if (prop.quantifiers.length === 0 && guards.length === 0) {
        return ast.strExpr(prop.body);
      }
      return ast.strExpr(ast.forall(prop.quantifiers, guards, prop.body));
    }
    case "unsupported":
      return `> UNSUPPORTED: ${prop.reason}`;
    case "raw":
      return prop.text;
    default: {
      const _exhaustive: never = prop;
      throw new Error(`Unhandled prop kind: ${JSON.stringify(_exhaustive)}`);
    }
  }
}

/**
 * Emit a PantDocument as Pantagruel source text.
 */
export function emitDocument(doc: PantDocument): string {
  const lines: string[] = [];
  lines.push(`module ${doc.moduleName}.`);
  for (const imp of doc.imports) {
    lines.push(`import ${imp.name}.`);
  }
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
        const params = decl.params
          .map((p) => `${p.name}: ${p.type}`)
          .join(", ");
        const guard = decl.guard ? `, ${getAst().strExpr(decl.guard)}` : "";
        lines.push(`${decl.name} ${params}${guard} => ${decl.returnType}.`);
        break;
      }
      case "action": {
        if (decl.params.length === 0) {
          lines.push(`~> ${decl.label}.`);
        } else {
          const params = decl.params
            .map((p) => `${p.name}: ${p.type}`)
            .join(", ");
          const guard = decl.guard ? `, ${getAst().strExpr(decl.guard)}` : "";
          lines.push(`~> ${decl.label} @ ${params}${guard}.`);
        }
        break;
      }
      case "unsupported":
        lines.push(`> UNSUPPORTED: ${decl.reason}`);
        break;
      default: {
        const _exhaustive: never = decl;
        throw new Error(
          `Unhandled declaration kind: ${JSON.stringify(_exhaustive)}`,
        );
      }
    }
  }

  lines.push("");
  lines.push("---");
  lines.push("");

  if (doc.propositions.length === 0) {
    // Keep chapter body non-empty (required when a check block is present).
    lines.push("true.");
  } else {
    for (const prop of doc.propositions) {
      lines.push(`${renderPropResult(prop)}.`);
    }
  }

  if (doc.checks.length > 0) {
    lines.push("");
    lines.push("check");
    lines.push("");
    for (const chk of doc.checks) {
      lines.push(`${chk.text}.`);
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

    const output = opts?.pantBin
      ? execFileSync(opts.pantBin, ["--check", filePath], {
          encoding: "utf-8",
          timeout: 60_000,
          cwd,
        })
      : execSync(`dune exec pant -- --check "${filePath}"`, {
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
    try {
      unlinkSync(filePath);
    } catch {
      /* ignore */
    }
    try {
      rmSync(dir, { recursive: true });
    } catch {
      /* ignore */
    }
  }
}

function parseCheckOutput(output: string): CheckItem[] {
  const items: CheckItem[] = [];
  for (const line of output.split("\n")) {
    const trimmed = line.trim();
    if (!trimmed) {
      continue;
    }
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
    if (existsSync(join(dir, "dune-project"))) {
      return dir;
    }
    const parent = resolve(dir, "..");
    if (parent === dir) {
      break;
    }
    dir = parent;
  }
  return process.cwd();
}

function isExecError(
  err: unknown,
): err is Error & { status: number; stdout: string; stderr: string } {
  return err instanceof Error && "status" in err;
}
