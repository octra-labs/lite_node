// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import fs from "node:fs";
import path from "node:path";
import {fileURLToPath} from "node:url";
import {spawnSync} from "node:child_process";

const root = fs.realpathSync(path.resolve(path.dirname(fileURLToPath(import.meta.url)), ".."));
const inside = (parent, value) => value.startsWith(parent + path.sep);
const requireValue = (value, reason) => {
  if (!value) throw new Error(reason);
};
const local = value => {
  const resolved = fs.realpathSync(path.resolve(value));
  requireValue(inside(root, resolved), "input is outside the node review directory");
  return resolved;
};

const config = () => {
  const entries = fs.readFileSync(path.join(root, "config", "network.env"), "utf8")
    .split(/\r?\n/).map(line => line.trim()).filter(line => line && !line.startsWith("#"))
    .map(line => {
      const match = line.match(/^([A-Z][A-Z0-9_]*)=(.*)$/);
      requireValue(match, "network environment line is invalid");
      const name = match[1];
      const value = match[2];
      const quote = value[0];
      if (quote === "\"" || quote === "'") {
        requireValue(value.length >= 2 && value.at(-1) === quote, "network value quote is incomplete");
        const body = value.slice(1, -1);
        requireValue(!body.includes(quote) && !body.includes("\\") && !body.includes("$")
          && !body.includes("`"), "network value needs unsupported shell evaluation");
        return [name, body];
      }
      requireValue(!/[\s\\$`'";]/.test(value), "network value needs unsupported shell evaluation");
      return [name, value];
    });
  requireValue(new Set(entries.map(([name]) => name)).size === entries.length,
    "network environment contains a duplicate name");
  return Object.fromEntries(entries);
};

try {
  requireValue(process.argv.length >= 9 && process.argv.length <= 264,
    "usage: replay_run.mjs BINARY WORKER DATA_COPY CERTIFICATE RANGE OUTPUT LABEL [RANGE ...]");
  const [binary, worker, data, certificate, range] = process.argv.slice(2, 7).map(local);
  const more = process.argv.slice(9).map(local);
  const output = path.resolve(process.argv[7]);
  const label = process.argv[8];
  const work = path.join(root, "runtime_data", "replay", "work");
  requireValue(inside(work, data), "replay data must be a disposable copy under replay/work");
  const outputDir = fs.realpathSync(path.dirname(output));
  requireValue(outputDir === work || inside(work, outputDir), "replay output is outside replay/work");
  requireValue(!fs.existsSync(output), "replay output already exists");
  requireValue(/^[a-z][a-z0-9-]{0,31}$/.test(label), "replay label is invalid");
  fs.accessSync(binary, fs.constants.X_OK);
  fs.accessSync(worker, fs.constants.X_OK);
  const env = {
    PATH: process.env.PATH || "/usr/bin:/bin",
    HOME: process.env.HOME,
    LANG: "C",
    DYLD_LIBRARY_PATH: path.join(root, "mcl", "lib"),
    TMPDIR: path.join(root, "runtime_data", "scratch"),
    ...config(),
    OCTRA_PVAC_VERIFY_WORKER: worker,
  };
  console.log("event = replay_run label = " + label + " mode = offline scope = ledger_execution");
  const result = spawnSync(binary, [data, certificate, range, output, ...more],
    {cwd: root, env, stdio: "inherit", timeout: 60 * 60 * 1000});
  if (result.error) throw result.error;
  requireValue(result.status === 0, "replay executable failed status = " + result.status);
} catch (error) {
  console.error("event = replay_run status = fail reason = " + error.message);
  process.exitCode = 1;
}