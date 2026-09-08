// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import fs from "node:fs";
import path from "node:path";
import {fileURLToPath} from "node:url";
import {spawnSync} from "node:child_process";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const work = path.join(root, "runtime_data", "replay-diff-" + process.pid);
const digest = value => value.repeat(64);
const fixture = binary => [
  {event: "start", scope: "ledger_execution", manifest: digest("a"),
    range_sha256: digest("b"), environment_sha256: digest("c"),
    binary_sha256: digest(binary), worker_sha256: digest(binary), first_epoch: "1"},
  {event: "epoch", epoch: "1", ledger_root: digest("a"), index_root: digest("b"),
    state_root: digest("c"), confirmed: [digest("d")], rejections: ["first", "second"],
    fees: "1", candidate_root: digest("e"), candidate_fees: "2"},
  {event: "epoch", epoch: "2", ledger_root: digest("a"), index_root: digest("b"),
    state_root: digest("d"), confirmed: [], rejections: [], fees: "0",
    candidate_root: digest("e"), candidate_fees: "0"},
  {event: "complete", epochs: 2, next_epoch: "3", state_root: digest("d")},
];
const encode = records => records.map(record => JSON.stringify(record)).join("\n") + "\n";
const builds = {
  reference: {source: "c54167b827ede56b20d94608f8d3a9f5fa138c09",
    binary_sha256: digest("e"), worker_sha256: digest("e")},
  candidate: {source: "1".repeat(40), binary_sha256: digest("f"), worker_sha256: digest("f")},
};
const cases = [
  ["matching", records => records, true],
  ["completion key order", records => {
    records[3] = Object.fromEntries(Object.entries(records[3]).reverse()); return records;
  }, true],
  ["wrong worker", records => { records[0].worker_sha256 = digest("d"); return records; }, false],
  ["same binary", records => { records[0].binary_sha256 = digest("e"); return records; }, false],
  ["environment", records => { records[0].environment_sha256 = digest("f"); return records; }, false],
  ["manifest", records => { records[0].manifest = digest("f"); return records; }, false],
  ["range", records => { records[0].range_sha256 = digest("f"); return records; }, false],
  ["scope", records => { records[0].scope = "fixture"; return records; }, false],
  ["provenance", records => { delete records[0].worker_sha256; return records; }, false],
  ["gap", records => { records[2].epoch = "3"; return records; }, false],
  ["count", records => { records[3].epochs = 3; return records; }, false],
  ["cursor", records => { records[3].next_epoch = "4"; return records; }, false],
  ["final root", records => { records[3].state_root = digest("f"); return records; }, false],
  ["ledger", records => { records[1].ledger_root = digest("f"); return records; }, false],
  ["index", records => { records[1].index_root = digest("f"); return records; }, false],
  ["candidate root", records => { records[1].candidate_root = digest("f"); return records; }, false],
  ["fee", records => { records[1].fees = "2"; return records; }, false],
  ["candidate fee", records => { records[1].candidate_fees = "3"; return records; }, false],
  ["negative fee", records => { records[1].fees = "-1"; return records; }, false],
  ["fee encoding", records => { records[1].fees = "01"; return records; }, false],
  ["duplicate", records => { records[1].confirmed.push(digest("d")); return records; }, false],
  ["rejection order", records => { records[1].rejections.reverse(); return records; }, false],
  ["truncated", records => records.slice(0, -1), false],
  ["incomplete line", records => encode(records).slice(0, -1), false],
];

fs.mkdirSync(work, {recursive: true});
try {
  const left = path.join(work, "reference.jsonl");
  const right = path.join(work, "candidate.jsonl");
  const pins = path.join(work, "builds.json");
  fs.writeFileSync(pins, JSON.stringify(builds));
  fs.writeFileSync(left, encode(fixture("e")));
  for (const [name, change, expected] of cases) {
    const changed = change(fixture("f"));
    fs.writeFileSync(right, typeof changed === "string" ? changed : encode(changed));
    const run = spawnSync(process.execPath, [path.join(root, "test", "replay_diff.mjs"), left, right, pins],
      {encoding: "utf8", timeout: 5000});
    if (run.error || (run.status === 0) !== expected) {
      throw new Error("case = " + name + " status = " + run.status);
    }
  }
  const withoutConfirmed = fixture("e");
  withoutConfirmed[1].confirmed = [];
  fs.writeFileSync(left, encode(withoutConfirmed));
  const matchingEmpty = fixture("f");
  matchingEmpty[1].confirmed = [];
  fs.writeFileSync(right, encode(matchingEmpty));
  const empty = spawnSync(process.execPath,
    [path.join(root, "test", "replay_diff.mjs"), left, right, pins],
    {encoding: "utf8", timeout: 5000});
  if (empty.error || empty.status === 0
    || !empty.stderr.includes("trace has no confirmed transactions")) {
    throw new Error("empty confirmed trace accepted");
  }
  fs.writeFileSync(left, encode(fixture("e")));
  fs.writeFileSync(right, encode(fixture("f")));
  const wrong = {...builds, reference: {...builds.reference, source: "2".repeat(40)}};
  fs.writeFileSync(pins, JSON.stringify(wrong));
  const rejected = spawnSync(process.execPath, [path.join(root, "test", "replay_diff.mjs"), left, right, pins],
    {encoding: "utf8", timeout: 5000});
  if (rejected.error || rejected.status === 0) throw new Error("wrong reference source accepted");
  console.log("event = replay_diff_checks status = pass cases = " + (cases.length + 2)
    + " execution = fixture");
} finally {
  fs.rmSync(work, {recursive: true});
}