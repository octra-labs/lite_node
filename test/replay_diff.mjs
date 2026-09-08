// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import fs from "node:fs";

const requireValue = (value, reason) => {
  if (!value) throw new Error(reason);
};

const hash = value => typeof value === "string" && /^[0-9a-f]{64}$/.test(value);
const root = value => typeof value === "string" && /^(?:[0-9a-f]{64}|[0-9a-f]{128})$/.test(value);
const natural = value => typeof value === "string" && /^(?:0|[1-9][0-9]*)$/.test(value);
const strings = value => Array.isArray(value) && value.every(item => typeof item === "string");
const same = (left, right) => JSON.stringify(left) === JSON.stringify(right);
const fields = (value, keys) => {
  requireValue(value && typeof value === "object" && !Array.isArray(value), "record is not an object");
  requireValue(same(Object.keys(value).sort(), [...keys].sort()), "record fields differ");
};

const startKeys = ["event", "scope", "manifest", "range_sha256", "environment_sha256",
  "binary_sha256", "worker_sha256", "first_epoch"];
const epochKeys = ["event", "epoch", "ledger_root", "index_root", "state_root", "confirmed",
  "rejections", "fees", "candidate_root", "candidate_fees"];
const endKeys = ["event", "epochs", "next_epoch", "state_root"];

const read = path => {
  requireValue(fs.statSync(path).size <= 64 * 1024 * 1024, "trace exceeds size limit");
  const raw = fs.readFileSync(path, "utf8");
  requireValue(raw.endsWith("\n"), "trace has an incomplete last line");
  const records = raw.slice(0, -1).split("\n").map(line => JSON.parse(line));
  requireValue(records.length >= 3, "trace is incomplete");
  const first = records[0];
  const last = records.at(-1);
  const epochs = records.slice(1, -1);
  fields(first, startKeys);
  fields(last, endKeys);
  requireValue(first.event === "start" && first.scope === "ledger_execution", "trace scope differs");
  requireValue([first.manifest, first.range_sha256, first.environment_sha256,
    first.binary_sha256, first.worker_sha256].every(hash), "trace provenance is invalid");
  requireValue(natural(first.first_epoch), "first epoch is invalid");
  requireValue(last.event === "complete" && Number.isSafeInteger(last.epochs)
    && last.epochs === epochs.length && natural(last.next_epoch) && hash(last.state_root),
  "trace completion is invalid");
  epochs.forEach((entry, index) => {
    fields(entry, epochKeys);
    requireValue(entry.event === "epoch" && natural(entry.epoch)
      && BigInt(entry.epoch) === BigInt(first.first_epoch) + BigInt(index), "trace epoch gap");
    requireValue(root(entry.ledger_root) && root(entry.candidate_root)
      && hash(entry.index_root) && hash(entry.state_root), "trace root is invalid");
    requireValue(strings(entry.confirmed) && entry.confirmed.every(hash)
      && new Set(entry.confirmed).size === entry.confirmed.length, "confirmed set is invalid");
    requireValue(strings(entry.rejections) && natural(entry.fees) && natural(entry.candidate_fees),
      "trace outcome is invalid");
  });
  requireValue(BigInt(last.next_epoch) === BigInt(first.first_epoch) + BigInt(epochs.length)
    && last.state_root === epochs.at(-1).state_root, "trace final cursor differs");
  return {first, last, epochs};
};

const compare = (leftPath, rightPath, buildsPath) => {
  requireValue(fs.statSync(buildsPath).size <= 4096, "build record exceeds size limit");
  const builds = JSON.parse(fs.readFileSync(buildsPath, "utf8"));
  fields(builds, ["reference", "candidate"]);
  for (const build of [builds.reference, builds.candidate]) {
    fields(build, ["source", "binary_sha256", "worker_sha256"]);
    requireValue(typeof build.source === "string" && /^[0-9a-f]{40}$/.test(build.source)
      && hash(build.binary_sha256) && hash(build.worker_sha256), "build provenance is invalid");
  }
  requireValue(builds.reference.source === "c54167b827ede56b20d94608f8d3a9f5fa138c09",
    "reference source is not the published baseline");
  const left = read(leftPath);
  const right = read(rightPath);
  for (const [trace, build] of [[left, builds.reference], [right, builds.candidate]]) {
    requireValue(trace.first.binary_sha256 === build.binary_sha256
      && trace.first.worker_sha256 === build.worker_sha256, "executable differs from build record");
  }
  for (const field of ["scope", "manifest", "range_sha256", "environment_sha256", "first_epoch"]) {
    requireValue(left.first[field] === right.first[field], "input differs: " + field);
  }
  requireValue(left.first.binary_sha256 !== right.first.binary_sha256,
    "both traces name the same executable");
  requireValue(endKeys.every(field => same(left.last[field], right.last[field])), "final cursor differs");
  left.epochs.forEach((entry, index) => {
    for (const field of epochKeys) {
      requireValue(same(entry[field], right.epochs[index][field]),
        "epoch = " + entry.epoch + " field = " + field + " differs");
    }
  });
  const confirmed = left.epochs.reduce((count, entry) => count + entry.confirmed.length, 0);
  const rejections = left.epochs.reduce((count, entry) => count + entry.rejections.length, 0);
  requireValue(confirmed > 0, "trace has no confirmed transactions");
  return {epochs: left.epochs.length, confirmed, rejections};
};

try {
  requireValue(process.argv.length === 5, "usage: replay_diff.mjs REFERENCE CANDIDATE BUILDS");
  const result = compare(process.argv[2], process.argv[3], process.argv[4]);
  console.log("event = replay_diff status = pass epochs = " + result.epochs
    + " confirmed = " + result.confirmed + " rejections = " + result.rejections
    + " scope = ledger_execution");
} catch (error) {
  console.error("event = replay_diff status = fail reason = " + error.message);
  process.exitCode = 1;
}