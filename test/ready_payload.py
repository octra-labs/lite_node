# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#!/usr/bin/env python3

import argparse
import hashlib
import json
import time
import urllib.request


def fail(msg):
    raise SystemExit(msg)


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def hex64(label, value, lengths = (64,)):
    if value is None:
        return None
    s = value.strip().lower()
    if len(s) not in lengths or any(char not in "0123456789abcdef" for char in s):
        sizes = " or ".join(str(length) for length in lengths)
        fail(f"{label} must be {sizes} hex chars")
    return s


def proposal_id(value):
    if not isinstance(value, str) or len(value) != 64 or any(
        char not in "0123456789abcdef" for char in value
    ):
        fail("head_proposal_id must be 64 lowercase hex chars")
    return value


def rpc(url, method, params=None):
    body = json.dumps({
        "jsonrpc": "2.0",
        "id": int(time.time() * 1000) % 1_000_000,
        "method": method,
        "params": params or [],
    }, separators=(",", ":")).encode()
    req = urllib.request.Request(url, data=body, headers={"content-type": "application/json"})
    data = urllib.request.urlopen(req, timeout=5).read().decode()
    res = json.loads(data)
    if "error" in res:
        fail(json.dumps(res["error"], separators=(",", ":")))
    return res["result"]


def int_arg(label, value):
    try:
        n = int(value)
    except Exception:
        fail(f"{label} must be integer")
    if n < 0:
        fail(f"{label} must be non-negative")
    return n


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--rpc")
    p.add_argument("--fingerprint", required=True)
    p.add_argument("--head-epoch")
    p.add_argument("--head-proposal-id")
    p.add_argument("--state-root")
    p.add_argument("--chain-id")
    p.add_argument("--binary-hash")
    p.add_argument("--binary-path")
    p.add_argument("--config-hash")
    p.add_argument("--catchup-head-epoch")
    p.add_argument("--shadow-epochs")
    args = p.parse_args()
    stats = rpc(args.rpc, "octra_validatorEnrollment") if args.rpc else {}
    ready = stats.get("ready", {})
    if not isinstance(ready, dict):
        fail("committed readiness parameters unavailable")
    head_epoch = args.head_epoch or stats.get("head_epoch")
    state_root = args.state_root or stats.get("state_root")
    if head_epoch is None:
        fail("head_epoch missing")
    if state_root is None:
        fail("state_root missing")
    proposal = args.head_proposal_id
    if args.rpc:
        if int_arg("head_epoch", head_epoch) != int_arg("ready head", ready.get("head_epoch")):
            fail("head_epoch differs from committed readiness")
        if str(stats.get("head_epoch")) != str(ready.get("head_epoch")):
            fail("readiness head differs from committed enrollment")
        if state_root != ready.get("state_root"):
            fail("state_root differs from committed readiness")
        if "head_proposal_id" in ready:
            captured = proposal_id(ready["head_proposal_id"])
            if proposal is not None and proposal != captured:
                fail("head_proposal_id differs from committed readiness")
            proposal = captured
        elif proposal is not None:
            fail("committed head_proposal_id unavailable")
    binary_hash = args.binary_hash
    if binary_hash is None and args.binary_path is not None:
        binary_hash = sha256_file(args.binary_path)
    payload = {
        "fingerprint": hex64("fingerprint", args.fingerprint),
        "head_epoch": str(int_arg("head_epoch", head_epoch)),
        "state_root": hex64("state_root", state_root, lengths = (64, 128)),
    }
    if proposal is not None:
        payload["head_proposal_id"] = proposal_id(proposal)
    if args.chain_id is not None:
        if args.chain_id == "":
            fail("chain_id must be non-empty")
        payload["chain_id"] = args.chain_id
    if binary_hash is not None:
        payload["binary_hash"] = hex64("binary_hash", binary_hash)
    if args.config_hash is not None:
        payload["config_hash"] = hex64("config_hash", args.config_hash)
    if args.catchup_head_epoch is not None:
        payload["catchup_head_epoch"] = str(int_arg("catchup_head_epoch", args.catchup_head_epoch))
    if args.shadow_epochs is not None:
        payload["shadow_epochs"] = str(int_arg("shadow_epochs", args.shadow_epochs))
    print(json.dumps(payload, separators=(",", ":"), sort_keys=True))


if __name__ == "__main__":
    main()