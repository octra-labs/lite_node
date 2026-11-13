# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import os
import hashlib
import json
import selectors
import shutil
import signal
import subprocess
import sys
import time
import uuid
from pathlib import Path

def check(binary, chain, mode, flags, root):
    assert not root.exists()
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("OCTRA_")}
    env.update({
        "OCTRA_DATA_DIR": str(root / "data"),
        "OCTRA_PVAC_VERIFY_WORKER": str(root / "worker"),
        "OCTRA_CONSENSUS_PORT": "0",
    })
    if chain is not None:
        env["OCTRA_CHAIN_ID"] = chain
    if mode is not None:
        env["OCTRA_CONSENSUS_MODE"] = mode
    result = subprocess.run(
        [binary, *flags], env = env, capture_output = True,
        text = True, timeout = 15,
    )
    output = result.stdout + result.stderr
    denied = "event = rule_plan status = rejected reason = unsupported_chain"
    assert result.returncode == 1, output
    assert not root.exists(), "startup wrote before validation"
    if chain == "octra-devnet-9871-cluster":
        assert denied not in output, output
        assert "event = pvac_worker status = rejected" in output, output
    else:
        assert denied in output, (chain, mode, flags, output)
        assert "event = pvac_worker" not in output, output

def anchor_case(binary, worker, prepare, mode, offset, work):
    root = work / ".keys" / f"{mode}_{offset}"
    root.mkdir(parents = True, mode = 0o700)
    setup = subprocess.run([prepare, "--rule-store", str(root), mode, str(offset)],
        capture_output = True, text = True, timeout = 60)
    assert setup.returncode == 0, setup.stdout + setup.stderr
    sample = json.loads(setup.stdout.splitlines()[-1])
    assert sample["activation"] == 1_299_000 and sample["anchor_epoch"] == 1_293_376
    assert sample["epoch"] == 1_298_999 if mode == "before" else sample["epoch"] == 1_299_000 + offset
    paths = [root / "HEAD.json", root / "commit_journal.log", root / "chaindata/epochlog/epochs.dat"]
    before = {path: path.read_bytes() if path.exists() else None for path in paths}
    cwd = work / "process" / f"{mode}_{offset}"
    cwd.mkdir(parents = True)
    (cwd / "nodes.config").write_text("")
    env = {key: value for key, value in os.environ.items() if not key.startswith("OCTRA_")}
    env.update({"OCTRA_CHAIN_ID": "octra-devnet-9871-cluster", "OCTRA_DATA_DIR": str(root),
        "OCTRA_PVAC_VERIFY_WORKER": worker,
        "OCTRA_PVAC_VERIFY_WORKER_HASH": hashlib.sha256(Path(worker).read_bytes()).hexdigest(),
        "OCTRA_PVAC_VERIFY_MAX_RSS_MB": "512", "OCTRA_CONSENSUS_MODE": "observer",
        "OCTRA_CONSENSUS_PORT": "0", "OCTRA_API_PORT": "0", "OCTRA_P2P_PORT": "0",
        "OCTRA_PEERS": "", "OCTRA_CONSENSUS_ALLOW_DYNAMIC_PEERS": "0",
        "OCTRA_VALIDATORS": sample["validators"], "OCTRA_CONSENSUS_CONFIG_HASH": sample["config_hash"]})
    process = subprocess.Popen([binary, "--observer"], cwd = cwd, env = env, stdout = subprocess.PIPE,
        stderr = subprocess.STDOUT, start_new_session = True)
    output = bytearray()
    completed = False
    try:
        with selectors.DefaultSelector() as selector:
            selector.register(process.stdout, selectors.EVENT_READ)
            deadline = time.monotonic() + 30
            while time.monotonic() < deadline:
                for key, _ in selector.select(0.1):
                    chunk = os.read(key.fd, 65536)
                    if chunk:
                        output.extend(chunk)
                if b"event = rule_graph rule = seatless status = awaiting_anchor" in output:
                    completed = True
                    break
                if b"node version = " in output or b"event = profile_switch" in output:
                    break
                if process.poll() is not None:
                    break
    finally:
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            rest, _ = process.communicate(timeout = 5)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            rest, _ = process.communicate(timeout = 5)
        output.extend(rest)
    text = output.decode(errors = "replace")
    assert "event = pvac_worker status = ready" in text, text
    assert "event = circle_worker status = ready" in text, text
    assert f"starting epoch = {sample['epoch']}" in text, text
    if mode == "before":
        assert completed, text
        assert "event = rule_graph rule = circle status = bound" in text, text
        assert "event = rule_graph rule = circle status = rejected" not in text, text
    else:
        reason = f"rule anchor missing epoch = {sample['anchor_epoch']}" if mode == "missing" else (
            f"rule anchor mismatch epoch = {sample['anchor_epoch']} expected = {sample['expected']} actual = {sample['actual']}")
        assert f"event = rule_graph rule = circle status = rejected reason = {reason}" in text, (
            mode, sample["epoch"], "expected anchor refusal missing", text)
        assert process.returncode == 1, text
        assert "event = profile_switch" not in text, text
        assert "event = epoch_processing" not in text, text
    assert before == {path: path.read_bytes() if path.exists() else None for path in paths}, text
    print(f"status = pass test = rule_anchor mode = {mode} epoch = {sample['epoch']}")

def main():
    assert len(sys.argv) == 4, "node, worker and store preparer required"
    binary = str(Path(sys.argv[1]).resolve())
    worker = str(Path(sys.argv[2]).resolve())
    prepare = str(Path(sys.argv[3]).resolve())
    assert all(Path(path).is_file() and os.access(path, os.X_OK) for path in [binary, worker, prepare])
    chains = [
        None, "", "octra-mainnet", "octra-test", "octra-test-4node",
        "octra-devnet", "OCTRA-DEVNET-9871-CLUSTER",
        "octra-devnet-9871-cluster ", "octra-devnet-9871-cluster\n",
        "octra-devnet-9871-cluster",
    ]
    roles = [
        (None, []), ("bft", []), ("observer", []),
        ("legacy", []), ("follower", []), (None, ["--observer"]),
        (None, ["--state-sync-publisher"]), (None, ["--init"]),
    ]
    work = Path.cwd() / "runtime_data" / "rule_start" / uuid.uuid4().hex
    try:
        for chain in chains:
            for mode, flags in roles:
                check(binary, chain, mode, flags, work)
        for mode, offset in [("missing", 0), ("wrong", 0), ("missing", 1), ("wrong", 1), ("before", 0)]:
            anchor_case(binary, worker, prepare, mode, offset, work)
    finally:
        if work.exists():
            shutil.rmtree(work)
    print(f"status = pass gate = rule_start cases = {len(chains) * len(roles) + 5}")

if __name__ == "__main__":
    main()