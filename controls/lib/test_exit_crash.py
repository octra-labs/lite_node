# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import os
import shutil
import signal
import stat
import subprocess
import sys
import unittest
import uuid
from pathlib import Path
from unittest import mock

import validator_common as common
import validator_exit as control

WALLET = {"address": "octTest", "pub": "test-key"}

def settings(root):
    return {
        "OCTRA_DATA_DIR": str(root / "data"),
        "OCTRA_CHAIN_ID": "octra-test",
        "OCTRA_OPERATOR_RPC_URL": "http://local-test/rpc",
    }

def append(path, value):
    with path.open("a", encoding = "utf-8") as stream:
        stream.write(json.dumps(value) + "\n")
        stream.flush()
        os.fsync(stream.fileno())

def run_child(root, cut, renew, operation):
    values = settings(root)
    amount = 1000000 if operation == "validator_bond" else 0
    current = root / "data" / "validator-control" / f"{operation}-1.json"
    replace = os.replace
    sync = os.fsync
    dump = json.dump

    def stop(point):
        if point == cut:
            os.kill(os.getpid(), signal.SIGKILL)

    def save(value, stream, *args, **kwargs):
        if cut == "partial" and isinstance(value, dict) and "tx" in value:
            stream.write("{")
            stream.flush()
            sync(stream.fileno())
            stop("partial")
        return dump(value, stream, *args, **kwargs)

    def rename(source, target):
        replace(source, target)
        name = Path(target).name
        if name == current.name:
            stop("outbox")
        elif name == "last.json" and current.exists():
            value = json.loads(Path(target).read_text())
            if value.get("tx_hash") == ("d" if renew else "b") * 64:
                stop("last")
        elif name.startswith(f"{operation}-1-"):
            stop("archive")

    def flush(fd):
        sync(fd)
        if stat.S_ISDIR(os.fstat(fd).st_mode) and current.exists():
            stop("directory")

    def prepare(prior = None):
        append(root / "signed.jsonl", "renew" if renew else "first")
        return {
            "tx_hash": ("d" if renew else "b") * 64,
            "tx": {
                "from": WALLET["address"],
                "to_": control.BOND_ESCROW if amount else WALLET["address"],
                "public_key": WALLET["pub"], "amount": str(amount), "ou": "1000",
                "op_type": operation, "nonce": 5,
                "timestamp": 2.0 if renew else 1.0,
                "signature": "renewed" if renew else "first",
            },
        }

    def rpc(url, method, params, **kwargs):
        if method == "octra_balance":
            return {"nonce": 4}
        if method != "octra_submit":
            raise AssertionError(method)
        saved = control.read_outbox(current)
        if saved["tx"] != params[0]:
            raise AssertionError("submission differs from saved bytes")
        append(root / "sent.jsonl", params[0])
        stop("sent")
        return {"status": "accepted", "tx_hash": saved["tx_hash"]}

    with control.command_lock(values), mock.patch.object(
        common.json, "dump", side_effect = save,
    ), mock.patch.object(common.os, "replace", side_effect = rename), mock.patch.object(
        common.os, "fsync", side_effect = flush,
    ), mock.patch.object(control, "transaction", return_value = None), mock.patch.object(
        control, "call", side_effect = rpc,
    ):
        if operation == "validator_bond":
            control.begin_bond(values, WALLET, 1)
            stop("intent")
        control.submit(values, WALLET, operation, 1, "http://local-test/rpc",
                       prepare, renew = renew, amount = amount)

class ExitCrashTest(unittest.TestCase):
    def setUp(self):
        self.root = Path(__file__).resolve().parent / "runtime_data" / "exit_crash" / uuid.uuid4().hex
        self.root.mkdir(parents = True)
        self.addCleanup(shutil.rmtree, self.root)

    def child(self, root, cut = "none", renew = False, operation = "validator_withdraw"):
        (root / "data").mkdir(parents = True, exist_ok = True)
        return subprocess.run(
            [sys.executable, "-B", str(Path(__file__).resolve()), str(root), cut, str(int(renew)), operation],
            capture_output = True, text = True, timeout = 10,
        )

    def records(self, root, name):
        path = root / name
        return [json.loads(line) for line in path.read_text().splitlines()] if path.exists() else []

    def accepted(self, result):
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_submit_cuts(self):
        for cut in ("partial", "outbox", "directory", "last", "sent"):
            with self.subTest(cut = cut):
                root = self.root / cut
                result = self.child(root, cut)
                self.assertEqual(result.returncode, -signal.SIGKILL, result.stderr)
                self.accepted(self.child(root))
                signed = self.records(root, "signed.jsonl")
                sent = self.records(root, "sent.jsonl")
                self.assertEqual(len(signed), 2 if cut == "partial" else 1)
                self.assertEqual(len(sent), 2 if cut == "sent" else 1)
                self.assertTrue(all(tx == sent[0] for tx in sent))
                path = control.outbox_path(settings(root), "validator_withdraw", 1)
                saved = control.read_outbox(path)
                self.assertEqual(sent[0], saved["tx"])
                self.assertEqual(control.read_outbox(path.parent / "last.json")["tx_hash"],
                                 saved["tx_hash"])

    def test_bond_cuts(self):
        for cut in ("intent", "partial", "outbox", "directory", "last", "sent"):
            with self.subTest(cut = cut):
                root = self.root / cut
                result = self.child(root, cut, operation = "validator_bond")
                self.assertEqual(result.returncode, -signal.SIGKILL, result.stderr)
                self.accepted(self.child(root, operation = "validator_bond"))
                signed = self.records(root, "signed.jsonl")
                sent = self.records(root, "sent.jsonl")
                self.assertEqual(len(signed), 2 if cut == "partial" else 1)
                self.assertEqual(len(sent), 2 if cut == "sent" else 1)
                self.assertTrue(all(tx == sent[0] for tx in sent))
                path = control.outbox_path(settings(root), "validator_bond", 1)
                saved = control.read_outbox(path)
                self.assertEqual(sent[0], saved["tx"])
                self.assertEqual(saved["head_epoch"], 1)
                latest = control.read_outbox(path.parent / "last.json")
                self.assertEqual(latest["tx_hash"], saved["tx_hash"])
                self.assertIsNone(latest["bonded_epoch"])

    def test_bond_renew_cuts(self):
        for cut in ("archive", "outbox", "last", "sent"):
            with self.subTest(cut = cut):
                root = self.root / cut
                self.accepted(self.child(root, operation = "validator_bond"))
                result = self.child(root, cut, renew = True, operation = "validator_bond")
                self.assertEqual(result.returncode, -signal.SIGKILL, result.stderr)
                self.accepted(self.child(root, operation = "validator_bond"))
                self.assertEqual(len(self.records(root, "signed.jsonl")), 2)
                path = control.outbox_path(settings(root), "validator_bond", 1)
                attempts = control.saved_attempts(path, settings(root), WALLET, "validator_bond", 1)
                self.assertEqual(len(attempts), 1 if cut == "archive" else 2)
                sent = self.records(root, "sent.jsonl")
                self.assertEqual(sent[-1], control.read_outbox(path)["tx"])
                self.assertTrue(all(tx["nonce"] == 5 and tx["ou"] == "1000"
                                    and tx["amount"] == "1000000" for tx in sent))

    def test_renew_cuts(self):
        for cut in ("archive", "outbox", "last", "sent"):
            with self.subTest(cut = cut):
                root = self.root / cut
                self.accepted(self.child(root))
                result = self.child(root, cut, renew = True)
                self.assertEqual(result.returncode, -signal.SIGKILL, result.stderr)
                self.accepted(self.child(root))
                self.assertEqual(len(self.records(root, "signed.jsonl")), 2)
                path = control.outbox_path(settings(root), "validator_withdraw", 1)
                attempts = control.saved_attempts(path, settings(root), WALLET, "validator_withdraw", 1)
                self.assertEqual(len(attempts), 1 if cut == "archive" else 2)
                self.assertIn("b" * 64, [saved["tx_hash"] for saved in attempts])
                sent = self.records(root, "sent.jsonl")
                self.assertEqual(sent[-1], control.read_outbox(path)["tx"])
                self.assertTrue(all(tx["nonce"] == 5 and tx["ou"] == "1000" for tx in sent))
                receipt = {
                    "status": "confirmed", "tx_hash": "b" * 64, "epoch": 20,
                    "from": WALLET["address"], "to": WALLET["address"],
                    "op_type": "validator_withdraw", "nonce": 5, "amount_raw": "0",
                }
                with mock.patch.object(control, "transaction", side_effect = lambda url, tx_hash:
                    receipt if tx_hash == receipt["tx_hash"] else None
                ), mock.patch.object(control, "call", return_value = {"nonce": 5}):
                    self.assertEqual(control.completed_withdraw(settings(root), WALLET, "local", 20),
                                     ("b" * 64, 1))

    def test_process_lock(self):
        root = self.root / "lock"
        (root / "data").mkdir(parents = True)
        with control.command_lock(settings(root)):
            result = self.child(root)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("another enrollment command", result.stderr)
            self.assertEqual(self.records(root, "signed.jsonl"), [])
            self.assertEqual(self.records(root, "sent.jsonl"), [])
        self.accepted(self.child(root))

if __name__ == "__main__":
    if len(sys.argv) == 5:
        run_child(Path(sys.argv[1]), sys.argv[2], sys.argv[3] == "1", sys.argv[4])
    else:
        unittest.main()