# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import base64
import copy
import http.server
import json
import os
from pathlib import Path
import subprocess
import threading
import unittest
from unittest import mock
import uuid

from nacl.signing import SigningKey

import bond_inc as inc
from bond_payment import FEE, amount_raw, payment
from validator_common import ValidatorError, address_from_pubkey, sha256_file, write_env, write_private_json
from validator_exit import command_lock

ROOT = Path(__file__).resolve().parents[2]
BINARY = ROOT / "_build/default/bin/bft_control_tx.exe"
HASH = ROOT / "_build/default/test/test_tx_hash.exe"

class Rpc(http.server.BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass

    def do_POST(self):
        request = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        try:
            result = self.server.case.reply(request["method"], request["params"])
            body = {"jsonrpc": "2.0", "id": request["id"], "result": result}
        except Exception as error:
            body = {"jsonrpc": "2.0", "id": request["id"],
                "error": {"code": 112 if isinstance(error, LookupError) else -32000,
                    "message": str(error)}}
        encoded = json.dumps(body).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(encoded)))
        self.end_headers()
        self.wfile.write(encoded)

class BondTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        for path in (BINARY, HASH):
            if not path.is_file():
                raise RuntimeError("build required test executable: " + str(path))

    def setUp(self):
        self.work = ROOT / ".keys/bond_inc" / uuid.uuid4().hex
        self.work.mkdir(mode = 0o700, parents = True)
        self.data = self.work / "data"
        self.data.mkdir(mode = 0o700)
        seed = bytes([65]) * 32
        public = bytes(SigningKey(seed).verify_key)
        self.wallet = {"address": address_from_pubkey(public),
            "pub": base64.b64encode(public).decode(), "priv": base64.b64encode(seed).decode()}
        self.wallet_path = self.data / "wallet.json"
        write_private_json(self.wallet_path, self.wallet)
        self.server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Rpc)
        self.server.case = self
        self.thread = threading.Thread(target = self.server.serve_forever)
        self.thread.start()
        self.addCleanup(self.server.server_close)
        self.addCleanup(self.thread.join)
        self.addCleanup(self.server.shutdown)
        self.values = {"OCTRA_CHAIN_ID": inc.CHAIN, "OCTRA_API_PORT": str(self.server.server_port),
            "OCTRA_DATA_DIR": str(self.data), "OCTRA_OPERATOR_CONTROL_BINARY": str(BINARY),
            "OCTRA_OPERATOR_CONTROL_BINARY_HASH": sha256_file(BINARY)}
        self.config = self.work / "node.env"
        write_env(self.config, self.values)
        self.state = {"head": 1737480, "bond": 15000000000, "bonded": 1300000,
            "ready": 1300001, "nonce": 9, "pending": 9, "balance": 100000000}
        self.runtime_chain = inc.CHAIN
        self.active = True
        self.exit_requested = False
        self.sent = []
        self.receipts = {}
        self.reject_submit = False
        self.change_on_balance = 0
        self.balance_reads = 0

    def reply(self, method, params):
        if method == "octra_runtimeVersion":
            return {"chain_id": self.runtime_chain, "validator": self.wallet["address"]}
        if method == "octra_validatorEnrollment":
            return {"chain_id": inc.CHAIN, "address": self.wallet["address"],
                "consensus_pubkey": self.wallet["pub"], "identity": "self_reported",
                "state": "ready", "head_epoch": self.state["head"], "state_root": "a" * 64,
                "bond": str(self.state["bond"]), "bonded_epoch": str(self.state["bonded"]),
                "ready_epoch": str(self.state["ready"]), "exit_epoch": None, "withdraw_epoch": None,
                "membership": {"epoch": self.state["head"], "active": self.active,
                    "scheduled": False, "activate_epoch": None, "next_set_epoch": None,
                    "validator_set_hash": "b" * 64},
                "local_control": {"exit_intent": True, "exit_requested": self.exit_requested,
                    "intent_id": "c" * 64 if self.exit_requested else None, "error": None}}
        if method == "node_status":
            return {"head_epoch": self.state["head"], "state_root": "a" * 64}
        if method == "octra_balance":
            self.balance_reads += 1
            if self.change_on_balance == self.balance_reads:
                self.state["nonce"] += 1
                self.state["pending"] += 1
            return {"address": self.wallet["address"], "balance_raw": str(self.state["balance"]),
                "nonce": self.state["nonce"], "pending_nonce": self.state["pending"]}
        if method == "octra_transaction":
            if params[0] not in self.receipts:
                raise LookupError("transaction not found")
            return self.receipts[params[0]]
        if method == "octra_submit":
            path = self.data / "validator-control/bond-increase.json"
            record = json.loads(path.read_text())
            self.assertEqual(record["stage"], "sent")
            self.assertEqual(record["tx"], params[0])
            digest = subprocess.check_output([str(HASH), "--signed"],
                input = json.dumps(params[0]).encode(), timeout = 10).decode().strip()
            self.assertEqual(digest, record["tx_hash"])
            self.sent.append(params[0])
            tx = params[0]
            self.receipts[digest] = {"tx_hash": digest, "from": tx["from"], "to": tx["to_"],
                "amount_raw": tx["amount"], "nonce": tx["nonce"], "op_type": tx["op_type"],
                "status": "pending"}
            self.state["pending"] = tx["nonce"]
            if self.reject_submit:
                raise ValueError("response lost after acceptance")
            return {"status": "accepted", "tx_hash": digest}
        raise ValueError("unexpected RPC method: " + method)

    def run_cli(self, amount = "1", *args):
        return subprocess.run([str(ROOT / "bond_inc.sh"), "-a", amount,
            "-w", str(self.wallet_path), "--config", str(self.config), *args],
            capture_output = True, text = True, timeout = 20,
            env = dict(os.environ, PYTHONDONTWRITEBYTECODE = "1"))

    def successful(self, result):
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertRegex(result.stdout, r"^tx = [0-9a-f]{64}\n$")
        return result.stdout.strip().split(" = ")[1]

    def confirm(self, digest):
        tx = self.sent[-1]
        self.state["head"] += 1
        self.state["bond"] += int(tx["amount"])
        self.state["nonce"] = tx["nonce"]
        self.state["pending"] = tx["nonce"]
        self.state["balance"] -= int(tx["amount"]) + FEE
        self.receipts[digest].update(status = "confirmed", epoch = self.state["head"])

    def test_amounts(self):
        for text, expected in (("1", 1000000), ("1.000001", 1000001),
            ("100.5", 100500000), ("15000", 15000000000), ("01.000000", 1000000)):
            self.assertEqual(amount_raw(text), expected)
        for text in ("0", "0.999999", "-1", "+1", "1e6", "NaN", "Infinity", "1,5",
            "1.", ".1", " 1", "1 ", "1.0000001", "\u0661", "9" * 41):
            with self.subTest(text = text), self.assertRaises(ValidatorError):
                amount_raw(text)

    def test_add_once(self):
        digest = self.successful(self.run_cli("1.000001"))
        self.assertEqual(self.sent[0]["amount"], "1000001")
        self.assertEqual(self.sent[0]["nonce"], 10)
        self.assertEqual(self.successful(self.run_cli("1.000001")), digest)
        self.confirm(digest)
        self.assertEqual(self.successful(self.run_cli("1.000001")), digest)
        self.assertEqual(len(self.sent), 1)

    def test_new(self):
        digest = self.successful(self.run_cli())
        self.assertNotEqual(self.run_cli("2", "--new").returncode, 0)
        self.confirm(digest)
        second = self.successful(self.run_cli("2", "--new"))
        self.assertNotEqual(second, digest)
        self.assertEqual(len(self.sent), 2)
        self.assertEqual(self.sent[-1]["nonce"], 11)
        self.assertEqual(len(list((self.data / "validator-control").glob("bond-increase-*.json"))), 1)

    def test_pending(self):
        self.state["pending"] += 1
        result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("pending transactions", result.stderr)
        self.assertEqual(self.sent, [])

    def test_preflight(self):
        for change, message in (({"balance": 1000999}, "insufficient"),
            ({"pending": 8}, "pending transactions")):
            self.state.update(change)
            result = self.run_cli()
            self.assertNotEqual(result.returncode, 0)
            self.assertIn(message, result.stderr)
            self.assertEqual(self.sent, [])
        self.state["pending"] = self.state["nonce"]
        self.runtime_chain = "mainnet"
        self.assertIn("network", self.run_cli().stderr)
        self.runtime_chain = inc.CHAIN
        self.exit_requested = True
        self.assertIn("exit is requested", self.run_cli().stderr)
        self.exit_requested = False
        self.active = False
        self.assertIn("active ready", self.run_cli().stderr)
        self.assertEqual(self.sent, [])

    def test_wallet(self):
        other = copy.deepcopy(self.wallet)
        other["address"] = "oct" + "1" * 44
        write_private_json(self.wallet_path, other)
        result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("wallet identity mismatch", result.stderr)
        self.assertEqual(self.sent, [])

    def test_hash(self):
        self.values["OCTRA_OPERATOR_CONTROL_BINARY_HASH"] = "0" * 64
        write_env(self.config, self.values)
        result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("binary hash mismatch", result.stderr)
        self.assertEqual(self.sent, [])

    def test_race(self):
        self.change_on_balance = 2
        result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("changed", result.stderr)
        self.assertEqual(self.sent, [])
        record = json.loads((self.data / "validator-control/bond-increase.json").read_text())
        self.assertEqual(record["stage"], "prepared")

    def test_lost_reply(self):
        self.reject_submit = True
        result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("submission unresolved", result.stderr)
        self.assertEqual(len(self.sent), 1)
        self.successful(self.run_cli())
        self.assertEqual(len(self.sent), 1)

    def test_receipt(self):
        digest = self.successful(self.run_cli())
        self.confirm(digest)
        self.receipts[digest]["amount_raw"] = "2"
        result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("receipt differs", result.stderr)
        self.assertEqual(len(self.sent), 1)

    def test_retry(self):
        digest = self.successful(self.run_cli())
        self.receipts.clear()
        self.state["pending"] = self.state["nonce"]
        self.assertIn("submission unresolved", self.run_cli().stderr)
        self.assertEqual(len(self.sent), 1)
        self.assertEqual(self.successful(self.run_cli("1", "--retry")), digest)
        self.assertEqual(len(self.sent), 2)
        self.assertEqual(self.sent[0], self.sent[1])

    def test_saved_prepare(self):
        current = inc.view(self.values, self.wallet)
        record = inc.prepare(self.values, self.wallet, self.wallet_path, 1000000, current)
        path = inc.directory(self.values) / "bond-increase.json"
        inc.save(path, record)
        self.assertEqual(self.successful(self.run_cli()), record["tx_hash"])
        self.assertEqual(self.sent, [record["tx"]])

    def test_save_failure(self):
        for failure in (1, 2):
            count = 0
            original = inc.save
            def save(path, value):
                nonlocal count
                count += 1
                if count == failure:
                    raise OSError("test durable write failure")
                original(path, value)
            with self.subTest(failure = failure), mock.patch.object(inc, "save", side_effect = save):
                with self.assertRaisesRegex(OSError, "durable write"):
                    inc.increase(self.values, self.wallet, self.wallet_path, 1000000)
            self.assertEqual(self.sent, [])

    def test_payment_expired(self):
        current = inc.view(self.values, self.wallet)
        record = inc.prepare(self.values, self.wallet, self.wallet_path, 1000000, current)
        inc.save(inc.directory(self.values) / "bond-increase.json", record)
        with mock.patch.object(inc.time, "time", return_value = record["tx"]["timestamp"] + 301):
            with self.assertRaisesRegex(ValidatorError, "transaction time changed"):
                inc.increase(self.values, self.wallet, self.wallet_path, 1000000)
        self.assertEqual(self.sent, [])

    def test_registry_confirmation(self):
        digest = self.successful(self.run_cli())
        self.confirm(digest)
        self.state["bond"] -= 1
        result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("absent from current registry", result.stderr)
        self.assertEqual(len(self.sent), 1)

    def test_signature(self):
        self.successful(self.run_cli())
        path = self.data / "validator-control/bond-increase.json"
        record = json.loads(path.read_text())
        for change in ({"nonce": 11}, {"signature": "AA=="}, {"amount": "2000000"},
            {"to_": self.wallet["address"]}, {"ou": "1001"}):
            altered = {**record, "tx": {**record["tx"], **change}}
            with self.subTest(change = change), self.assertRaises(ValidatorError):
                payment(altered, self.wallet, inc.CHAIN, 1000000)
        with self.assertRaises(ValidatorError):
            payment(record, self.wallet, "another-network", 1000000)
        record["tx_hash"] = "0" * 64
        write_private_json(path, record)
        self.assertIn("hash differs", self.run_cli().stderr)
        self.assertEqual(len(self.sent), 1)

    def test_lock(self):
        with command_lock(self.values):
            result = self.run_cli()
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("another enrollment", result.stderr)
        self.assertEqual(self.sent, [])

if __name__ == "__main__":
    unittest.main()