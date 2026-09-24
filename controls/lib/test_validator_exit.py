# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import io
import http.client
import json
import os
import runpy
import shutil
import subprocess
import unittest
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

import validator_enroll as enroll
import validator_exit as exit_control
import validator_rpc as rpc_client
import validator_common as common
from validator_common import ValidatorError

class ValidatorExitTest(unittest.TestCase):
    def setUp(self):
        self.work = Path(__file__).resolve().parent / "runtime_data" / f"exit_test_{os.getpid()}"
        self.work.mkdir(parents=True, exist_ok=True)
        self.addCleanup(shutil.rmtree, self.work)
        self.wallet = {"address": "octTest", "pub": "test-key"}
        self.values = {
            "OCTRA_DATA_DIR": str(self.work),
            "OCTRA_CHAIN_ID": "octra-test",
            "OCTRA_API_PORT": "8080",
            "OCTRA_OPERATOR_RPC_URL": "http://submission/rpc",
            "OCTRA_OPERATOR_CONTROL_BINARY": str(self.work / "control"),
            "OCTRA_OPERATOR_CONTROL_BINARY_HASH": "c" * 64,
        }
        self.url = "http://127.0.0.1:8080/rpc"
        self.id = "a" * 64
        self.hash = "b" * 64
        self.tx = {
            "from": "octTest", "to_": "octTest", "amount": "0",
            "op_type": "validator_exit", "public_key": "test-key",
            "nonce": 5, "timestamp": 1.0, "signature": "signed", "ou": "1000",
        }
        self.prepared = {"tx": self.tx, "tx_hash": self.hash}
        self.ready = enroll.Enrollment(enroll.EnrollmentState.READY, 20, 1000000, 1, 2, None)

    def view(self, intent_id=None):
        return {
            "chain_id": "octra-test", "address": "octTest", "consensus_pubkey": "test-key",
            "bonded_epoch": "1", "exit_epoch": None,
            "local_control": {"exit_intent": True, "exit_requested": intent_id is not None,
                              "intent_id": intent_id},
        }

    def submit(self, prepare):
        return exit_control.submit(
            self.values, self.wallet, "validator_exit", 1, self.url, prepare,
        )

    def bond(self, amount = 1000000, renew = False):
        return enroll.submit_bond(
            self.work / "node.env", self.values, self.wallet, self.work / "wallet.json",
            amount, SimpleNamespace(no_wait = True, renew = renew),
        )

    def save_bond(self):
        tx = {**self.tx, "amount": "1000000", "to_": exit_control.BOND_ESCROW,
              "op_type": "validator_bond"}
        exit_control.begin_bond(self.values, self.wallet, 30)
        with mock.patch.object(exit_control, "transaction", return_value = {"status": "pending"}):
            exit_control.submit(self.values, self.wallet, "validator_bond", 30, self.url,
                                lambda: {"tx": tx, "tx_hash": self.hash}, amount = 1000000)
        return {"status": "confirmed", "tx_hash": self.hash, "epoch": 31,
                "from": self.wallet["address"], "to": exit_control.BOND_ESCROW,
                "op_type": "validator_bond", "nonce": 5, "amount_raw": "1000000"}

    def test_bond_loss(self):
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 30, None, None, None, None)
        sends = []
        signed = []
        nonce = [4]

        def receipt():
            return {
                "status": "confirmed", "tx_hash": self.hash, "epoch": 31,
                "from": self.wallet["address"], "to": exit_control.BOND_ESCROW,
                "op_type": "validator_bond", "nonce": 5, "amount_raw": "1000000",
            } if nonce[0] >= 5 else None

        def send(tx):
            sends.append(dict(tx))
            if len(sends) == 1:
                raise ValidatorError("reply lost")
            nonce[0] = tx["nonce"]
            return {"status": "accepted", "tx_hash": self.hash}

        def rpc(_url, method, params, **_options):
            if method == "octra_balance":
                if sends:
                    nonce[0] = max(nonce[0], sends[0]["nonce"])
                return {"nonce": nonce[0], "pending_nonce": nonce[0]}
            if method == "octra_submit":
                return send(params[0])
            raise AssertionError(method)

        def run(command, **_options):
            if "--capabilities" in command:
                value = {"prepare_transaction": True}
            else:
                number = int(command[command.index("--nonce") + 1])
                tx = {**self.tx, "nonce": number, "to_": exit_control.BOND_ESCROW,
                      "amount": "1000000", "op_type": "validator_bond"}
                signed.append(tx)
                if "--prepare" in command:
                    value = {"tx": tx, "tx_hash": self.hash}
                else:
                    try:
                        value = send(tx)
                    except ValidatorError:
                        raise subprocess.TimeoutExpired(command, 30)
            return subprocess.CompletedProcess(command, 0, json.dumps(value), "")

        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            enroll, "call", side_effect = rpc,
        ), mock.patch.object(exit_control, "call", side_effect = rpc), mock.patch.object(
            exit_control, "transaction", side_effect = lambda *_: receipt(),
        ), mock.patch.object(enroll, "node_status", return_value = (30, "a" * 64)), mock.patch.object(
            enroll, "sha256_file", return_value = "c" * 64,
        ), mock.patch.object(enroll.subprocess, "run", side_effect = run), mock.patch.object(enroll, "emit"):
            for _ in range(2):
                try:
                    self.bond()
                except ValidatorError:
                    pass
        self.assertEqual(len(signed), 1)
        self.assertEqual(nonce[0], 5)
        records = [json.loads(path.read_text())
                   for path in (self.work / "validator-control").glob("*.json")]
        self.assertTrue(any(value.get("tx") == signed[0] for value in records))

    def test_bond_restore(self):
        self.save_bond()
        saved = self.work / "prior"
        saved.mkdir(mode = 0o700)
        active = self.work / "validator-control"
        active.rename(saved / "validator-control")
        exit_control.restore_control(saved, self.work)
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 30, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            exit_control, "transaction", return_value = {"status": "staging"},
        ), mock.patch.object(enroll, "control_result") as sign, mock.patch.object(exit_control, "call") as rpc:
            self.assertEqual(self.bond(), self.hash)
            sign.assert_not_called()
            rpc.assert_not_called()
        for path in (saved / "validator-control").glob("*.json"):
            self.assertEqual(path.read_bytes(), (active / path.name).read_bytes())

    def test_bond_confirmed(self):
        receipt = self.save_bond()
        bonded = enroll.Enrollment(enroll.EnrollmentState.BONDED, 31, 1000000, 31, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = bonded), mock.patch.object(
            exit_control, "transaction", return_value = receipt,
        ), mock.patch.object(enroll, "control_result") as sign, mock.patch.object(exit_control, "call") as rpc:
            self.assertEqual(self.bond(), self.hash)
            sign.assert_not_called()
            rpc.assert_not_called()
        for change in ({"epoch": 32}, {"amount_raw": "2000000"}, {"nonce": 6}, {"to": "other"}):
            with mock.patch.object(enroll, "committed_enrollment", return_value = bonded), mock.patch.object(
                exit_control, "transaction", return_value = {**receipt, **change},
            ), mock.patch.object(enroll, "control_result") as sign, self.assertRaises(ValidatorError):
                self.bond()
            sign.assert_not_called()

    def test_bond_cycle(self):
        receipt = self.save_bond()
        old = exit_control.outbox_path(self.values, "validator_bond", 30)
        before = old.read_bytes()
        tx = {**json.loads(before)["tx"], "nonce": 6}
        prepared = {"tx": tx, "tx_hash": "d" * 64}
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 40, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            exit_control, "transaction", side_effect = lambda _url, key: receipt if key == self.hash else None,
        ), mock.patch.object(enroll, "control_result", return_value = prepared) as sign, mock.patch.object(
            exit_control, "call", side_effect = [{"nonce": 5}, {"status": "accepted", "tx_hash": "d" * 64}],
        ):
            self.assertEqual(self.bond(), "d" * 64)
        self.assertTrue(sign.call_args.kwargs["prepare"])
        self.assertEqual(sign.call_args.kwargs["head"], (40, None))
        self.assertEqual(old.read_bytes(), before)
        self.assertEqual(exit_control.read_outbox(
            exit_control.outbox_path(self.values, "validator_bond", 40),
        )["tx"], tx)

    def test_bond_legacy(self):
        exit_control.begin_bond(self.values, self.wallet)
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 30, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            enroll, "control_result",
        ) as sign, self.assertRaisesRegex(ValidatorError, "inspect before retrying"):
            self.bond()
        sign.assert_not_called()

    def test_bond_after_restore(self):
        self.save_bond()
        enroll.record_transaction(self.work / "node.env", "bond", self.hash)
        self.tx = {**self.tx, "nonce": 7}
        self.hash = "e" * 64
        self.withdraw_attempt(epoch = 31)
        saved = self.work / "prior"
        saved.mkdir(mode = 0o700)
        active = self.work / "validator-control"
        active.rename(saved / "validator-control")
        exit_control.restore_control(saved, self.work)
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 40, None, None, None, None)
        tx = {**self.tx, "nonce": 8, "op_type": "validator_bond",
              "amount": "1000000", "to_": exit_control.BOND_ESCROW}
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            enroll, "transaction", return_value = None,
        ), mock.patch.object(exit_control, "transaction", return_value = None), mock.patch.object(
            enroll, "control_result", return_value = {"tx": tx, "tx_hash": "d" * 64},
        ) as sign, mock.patch.object(exit_control, "call", side_effect = [
            {"nonce": 7}, {"nonce": 7}, {"status": "accepted", "tx_hash": "d" * 64},
        ]) as rpc:
            self.assertEqual(self.bond(), "d" * 64)
            sign.assert_called_once()
            self.assertEqual(rpc.call_args.args[2], [tx])
        self.assertEqual(exit_control.read_outbox(
            exit_control.outbox_path(self.values, "validator_withdraw", 31),
        )["tx"]["nonce"], 7)

    def test_rebond_checks(self):
        self.withdraw_attempt()
        for nonce in [None, False, -1, 4, "invalid"]:
            with self.subTest(nonce = nonce), mock.patch.object(
                exit_control, "call", return_value = {"nonce": nonce},
            ), self.assertRaisesRegex(ValidatorError, "nonce consumption unproved"):
                exit_control.can_rebond(self.values, self.wallet, self.url, 20)
        for nonce in [5, "5", 7]:
            with self.subTest(nonce = nonce), mock.patch.object(
                exit_control, "call", return_value = {"nonce": nonce},
            ):
                self.assertTrue(exit_control.can_rebond(self.values, self.wallet, self.url, 20))
        with self.assertRaisesRegex(ValidatorError, "later committed absent"):
            exit_control.can_rebond(self.values, self.wallet, self.url, 1)
        path = self.work / "validator-control" / "last.json"
        latest = exit_control.read_outbox(path)
        for change in ({"address": "other"}, {"chain_id": "other"}, {"tx_hash": "f" * 64}):
            exit_control.write_private_json(path, {**latest, **change})
            with self.subTest(change = change), self.assertRaises(ValidatorError):
                exit_control.can_rebond(self.values, self.wallet, self.url, 20)

    def test_bond_amount(self):
        self.save_bond()
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 30, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            exit_control, "transaction", return_value = None,
        ), mock.patch.object(enroll, "control_result") as sign, mock.patch.object(
            exit_control, "call",
        ) as rpc, self.assertRaisesRegex(ValidatorError, "amount differs"):
            self.bond(amount = 2000000)
        sign.assert_not_called()
        rpc.assert_not_called()

    def test_bond_renew(self):
        self.save_bond()
        path = exit_control.outbox_path(self.values, "validator_bond", 30)
        before = path.read_bytes()
        tx = {**json.loads(before)["tx"], "timestamp": 2., "signature": "renewed"}
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 32, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            exit_control, "transaction", return_value = None,
        ), mock.patch.object(enroll, "control_result", return_value = {"tx": tx, "tx_hash": "d" * 64}), mock.patch.object(
            exit_control, "call", side_effect = [{"nonce": 4}, {"status": "accepted", "tx_hash": "d" * 64}],
        ) as rpc:
            self.assertEqual(self.bond(renew = True), "d" * 64)
            self.assertEqual(rpc.call_args.args[2], [tx])
        self.assertEqual(path.with_name(path.stem + "-" + self.hash + ".json").read_bytes(), before)

    def test_bond_head_race(self):
        with mock.patch.object(enroll, "sha256_file", return_value = "c" * 64), mock.patch.object(
            exit_control, "require_prepare",
        ), mock.patch.object(enroll, "next_nonce", return_value = 6), mock.patch.object(
            enroll, "node_status", return_value = (31, "b" * 64),
        ), mock.patch.object(enroll.subprocess, "run") as run, self.assertRaisesRegex(ValidatorError, "nothing sent"):
            enroll.control_result(self.values, self.wallet, self.work / "wallet.json",
                                  "validator_bond", amount = 1000000, prepare = True,
                                  head = (30, "a" * 64))
        run.assert_not_called()

    def test_rejected_renew(self):
        for operation in ("validator_bond", "validator_exit", "validator_withdraw"):
            with self.subTest(operation = operation):
                amount = 1000000 if operation == "validator_bond" else 0
                tx = {**self.tx, "op_type": operation, "amount": str(amount),
                      "to_": exit_control.BOND_ESCROW if amount else self.wallet["address"]}
                old = {"tx": tx, "tx_hash": self.hash}
                with mock.patch.object(exit_control, "transaction", return_value = {"status": "pending"}):
                    exit_control.submit(self.values, self.wallet, operation, 1,
                                        self.url, lambda: old, amount = amount)
                path = exit_control.outbox_path(self.values, operation, 1)
                before = path.read_bytes()
                renewed = {"tx_hash": "d" * 64, "tx": {**tx, "timestamp": 2.0}}
                prepare = mock.Mock(return_value = renewed)
                with mock.patch.object(exit_control, "transaction", return_value = {"status": "rejected"}):
                    with self.assertRaisesRegex(ValidatorError, "--renew"):
                        exit_control.submit(self.values, self.wallet, operation, 1,
                                            self.url, prepare, amount = amount)
                    prepare.assert_not_called()
                    for nonce in (5, 6, True, None, "bad"):
                        with mock.patch.object(exit_control, "call", return_value = {"nonce": nonce}), self.assertRaises(
                            ValidatorError
                        ):
                            exit_control.submit(self.values, self.wallet, operation, 1,
                                                self.url, prepare, renew = True, amount = amount)
                        prepare.assert_not_called()
                        self.assertEqual(path.read_bytes(), before)
                    with mock.patch.object(exit_control, "call", side_effect = [
                        {"nonce": 4}, {"status": "accepted", "tx_hash": "d" * 64},
                    ]) as rpc:
                        self.assertEqual(exit_control.submit(
                            self.values, self.wallet, operation, 1, self.url,
                            prepare, renew = True, amount = amount,
                        ), "d" * 64)
                    prepare.assert_called_once_with(tx)
                    self.assertEqual(rpc.call_args.args[2], [renewed["tx"]])
                    self.assertEqual(path.with_name(path.stem + "-" + self.hash + ".json").read_bytes(), before)

    def test_bond_lost_pointer(self):
        self.save_bond()
        (self.work / "validator-control" / "last.json").unlink()
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 40, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            enroll, "control_result",
        ) as sign, self.assertRaisesRegex(ValidatorError, "pointer missing"):
            self.bond()
        sign.assert_not_called()

    def test_pointer_repair(self):
        self.save_bond()
        root = exit_control.directory(self.values)
        pointer = root / "last.json"
        before = {path.name: path.read_bytes() for path in root.glob("*.json")}
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        with mock.patch.object(exit_control, "call") as rpc, mock.patch.object(
            enroll, "control_result",
        ) as sign:
            self.assertEqual(exit_control.repair_pointer(self.values, self.wallet, backup), self.hash)
            self.assertEqual(exit_control.repair_pointer(self.values, self.wallet, backup), self.hash)
            rpc.assert_not_called()
            sign.assert_not_called()
        self.assertEqual(before, {path.name: path.read_bytes() for path in root.glob("*.json")})
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 40, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            exit_control, "transaction", return_value = {"status": "staging"},
        ), mock.patch.object(enroll, "control_result") as sign:
            self.assertEqual(self.bond(), self.hash)
            sign.assert_not_called()

    def test_withdraw_lost_pointer(self):
        self.save_bond()
        (exit_control.directory(self.values) / "last.json").unlink()
        with self.assertRaisesRegex(ValidatorError, "repair-pointer"):
            exit_control.completed_withdraw(self.values, self.wallet, self.url, 40)

    def test_pointer_cycles(self):
        self.save_bond()
        root = exit_control.directory(self.values)
        tx = {**self.tx, "op_type": "validator_withdraw", "nonce": 8}
        path = exit_control.outbox_path(self.values, "validator_withdraw", 31)
        saved = {"tx": tx, "tx_hash": "d" * 64, "chain_id": "octra-test", "bonded_epoch": 31}
        exit_control.write_private_json(path, saved)
        exit_control.remember_attempt(path, saved, self.wallet)
        pointer = root / "last.json"
        before = pointer.read_bytes()
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        self.assertEqual(exit_control.repair_pointer(self.values, self.wallet, backup), "d" * 64)
        self.assertEqual(pointer.read_bytes(), before)
        receipt = {"status": "confirmed", "tx_hash": "d" * 64, "epoch": 50,
                   "from": "octTest", "to": "octTest", "op_type": "validator_withdraw",
                   "nonce": 8, "amount_raw": "0"}
        with mock.patch.object(exit_control, "transaction", return_value = receipt), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 8},
        ):
            self.assertEqual(exit_control.completed_withdraw(
                self.values, self.wallet, self.url, 60,
            ), ("d" * 64, 31))

    def test_pointer_archives(self):
        receipt, path = self.renew_withdraw()
        pointer = path.parent / "last.json"
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        self.assertEqual(exit_control.repair_pointer(self.values, self.wallet, backup), "d" * 64)
        with mock.patch.object(exit_control, "transaction", side_effect = lambda _url, key:
                               receipt if key == self.hash else {"status": "rejected"}), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 5},
        ):
            self.assertEqual(exit_control.completed_withdraw(
                self.values, self.wallet, self.url, 30,
            ), (self.hash, 1))
        pointer.unlink()
        value = exit_control.read_outbox(backup)
        exit_control.write_private_json(backup, {**value, "tx_hash": self.hash})
        self.assertEqual(exit_control.repair_pointer(self.values, self.wallet, backup), self.hash)
        self.assertEqual(exit_control.read_outbox(pointer)["tx_hash"], self.hash)

    def test_pointer_refusals(self):
        self.save_bond()
        root = exit_control.directory(self.values)
        pointer = root / "last.json"
        path = exit_control.outbox_path(self.values, "validator_bond", 30)
        saved = exit_control.read_outbox(path)
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        for kind in ("primary", "archive", "identity", "nonce", "name", "pointer", "mode", "link"):
            with self.subTest(kind = kind):
                for item in root.iterdir():
                    item.unlink()
                exit_control.write_private_json(path, saved)
                archive = path.with_name(path.stem + "-" + self.hash + ".json")
                if kind == "primary":
                    path.rename(archive)
                elif kind == "archive":
                    exit_control.write_private_json(archive, {**saved, "tx_hash": "c" * 64})
                elif kind == "identity":
                    exit_control.write_private_json(path, {**saved, "chain_id": "other"})
                elif kind == "nonce":
                    exit_control.write_private_json(root / "validator_bond-31.json", {**saved, "head_epoch": 31})
                elif kind == "name":
                    path.rename(root / "validator_bond-030.json")
                elif kind == "pointer":
                    exit_control.copy_private(backup, pointer)
                    exit_control.begin_bond(self.values, self.wallet, 40)
                elif kind == "mode":
                    path.chmod(0o644)
                elif kind == "link":
                    path.rename(self.work / "signed.json")
                    path.symlink_to(self.work / "signed.json")
                before = pointer.read_bytes() if pointer.exists() else None
                with self.assertRaises((ValidatorError, OSError)):
                    exit_control.repair_pointer(self.values, self.wallet, backup)
                self.assertEqual(pointer.read_bytes() if pointer.exists() else None, before)

    def test_pointer_write_retry(self):
        self.save_bond()
        root = exit_control.directory(self.values)
        pointer = root / "last.json"
        before = pointer.read_bytes()
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        sync = exit_control.sync_directory
        def interrupted(path):
            if path == root:
                raise OSError("interrupted")
            sync(path)
        with mock.patch.object(exit_control, "sync_directory", side_effect = interrupted):
            with self.assertRaises(OSError):
                exit_control.repair_pointer(self.values, self.wallet, backup)
        self.assertEqual(exit_control.repair_pointer(self.values, self.wallet, backup), self.hash)
        self.assertEqual(pointer.read_bytes(), before)

    def test_repair_cli_lock(self):
        self.save_bond()
        pointer = exit_control.directory(self.values) / "last.json"
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        args = enroll.parser().parse_args(["repair-pointer", "--from", str(backup)])
        repair = exit_control.repair_pointer
        def locked(values, wallet, source):
            with self.assertRaisesRegex(ValidatorError, "another enrollment command"):
                with exit_control.command_lock(values):
                    self.fail("repair must hold the command lock")
            return repair(values, wallet, source)
        with mock.patch.object(enroll, "parser") as parser, mock.patch.object(
            enroll, "private_mode",
        ), mock.patch.object(enroll, "parse_env", return_value = self.values), mock.patch.object(
            enroll, "load_wallet", return_value = self.wallet,
        ), mock.patch.object(exit_control, "repair_pointer", side_effect = locked), mock.patch.object(
            enroll, "require_join",
        ) as rpc, mock.patch.object(enroll, "emit"), mock.patch.object(enroll.subprocess, "run") as run:
            parser.return_value.parse_args.return_value = args
            enroll.main()
            rpc.assert_not_called()
            run.assert_not_called()

    def test_pointer_unsigned(self):
        self.renew_withdraw()
        exit_control.begin_bond(self.values, self.wallet, 40)
        pointer = exit_control.directory(self.values) / "last.json"
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        self.assertIsNone(exit_control.repair_pointer(self.values, self.wallet, backup))
        with mock.patch.object(exit_control, "transaction") as rpc:
            self.assertIsNone(exit_control.completed_withdraw(self.values, self.wallet, self.url, 40))
            rpc.assert_not_called()
        self.assertEqual(pointer.read_bytes(), backup.read_bytes())

    def test_pointer_signed_intent(self):
        exit_control.begin_bond(self.values, self.wallet, 30)
        pointer = exit_control.directory(self.values) / "last.json"
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        self.save_bond()
        pointer.unlink()
        with self.assertRaisesRegex(ValidatorError, "intent predates"):
            exit_control.repair_pointer(self.values, self.wallet, backup)
        self.assertFalse(pointer.exists())

    def test_submit_partial_tx(self):
        self.save_bond()
        for field in ("signature", "timestamp", "ou"):
            with self.subTest(field = field):
                path = exit_control.outbox_path(self.values, "validator_bond", 30)
                saved = exit_control.read_outbox(path)
                tx = {key: value for key, value in saved["tx"].items() if key != field}
                exit_control.write_private_json(path, {**saved, "tx": tx})
                before = path.read_bytes()
                with mock.patch.object(exit_control, "call") as send, mock.patch.object(
                    exit_control, "transaction",
                ) as read, mock.patch.object(enroll, "control_result") as sign:
                    with self.assertRaisesRegex(ValidatorError, "incomplete signed transaction"):
                        exit_control.submit(self.values, self.wallet, "validator_bond", 30,
                                            self.url, sign, renew = True, amount = 1000000)
                send.assert_not_called()
                read.assert_not_called()
                sign.assert_not_called()
                self.assertEqual(path.read_bytes(), before)
                exit_control.write_private_json(path, saved)

    def test_bond_keeps_pointer(self):
        self.save_bond()
        pointer = exit_control.directory(self.values) / "last.json"
        before = pointer.read_bytes()
        with mock.patch.object(exit_control, "write_private_json") as write:
            exit_control.begin_bond(self.values, self.wallet, 30)
        write.assert_not_called()
        self.assertEqual(pointer.read_bytes(), before)
        exit_control.outbox_path(self.values, "validator_bond", 30).unlink()
        with self.assertRaisesRegex(ValidatorError, "outbox missing"):
            exit_control.begin_bond(self.values, self.wallet, 30)
        self.assertEqual(pointer.read_bytes(), before)

    def test_copy_source_types(self):
        source = self.work / "source"
        source.write_bytes(b"public bundle")
        source.chmod(0o644)
        target = self.work / "target"
        common.copy_private(source, target)
        self.assertEqual(target.read_bytes(), source.read_bytes())
        linked = self.work / "linked"
        for kind in ("link", "fifo"):
            with self.subTest(kind = kind):
                if kind == "link":
                    linked.symlink_to(source)
                else:
                    os.mkfifo(linked, 0o600)
                try:
                    with self.assertRaises((ValidatorError, OSError)):
                        common.copy_private(linked, target)
                    self.assertEqual(target.read_bytes(), b"public bundle")
                finally:
                    linked.unlink()

    def test_pointer_fifo(self):
        pointer = exit_control.directory(self.values) / "last.json"
        os.mkfifo(pointer, 0o600)
        self.assertEqual(exit_control.pointer_status(self.values, self.wallet)["status"], "invalid")

    def test_pointer_status_values(self):
        exit_control.begin_bond(self.values, self.wallet, 30)
        pointer = exit_control.directory(self.values) / "last.json"
        saved = exit_control.read_outbox(pointer)
        self.assertEqual(exit_control.pointer_status(self.values, self.wallet)["status"], "unsigned")
        for operation in ([], {}, None, True, 1):
            with self.subTest(operation = operation):
                exit_control.write_private_json(pointer, {**saved, "operation": operation})
                self.assertEqual(exit_control.pointer_status(self.values, self.wallet)["status"], "invalid")
                with self.assertRaises(ValidatorError):
                    exit_control.outbox_path(self.values, operation, 30)
        exit_control.write_private_json(pointer, saved)
        self.save_bond()
        path = exit_control.outbox_path(self.values, "validator_bond", 30)
        path.rename(path.with_name(path.stem + "-" + self.hash + ".json"))
        exit_control.write_private_json(pointer, saved)
        view = exit_control.pointer_status(self.values, self.wallet)
        self.assertEqual(view["status"], "invalid")
        self.assertIn("restore control records", view["reason"])

    def test_private_write_paths(self):
        source = self.work / "source"
        source.write_bytes(b"preserved")
        source.chmod(0o600)
        writers = {
            "json": lambda target: common.write_private_json(target, {"next": True}),
            "env": lambda target: common.write_env(target, {"NEXT": "true"}),
            "copy": lambda target: common.copy_private(source, target),
            "wallet": common.ensure_wallet,
        }
        for name, write in writers.items():
            target = self.work / name
            staged = target.with_name(target.name + ".new")
            for kind in ("symlink", "hardlink", "fifo", "public"):
                with self.subTest(writer = name, kind = kind):
                    if kind == "symlink":
                        staged.symlink_to(source)
                    elif kind == "hardlink":
                        os.link(source, staged)
                    elif kind == "fifo":
                        os.mkfifo(staged, 0o600)
                    else:
                        staged.write_bytes(b"partial")
                        staged.chmod(0o644)
                    try:
                        with self.assertRaisesRegex(ValidatorError, str(staged)):
                            write(target)
                        self.assertFalse(target.exists())
                        self.assertEqual(source.read_bytes(), b"preserved")
                    finally:
                        staged.unlink()
            staged.write_bytes(b"partial")
            staged.chmod(0o600)
            write(target)
            self.assertTrue(target.is_file())
            self.assertEqual(target.stat().st_mode & 0o777, 0o600)
            self.assertFalse(staged.exists())

    def test_pointer_status(self):
        self.values["OCTRA_OPERATOR_ROLE"] = "validator"
        cases = ("empty", "signed", "missing", "invalid")
        for state in cases:
            with self.subTest(state = state):
                root = self.work / "validator-control"
                if root.exists():
                    shutil.rmtree(root)
                if state != "empty":
                    self.save_bond()
                if state == "missing":
                    (root / "last.json").unlink()
                if state == "invalid":
                    exit_control.write_private_json(root / "last.json", {"chain_id": "other"})
                before = {path.name: path.read_bytes() for path in root.glob("*")}
                with mock.patch.object(enroll, "call", side_effect = ValidatorError("offline")), mock.patch.object(
                    enroll, "emit",
                ) as emit:
                    with self.assertRaisesRegex(ValidatorError, "offline"):
                        enroll.show_status(self.work / "node.env", self.values, self.wallet)
                rows = [call.kwargs for call in emit.call_args_list if call.kwargs.get("event") == "validator_pointer"]
                self.assertEqual(len(rows), 1)
                self.assertEqual(rows[0]["status"], state)
                self.assertEqual(before, {path.name: path.read_bytes() for path in root.glob("*")})
                if state == "empty":
                    self.assertFalse(root.exists())

    def test_pointer_backup_checks(self):
        self.save_bond()
        pointer = exit_control.directory(self.values) / "last.json"
        value = exit_control.read_outbox(pointer)
        backup = self.work / "last.backup"
        pointer.unlink()
        for patch in ({"chain_id": "other"}, {"tx_hash": "e" * 64}, {"extra": True},
                      {"head_epoch": True}, {"operation": "validator_ready"},
                      {"tx_hash": None, "head_epoch": 29}):
            with self.subTest(patch = patch):
                exit_control.write_private_json(backup, {**value, **patch})
                with self.assertRaises(ValidatorError):
                    exit_control.repair_pointer(self.values, self.wallet, backup)
                self.assertFalse(pointer.exists())
        exit_control.write_private_json(backup, value)
        backup.chmod(0o644)
        with self.assertRaisesRegex(ValidatorError, "outbox file"):
            exit_control.repair_pointer(self.values, self.wallet, backup)

    def test_pointer_requires_backup(self):
        args = enroll.parser().parse_args(["repair-pointer"])
        with mock.patch.object(enroll, "parser") as parser, mock.patch.object(enroll, "parse_env") as read:
            parser.return_value.parse_args.return_value = args
            with self.assertRaisesRegex(ValidatorError, "latest private last.json backup"):
                enroll.main()
            read.assert_not_called()

    def test_pointer_partial_tx(self):
        self.save_bond()
        root = exit_control.directory(self.values)
        pointer = root / "last.json"
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        path = exit_control.outbox_path(self.values, "validator_bond", 30)
        saved = exit_control.read_outbox(path)
        for field in ("signature", "timestamp", "ou"):
            with self.subTest(field = field):
                tx = {key: value for key, value in saved["tx"].items() if key != field}
                exit_control.write_private_json(path, {**saved, "tx": tx})
                with self.assertRaisesRegex(ValidatorError, "incomplete signed transaction"):
                    exit_control.repair_pointer(self.values, self.wallet, backup)
                self.assertFalse(pointer.exists())

    def test_pointer_write_links(self):
        self.save_bond()
        root = exit_control.directory(self.values)
        pointer = root / "last.json"
        backup = self.work / "last.backup"
        exit_control.copy_private(pointer, backup)
        pointer.unlink()
        path = exit_control.outbox_path(self.values, "validator_bond", 30)
        before = path.read_bytes()
        staged = root / "last.json.new"
        for kind in ("symlink", "hardlink", "fifo"):
            with self.subTest(kind = kind):
                if kind == "symlink":
                    staged.symlink_to(path)
                elif kind == "hardlink":
                    os.link(path, staged)
                else:
                    os.mkfifo(staged, 0o600)
                with self.assertRaises((ValidatorError, OSError)):
                    exit_control.repair_pointer(self.values, self.wallet, backup)
                self.assertFalse(pointer.exists())
                self.assertEqual(path.read_bytes(), before)
                staged.unlink()

    def test_join_renew_cli(self):
        args = enroll.parser().parse_args(["join", "--renew"])
        with mock.patch.object(enroll, "parser") as parser, mock.patch.object(
            enroll, "private_mode",
        ), mock.patch.object(enroll, "parse_env", return_value = self.values), mock.patch.object(
            enroll, "load_wallet", return_value = self.wallet,
        ), mock.patch.object(enroll, "run_command", return_value = False) as run:
            parser.return_value.parse_args.return_value = args
            enroll.main()
        self.assertTrue(run.call_args.args[0].renew)

    def test_rpc_read_errors(self):
        for value, missing in (({"error": {"code": 112}}, True),
                               ({"error": {"code": 429}}, False),
                               ({"result": []}, False), ({}, False),
                               ({"result": None}, False), ({"result": {}}, False),
                               ({"result": {"status": "confirmed"}}, False),
                               ({"result": {"status": "pending", "tx_hash": "d" * 64}}, False),
                               ({"result": {"status": "nonsense"}}, False)):
            with self.subTest(value = value), mock.patch.object(
                rpc_client.urllib.request, "urlopen", return_value = io.BytesIO(json.dumps(value).encode()),
            ):
                if missing:
                    self.assertIsNone(rpc_client.transaction(self.url, self.hash))
                else:
                    with self.assertRaises(ValidatorError):
                        rpc_client.transaction(self.url, self.hash)
        with mock.patch.object(rpc_client.urllib.request, "urlopen", side_effect = OSError("offline")):
            with self.assertRaisesRegex(ValidatorError, "RPC unavailable"):
                rpc_client.transaction(self.url, self.hash)

    def test_rpc_receipt_reads(self):
        for status in ("confirmed", "rejected", "dropped", "pending", "staging"):
            value = {"status": status, "tx_hash": self.hash}
            with self.subTest(status = status), mock.patch.object(
                rpc_client.urllib.request, "urlopen", return_value = io.BytesIO(json.dumps({"result": value}).encode()),
            ):
                self.assertEqual(rpc_client.transaction(self.url, self.hash), value)

    def test_rpc_short_body(self):
        for error in (http.client.IncompleteRead(b"part", 40), http.client.BadStatusLine("broken")):
            with self.subTest(error = type(error).__name__), mock.patch.object(
                rpc_client.urllib.request, "urlopen",
            ) as open_url:
                open_url.return_value.__enter__.return_value.read.side_effect = error
                with self.assertRaisesRegex(ValidatorError, "RPC unavailable"):
                    rpc_client.transaction(self.url, self.hash)

    def test_rpc_poll_retry(self):
        receipt = {"status": "confirmed"}
        with mock.patch.object(rpc_client, "transaction", side_effect = [
            ValidatorError("RPC unavailable"), None, receipt,
        ]), mock.patch.object(rpc_client.time, "sleep"):
            self.assertEqual(rpc_client.wait_transaction(self.url, self.hash, 10, 1), receipt)

    def test_rpc_poll_error(self):
        with mock.patch.object(rpc_client, "transaction", side_effect = ValidatorError("RPC unavailable")), mock.patch.object(
            rpc_client.time, "monotonic", side_effect = [0, 1, 11],
        ), mock.patch.object(rpc_client.time, "sleep"), self.assertRaisesRegex(ValidatorError, "last read error: RPC unavailable"):
            rpc_client.wait_transaction(self.url, self.hash, 10, 1)

    def test_rpc_error_no_send(self):
        self.save_bond()
        path = exit_control.outbox_path(self.values, "validator_bond", 30)
        before = path.read_bytes()
        with mock.patch.object(exit_control, "transaction", side_effect = ValidatorError("RPC unavailable")), mock.patch.object(
            exit_control, "call",
        ) as rpc, mock.patch.object(enroll, "control_result") as sign, self.assertRaisesRegex(ValidatorError, "RPC unavailable"):
            exit_control.submit(self.values, self.wallet, "validator_bond", 30,
                                self.url, sign, renew = True, amount = 1000000)
        rpc.assert_not_called()
        sign.assert_not_called()
        self.assertEqual(path.read_bytes(), before)

    def test_control_renew_payment(self):
        prior = {**self.tx, "ou": "12000"}
        result = subprocess.CompletedProcess([], 0, json.dumps({"tx_hash": self.hash, "tx": prior}), "")
        with mock.patch.object(enroll, "sha256_file", return_value = "c" * 64), mock.patch.object(
            exit_control, "require_prepare",
        ), mock.patch.object(enroll, "next_nonce", return_value = 5) as nonce, mock.patch.object(
            enroll.subprocess, "run", return_value = result,
        ) as run:
            enroll.control_result(self.values, self.wallet, self.work / "wallet.json",
                                  "validator_exit", prepare = True, prior = prior)
        nonce.assert_called_once_with(self.values, self.wallet)
        command = run.call_args.args[0]
        self.assertEqual(command[command.index("--nonce") + 1], "5")
        self.assertEqual(command[command.index("--ou") + 1], "12000")
        for fresh in (6, ValidatorError("account has pending transactions")):
            with self.subTest(fresh = str(fresh)), mock.patch.object(
                enroll, "sha256_file", return_value = "c" * 64,
            ), mock.patch.object(exit_control, "require_prepare"), mock.patch.object(
                enroll, "next_nonce", side_effect = [fresh],
            ), mock.patch.object(enroll.subprocess, "run") as run, self.assertRaises(ValidatorError):
                enroll.control_result(self.values, self.wallet, self.work / "wallet.json",
                                      "validator_exit", prepare = True, prior = prior)
            run.assert_not_called()

    def test_missing_outbox(self):
        for operation in ("validator_bond", "validator_exit", "validator_withdraw"):
            amount = 1000000 if operation == "validator_bond" else 0
            tx = {**self.tx, "op_type": operation, "amount": str(amount),
                  "to_": exit_control.BOND_ESCROW if amount else self.wallet["address"]}
            prepared = {"tx": tx, "tx_hash": self.hash}
            path = exit_control.outbox_path(self.values, operation, 1)
            with mock.patch.object(exit_control, "transaction", return_value = {"status": "pending"}):
                exit_control.submit(self.values, self.wallet, operation, 1,
                                    self.url, lambda: prepared, amount = amount)
            saved = path.read_bytes()
            path.unlink()
            for evidence in ("pointer", "archive"):
                with self.subTest(operation = operation, evidence = evidence):
                    if evidence == "archive":
                        (path.parent / "last.json").unlink()
                        archive = path.with_name(path.stem + "-" + self.hash + ".json")
                        archive.write_bytes(saved)
                        archive.chmod(0o600)
                    with mock.patch.object(exit_control, "call") as rpc, mock.patch.object(
                        enroll, "control_result",
                    ) as sign, self.assertRaisesRegex(ValidatorError, "restore control"):
                        exit_control.submit(self.values, self.wallet, operation, 1,
                                            self.url, sign, renew = True, amount = amount)
                    sign.assert_not_called()
                    rpc.assert_not_called()
                    if operation == "validator_exit":
                        with self.assertRaisesRegex(ValidatorError, "restore control"):
                            exit_control.allow_resume(self.values, self.wallet, 1, self.url)

    def test_renew_prior_pending(self):
        _, path = self.renew_withdraw()
        before = path.read_bytes()
        for status in ("pending", "staging", "confirmed"):
            with self.subTest(status = status), mock.patch.object(
                exit_control, "transaction", side_effect = lambda _url, key:
                    {"status": status if key == self.hash else "rejected"},
            ), mock.patch.object(exit_control, "call") as rpc:
                prepare = mock.Mock()
                self.assertEqual(exit_control.submit(
                    self.values, self.wallet, "validator_withdraw", 1, self.url,
                    prepare, renew = True,
                ), self.hash)
                prepare.assert_not_called()
                rpc.assert_not_called()
                self.assertEqual(path.read_bytes(), before)

    def test_intent_ack(self):
        result = mock.Mock(returncode=0, stdout=json.dumps(
            {"status": "request", "intent_id": self.id},
        ))
        with mock.patch.object(exit_control, "call", side_effect=[
            self.view(), self.view(self.id),
        ]), mock.patch.object(exit_control, "sha256_file", return_value="c" * 64), mock.patch(
            "validator_exit.subprocess.run", return_value=result,
        ) as run:
            self.assertEqual(exit_control.intent(
                self.values, self.wallet, self.work / "wallet.json", 1, self.url, "request",
            ), self.id)
            self.assertIn("--exit-intent", run.call_args.args[0])

    def test_intent_refusals(self):
        views = [
            {**self.view(), "local_control": None},
            {**self.view(), "chain_id": "other"},
            {**self.view(), "bonded_epoch": "2"},
            {**self.view(), "local_control": {"exit_intent": True, "error": "invalid file"}},
            self.view("invalid"),
        ]
        for value in views:
            with self.subTest(value=value), mock.patch.object(
                exit_control, "call", return_value=value,
            ), mock.patch("validator_exit.subprocess.run") as run:
                with self.assertRaises(ValidatorError):
                    exit_control.intent(self.values, self.wallet, self.work / "wallet.json",
                                        1, self.url, "request")
                run.assert_not_called()

    def test_unacknowledged_intent(self):
        result = mock.Mock(returncode=0, stdout=json.dumps(
            {"status": "request", "intent_id": self.id},
        ))
        with mock.patch.object(exit_control, "call", return_value=self.view()), mock.patch.object(
            exit_control, "sha256_file", return_value="c" * 64,
        ), mock.patch("validator_exit.subprocess.run", return_value=result):
            with self.assertRaisesRegex(ValidatorError, "not acknowledged"):
                exit_control.intent(self.values, self.wallet, self.work / "wallet.json",
                                    1, self.url, "request")

    def test_submission_restart(self):
        prepare = mock.Mock(return_value=self.prepared)
        with mock.patch.object(exit_control, "transaction", return_value=None), mock.patch.object(
            exit_control, "call", side_effect=[{"nonce": 4}, ValidatorError("lost response")],
        ):
            with self.assertRaisesRegex(ValidatorError, "signed transaction retained"):
                self.submit(prepare)
        path = exit_control.outbox_path(self.values, "validator_exit", 1)
        self.assertEqual(json.loads(path.read_text())["tx"], self.tx)
        with mock.patch.object(exit_control, "transaction", return_value=None), mock.patch.object(
            exit_control, "call", side_effect=[
                {"nonce": "4"}, {"status": "accepted", "tx_hash": self.hash},
            ],
        ) as rpc:
            self.assertEqual(self.submit(prepare), self.hash)
            self.assertEqual(rpc.call_args.args[2], [self.tx])
        prepare.assert_called_once()

    def test_saved_pending(self):
        prepare = mock.Mock(return_value=self.prepared)
        with mock.patch.object(exit_control, "transaction", return_value={"status": "pending"}):
            self.assertEqual(self.submit(prepare), self.hash)
            self.assertEqual(self.submit(prepare), self.hash)
        prepare.assert_called_once()

    def withdraw_attempt(self, epoch = 1):
        tx = {**self.tx, "op_type": "validator_withdraw"}
        with mock.patch.object(exit_control, "transaction", return_value = {"status": "pending"}):
            exit_control.submit(self.values, self.wallet, "validator_withdraw", epoch,
                                self.url, lambda: {"tx": tx, "tx_hash": self.hash})
        return {
            "status": "confirmed", "tx_hash": self.hash, "epoch": 20,
            "from": self.wallet["address"], "to": self.wallet["address"],
            "op_type": "validator_withdraw", "nonce": 5, "amount_raw": "0",
        }

    def test_withdraw_complete(self):
        receipt = self.withdraw_attempt()
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 20, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            exit_control, "transaction", return_value = receipt,
        ), mock.patch.object(exit_control, "call", return_value = {"nonce": "5"}) as rpc, mock.patch.object(
            enroll, "control_result",
        ) as sign, mock.patch.object(enroll, "record_transaction"), mock.patch.object(enroll, "emit") as emit:
            for _ in range(2):
                self.assertEqual(enroll.submit_self(
                    self.work / "node.env", self.values, self.wallet, self.work / "wallet.json",
                    "validator_withdraw", SimpleNamespace(no_wait = True, renew = True),
                ), self.hash)
            sign.assert_not_called()
            self.assertTrue(all(call.args[1] == "octra_balance" for call in rpc.call_args_list))
            emit.assert_called_with(event = "validator_withdraw", status = "previously_confirmed",
                                    tx = self.hash, bonded_epoch = 1, action = "nothing_submitted")

    def test_withdraw_later_nonce(self):
        receipt = self.withdraw_attempt()
        with mock.patch.object(exit_control, "transaction", return_value = receipt), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 7},
        ), self.assertRaisesRegex(ValidatorError, "advanced"):
            exit_control.completed_withdraw(self.values, self.wallet, self.url, 30)

    def test_bond_clears_attempt(self):
        self.withdraw_attempt()
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 30, None, None, None, None)
        def sign(*args, **kwargs):
            self.assertIsNone(exit_control.completed_withdraw(
                self.values, self.wallet, self.url, 30,
            ))
            raise ValidatorError("signing interrupted")
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            enroll, "control_result", side_effect = sign,
        ), mock.patch.object(exit_control, "transaction") as read, mock.patch.object(
            exit_control, "call", return_value = {"nonce": 5},
        ):
            with self.assertRaisesRegex(ValidatorError, "signing interrupted"):
                enroll.submit_bond(self.work / "node.env", self.values, self.wallet,
                                   self.work / "wallet.json", 1000000, mock.Mock())
            read.assert_not_called()
        self.assertIsNotNone(exit_control.read_outbox(
            exit_control.outbox_path(self.values, "validator_withdraw", 1),
        ))

    def renew_withdraw(self):
        receipt = self.withdraw_attempt()
        path = exit_control.outbox_path(self.values, "validator_withdraw", 1)
        saved = exit_control.read_outbox(path)
        renewed = {**saved, "tx_hash": "d" * 64,
                   "tx": {**saved["tx"], "timestamp": 2.0, "signature": "new"}}
        with mock.patch.object(exit_control, "transaction", return_value = {"status": "missing"}), mock.patch.object(
            exit_control, "call", side_effect = [
                {"nonce": 4}, {"status": "accepted", "tx_hash": renewed["tx_hash"]},
            ],
        ):
            exit_control.submit(self.values, self.wallet, "validator_withdraw", 1,
                                self.url, lambda prior: renewed, renew = True)
        latest = exit_control.read_outbox(path.parent / "last.json")
        self.assertEqual(latest["tx_hash"], renewed["tx_hash"])
        return receipt, path

    def test_renew_prior_receipt(self):
        receipt, _ = self.renew_withdraw()
        def lookup(url, tx_hash):
            return receipt if tx_hash == self.hash else {"status": "missing"}
        with mock.patch.object(exit_control, "transaction", side_effect = lookup), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 5},
        ):
            self.assertEqual(exit_control.completed_withdraw(self.values, self.wallet, self.url, 20),
                             (self.hash, 1))

    def test_renew_pointer_loss(self):
        receipt, path = self.renew_withdraw()
        latest = exit_control.read_outbox(path.parent / "last.json")
        exit_control.write_private_json(path.parent / "last.json", {**latest, "tx_hash": self.hash})
        def lookup(url, tx_hash):
            return receipt if tx_hash == self.hash else {"status": "missing"}
        with mock.patch.object(exit_control, "transaction", side_effect = lookup), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 5},
        ):
            self.assertEqual(exit_control.completed_withdraw(self.values, self.wallet, self.url, 20),
                             (self.hash, 1))

    def test_renew_archive_identity(self):
        _, path = self.renew_withdraw()
        archive = path.with_name(path.stem + "-" + self.hash + ".json")
        value = exit_control.read_outbox(archive)
        exit_control.write_private_json(archive, {**value, "bonded_epoch": 2})
        with mock.patch.object(exit_control, "transaction") as read, self.assertRaises(ValidatorError):
            exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)
        read.assert_not_called()

    def test_renew_double_receipt(self):
        receipt, _ = self.renew_withdraw()
        with mock.patch.object(exit_control, "transaction", side_effect = lambda url, tx_hash:
            {**receipt, "tx_hash": tx_hash}
        ), mock.patch.object(exit_control, "call", return_value = {"nonce": 5}):
            with self.assertRaisesRegex(ValidatorError, "multiple"):
                exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)

    def test_renew_new_receipt(self):
        receipt, path = self.renew_withdraw()
        current = exit_control.read_outbox(path)
        def lookup(url, tx_hash):
            return {**receipt, "tx_hash": tx_hash} if tx_hash == current["tx_hash"] else None
        with mock.patch.object(exit_control, "transaction", side_effect = lookup), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 5},
        ):
            self.assertEqual(exit_control.completed_withdraw(self.values, self.wallet, self.url, 20),
                             (current["tx_hash"], 1))

    def test_renew_limits(self):
        _, path = self.renew_withdraw()
        saved = exit_control.read_outbox(path)
        prepare = mock.Mock()
        with mock.patch.object(exit_control, "ATTEMPT_LIMIT", 2), self.assertRaisesRegex(
            ValidatorError, "not signed",
        ):
            exit_control.renew_outbox(path, saved, self.values, self.wallet,
                                     "validator_withdraw", 1, prepare)
        prepare.assert_not_called()
        with mock.patch.object(exit_control, "ATTEMPT_LIMIT", 1), mock.patch.object(
            exit_control, "transaction",
        ) as read, self.assertRaisesRegex(ValidatorError, "limit exceeded"):
            exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)
        read.assert_not_called()

    def test_renew_archive_fields(self):
        _, path = self.renew_withdraw()
        archive = path.with_name(path.stem + "-" + self.hash + ".json")
        saved = exit_control.read_outbox(archive)
        for field, value in [("nonce", 6), ("ou", "2000"), ("from", "other")]:
            exit_control.write_private_json(archive, {**saved, "tx": {**saved["tx"], field: value}})
            with self.subTest(field = field), mock.patch.object(
                exit_control, "transaction",
            ) as read, self.assertRaises(ValidatorError):
                exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)
            read.assert_not_called()

    def test_renew_same_bytes(self):
        self.withdraw_attempt()
        path = exit_control.outbox_path(self.values, "validator_withdraw", 1)
        saved = exit_control.read_outbox(path)
        self.assertEqual(exit_control.renew_outbox(
            path, saved, self.values, self.wallet, "validator_withdraw", 1, lambda prior: saved,
        ), saved)
        self.assertEqual(list(path.parent.glob(path.stem + "-*.json")), [])

    def test_bond_write_failure(self):
        self.withdraw_attempt()
        absent = enroll.Enrollment(enroll.EnrollmentState.ABSENT, 30, None, None, None, None)
        with mock.patch.object(enroll, "committed_enrollment", return_value = absent), mock.patch.object(
            exit_control, "write_private_json", side_effect = OSError("write failed"),
        ), mock.patch.object(enroll, "control_result") as sign, mock.patch.object(
            exit_control, "call", return_value = {"nonce": 5},
        ), self.assertRaises(OSError):
            enroll.submit_bond(self.work / "node.env", self.values, self.wallet,
                               self.work / "wallet.json", 1000000, mock.Mock())
        sign.assert_not_called()

    def test_bond_sync_retry(self):
        exit_control.begin_bond(self.values, self.wallet)
        path = exit_control.directory(self.values)
        with mock.patch.object(exit_control, "directory", return_value = path), mock.patch.object(
            exit_control, "sync_directory", side_effect = OSError("sync failed"),
        ) as sync, self.assertRaisesRegex(OSError, "sync failed"):
            exit_control.begin_bond(self.values, self.wallet)
        sync.assert_called_once_with(path)

    def test_python_gate(self):
        script = enroll.ROOT / "test" / "python_check.py"
        for version in [(3, 8), (3, 9)]:
            with self.subTest(version = version), mock.patch("sys.version_info", version):
                with self.assertRaisesRegex(SystemExit, "minimum = 3.10"):
                    runpy.run_path(str(script), run_name = "__main__")
        for version in [(3, 10), (3, 13)]:
            with self.subTest(version = version), mock.patch("sys.version_info", version):
                runpy.run_path(str(script), run_name = "__main__")

    def test_withdraw_receipt(self):
        receipt = self.withdraw_attempt()
        changes = [
            {"status": "pending"}, {"status": "rejected"}, {"tx_hash": "d" * 64},
            {"from": "other"}, {"to": "other"}, {"op_type": "validator_exit"},
            {"nonce": 6}, {"nonce": "5"}, {"amount_raw": "1"},
            {"epoch": 0}, {"epoch": 21}, {"epoch": True}, {"epoch": "20"},
        ]
        for value in [None, {}, *({**receipt, **change} for change in changes)]:
            with self.subTest(value = value), mock.patch.object(
                exit_control, "transaction", return_value = value,
            ), mock.patch.object(exit_control, "call") as rpc:
                with self.assertRaises(ValidatorError):
                    exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)
                rpc.assert_not_called()
        with mock.patch.object(exit_control, "transaction", return_value = receipt):
            for nonce in [None, -1, 4, True, 5.0, "invalid"]:
                with self.subTest(nonce = nonce), mock.patch.object(
                    exit_control, "call", return_value = {"nonce": nonce},
                ), self.assertRaisesRegex(ValidatorError, "nonce consumption"):
                    exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)

    def test_withdraw_identity(self):
        self.withdraw_attempt()
        path = exit_control.directory(self.values) / "last.json"
        original = exit_control.read_outbox(path)
        changes = [
            {"chain_id": "other"}, {"address": "other"}, {"pubkey": "other"},
            {"bonded_epoch": True}, {"bonded_epoch": 2}, {"operation": "standard"},
            {"tx_hash": "d" * 64},
        ]
        for change in changes:
            exit_control.write_private_json(path, {**original, **change})
            with self.subTest(change = change), mock.patch.object(exit_control, "transaction") as read:
                with self.assertRaises(ValidatorError):
                    exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)
                read.assert_not_called()

    def test_withdraw_private(self):
        self.withdraw_attempt()
        directory = exit_control.directory(self.values)
        for name in ["last.json", "validator_withdraw-1.json"]:
            path = directory / name
            path.chmod(0o644)
            with self.subTest(name = name), self.assertRaisesRegex(ValidatorError, "outbox file"):
                exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)
            path.chmod(0o600)
        path = directory / "last.json"
        path.unlink()
        path.symlink_to(directory / "validator_withdraw-1.json")
        with self.assertRaises(OSError):
            exit_control.completed_withdraw(self.values, self.wallet, self.url, 20)

    def test_withdraw_last_attempt(self):
        self.withdraw_attempt()
        with mock.patch.object(exit_control, "transaction", return_value = {"status": "pending"}):
            exit_control.submit(self.values, self.wallet, "validator_exit", 2,
                                self.url, lambda: self.prepared)
        with mock.patch.object(exit_control, "transaction") as read:
            self.assertIsNone(exit_control.completed_withdraw(self.values, self.wallet, self.url, 20))
            read.assert_not_called()

    def test_withdraw_restore(self):
        receipt = self.withdraw_attempt()
        data = self.work / "restored"
        data.mkdir()
        exit_control.restore_control(self.work, data)
        values = {**self.values, "OCTRA_DATA_DIR": str(data)}
        with mock.patch.object(exit_control, "transaction", return_value = receipt), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 5},
        ):
            self.assertEqual(exit_control.completed_withdraw(values, self.wallet, self.url, 20),
                             (self.hash, 1))

    def test_attempt_before_post(self):
        prepare = mock.Mock(return_value = self.prepared)
        write = exit_control.write_private_json
        def save(path, value):
            if path.name == "last.json":
                raise OSError("attempt fsync failure")
            write(path, value)
        with mock.patch.object(exit_control, "write_private_json", side_effect = save), mock.patch.object(
            exit_control, "call",
        ) as rpc, self.assertRaisesRegex(OSError, "fsync"):
            self.submit(prepare)
        rpc.assert_not_called()
        self.assertTrue(exit_control.outbox_path(self.values, "validator_exit", 1).exists())
        with mock.patch.object(exit_control, "transaction", return_value = {"status": "pending"}):
            self.assertEqual(self.submit(prepare), self.hash)
        prepare.assert_called_once()

    def test_outbox_no_resign(self):
        prepare = mock.Mock(return_value=self.prepared)
        with mock.patch.object(exit_control, "transaction", return_value=None), mock.patch.object(
            exit_control, "call", return_value={"nonce": 6},
        ):
            with self.assertRaisesRegex(ValidatorError, "nonce differs"):
                self.submit(prepare)
        with mock.patch.object(exit_control, "transaction", return_value={"status": "rejected"}):
            with self.assertRaisesRegex(ValidatorError, "saved exit transaction"):
                self.submit(prepare)
        prepare.assert_called_once()

    def test_outbox_identity(self):
        bad = {**self.prepared, "tx": {**self.tx, "to_": "octOther"}}
        with self.assertRaisesRegex(ValidatorError, "transaction differs"):
            self.submit(lambda: bad)
        path = exit_control.outbox_path(self.values, "validator_exit", 1)
        self.assertFalse(path.exists())

    def test_command_lock(self):
        with exit_control.command_lock(self.values):
            with self.assertRaisesRegex(ValidatorError, "another enrollment"):
                with exit_control.command_lock(self.values):
                    self.fail("second command entered")
        with exit_control.command_lock(self.values):
            pass

    def test_lock_survives_move(self):
        data = self.work / "data"
        data.mkdir()
        values = {**self.values, "OCTRA_DATA_DIR": str(data)}
        with exit_control.command_lock(values):
            data.rename(self.work / "prior")
            data.mkdir()
            with self.assertRaisesRegex(ValidatorError, "another enrollment"):
                with exit_control.command_lock(values):
                    self.fail("recovery lost lock")

    def test_restore_control(self):
        directory = exit_control.directory(self.values)
        intent = directory / "exit.json"
        intent.write_text('{"signed":"intent"}')
        intent.chmod(0o600)
        with mock.patch.object(exit_control, "transaction", return_value={"status": "pending"}):
            self.submit(lambda: self.prepared)
        next_data = self.work / "next"
        next_data.mkdir()
        exit_control.restore_control(self.work, next_data)
        restored = next_data / "validator-control"
        self.assertEqual(json.loads((restored / "exit.json").read_text()), {"signed": "intent"})
        self.assertEqual(exit_control.read_outbox(restored / "validator_exit-1.json")["tx"], self.tx)
        self.assertEqual(restored.stat().st_mode & 0o777, 0o700)
        self.assertEqual((restored / "exit.json").stat().st_mode & 0o777, 0o600)
        with self.assertRaisesRegex(ValidatorError, "snapshot contains"):
            exit_control.restore_control(self.work, next_data)

    def test_restore_refuses_link(self):
        directory = exit_control.directory(self.values)
        (directory / "exit.json").symlink_to(self.work / "elsewhere")
        next_data = self.work / "next"
        next_data.mkdir()
        with self.assertRaises(OSError):
            exit_control.restore_control(self.work, next_data)

    def test_exit_order(self):
        events = []
        def pause(*args):
            events.append("pause")
        def submit(*args, **kwargs):
            events.append("submit")
            return self.hash
        with mock.patch.object(enroll, "committed_enrollment", return_value=self.ready), mock.patch.object(
            exit_control, "intent", side_effect=pause,
        ), mock.patch.object(exit_control, "submit", side_effect=submit), mock.patch.object(
            enroll, "record_transaction",
        ), mock.patch.object(enroll, "wait_confirmed"), mock.patch.object(enroll, "emit"):
            enroll.submit_self(self.work / "node.env", self.values, self.wallet,
                               self.work / "wallet.json", "validator_exit", mock.Mock())
        self.assertEqual(events, ["pause", "submit"])

    def test_exit_ack_required(self):
        with mock.patch.object(enroll, "committed_enrollment", return_value=self.ready), mock.patch.object(
            exit_control, "intent", side_effect=ValidatorError("not acknowledged"),
        ), mock.patch.object(exit_control, "submit") as submit:
            with self.assertRaisesRegex(ValidatorError, "not acknowledged"):
                enroll.submit_self(self.work / "node.env", self.values, self.wallet,
                                   self.work / "wallet.json", "validator_exit", mock.Mock())
            submit.assert_not_called()

    def test_resume_unresolved(self):
        with mock.patch.object(exit_control, "transaction", return_value={"status": "pending"}):
            self.submit(lambda: self.prepared)
            with self.assertRaisesRegex(ValidatorError, "unresolved"):
                exit_control.allow_resume(self.values, self.wallet, 1, self.url)

    def test_resume_confirmed_exit(self):
        exiting = enroll.Enrollment(enroll.EnrollmentState.EXITING, 20, 1000000, 1, 2, 19, 30)
        with mock.patch.object(enroll, "committed_enrollment", return_value=exiting), mock.patch.object(
            exit_control, "intent",
        ) as intent:
            with self.assertRaisesRegex(ValidatorError, "without confirmed exit"):
                enroll.resume_duty(self.values, self.wallet, self.work / "wallet.json")
            intent.assert_not_called()

    def test_resume_prior_attempt(self):
        with mock.patch.object(exit_control, "transaction", return_value = {"status": "pending"}):
            self.submit(lambda: self.prepared)
        path = exit_control.outbox_path(self.values, "validator_exit", 1)
        saved = exit_control.read_outbox(path)
        renewed = {"tx_hash": "d" * 64, "tx": {**self.tx, "timestamp": 2.0, "signature": "new"}}
        exit_control.renew_outbox(path, saved, self.values, self.wallet,
                                 "validator_exit", 1, lambda prior: renewed)
        for status in [None, "confirmed", "pending", "dropped"]:
            def receipt(url, tx_hash):
                return {"status": status if tx_hash == self.hash else "rejected"}
            with self.subTest(status = status), mock.patch.object(
                exit_control, "transaction", side_effect = receipt,
            ), mock.patch.object(exit_control, "call") as rpc:
                with self.assertRaisesRegex(ValidatorError, "unresolved"):
                    exit_control.allow_resume(self.values, self.wallet, 1, self.url)
                rpc.assert_not_called()
        with mock.patch.object(exit_control, "transaction", return_value = {"status": "rejected"}):
            for nonce in [None, True, 5.0, "invalid", 4]:
                with self.subTest(nonce = nonce), mock.patch.object(
                    exit_control, "call", return_value = {"nonce": nonce},
                ), self.assertRaises(ValidatorError):
                    exit_control.allow_resume(self.values, self.wallet, 1, self.url)
            for nonce in [5, "5", 6]:
                with mock.patch.object(exit_control, "call", return_value = {"nonce": nonce}):
                    exit_control.allow_resume(self.values, self.wallet, 1, self.url)

    def test_renew_same_nonce(self):
        with mock.patch.object(exit_control, "transaction", return_value={"status": "pending"}):
            self.submit(lambda: self.prepared)
        renewed = {"tx_hash": "d" * 64, "tx": {**self.tx, "timestamp": 2.0, "signature": "new"}}
        with mock.patch.object(exit_control, "transaction", return_value={"status": "dropped"}), mock.patch.object(
            exit_control, "call", side_effect=[
                {"nonce": 4}, {"status": "accepted", "tx_hash": "d" * 64},
            ],
        ):
            self.assertEqual(exit_control.submit(
                self.values, self.wallet, "validator_exit", 1, self.url,
                lambda prior: renewed, renew = True,
            ), "d" * 64)
        archive = exit_control.directory(self.values) / f"validator_exit-1-{self.hash}.json"
        self.assertEqual(exit_control.read_outbox(archive)["tx"], self.tx)

    def test_renew_no_nonce_change(self):
        with mock.patch.object(exit_control, "transaction", return_value={"status": "pending"}):
            self.submit(lambda: self.prepared)
        with mock.patch.object(exit_control, "transaction", return_value = None), mock.patch.object(
            exit_control, "call", return_value = {"nonce": 4},
        ):
            with self.assertRaisesRegex(ValidatorError, "same nonce"):
                exit_control.submit(
                    self.values, self.wallet, "validator_exit", 1, self.url,
                    lambda prior: {"tx_hash": "d" * 64, "tx": {**self.tx, "nonce": 6}}, renew = True,
                )

    def test_prepare_capability(self):
        for response in [
            mock.Mock(returncode=1, stdout="", stderr="missing wallet"),
            mock.Mock(returncode=0, stdout="{}", stderr=""),
            mock.Mock(returncode=0, stdout='{"prepare_transaction":false}', stderr=""),
        ]:
            with self.subTest(response=response), mock.patch(
                "validator_exit.subprocess.run", return_value=response,
            ) as run, mock.patch.object(enroll, "sha256_file", return_value="c" * 64):
                with self.assertRaisesRegex(ValidatorError, "safe preparation"):
                    enroll.control_result(self.values, self.wallet, self.work / "wallet.json",
                                          "validator_withdraw", prepare=True)
                self.assertEqual(run.call_count, 1)
                self.assertEqual(run.call_args.args[0][-1], "--capabilities")

    def test_restart_releases_lock(self):
        args = SimpleNamespace(
            poll_seconds=1, wait_seconds=1, command="activate", no_wait=False,
            renew=False, config=self.work / "node.env", rpc=None,
        )
        def restart(*args, **kwargs):
            with exit_control.command_lock(self.values):
                pass
        with mock.patch.object(enroll, "parser") as parser, mock.patch.object(
            enroll, "private_mode",
        ), mock.patch.object(enroll, "parse_env", return_value=self.values), mock.patch.object(
            enroll, "load_wallet", return_value=self.wallet,
        ), mock.patch.object(enroll, "run_command", return_value=True), mock.patch(
            "validator_enroll.subprocess.run", side_effect=restart,
        ) as run:
            parser.return_value.parse_args.return_value = args
            enroll.main()
            run.assert_called_once()

if __name__ == "__main__":
    unittest.main()