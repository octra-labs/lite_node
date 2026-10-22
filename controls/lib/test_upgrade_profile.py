# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import unittest
import base64
import json
import os
import shutil
import signal
import subprocess
import sys
import time
import uuid
from contextlib import ExitStack, contextmanager
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
from unittest import mock

import upgrade
from validator_common import ValidatorError
import validator_recover
import validator_config

ROOT = Path(__file__).resolve().parent
MARKER = {
    "sequence": 24,
    "action": "required",
    "chain_id": "octra-devnet-9871-cluster",
    "public_commit": "a" * 40,
    "source_commit": "b" * 40,
    "network_sha256": "f" * 64,
    "runtime_profile_hash": "c" * 64,
    "consensus_profile": 16,
    "consensus_rules_id": "finalized_rejection_commitment",
}
STATE = {
    "process": "online",
    "rpc": "ready",
    "source_match": True,
    "runtime_match": True,
    "binary_match": True,
    "head_epoch": 1_580_000,
    "lag": 0,
    "live_source": MARKER["source_commit"],
    "live_profile": MARKER["runtime_profile_hash"],
    "live_consensus_profile": MARKER["consensus_profile"],
    "live_rules_id": MARKER["consensus_rules_id"],
}
VALUES = {
    "OCTRA_CHAIN_ID": MARKER["chain_id"],
    "OCTRA_DATA_DIR": str(ROOT / "unused-data"),
    "OCTRA_OPERATOR_BINARY": str(ROOT / "unused-node"),
    "OCTRA_OPERATOR_ROLE": "validator",
    "OCTRA_PVAC_VERIFY_WORKER": str(ROOT / "unused-worker"),
}
SUP = {
    "pid": 41, "kind": "pm2", "config": ROOT / "unused.env",
    "active": True, "name": "test-node", "state": "online",
}
ARGS = SimpleNamespace(
    sudo=False, public_commit=None, source_commit=None,
    wait_seconds=1.0, interval=0.01,
)

@contextmanager
def effects(states):
    replies = {
        "preflight": None, "verify_unit": None,
        "git_update": ("a" * 40, "b" * 40, False),
        "verify_release_tree": None, "release_sync_values": VALUES,
        "sync_head": None, "sync_plan": None, "read_need": None,
        "call": None, "parse_env": VALUES,
        "inspect_pending": [], "inspect_votes": [], "stop": None,
        "data_pids": [], "disk_state": None, "restore_cycle": None,
        "wait_node": 0, "sync_wait": {"required": 42},
        "check_built": None,
        "remember_plan": None,
    }
    with ExitStack() as stack:
        mocks = {
            name: stack.enter_context(mock.patch.object(upgrade, name, autospec = True, return_value = value))
            for name, value in replies.items()
        }
        stack.enter_context(mock.patch.object(upgrade, "view", side_effect=states))
        yield mocks

class UpgradeProfileTest(unittest.TestCase):
    def test_root_install(self):
        work = ROOT / "runtime_data"
        work.mkdir(exist_ok = True)
        with TemporaryDirectory(dir = work, prefix = "root-journal-") as directory:
            data = Path(directory)
            finality = data / "finality"
            finality.mkdir()
            height = 1_728_098
            chain = MARKER["chain_id"].encode()
            def journal(epoch, chain = chain):
                raw = len(chain).to_bytes(4, "big") + chain + epoch.to_bytes(8, "big")
                return json.dumps({"schema": "octra_finality_journal",
                    "finalize": base64.b64encode(raw).decode()})
            head = data / "HEAD.json"
            head.write_text(json.dumps({"epoch_id": height,
                "state_root": "a" * 64, "txid_hi": 12}))
            path = finality / "pending_finalized.json"
            path.write_text(journal(height + 1))
            values = {**VALUES, "OCTRA_DATA_DIR": str(data)}
            marked = upgrade.make(MARKER["chain_id"], "root", height + 1, height, None)
            state = {**STATE, "source_match": False, "runtime_match": False,
                "head_epoch": height, "lag": None}
            before = {item.name: item.read_bytes() for item in (head, path)}
            with effects([state] * 4) as calls, mock.patch.object(upgrade, "start", autospec = True) as start:
                calls["read_need"].return_value = marked
                calls["parse_env"].return_value = values
                calls["disk_state"].return_value = upgrade.DiskState(
                    height, "a" * 64, 12, "owner", "public", marked)
                order = mock.Mock()
                for name in ("call", "stop", "disk_state", "wait_node"):
                    order.attach_mock(calls[name], name)
                order.attach_mock(start, "start")
                self.assertEqual(upgrade.apply(ROOT, SUP, values, ARGS, MARKER), 0)
                calls["sync_wait"].assert_not_called()
                calls["restore_cycle"].assert_not_called()
                self.assertEqual([Path(row.args[0][1]).name
                    for row in calls["call"].call_args_list][:2], ["check.sh", "build.sh"])
                calls["stop"].assert_called_once()
                start.assert_called_once_with(ROOT, SUP, SUP["config"], False)
                calls["wait_node"].assert_called_once_with(ROOT, SUP, values, ARGS, MARKER,
                    Path(VALUES["OCTRA_OPERATOR_BINARY"]).resolve(), journal = marked, guard = None)
                names = [call[0] for call in order.mock_calls]
                self.assertEqual(names[-4:], ["stop", "disk_state", "start", "wait_node"])
            self.assertEqual(before, {item.name: item.read_bytes() for item in (head, path)})
            with effects([state]) as calls, mock.patch.object(upgrade, "git_release", return_value = {
                    "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"]}), \
                    mock.patch.object(upgrade, "emit") as emit:
                calls["read_need"].return_value = marked
                self.assertEqual(upgrade.diagnose(ROOT, SUP, values, MARKER), 2)
                self.assertIn("local_journal_check", [call.kwargs.get("reason")
                    for call in emit.call_args_list])
                self.assertNotIn("signed_snapshot_required", [call.kwargs.get("reason")
                    for call in emit.call_args_list])
                calls["call"].assert_not_called()
                calls["stop"].assert_not_called()
            for label, need, contents, plan in (
                ("old_head", upgrade.make(MARKER["chain_id"], "root", height, height - 1, None),
                    journal(height + 1), None),
                ("target", upgrade.make(MARKER["chain_id"], "root", height + 1, height, height + 100),
                    journal(height + 1), None),
                ("range", upgrade.make(MARKER["chain_id"], "range", height + 1, height, height + 100),
                    journal(height + 1), None),
                ("conflict", upgrade.make(MARKER["chain_id"], "conflict", height + 1, height, None),
                    journal(height + 1), None),
                ("missing", marked, None, None),
                ("other_epoch", marked, journal(height + 2), None),
                ("old_journal", marked, journal(height), None),
                ("other_chain", marked, journal(height + 1, b"another-chain"), None),
                ("invalid", marked, "{", None),
                ("short", marked, json.dumps({"schema": "octra_finality_journal",
                    "finalize": "AAAA/w=="}), None),
                ("range_plan", marked, journal(height + 1),
                    upgrade.make(MARKER["chain_id"], "range", height + 1, height, height + 5000)),
            ):
                with self.subTest(case = label):
                    if path.exists():
                        path.unlink()
                    if contents is not None:
                        path.write_text(contents)
                    with effects([state] * 4) as calls, mock.patch.object(upgrade, "start", autospec = True) as start:
                        calls["read_need"].return_value = need
                        calls["parse_env"].return_value = values
                        calls["remember_plan"].return_value = plan
                        self.assertEqual(upgrade.apply(ROOT, SUP, values, ARGS, MARKER), 0)
                        calls["sync_wait"].assert_called_once()
                        calls["restore_cycle"].assert_called_once()
                        start.assert_not_called()
                    if contents is not None:
                        self.assertEqual(path.read_text(), contents)
            path.write_text(journal(height + 1))
            with effects([state] * 4) as calls, mock.patch.object(upgrade, "start", autospec = True) as start:
                calls["read_need"].return_value = marked
                calls["parse_env"].return_value = values
                def changed(*args):
                    path.write_text(journal(height + 2))
                    return upgrade.DiskState(height, "a" * 64, 12, "owner", "public", marked)
                calls["disk_state"].side_effect = changed
                with self.assertRaisesRegex(ValidatorError, "local journal changed"):
                    upgrade.apply(ROOT, SUP, values, ARGS, MARKER)
                calls["restore_cycle"].assert_not_called()
                start.assert_not_called()
                self.assertEqual(path.read_text(), journal(height + 2))

    def test_journal_restart_positions(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        self.assertTrue(upgrade.journal_restart(need, 100, 101))
        for head, epoch in ((99, 100), (101, 102), (100, 100), (100, 102),
                (True, 101), (100, None), (100, "101")):
            self.assertFalse(upgrade.journal_restart(need, head, epoch))

    def test_start_supervisors(self):
        with mock.patch.object(upgrade.subprocess, "run", autospec = True) as run, \
                mock.patch.object(upgrade, "call", autospec = True) as call:
            upgrade.start(ROOT, SUP, SUP["config"], False)
            run.assert_called_once()
            self.assertEqual(run.call_args.args[0],
                ["sh", str(ROOT / "controls/run.sh"), "--rebind-runtime"])
            self.assertEqual(run.call_args.kwargs["env"]["OCTRA_OPERATOR_CONFIG"], str(SUP["config"]))
            self.assertEqual(run.call_args.kwargs["cwd"], ROOT)
            self.assertTrue(run.call_args.kwargs["check"])
            call.assert_not_called()
            system = {**SUP, "kind": "systemd", "scope": "system"}
            for sudo in (False, True):
                upgrade.start(ROOT, system, SUP["config"], sudo)
                command = ["systemctl", "start", SUP["name"]]
                call.assert_called_with(["sudo", "-n", *command] if sudo else command)
            self.assertEqual(run.call_count, 1)

    def test_journal_wait(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        changed = upgrade.make(MARKER["chain_id"], "range", 101, 100, 5000)
        state = {**STATE, "voting": True, "validator_member": True}
        values = {**VALUES, "OCTRA_STATE_SYNC_SOURCES": "https://unused.test"}
        args = SimpleNamespace(wait_seconds = 3600, interval = 15, sudo = False)
        for label, marks, expected, reason in (
            ("confirmed", [None], 0, None),
            ("checked_later", [need, need, None], 0, None),
            ("unconfirmed", [need] * 20, 2, "local_journal_unconfirmed"),
            ("changed", [changed], 2, "local_journal_changed"),
        ):
            with self.subTest(case = label), ExitStack() as stack:
                clock = [0.0]
                def sleep(seconds):
                    clock[0] += seconds
                stack.enter_context(mock.patch.object(upgrade.time, "monotonic", side_effect = lambda: clock[0]))
                stack.enter_context(mock.patch.object(upgrade.time, "sleep", side_effect = sleep))
                stack.enter_context(mock.patch.object(upgrade, "pm2_entries", return_value = []))
                stack.enter_context(mock.patch.object(upgrade, "current", return_value = SUP))
                stack.enter_context(mock.patch.object(upgrade, "view", return_value = state))
                read = stack.enter_context(mock.patch.object(upgrade, "read_need", side_effect = marks))
                emit = stack.enter_context(mock.patch.object(upgrade, "emit"))
                untouched = [stack.enter_context(mock.patch.object(upgrade, name, autospec = True))
                    for name in ("stop", "start", "restore_cycle", "install_floor", "sync_head", "remember_plan")]
                self.assertEqual(upgrade.wait_node(ROOT, SUP, values, args, MARKER,
                    ROOT / "unused-node", journal = need), expected)
                read.assert_called_with(Path(VALUES["OCTRA_DATA_DIR"]), MARKER["chain_id"])
                for call in untouched:
                    call.assert_not_called()
                self.assertLessEqual(clock[0], 120)
                if reason is not None:
                    self.assertEqual(emit.call_args.kwargs["status"], "hold")
                    self.assertEqual(emit.call_args.kwargs["reason"], reason)
                    self.assertEqual(emit.call_args.kwargs["action"], "keep_records_report_node_log")
                else:
                    self.assertEqual(emit.call_args.kwargs["status"], "validator_active")

    def test_journal_long_start(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        clock = [0.0]
        waiting = {**STATE, "rpc": "unavailable", "head_epoch": None, "lag": None}
        ready = {**STATE, "voting": True, "validator_member": True}
        args = SimpleNamespace(wait_seconds = 3600, interval = 5, sudo = False)
        def sleep(seconds):
            clock[0] += seconds
        def state(*args):
            if clock[0] < 990 or 1010 <= clock[0] < 2000:
                return waiting
            if 1000 <= clock[0] < 1010:
                return {"process": "offline", "rpc": "unavailable"}
            return ready
        with mock.patch.object(upgrade, "pm2_entries", return_value = []), \
                mock.patch.object(upgrade, "current", return_value = SUP), \
                mock.patch.object(upgrade, "view", side_effect = state), \
                mock.patch.object(upgrade, "read_need", side_effect = lambda *args: need if clock[0] < 1000 else None), \
                mock.patch.object(upgrade.time, "monotonic", side_effect = lambda: clock[0]), \
                mock.patch.object(upgrade.time, "sleep", side_effect = sleep), \
                mock.patch.object(upgrade, "stop", autospec = True) as stop, \
                mock.patch.object(upgrade, "emit") as emit:
            self.assertEqual(upgrade.wait_node(ROOT, SUP, VALUES, args, MARKER,
                ROOT / "unused-node", journal = need), 0)
            self.assertEqual(clock[0], 2000)
            self.assertEqual(emit.call_args.kwargs["status"], "validator_active")
            stop.assert_not_called()

    def test_repeat_startup(self):
        values = {**VALUES, "OCTRA_BINARY_HASH": "e" * 64,
            "OCTRA_SOURCE_COMMIT": MARKER["source_commit"]}
        state = {**STATE, "rpc": "unavailable", "live_binary": "e" * 64,
            "head_epoch": None, "lag": None, "source_match": False,
            "runtime_match": False, "binary_match": False, "live_profile": None}
        for need in (None, upgrade.make(MARKER["chain_id"], "root", 101, 100, None)):
            with self.subTest(marker = need), effects([state] * 4) as calls, \
                    mock.patch.object(upgrade, "local_journal", return_value = need is not None), \
                    mock.patch.object(upgrade, "git_release", return_value = {
                        "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"]}), \
                    mock.patch.object(upgrade, "emit") as emit:
                calls["read_need"].return_value = need
                self.assertEqual(upgrade.diagnose(ROOT, SUP, values, MARKER), 2)
                self.assertEqual(emit.call_args.kwargs["status"], "recovering")
                self.assertEqual(emit.call_args.kwargs["action"], "leave_running")
                self.assertEqual(upgrade.apply(ROOT, SUP, values, ARGS, MARKER), 0)
                calls["wait_node"].assert_called_once_with(ROOT, SUP, values, ARGS, MARKER,
                    Path(values["OCTRA_OPERATOR_BINARY"]).resolve(), journal = need, guard = None)
                for name in ("call", "stop", "restore_cycle", "sync_wait", "check_built"):
                    calls[name].assert_not_called()

    def test_startup_identity(self):
        values = {**VALUES, "OCTRA_BINARY_HASH": "e" * 64,
            "OCTRA_SOURCE_COMMIT": MARKER["source_commit"]}
        state = {"process": "online", "rpc": "unavailable", "live_binary": "e" * 64}
        public = MARKER["public_commit"]
        self.assertTrue(upgrade.startup_running(SUP, values, state, MARKER, public))
        for sup, config, observed, commit in (
            ({key: value for key, value in SUP.items() if key != "active"}, values, state, public),
            ({key: value for key, value in SUP.items() if key != "pid"}, values, state, public),
            ({**SUP, "pid": True}, values, state, public),
            ({**SUP, "pid": "41"}, values, state, public),
            ({**SUP, "active": False}, values, state, public),
            ({**SUP, "pid": 0}, values, state, public),
            (SUP, {**values, "OCTRA_SOURCE_COMMIT": "c" * 40}, state, public),
            (SUP, {**values, "OCTRA_BINARY_HASH": "unknown"}, {**state, "live_binary": "unknown"}, public),
            (SUP, values, {**state, "live_binary": None}, public),
            (SUP, values, {**state, "live_binary": "f" * 64}, public),
            (SUP, values, {**state, "process": "offline"}, public),
            (SUP, values, {**state, "rpc": "ready"}, public),
            (SUP, values, state, "c" * 40),
        ):
            self.assertFalse(upgrade.startup_running(sup, config, observed, MARKER, commit))

    def test_repeat_ready_journal(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        with effects([STATE] * 4) as calls, \
                mock.patch.object(upgrade, "local_journal", return_value = True), \
                mock.patch.object(upgrade, "git_release", return_value = {
                    "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"]}), \
                mock.patch.object(upgrade, "emit") as emit:
            calls["read_need"].return_value = need
            self.assertEqual(upgrade.diagnose(ROOT, SUP, VALUES, MARKER), 2)
            self.assertEqual(emit.call_args.kwargs["action"], "leave_running")
            self.assertEqual(upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER), 0)
            calls["wait_node"].assert_called_once()
            calls["call"].assert_not_called()
            calls["stop"].assert_not_called()

    def test_repeat_apply_only(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        values = {**VALUES, "OCTRA_BINARY_HASH": "e" * 64,
            "OCTRA_SOURCE_COMMIT": MARKER["source_commit"]}
        state = {**STATE, "rpc": "unavailable", "live_binary": "e" * 64,
            "source_match": False, "runtime_match": False, "binary_match": False, "live_profile": None}
        with effects([state] * 4) as calls, \
                mock.patch.object(upgrade, "local_journal", return_value = True), \
                mock.patch.object(upgrade, "start", autospec = True):
            calls["read_need"].return_value = need
            calls["disk_state"].return_value = upgrade.DiskState(100, "a" * 64, 12, "owner", "public", need)
            self.assertEqual(upgrade.apply(ROOT, SUP, values, ARGS, MARKER), 0)
            calls["wait_node"].assert_called_once()
            calls["stop"].assert_not_called()
            calls["call"].assert_not_called()

    def test_recovery_phases(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        release = {**MARKER, "notice_code": "consensus_recovery"}
        for label, wait, rpc_at, consumed, ready_at, online, finish, reason in (
            ("startup", 30, 50, 60, 60, True, 30, "startup_deadline"),
            ("offline", 3600, 9999, 9999, 9999, False, 120, "node_offline"),
            ("rpc", 3600, 990, 9999, 9999, True, 1110, "local_journal_unconfirmed"),
            ("consumed", 120, 0, 105, 224, True, 224, None),
            ("health", 120, 0, 105, 226, True, 225, "health_deadline"),
        ):
            with self.subTest(case = label), ExitStack() as stack:
                clock = [0.0]
                def sleep(seconds):
                    clock[0] += seconds
                def state(*args):
                    return {**STATE, "process": "online" if online else "offline",
                        "rpc": "ready" if clock[0] >= rpc_at else "unavailable",
                        "voting": clock[0] >= ready_at, "validator_member": True}
                stack.enter_context(mock.patch.object(upgrade.time, "monotonic", side_effect = lambda: clock[0]))
                stack.enter_context(mock.patch.object(upgrade.time, "sleep", side_effect = sleep))
                stack.enter_context(mock.patch.object(upgrade, "pm2_entries", return_value = []))
                stack.enter_context(mock.patch.object(upgrade, "current", return_value = SUP))
                stack.enter_context(mock.patch.object(upgrade, "view", side_effect = state))
                stack.enter_context(mock.patch.object(upgrade, "read_need",
                    side_effect = lambda *args: need if clock[0] < consumed else None))
                emit = stack.enter_context(mock.patch.object(upgrade, "emit"))
                stop = stack.enter_context(mock.patch.object(upgrade, "stop", autospec = True))
                args = SimpleNamespace(wait_seconds = wait, interval = 1, sudo = False)
                result = upgrade.wait_node(ROOT, SUP, VALUES, args, release, ROOT / "unused-node", journal = need)
                self.assertEqual(result, 0 if reason is None else 2)
                self.assertEqual(clock[0], finish)
                self.assertEqual(emit.call_args.kwargs.get("reason"), reason)
                if not online:
                    self.assertEqual(emit.call_args.kwargs["action"], "operator_start")
                stop.assert_not_called()

    def test_journal_blocks_actions(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        values = {**VALUES, "OCTRA_STATE_SYNC_SOURCES": "https://unused.test"}
        for lag in (None, 0):
            with self.subTest(lag = lag), ExitStack() as stack:
                clock = [0.0]
                def sleep(seconds):
                    clock[0] += seconds
                state = {**STATE, "lag": lag, "voting": False, "voting_reason": "vote_log_bootstrap",
                    "round_epoch": STATE["head_epoch"] + 1, "round": 0}
                stack.enter_context(mock.patch.object(upgrade.time, "monotonic", side_effect = lambda: clock[0]))
                stack.enter_context(mock.patch.object(upgrade.time, "sleep", side_effect = sleep))
                stack.enter_context(mock.patch.object(upgrade, "pm2_entries", return_value = []))
                stack.enter_context(mock.patch.object(upgrade, "current", return_value = SUP))
                stack.enter_context(mock.patch.object(upgrade, "view", return_value = state))
                stack.enter_context(mock.patch.object(upgrade, "read_need", return_value = need))
                stack.enter_context(mock.patch.object(upgrade, "emit"))
                effects = [stack.enter_context(mock.patch.object(upgrade, name, autospec = True,
                    side_effect = AssertionError("unexpected recovery effect")))
                    for name in ("stop", "start", "restore_cycle", "install_floor", "sync_head", "remember_plan")]
                args = SimpleNamespace(wait_seconds = 30, interval = 1, sudo = False)
                self.assertEqual(upgrade.wait_node(ROOT, SUP, values, args, MARKER,
                    ROOT / "unused-node", journal = need), 2)
                for effect in effects:
                    effect.assert_not_called()

    def test_journal_unit_restart(self):
        need = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        state = {**STATE, "source_match": False, "runtime_match": False}
        for policy, accepted in (
            ({"Restart": "no"}, False),
            ({"Restart": "on-failure"}, True),
            ({"Restart": "always"}, True),
            ({"Restart": "on-failure", "SuccessExitStatus": "75"}, False),
            ({"Restart": "on-success", "SuccessExitStatus": "75"}, True),
            ({"Restart": "always", "RestartPreventExitStatus": "75"}, False),
            ({"Restart": "no", "RestartForceExitStatus": "75"}, True),
        ):
            sup = {**SUP, "kind": "systemd", "restart": {**policy, "RestartPreventExitStatus":
                policy.get("RestartPreventExitStatus", "") + " 78"}}
            with self.subTest(policy = policy), effects([state] * 4) as calls, \
                mock.patch.object(upgrade, "local_journal", return_value = True), \
                mock.patch.object(upgrade, "start", autospec = True) as start:
                calls["read_need"].return_value = need
                calls["disk_state"].return_value = upgrade.DiskState(
                    100, "a" * 64, 12, "owner", "public", need)
                self.assertTrue(upgrade.storage_stop_ready(sup["restart"]))
                if accepted:
                    self.assertEqual(upgrade.apply(ROOT, sup, VALUES, ARGS, MARKER), 0)
                    start.assert_called_once_with(ROOT, sup, sup["config"], False)
                else:
                    with self.assertRaisesRegex(ValidatorError, "does not restart on exit 75"):
                        upgrade.apply(ROOT, sup, VALUES, ARGS, MARKER)
                    start.assert_not_called()
                    for name in ("call", "stop", "restore_cycle", "sync_wait"):
                        calls[name].assert_not_called()

    def test_need_reached(self):
        root = upgrade.make(MARKER["chain_id"], "root", 101, 100, None)
        span = upgrade.make(MARKER["chain_id"], "range", 101, 100, 25_000)
        conflict = upgrade.make(MARKER["chain_id"], "conflict", 101, 100, None)
        for need, head, limit, expected in (
            (None, 25_000, 3000, False), (root, 100, 3000, False),
            (root, 101, 3000, True), (span, 21_999, 3000, False),
            (span, 22_000, 3000, True), (span, 22_001, 3000, True),
            (span, 24_999, 0, False), (span, 25_000, 0, True),
            (conflict, 25_000, 3000, False), (span, None, 3000, False),
            (span, "25000", 3000, False), (root, True, 3000, False),
        ):
            with self.subTest(need = need, head = head, limit = limit):
                self.assertEqual(upgrade.need_reached(need, head, limit), expected)

    def test_snapshot_resume(self):
        work = ROOT / "runtime_data"
        work.mkdir(exist_ok = True)
        read = upgrade.read_need
        remember = upgrade.remember_plan
        with TemporaryDirectory(dir = work, prefix = "snapshot-resume-") as directory:
            data = Path(directory)
            config = data / "node.env"
            values = {**VALUES, "OCTRA_DATA_DIR": str(data), "OCTRA_BINARY_HASH": "e" * 64,
                "OCTRA_SOURCE_COMMIT": MARKER["source_commit"]}
            waiting = {**STATE, "rpc": "unavailable", "head_epoch": None, "lag": None,
                "live_binary": "e" * 64, "source_match": False, "binary_match": False,
                "runtime_match": False, "live_profile": None}
            (data / "recovery").mkdir()
            marker = data / "recovery/sync_need.json"
            for kind in ("pm2", "systemd"):
                sup = {**SUP, "config": config, "kind": kind,
                    "restart": {"Restart": "on-failure", "RestartPreventExitStatus": "78"}}
                for cause, head, rpc, expected in (
                    ("range", 100, False, False), ("range", 21_999, False, False),
                    ("range", 22_000, False, True), ("range", 22_000, True, True),
                    ("root", 101, False, True), ("root", 100, False, False),
                    ("conflict", 25_000, False, False),
                ):
                    need = upgrade.make(MARKER["chain_id"], cause, 101, 100,
                        25_000 if cause == "range" else None)
                    marker.write_text(json.dumps({"schema": upgrade.NEED_SCHEMA,
                        "chain_id": need.chain, "cause": cause, "epoch": 101, "head": 100,
                        "target": need.target}))
                    (data / "HEAD.json").write_text(json.dumps({"epoch_id": head,
                        "state_root": "a" * 64, "txid_hi": 12}))
                    if cause == "range":
                        remember(config, values, {"rpc": "ready", "head_epoch": 100}, need)
                    before = marker.read_bytes()
                    state = {**STATE, "head_epoch": head} if rpc else waiting
                    with self.subTest(kind = kind, cause = cause, head = head, rpc = rpc), \
                            effects([state] * 5) as calls, \
                            mock.patch.object(upgrade, "git_release", return_value = {
                                "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"]}), \
                            mock.patch.object(upgrade, "emit") as emit:
                        calls["read_need"].side_effect = read
                        calls["remember_plan"].side_effect = remember
                        calls["parse_env"].return_value = values
                        calls["restore_cycle"].return_value = need
                        self.assertEqual(upgrade.diagnose(ROOT, sup, values, MARKER), 2)
                        self.assertEqual(emit.call_args.kwargs["action"],
                            "leave_running" if expected else "apply_upgrade")
                        self.assertEqual(upgrade.apply(ROOT, sup, values, ARGS, MARKER), 0)
                        if expected:
                            for name in ("call", "stop", "sync_wait", "restore_cycle"):
                                calls[name].assert_not_called()
                        else:
                            calls["stop"].assert_called_once()
                            calls["sync_wait"].assert_called_once()
                            calls["restore_cycle"].assert_called_once()
                        calls["wait_node"].assert_called_once_with(ROOT, sup, values, ARGS, MARKER,
                            Path(values["OCTRA_OPERATOR_BINARY"]).resolve(), journal = None, guard = need)
                        self.assertEqual(marker.read_bytes(), before)

    def test_snapshot_apply_only(self):
        need = upgrade.make(MARKER["chain_id"], "range", 101, 100, 25_000)
        values = {**VALUES, "OCTRA_BINARY_HASH": "e" * 64,
            "OCTRA_SOURCE_COMMIT": MARKER["source_commit"]}
        state = {**STATE, "rpc": "unavailable", "head_epoch": None, "lag": None,
            "live_binary": "e" * 64, "source_match": False, "binary_match": False,
            "runtime_match": False, "live_profile": None}
        with effects([state] * 4) as calls, mock.patch.object(upgrade, "load_head",
                return_value = {"epoch": 22_000, "state_root": "a" * 64, "txid_hi": 12}):
            calls["read_need"].return_value = need
            self.assertEqual(upgrade.apply(ROOT, SUP, values, ARGS, MARKER), 0)
            calls["stop"].assert_not_called()
            calls["restore_cycle"].assert_not_called()
            calls["call"].assert_not_called()

    def test_snapshot_wait(self):
        need = upgrade.make(MARKER["chain_id"], "range", 101, 100, 25_000)
        other = upgrade.make(MARKER["chain_id"], "conflict", 22_001, 22_000, None)
        for label, consumed, online, change, finish, reason in (
            ("complete", 1800, True, False, 3000, None),
            ("pending", 9999, True, False, 3600, "snapshot_unconfirmed"),
            ("offline", 9999, False, False, 120, "node_offline"),
            ("stopped", 0, False, False, 120, "node_offline"),
            ("changed", 9999, True, True, 0, "sync_marker_changed"),
        ):
            with self.subTest(case = label), ExitStack() as stack:
                clock = [0.0]
                def sleep(seconds):
                    clock[0] += seconds
                def state(*args):
                    ready = 1700 <= clock[0] < consumed or clock[0] >= 3000
                    return {**STATE, "process": "online" if online else "offline",
                        "rpc": "ready" if ready else "unavailable", "lag": 0 if ready else None,
                        "voting": ready, "validator_member": True}
                def live(*args):
                    return {**SUP, "pid": 0} if consumed <= clock[0] < consumed + 10 else SUP
                stack.enter_context(mock.patch.object(upgrade.time, "monotonic", side_effect = lambda: clock[0]))
                stack.enter_context(mock.patch.object(upgrade.time, "sleep", side_effect = sleep))
                stack.enter_context(mock.patch.object(upgrade, "pm2_entries", return_value = []))
                stack.enter_context(mock.patch.object(upgrade, "current", side_effect = live))
                stack.enter_context(mock.patch.object(upgrade, "view", side_effect = state))
                stack.enter_context(mock.patch.object(upgrade, "read_need",
                    side_effect = lambda *args: other if change else need if clock[0] < consumed else None))
                emit = stack.enter_context(mock.patch.object(upgrade, "emit"))
                untouched = [stack.enter_context(mock.patch.object(upgrade, name, autospec = True,
                    side_effect = AssertionError("unexpected snapshot effect")))
                    for name in ("stop", "start", "restore_cycle", "install_floor", "sync_head", "remember_plan")]
                args = SimpleNamespace(wait_seconds = 3600, interval = 5, sudo = False)
                self.assertEqual(upgrade.wait_node(ROOT, SUP, VALUES, args, MARKER,
                    ROOT / "unused-node", guard = need), 0 if reason is None else 2)
                self.assertEqual(clock[0], finish)
                self.assertEqual(emit.call_args.kwargs.get("reason"), reason)
                if reason == "node_offline":
                    self.assertEqual(emit.call_args.kwargs["action"], "operator_start")
                for action in untouched:
                    action.assert_not_called()

    def test_snapshot_unit_restart(self):
        state = {**STATE, "source_match": False, "runtime_match": False}
        for cause in ("root", "range"):
            need = upgrade.make(MARKER["chain_id"], cause, 101, 100, 25_000 if cause == "range" else None)
            for policy, accepted in (
                ({"Restart": "no"}, False), ({"Restart": "on-failure"}, True),
                ({"Restart": "always"}, True),
                ({"Restart": "on-failure", "SuccessExitStatus": "75"}, False),
                ({"Restart": "on-success", "SuccessExitStatus": "75"}, True),
                ({"Restart": "always", "RestartPreventExitStatus": "75"}, False),
                ({"Restart": "no", "RestartForceExitStatus": "75"}, True),
            ):
                sup = {**SUP, "kind": "systemd", "restart": {**policy, "RestartPreventExitStatus":
                    policy.get("RestartPreventExitStatus", "") + " 78"}}
                with self.subTest(cause = cause, policy = policy), effects([state] * 5) as calls, \
                        mock.patch.object(upgrade, "git_release", return_value = {
                            "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"]}), \
                        mock.patch.object(upgrade, "emit") as emit:
                    calls["read_need"].return_value = need
                    self.assertTrue(upgrade.storage_stop_ready(sup["restart"]))
                    if accepted:
                        self.assertEqual(upgrade.apply(ROOT, sup, VALUES, ARGS, MARKER), 0)
                        calls["restore_cycle"].assert_called_once()
                    else:
                        with self.assertRaisesRegex(ValidatorError, "does not restart on exit 75"):
                            upgrade.apply(ROOT, sup, VALUES, ARGS, MARKER)
                        for name in ("call", "stop", "restore_cycle", "sync_wait"):
                            calls[name].assert_not_called()
                    self.assertEqual(upgrade.diagnose(ROOT, sup, VALUES, MARKER), 2)
                    self.assertEqual(emit.call_args.kwargs["reason"],
                        "signed_snapshot_required" if accepted else "unit_restart_75_required")

    def test_rollback_notice(self):
        for cause in [None, "root", "range"]:
            marked = None if cause is None else upgrade.make(MARKER["chain_id"], cause, 101, 100, 5000)
            with self.subTest(cause = cause), effects([STATE]) as calls, \
                    mock.patch.object(upgrade, "git_release", return_value = {
                        "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"]}), \
                    mock.patch.object(upgrade, "durable_faults", return_value = []), \
                    mock.patch.object(upgrade, "emit") as emit:
                calls["read_need"].return_value = marked
                upgrade.diagnose(ROOT, SUP, VALUES, MARKER)
                notices = [call.kwargs for call in emit.call_args_list
                    if call.kwargs.get("event") == "binary_rollback"]
                expected = [{"event": "binary_rollback", "status": "hold",
                    "target_source": "f7637a7e", "reason": "range_marker", "action": "use_upgrade",
                    "path": Path(VALUES["OCTRA_DATA_DIR"]) / "recovery" / "sync_need.json"}]
                self.assertEqual(notices, expected if cause == "range" else [])
                calls["stop"].assert_not_called()
                calls["call"].assert_not_called()

    def test_profile_epochs(self):
        rows = [
            {"from_epoch": 0, "hash": "a" * 64},
            {"from_epoch": 1_614_500, "hash": "d" * 64},
            {"from_epoch": 1_663_000, "hash": "c" * 64},
            {"from_epoch": 1_688_734, "hash": "e" * 64},
        ]
        for epoch, digest in ((1_654_002, "d"), (1_662_999, "d"),
                              (1_663_000, "c"), (1_688_733, "c"), (1_688_734, "e")):
            version = {"chain_id": MARKER["chain_id"], "profile_epoch": epoch,
                "runtime_profile_hash": digest * 64, "runtime_profiles": rows}
            self.assertTrue(upgrade.profile_match(version, MARKER))
            for change in ({"chain_id": "another-chain"}, {"profile_epoch": -1},
                           {"profile_epoch": True}, {"profile_epoch": None},
                           {"runtime_profile_hash": "f" * 64}, {"runtime_profiles": rows[::-1]},
                           {"runtime_profiles": rows[1:]}):
                self.assertFalse(upgrade.profile_match({**version, **change}, MARKER))
            self.assertFalse(upgrade.profile_match(version,
                {**MARKER, "runtime_profile_hash": "f" * 64}))

    def test_view_old_epoch(self):
        version = {
            "source_commit": MARKER["source_commit"], "chain_id": MARKER["chain_id"],
            "runtime_profile_hash": "d" * 64, "profile_epoch": 1_654_003,
            "runtime_profiles": [{"from_epoch": 0, "hash": "d" * 64},
                {"from_epoch": 1_663_000, "hash": MARKER["runtime_profile_hash"]}],
            "consensus_profile": MARKER["consensus_profile"],
            "consensus_rules_id": MARKER["consensus_rules_id"], "binary_hash": "e" * 64,
        }
        values = {**VALUES, "OCTRA_API_PORT": "8080", "OCTRA_BINARY_HASH": "e" * 64}
        with mock.patch.object(upgrade, "rpc_status", return_value = {"head_epoch": 1_654_002}), \
                mock.patch.object(upgrade, "load_wallet", return_value = {}), \
                mock.patch.object(upgrade, "membership", return_value = {
                    "active": True, "scheduled": True, "activate_epoch": None}), \
                mock.patch.object(upgrade, "proc_hash", return_value = "e" * 64), \
                mock.patch.object(upgrade, "process_alive", return_value = True):
            for change, accepted in (({}, True), ({"consensus_rules_id": "wrong"}, False),
                    ({"consensus_profile": 1}, False), ({"source_commit": "a" * 40}, False),
                    ({"binary_hash": "f" * 64}, False), ({"profile_epoch": 1_654_104}, True),
                    ({"profile_epoch": 1_663_000}, False)):
                with self.subTest(change = change), mock.patch.object(upgrade, "rpc_method",
                        side_effect = lambda _, name: {**version, **change}
                        if name == "octra_runtimeVersion" else {}):
                    self.assertEqual(upgrade.matches(upgrade.view(values, 41, MARKER)), accepted)

    def test_remember_plan(self):
        work = ROOT / "runtime_data"
        work.mkdir(exist_ok = True)
        with TemporaryDirectory(dir = work) as directory:
            config = Path(directory) / "node.env"
            values = {**VALUES, "OCTRA_DATA_DIR": str(Path(directory) / "data")}
            plan = upgrade.make(MARKER["chain_id"], "range", 101, 100, 10_000)
            state = {"head_epoch": 100, "rpc": "ready"}
            self.assertEqual(upgrade.remember_plan(config, values, state, plan), plan)
            self.assertEqual(upgrade.remember_plan(config, values, state, None), plan)
            self.assertEqual(upgrade.remember_plan(config, values, {"head_epoch": 9900}, None), plan)
            self.assertIsNone(upgrade.remember_plan(config, values,
                {"head_epoch": 5904, "rpc": "ready"}, None))
            self.assertFalse(config.with_name("node.env.sync.json").exists())

    def test_plan_identity(self):
        work = ROOT / "runtime_data"
        work.mkdir(exist_ok = True)
        with TemporaryDirectory(dir = work) as directory:
            config = Path(directory) / "node.env"
            values = {**VALUES, "OCTRA_DATA_DIR": str(Path(directory) / "data")}
            state = {"head_epoch": 100, "rpc": "ready"}
            first = upgrade.make(MARKER["chain_id"], "range", 101, 100, 10_000)
            later = upgrade.make(MARKER["chain_id"], "range", 101, 100, 12_000)
            upgrade.remember_plan(config, values, state, first)
            self.assertEqual(upgrade.remember_plan(config, values, state, later), later)
            self.assertEqual(upgrade.remember_plan(config, values, state, first), later)
            path = config.with_name("node.env.sync.json")
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            with self.assertRaisesRegex(ValidatorError, "identity differs"):
                upgrade.remember_plan(config, {**values, "OCTRA_DATA_DIR": directory}, state, None)
            with self.assertRaisesRegex(ValidatorError, "binding differs"):
                upgrade.remember_plan(config, {**values, "OCTRA_CHAIN_ID": "wrong"}, state, None)
            path.chmod(0o644)
            with self.assertRaisesRegex(ValidatorError, "invalid.*file"):
                upgrade.remember_plan(config, values, state, None)

    def test_plan_disk(self):
        work = ROOT / "runtime_data"
        work.mkdir(exist_ok = True)
        with TemporaryDirectory(dir = work, prefix = "plan-disk-") as directory:
            data = Path(directory)
            config = data / "node.env"
            values = {**VALUES, "OCTRA_DATA_DIR": str(data)}
            plan = upgrade.make(MARKER["chain_id"], "range", 101, 100, 25_000)
            state = {"rpc": "unavailable", "head_epoch": 25_000}
            self.assertEqual(upgrade.remember_plan(config, values, state, plan), plan)
            path = data / "HEAD.json"
            saved = config.with_name("node.env.sync.json")
            path.write_text("{")
            self.assertEqual(upgrade.remember_plan(config, values, state, None), plan)
            self.assertTrue(saved.exists())
            for head in (20_903, 20_904):
                path.write_text(json.dumps({"epoch_id": head, "state_root": "a" * 64, "txid_hi": 12}))
                self.assertEqual(upgrade.remember_plan(config, values, state, None),
                    plan if head == 20_903 else None)
            self.assertFalse(saved.exists())
            self.assertIsNone(upgrade.remember_plan(config, values, state, plan))
            self.assertFalse(saved.exists())
            later = upgrade.make(MARKER["chain_id"], "range", 101, 100, 28_000)
            self.assertEqual(upgrade.remember_plan(config, values, state, later), later)
            path.write_text(json.dumps({"epoch_id": 22_000, "state_root": "a" * 64, "txid_hi": 12}))
            self.assertFalse(upgrade.snapshot_running(values, plan, later))
            self.assertTrue(upgrade.snapshot_running(values, plan, None))

    def test_busy_after_diagnose(self):
        work = ROOT / "runtime_data"
        work.mkdir(exist_ok = True)
        with TemporaryDirectory(dir = work) as directory:
            config = Path(directory) / "node.env"
            values = {**VALUES, "OCTRA_DATA_DIR": str(Path(directory) / "data")}
            sup = {**SUP, "config": config}
            state = {**STATE, "head_epoch": 1_654_002, "lag": None}
            remember = upgrade.remember_plan
            with effects([state] * 5) as calls, mock.patch.object(upgrade, "git_release", return_value = {
                    "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"],
                }), mock.patch.object(upgrade, "emit") as emit:
                calls["remember_plan"].side_effect = remember
                calls["sync_plan"].side_effect = lambda vals, view, target: (
                    upgrade.make(MARKER["chain_id"], "range", 1_654_003, 1_654_002, target)
                    if target is not None else None)
                calls["sync_head"].side_effect = [{"head": 1_686_600, "snapshot": 1_686_500}, None]
                calls["sync_wait"].side_effect = ValidatorError("signed snapshot metadata is unavailable")
                self.assertEqual(upgrade.diagnose(ROOT, sup, values, MARKER), 2)
                self.assertEqual(emit.call_args.kwargs["reason"], "signed_snapshot_required")
                with self.assertRaisesRegex(ValidatorError, "metadata is unavailable"):
                    upgrade.apply(ROOT, sup, values, ARGS, MARKER)
                self.assertEqual(calls["sync_wait"].call_args.args[1].target, 1_686_600)
                calls["stop"].assert_not_called()
                calls["call"].assert_not_called()

    def test_history_window(self):
        for requested, expected in ((5000, 4096), (4096, 4096), (2000, 2000),
                (0, 1), (-1, 1), ("invalid", 4096), ("0x40", 4096), (" 64", 4096),
                ("+64", 64), ("1_000", 4096), ("9" * 20, 4096), ("-9" * 20, 4096)):
            with self.subTest(requested = requested):
                self.assertEqual(upgrade.sync_limit({"OCTRA_CATCHUP_MAX_LAG": str(requested)}), expected)

    def test_wait_without_peers(self):
        state = {**STATE, "lag": None, "head_epoch": 1_654_002}
        plan = upgrade.make(MARKER["chain_id"], "range", 1_654_003, 1_654_002, 1_686_600)
        values = {**VALUES, "OCTRA_STATE_SYNC_SOURCES": "https://snapshot.example"}
        release = {**MARKER, "notice_code": "consensus_recovery"}
        with mock.patch.object(upgrade, "pm2_entries", return_value = []), \
                mock.patch.object(upgrade, "current", return_value = SUP), \
                mock.patch.object(upgrade, "view", return_value = state), \
                mock.patch.object(upgrade, "sync_head", return_value = {"head": plan.target}), \
                mock.patch.object(upgrade, "remember_plan", return_value = plan), \
                mock.patch.object(upgrade, "install_floor") as floor, \
                mock.patch.object(upgrade, "emit") as emit:
            self.assertEqual(upgrade.wait_node(ROOT, SUP, values, ARGS, release, ROOT / "unused-node"), 2)
            self.assertEqual(emit.call_args.kwargs["reason"], "signed_snapshot_required")
            floor.assert_not_called()

    def test_missing_head(self):
        for tip in [None, {"head": None, "snapshot": None}, {"head": None, "snapshot": 1_580_000}]:
            with self.subTest(tip = tip), effects([{**STATE, "lag": None}]) as calls:
                calls["sync_head"].return_value = tip
                with self.assertRaisesRegex(ValidatorError, "network head is unavailable"):
                    upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
                calls["stop"].assert_not_called()
                calls["call"].assert_not_called()

    def test_saved_plan_busy(self):
        plan = upgrade.make(MARKER["chain_id"], "range", 101, 100, 10_000)
        with effects([STATE]) as calls, mock.patch.object(upgrade, "remember_plan", return_value = plan):
            calls["sync_wait"].side_effect = ValidatorError("signed snapshot metadata is unavailable")
            with self.assertRaisesRegex(ValidatorError, "metadata is unavailable"):
                upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
            calls["sync_wait"].assert_called_once()
            calls["stop"].assert_not_called()
            calls["call"].assert_not_called()

    def test_recovery_signals(self):
        for number in (signal.SIGINT, signal.SIGTERM, signal.SIGHUP):
            prior = signal.getsignal(number)
            with self.assertRaisesRegex(ValidatorError, "recovery interrupted"):
                with validator_recover.interrupts():
                    signal.raise_signal(number)
            self.assertEqual(signal.getsignal(number), prior)

    def test_sync_child_stop(self):
        child = mock.Mock(pid = 4242)
        child.wait.side_effect = [KeyboardInterrupt(), -9]
        child.poll.return_value = None
        with mock.patch.object(validator_config.subprocess, "Popen") as start, \
                mock.patch.object(validator_config.os, "killpg") as kill:
            start.return_value.__enter__.return_value = child
            with self.assertRaises(KeyboardInterrupt):
                validator_config.run(["sync"], cwd = ROOT, interruptible = True)
            kill.assert_called_once_with(4242, signal.SIGKILL)
            self.assertEqual(child.wait.call_count, 2)

    def test_call_timeout(self):
        child = mock.Mock(pid=4242)
        child.wait.side_effect = [subprocess.TimeoutExpired(["build"], 1), -9]
        with mock.patch.object(upgrade.subprocess, "Popen") as start, \
                mock.patch.object(upgrade.os, "killpg") as kill:
            start.return_value.__enter__.return_value = child
            with self.assertRaisesRegex(ValidatorError, "command timed out"):
                upgrade.call(["build"], cwd=ROOT, timeout=1)
        start.assert_called_once_with(["build"], cwd=ROOT, start_new_session=True)
        kill.assert_called_once_with(4242, signal.SIGKILL)
        self.assertEqual(child.wait.call_count, 2)

    def test_call_exit(self):
        for code in (0, 3):
            with self.subTest(code=code), mock.patch.object(upgrade.subprocess, "Popen") as start:
                start.return_value.__enter__.return_value.wait.return_value = code
                if code:
                    with self.assertRaises(subprocess.CalledProcessError):
                        upgrade.call(["build"], timeout=1)
                else:
                    self.assertEqual(upgrade.call(["build"], timeout=1).returncode, 0)

    def test_timeout_processes(self):
        work = ROOT / "runtime_data"
        work.mkdir(exist_ok=True)
        with TemporaryDirectory(dir=work) as directory:
            path = Path(directory)
            pulse = path / "pulse"
            child_id = path / "child"
            child = (
                "import pathlib, sys, time\n"
                "path = pathlib.Path(sys.argv[1])\n"
                "while True:\n"
                " with path.open('a') as stream: stream.write('x')\n"
                " time.sleep(0.02)\n"
            )
            parent = (
                "import pathlib, subprocess, sys\n"
                "child = subprocess.Popen([sys.executable, '-c', sys.argv[1], sys.argv[2]])\n"
                "pathlib.Path(sys.argv[3]).write_text(str(child.pid))\n"
                "child.wait()\n"
            )
            with self.assertRaisesRegex(ValidatorError, "command timed out"):
                upgrade.call([sys.executable, "-c", parent, child, str(pulse), str(child_id)], timeout=2)
            self.assertGreater(int(child_id.read_text()), 0)
            before = pulse.stat().st_size
            self.assertGreater(before, 1)
            time.sleep(0.15)
            self.assertEqual(pulse.stat().st_size, before)

    def test_build_timeout_live(self):
        def call(command, **options):
            if Path(command[1]).name == "build.sh":
                self.assertEqual(options["timeout"], 3600)
                raise ValidatorError("command timed out: sh")
        with effects([STATE]) as calls:
            calls["call"].side_effect = call
            with self.assertRaisesRegex(ValidatorError, "command timed out"):
                upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
            for name in ("stop", "restore_cycle", "wait_node"):
                calls[name].assert_not_called()

    def test_wait_default(self):
        args = upgrade.parser().parse_args(["--root", str(ROOT)])
        self.assertEqual(args.wait_seconds, 3600)

    def test_snapshot_recovery_floor(self):
        head = 1_620_000
        for lag, floor, limit, accepted in (
            (721, 1_614_500, 5000, True),
            (1060, 1_614_500, 5000, True),
            (3000, 1_614_500, 5000, True),
            (3000, 1_617_000, 5000, True),
            (3000, 1_617_001, 5000, False),
            (2999, 1_614_500, 3000, True),
            (3000, 1_614_500, 3000, False),
            (3000, 1_614_500, 2999, False),
            (5001, 1_614_500, 5000, False),
        ):
            values = {**VALUES, "OCTRA_CATCHUP_MAX_LAG": str(limit)}
            need = upgrade.make(MARKER["chain_id"], "root", floor, floor - 1, None)
            tip = {"head": head, "snapshot": head - lag}
            with self.subTest(lag = lag, floor = floor, limit = limit), \
                mock.patch.object(upgrade, "sync_head", return_value = tip), \
                mock.patch.object(upgrade, "emit"), \
                mock.patch.object(upgrade.time, "sleep") as sleep:
                if accepted:
                    result = upgrade.sync_wait(values, need, 0, 1)
                    self.assertEqual(result, {**tip, "required": max(floor, head - min(3000, limit - 1))})
                else:
                    with self.assertRaisesRegex(ValidatorError, "snapshot_status = below_minimum"):
                        upgrade.sync_wait(values, need, 0, 1)
                sleep.assert_not_called()

    def test_profile_fields(self):
        version = {
            "source_commit": MARKER["source_commit"],
            "runtime_profile_hash": MARKER["runtime_profile_hash"],
            "consensus_profile": MARKER["consensus_profile"],
            "consensus_rules_id": MARKER["consensus_rules_id"],
            "binary_hash": "d" * 64,
        }
        with mock.patch.object(upgrade, "rpc_status", return_value={"head_epoch": 41}), \
            mock.patch.object(upgrade, "load_wallet", return_value={}), \
            mock.patch.object(upgrade, "membership", return_value={
                "active": True, "scheduled": True, "activate_epoch": 42,
            }), mock.patch.object(upgrade, "proc_hash", return_value="d" * 64), \
            mock.patch.object(upgrade, "process_alive", return_value=True):
            values = {**VALUES, "OCTRA_API_PORT": "8080", "OCTRA_BINARY_HASH": "d" * 64}
            rows = [version]
            for field in version:
                rows += [{**version, field: None}, {**version, field: "wrong"}]
            for row in rows:
                with self.subTest(row=row), mock.patch.object(
                    upgrade, "rpc_method", side_effect=lambda _, method: row
                    if method == "octra_runtimeVersion" else {}
                ):
                    state = upgrade.view(values, 41, MARKER)
                    self.assertEqual(upgrade.matches(state), row == version)

    def test_apply_profile_refusal(self):
        for runtime in (False, None):
            state = {**STATE, "runtime_match": runtime, "live_profile": "e" * 64}
            with self.subTest(runtime=runtime), effects([state]) as calls:
                with self.assertRaisesRegex(ValidatorError, "release runtime mismatch"):
                    upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
                for name in ("git_update", "call", "stop", "restore_cycle", "wait_node"):
                    calls[name].assert_not_called()

    def test_build_profile_change(self):
        changed = {**STATE, "runtime_match": False, "live_profile": "e" * 64}
        with effects([STATE, changed]) as calls:
            with self.assertRaisesRegex(ValidatorError, "release runtime mismatch"):
                upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
            self.assertEqual([Path(row.args[0][1]).name
                for row in calls["call"].call_args_list], ["check.sh", "build.sh"])
            for name in ("stop", "restore_cycle", "wait_node"):
                calls[name].assert_not_called()

    def test_stop_profile_change(self):
        changed = {**STATE, "runtime_match": False, "live_profile": "e" * 64}
        with effects([STATE, STATE, changed]) as calls:
            with self.assertRaisesRegex(ValidatorError, "release runtime mismatch"):
                upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
            for name in ("stop", "restore_cycle", "wait_node"):
                calls[name].assert_not_called()

    def test_source_upgrade_allowed(self):
        state = {**STATE, "source_match": False, "runtime_match": False}
        with effects([state, state, state]) as calls:
            self.assertEqual(upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER), 0)
            calls["stop"].assert_called_once()
            calls["restore_cycle"].assert_called_once()
            calls["wait_node"].assert_called_once()

    def test_matching_profile_allowed(self):
        with effects([STATE, STATE, STATE]) as calls:
            self.assertEqual(upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER), 0)
            calls["stop"].assert_called_once()

    def test_diagnostic_refusal(self):
        state = {**STATE, "runtime_match": False}
        with effects([state]), mock.patch.object(upgrade, "git_release", return_value={
            "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"],
        }) as git:
            self.assertEqual(upgrade.diagnose(ROOT, SUP, VALUES, MARKER), 2)
            git.assert_called_once()

    def test_wait_profile_refusal(self):
        state = {
            **STATE, "runtime_match": False, "voting": False,
            "voting_reason": "vote_log_bootstrap",
            "round_epoch": STATE["head_epoch"] + 1, "round": 0,
        }
        with mock.patch.object(upgrade, "pm2_entries", return_value=[]), \
            mock.patch.object(upgrade, "current", return_value=SUP), \
            mock.patch.object(upgrade, "view", return_value=state), \
            mock.patch.object(upgrade, "install_floor",
                side_effect=AssertionError("unexpected floor install")) as floor, \
            mock.patch.object(upgrade.time, "sleep", side_effect=AssertionError("unexpected wait")):
            self.assertEqual(upgrade.wait_node(
                ROOT, SUP, VALUES, ARGS, MARKER, ROOT / "unused-node"), 2)
            floor.assert_not_called()

    def test_null_recovery(self):
        state = {**STATE, "runtime_match": False, "live_profile": None}
        need = upgrade.make(MARKER["chain_id"], "conflict", 42, 41, None)
        with effects([state, state, state]) as calls:
            calls["read_need"].return_value = need
            self.assertEqual(upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER), 0)
            calls["restore_cycle"].assert_called_once()
        with effects([state]) as calls, mock.patch.object(upgrade, "git_release", return_value = {
            "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"],
        }), mock.patch.object(upgrade, "emit") as emit:
            calls["read_need"].return_value = need
            self.assertEqual(upgrade.diagnose(ROOT, SUP, VALUES, MARKER), 2)
            self.assertTrue(any(call.kwargs.get("reason") == "signed_snapshot_required"
                for call in emit.call_args_list))

    def test_null_without_need(self):
        state = {**STATE, "runtime_match": False, "live_profile": None}
        with effects([state]) as calls:
            with self.assertRaisesRegex(ValidatorError, "release runtime mismatch"):
                upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
            calls["git_update"].assert_not_called()

    def test_recovery_with_disk_fault(self):
        need = upgrade.make(MARKER["chain_id"], "conflict", 42, 41, None)
        fault = ("wal_store_unreadable", ROOT / "unused-wal", "invalid")
        with effects([STATE]) as calls, mock.patch.object(upgrade, "git_release", return_value = {
            "repo_head": MARKER["public_commit"], "upstream_head": MARKER["public_commit"],
        }), mock.patch.object(upgrade, "emit") as emit:
            calls["read_need"].return_value = need
            calls["inspect_pending"].return_value = [fault]
            self.assertEqual(upgrade.diagnose(ROOT, SUP, VALUES, MARKER), 2)
            self.assertEqual(emit.call_args.kwargs["action"], "do_not_delete")
            self.assertFalse(any(call.kwargs.get("action") in ("recover", "apply", "apply_upgrade")
                for call in emit.call_args_list))
            for name in ("call", "stop", "restore_cycle", "wait_node"):
                calls[name].assert_not_called()

    def test_null_wrong_rules(self):
        state = {**STATE, "runtime_match": False, "live_profile": None,
            "live_rules_id": "wrong"}
        with effects([state]) as calls:
            calls["read_need"].return_value = upgrade.make(
                MARKER["chain_id"], "conflict", 42, 41, None)
            with self.assertRaisesRegex(ValidatorError, "release runtime mismatch"):
                upgrade.apply(ROOT, SUP, VALUES, ARGS, MARKER)
            calls["git_update"].assert_not_called()

    def test_wait_crossing(self):
        starting = {**STATE, "source_match": False, "runtime_match": False,
            "live_source": "a" * 40, "lag": None}
        waiting = {**STATE, "runtime_match": False, "live_profile": None,
            "voting": False, "voting_reason": "sync_need", "lag": None}
        catching = {**waiting, "live_profile": "e" * 64, "lag": 2}
        boot = {**STATE, "voting": False, "voting_reason": "vote_log_bootstrap",
            "round_epoch": STATE["head_epoch"] + 1, "round": 0}
        ready = {**STATE, "voting": True, "validator_member": True}
        with mock.patch.object(upgrade, "pm2_entries", return_value = []), \
            mock.patch.object(upgrade, "current", return_value = SUP), \
            mock.patch.object(upgrade, "view", side_effect = [starting, waiting, catching, boot, ready]), \
            mock.patch.object(upgrade, "install_floor") as floor, \
            mock.patch.object(upgrade.time, "sleep"):
            self.assertEqual(upgrade.wait_node(
                ROOT, SUP, VALUES, ARGS, MARKER, ROOT / "unused-node"), 0)
            floor.assert_called_once()

    def test_wait_crossing_refusal(self):
        state = {**STATE, "runtime_match": False, "live_profile": "e" * 64,
            "voting": False, "voting_reason": "vote_log_bootstrap",
            "round_epoch": STATE["head_epoch"] + 1, "round": 0}
        with mock.patch.object(upgrade, "pm2_entries", return_value = []), \
            mock.patch.object(upgrade, "current", return_value = SUP), \
            mock.patch.object(upgrade, "view", side_effect = [{**state, "lag": 1}, state]), \
            mock.patch.object(upgrade, "install_floor") as floor, \
            mock.patch.object(upgrade.time, "sleep") as sleep, \
            mock.patch.object(upgrade, "emit") as emit:
            prior = ROOT / "unused-node"
            self.assertEqual(upgrade.wait_node(ROOT, SUP, VALUES, ARGS, MARKER, prior), 2)
            floor.assert_not_called()
            sleep.assert_called_once()
            self.assertEqual(emit.call_args.kwargs["action"], "do_not_restart")
            self.assertEqual(emit.call_args.kwargs["config"], SUP["config"])
            self.assertEqual(emit.call_args.kwargs["prior_binary"], prior)

    def test_wait_null_deadline(self):
        state = {**STATE, "runtime_match": False, "live_profile": None,
            "voting": False, "voting_reason": "sync_need"}
        args = SimpleNamespace(wait_seconds = 0, interval = 0, sudo = False)
        release = {**MARKER, "notice_code": "consensus_recovery"}
        with mock.patch.object(upgrade, "pm2_entries", return_value = []), \
            mock.patch.object(upgrade, "current", return_value = SUP), \
            mock.patch.object(upgrade, "view", return_value = state), \
            mock.patch.object(upgrade, "install_floor") as floor, \
            mock.patch.object(upgrade, "emit") as emit:
            self.assertEqual(upgrade.wait_node(
                ROOT, SUP, VALUES, args, release, ROOT / "unused-node"), 2)
            floor.assert_not_called()
            self.assertEqual(emit.call_args.kwargs["action"], "do_not_restart")
            self.assertEqual(emit.call_args.kwargs["config"], SUP["config"])

    def test_wal_preflight(self):
        data = ROOT / "runtime_data" / ("wal_" + uuid.uuid4().hex)
        wal = data / "wal"
        wal.mkdir(parents = True)
        path = wal / "0000000041.wal"
        try:
            path.write_text("{", encoding = "utf-8")
            faults = upgrade.inspect_pending(data)
            self.assertTrue(faults, "ordinary WAL was not inspected")
            self.assertEqual(faults[0][1], path)
            with mock.patch.object(upgrade, "data_pids", return_value = []), \
                mock.patch.object(upgrade.shutil, "disk_usage", return_value = SimpleNamespace(free = 10**10)):
                with self.assertRaisesRegex(ValidatorError, "durable record requires review"):
                    upgrade.preflight(ROOT, SUP, {**VALUES, "OCTRA_DATA_DIR": str(data)}, False)
            self.assertEqual(path.read_text(), "{")
            value = {"epoch_id": 41, "pre_state_root": "a", "post_state_root": "b",
                "parent_commit": "p", "start_txid": "0", "tx_count": 0,
                "finalized_by": "v", "finalized_at": 1.0, "irmin_last_epoch_before": 40}
            path.write_text(json.dumps(value), encoding = "utf-8")
            self.assertEqual(upgrade.inspect_pending(data), [])
            path.write_text(json.dumps({**value, "epoch_id": 42}), encoding = "utf-8")
            self.assertTrue(upgrade.inspect_pending(data))
        finally:
            shutil.rmtree(data)

    def test_vote_preflight(self):
        path = ROOT / ("00000000000001580000_00000000_" + "a" * 64 + ".vote")
        fault = ("vote_record_unreadable", path)
        with mock.patch.object(upgrade, "data_pids", return_value = []), \
                mock.patch.object(upgrade, "inspect_pending", return_value = []), \
                mock.patch.object(upgrade, "inspect_votes", return_value = [fault]), \
                mock.patch.object(upgrade, "emit") as emit, \
                mock.patch.object(upgrade.shutil, "disk_usage") as disk:
            with self.assertRaisesRegex(ValidatorError, "durable record requires review"):
                upgrade.preflight(ROOT, SUP, VALUES, False, head = STATE["head_epoch"])
            emit.assert_called_once_with(fault = fault[0], path = path,
                detail = "invalid", action = "do_not_delete")
            disk.assert_not_called()

    def test_wal_types(self):
        data = ROOT / "runtime_data" / ("wal_" + uuid.uuid4().hex)
        wal = data / "wal"
        wal.mkdir(parents = True)
        path = wal / "0000000041_0000.pending"
        value = {"epoch_id": 41, "round": 0, "proposal_id": "p",
            "proposed_state_root": "r", "txid_hi": "1", "ts": 1,
            "validator_addr": "v"}
        try:
            path.write_text(json.dumps(value), encoding = "utf-8")
            self.assertEqual(upgrade.inspect_pending(data), [])
            for field, invalid in (("proposal_b64", 1), ("vote_b64", []),
                ("tx_hashes", [1]), ("txs_json", "bad"), ("receipts_json", False)):
                with self.subTest(field = field):
                    path.write_text(json.dumps({**value, field: invalid}), encoding = "utf-8")
                    faults = upgrade.inspect_pending(data)
                    self.assertTrue(faults, "invalid pending field was accepted")
                    self.assertEqual(faults[0][1], path)
            path.unlink()
            path.symlink_to(data / "missing")
            self.assertTrue(upgrade.inspect_pending(data))
            path.unlink()
            wal.rmdir()
            wal.symlink_to(data / "missing")
            self.assertTrue(upgrade.inspect_pending(data))
        finally:
            shutil.rmtree(data)

if __name__ == "__main__":
    unittest.main()