# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import unittest
import json
import shutil
import uuid
from contextlib import ExitStack, contextmanager
from pathlib import Path
from types import SimpleNamespace
from unittest import mock

import upgrade
from validator_common import ValidatorError

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
    }
    with ExitStack() as stack:
        mocks = {
            name: stack.enter_context(mock.patch.object(upgrade, name, return_value=value))
            for name, value in replies.items()
        }
        stack.enter_context(mock.patch.object(upgrade, "view", side_effect=states))
        yield mocks

class UpgradeProfileTest(unittest.TestCase):
    def test_snapshot_recovery_floor(self):
        head = 1_620_000
        for lag, floor, limit, accepted in (
            (721, 1_614_500, 5000, True),
            (1060, 1_614_500, 5000, True),
            (3000, 1_614_500, 5000, True),
            (3000, 1_617_000, 5000, True),
            (3000, 1_617_001, 5000, False),
            (3000, 1_614_500, 3000, True),
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
                    self.assertEqual(result, {**tip, "required": max(floor, head - limit)})
                else:
                    with self.assertRaisesRegex(ValidatorError, "signed snapshot is below recovery"):
                        upgrade.sync_wait(values, need, 0, 1)
                sleep.assert_not_called()

    def test_profile_fields(self):
        version = {
            "source_commit": MARKER["source_commit"],
            "runtime_profile_hash": MARKER["runtime_profile_hash"],
            "consensus_profile": MARKER["consensus_profile"],
            "consensus_rules_id": MARKER["consensus_rules_id"],
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
                with self.assertRaisesRegex(ValidatorError, "WAL"):
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