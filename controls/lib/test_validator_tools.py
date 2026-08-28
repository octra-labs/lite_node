# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import base64
import datetime
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import unittest
from pathlib import Path
from unittest import mock

from nacl.signing import SigningKey

from sync_need import make as make_need

from validator_common import ValidatorError
from validator_common import address_from_pubkey
from validator_common import ensure_wallet
from validator_common import load_network
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import rpc_url
from validator_common import validate_checkpoint
from validator_common import validate_network
from validator_common import write_env
from validator_config import operator_pm2_name
from validator_config import adopt_network
from validator_config import BUILD_WORK
from validator_config import build_candidate
from validator_config import CARGO_HOME
from validator_config import check_sync_sources
from validator_config import ensure_build_toolchain
from validator_config import OPAM_SWITCH
from validator_config import ROOT as CONFIG_ROOT
from validator_config import RUSTUP_HOME
from validator_config import TOOLCHAIN_ROOT
from validator_config import rust_environment
from validator_config import tool_version
from validator_config import install_verified_snapshot
from validator_config import install_runtime
from validator_config import load_verified_snapshot
from validator_config import maybe_sync
from validator_config import parser
from validator_config import pm2_service_enabled
from validator_config import resolve_sync_stage
from validator_config import resource_report
from validator_config import require_validator_membership
from validator_config import require_runtime_binding
from validator_config import rebind_runtime
from validator_config import source_commit
from validator_config import sync_client_command
from validator_config import validate_advertise
from validator_config import validate_sync_layout
from validator_bundle import validate_bundle
from validator_enroll import exact_member
from validator_enroll import committed_enrollment
from validator_enroll import Enrollment
from validator_enroll import EnrollmentState
from validator_enroll import join_step
from validator_enroll import JoinStep
from validator_enroll import membership
from validator_enroll import next_nonce
from validator_enroll import require_admission_active
from validator_enroll import resume_join_transaction
from validator_enroll import set_validator_mode
from validator_enroll import submit_bond
from validator_enroll import wait_active_commit
from validator_enroll import wait_scheduled
from validator_guard import require_hashed_file
from validator_process import process_plan
from validator_process import process_pids
from validator_process import remaining_owners
from validator_process import active_data_owners
from validator_process import data_pids
from validator_process import entry_data
from validator_process import pm2_entries
from validator_process import wait_stopped
from validator_recover import recover
from validator_recover import need_of
from validator_recover import read_need
from validator_recover import preserved_state_path
from validator_rejoin import configured_data
from validator_rejoin import confirmed_node
from validator_rejoin import captured_round
from validator_rejoin import checked_floor
from validator_rejoin import floor_binary
from validator_rejoin import floor_source
from validator_rejoin import launch
from validator_rejoin import legacy_handoff
from validator_rejoin import log_round
from validator_rejoin import MEET_CAP
from validator_rejoin import MeetError
from validator_rejoin import meet_stage
from validator_rejoin import meet_start
from validator_rejoin import meet_round
from validator_rejoin import meet_value
from validator_rejoin import active_member
from validator_rejoin import meet_ready
from validator_rejoin import node_binary
from validator_rejoin import place_floor
from validator_rejoin import prepared_floor
from validator_rejoin import rejoin
from validator_rejoin import online_entry
from validator_rejoin import peer_head
from validator_rejoin import positive_seconds
from validator_rejoin import rpc_call
from validator_rejoin import round_alignment
from validator_rejoin import report_ready
from validator_rejoin import report_meet
from validator_rejoin import signed_round
from validator_rejoin import snapshot
from validator_rejoin import staged_floor
from validator_rejoin import sync_state
from validator_rejoin import vote_state
from validator_status import promotion_readiness
from validator_status import round_view
from validator_store import data_dir
from validator_store import prior_scan
from validator_store import pack_bytes
from validator_store import report
from validator_store import remove_prior
from validator_store import snapshot_stats
from validator_store import suffix_bytes
from validator_store import tree_bytes
import release as release_tool
import upgrade as upgrade_tool
from upgrade import choose
from upgrade import deadline_result
from upgrade import env_paths
from upgrade import inspect_pending
from upgrade import inspect_votes
from upgrade import recovery_installed
from upgrade import ready
from upgrade import restore_need
from upgrade import sync_plan
from upgrade import sync_target

RUNTIME_DATA_ROOT = CONFIG_ROOT / "runtime_data"
WORK = RUNTIME_DATA_ROOT / "validator_tools_test"

def identity():
    key = SigningKey.generate()
    public_key = bytes(key.verify_key)
    encoded = base64.b64encode(public_key).decode("ascii")
    return address_from_pubkey(public_key), encoded

def network_values():
    validators = ",".join(":".join(identity()) for _ in range(5))
    exporter = ":".join(identity())
    program_key = base64.b64encode(bytes(SigningKey.generate().verify_key)).decode("ascii")
    return {
        "OCTRA_BFT_PROPOSAL_BUILD_GRACE_MS": "180000",
        "OCTRA_BFT_PROPOSAL_VERIFY_GRACE_MS": "180000",
        "OCTRA_BFT_PROPOSE_TIMEOUT_MS": "180000",
        "OCTRA_BFT_RELEASE_PROFILE": "devnet_full_v1",
        "OCTRA_BOOTSTRAP_PEERS": "10.0.0.1:19000,10.0.0.2:19000",
        "OCTRA_CHAIN_ID": "octra-devnet-bft-v1",
        "OCTRA_CHECKPOINT_EPOCH": "99",
        "OCTRA_CHECKPOINT_STATE_ROOT": "3" * 64,
        "OCTRA_CHECKPOINT_TXID_HI": "500",
        "OCTRA_CONSENSUS_CONFIG_HASH": "4" * 64,
        "OCTRA_EMISSION_ACTIVATION_EPOCH": "100",
        "OCTRA_FHE_MAX_PER_EPOCH": "1",
        "OCTRA_P2P_REQUIRE_BINARY_HASH": "0",
        "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH": "100",
        "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH": "200",
        "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH": "150",
        "OCTRA_PROGRAM_RELEASE_KEYS": f"release={program_key}",
        "OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH": "100",
        "OCTRA_PVAC_MIGRATION_ROOT": "2" * 64,
        "OCTRA_STEALTH_MAX_PER_EPOCH": "1",
        "OCTRA_STATE_SYNC_EXPORTERS": exporter,
        "OCTRA_STATE_SYNC_SOURCES": "https://seed-a.example,https://seed-b.example",
        "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH": "200",
        "OCTRA_VALIDATORS": validators,
    }

def release_value(public="a" * 40, source="b" * 40, notice="consensus_recovery"):
    return {
        "schema": "octra-devnet-release-v2",
        "chain_id": "octra-devnet-9871-cluster",
        "key_id": "devnet-release-f912b4891be62acc",
        "sequence": 2,
        "action": "required",
        "notice_code": notice,
        "public_commit": public,
        "source_commit": source,
        "network_sha256": "c" * 64,
        "runtime_profile_hash": "d" * 64,
        "consensus_profile": 16,
        "consensus_rules_id": "finalized_rejection_commitment",
        "issued_at": "2026-08-16T18:47:00Z",
        "expires_at": "2026-08-19T18:46:00Z",
        "signature": "A" * 86 + "==",
    }

def write_need(data, chain, cause="root", epoch=100, head=99, target=None):
    path = data / "recovery/sync_need.json"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps({
        "schema": "octra_sync_need_v1",
        "chain_id": chain,
        "cause": cause,
        "epoch": epoch,
        "head": head,
        "target": target,
    }) + "\n", encoding="utf-8")
    return path

def signed_release_value():
    return {
        "schema": "octra-devnet-release-v2",
        "chain_id": "octra-devnet-9871-cluster",
        "key_id": "devnet-release-f912b4891be62acc",
        "sequence": 1,
        "action": "current",
        "notice_code": "release_current",
        "public_commit": "dd0698132d46af57e59e90c52886827483e33fac",
        "source_commit": "c544ebd5b4f1feedf54ee9d86faf589ba44a78e1",
        "network_sha256": "26e5ca5a46753eb6f41962312991254fcbc2bdf55fb3ce73679dbf2511627a78",
        "runtime_profile_hash": "14bac4b24aa67795bcaecf618b6c10a6ef0f3b2a113ded7f5f9117362375cee6",
        "consensus_profile": 16,
        "consensus_rules_id": "finalized_rejection_commitment",
        "issued_at": "2026-08-16T18:47:00Z",
        "expires_at": "2026-08-19T18:46:00Z",
        "signature": "xpCmekCmFyhuEqF9CbT6T239rNbjGqzyu+6zjmFtrK+quNUg+Z4J6Ij55Ek6K3ZsfoF1YrShFYfKYbUZhn/aBg==",
    }

class ValidatorToolsTest(unittest.TestCase):
    def setUp(self):
        if WORK.exists():
            shutil.rmtree(WORK)
        WORK.mkdir(parents=True)

    def test_store_report_counts_exact_prior_paths(self):
        data = WORK / "devnet"
        data.mkdir()
        prior_a = WORK / "devnet.prior-10"
        prior_b = WORK / "devnet.prior-20-1"
        prior_a.mkdir()
        prior_b.mkdir()
        (prior_a / "a").write_bytes(b"abc")
        (prior_b / "b").write_bytes(b"defg")
        (WORK / "devnet.prior-x").mkdir()
        self.assertEqual(prior_scan(data), [(prior_a, None), (prior_b, None)])
        self.assertEqual(tree_bytes(prior_a), 3)
        self.assertEqual(tree_bytes(prior_b), 4)

    def test_store_report_skips_prior_link(self):
        data = WORK / "devnet"
        data.mkdir()
        target = WORK / "target"
        target.mkdir()
        link = WORK / "devnet.prior-10"
        link.symlink_to(target, target_is_directory=True)
        self.assertEqual(prior_scan(data), [(link, "link")])

    def test_store_report_resolves_data_link_and_measures_store(self):
        target = WORK / "volume/devnet"
        store = target / "irmin_store"
        store.mkdir(parents=True)
        link = WORK / "devnet"
        link.symlink_to(target, target_is_directory=True)
        values = {"OCTRA_DATA_DIR": str(link), "OCTRA_API_PORT": "18080"}
        self.assertEqual(data_dir(values), target.resolve())
        with mock.patch(
            "validator_store.shutil.disk_usage",
            return_value=mock.Mock(free=123),
        ) as disk, mock.patch(
            "validator_store.rpc_method",
            return_value=None,
        ), mock.patch("validator_store.emit"):
            report(values)
        disk.assert_called_once_with(store)

    def test_store_report_accepts_removed_live_file(self):
        data = WORK / "devnet"
        store = data / "irmin_store"
        store.mkdir(parents=True)
        transient = data / "epoch_commit_in_progress.json"
        transient.write_bytes(b"pending")
        values = {"OCTRA_DATA_DIR": str(data), "OCTRA_API_PORT": "18080"}
        original = Path.lstat

        def vanish(path):
            if path == transient and path.exists():
                path.unlink()
                raise FileNotFoundError(path)
            return original(path)

        with mock.patch.object(Path, "lstat", vanish), mock.patch(
            "validator_store.shutil.disk_usage",
            return_value=mock.Mock(free=123),
        ), mock.patch(
            "validator_store.rpc_method",
            return_value=None,
        ), mock.patch("validator_store.emit"):
            report(values)

    def test_store_report_accepts_removed_live_dir(self):
        data = WORK / "devnet"
        store = data / "irmin_store"
        store.mkdir(parents=True)
        transient = data / "pending"
        transient.mkdir()
        values = {"OCTRA_DATA_DIR": str(data), "OCTRA_API_PORT": "18080"}
        original = Path.iterdir

        def vanish(path):
            if path == transient and path.exists():
                path.rmdir()
                raise FileNotFoundError(path)
            return original(path)

        with mock.patch.object(Path, "iterdir", vanish), mock.patch(
            "validator_store.shutil.disk_usage",
            return_value=mock.Mock(free=123),
        ), mock.patch(
            "validator_store.rpc_method",
            return_value=None,
        ), mock.patch("validator_store.emit"):
            report(values)

    def test_store_report_skips_bad_prior_tree(self):
        data = WORK / "devnet"
        store = data / "irmin_store"
        store.mkdir(parents=True)
        prior = WORK / "devnet.prior-10"
        prior.mkdir()
        (prior / "link").symlink_to(store, target_is_directory=True)
        values = {"OCTRA_DATA_DIR": str(data), "OCTRA_API_PORT": "18080"}
        with mock.patch(
            "validator_store.shutil.disk_usage",
            return_value=mock.Mock(free=123),
        ), mock.patch(
            "validator_store.rpc_method",
            return_value=None,
        ), mock.patch("validator_store.emit") as output:
            report(values)
        events = [call.kwargs for call in output.call_args_list]
        self.assertIn(
            {"event": "prior_skipped", "path": prior, "reason": "tree"},
            events,
        )

    def test_store_report_skips_nested_mount(self):
        data = WORK / "devnet"
        data.mkdir()
        prior = WORK / "devnet.prior-10"
        nested = prior / "volume"
        nested.mkdir(parents=True)
        values = {"OCTRA_DATA_DIR": str(data), "OCTRA_API_PORT": "18080"}
        real_mount = os.path.ismount
        with mock.patch(
            "validator_store.os.path.ismount",
            side_effect=lambda path: Path(path) == nested or real_mount(path),
        ), mock.patch(
            "validator_store.shutil.disk_usage",
            return_value=mock.Mock(free=123),
        ), mock.patch(
            "validator_store.rpc_method",
            return_value=None,
        ), mock.patch("validator_store.emit") as output:
            report(values)
        events = [call.kwargs for call in output.call_args_list]
        self.assertIn(
            {"event": "prior_skipped", "path": prior, "reason": "tree"},
            events,
        )

    def test_store_report_counts_suffix_chunks(self):
        data = WORK / "devnet"
        store = data / "irmin_store"
        store.mkdir(parents=True)
        (store / "store.0.suffix").write_bytes(b"abc")
        (store / "store.1.suffix").write_bytes(b"defg")
        (store / "store.1.prefix").write_bytes(b"ignored")
        self.assertEqual(suffix_bytes(data), 7)
        self.assertEqual(pack_bytes(data), 14)

    def test_store_reports_snapshot_payload(self):
        data = WORK / "devnet"
        data.mkdir()
        root = WORK / "snapshots"
        first = root / ("a" * 64)
        second = root / ("b" * 64)
        invalid = root / ("c" * 64)
        first.mkdir(parents=True)
        second.mkdir()
        invalid.mkdir()
        (first / ".certificate.json").write_text(
            json.dumps({"manifest": {"total_size": "11"}}),
            encoding="utf-8",
        )
        (second / ".certificate.json").write_text(
            json.dumps({"manifest": {"total_size": 13}}),
            encoding="utf-8",
        )
        (first / "download.lease").write_bytes(b"")
        (invalid / ".certificate.json").write_text("{}", encoding="utf-8")
        values = {"OCTRA_STATE_SYNC_SNAPSHOT_DIR": str(root)}
        self.assertEqual(
            snapshot_stats(values, data),
            {"count": 2, "payload": 24, "leased": 1, "skipped": 1},
        )

    def test_store_prune_checks_all_states_before_removal(self):
        data = WORK / "devnet"
        data.mkdir()
        identity_path = WORK / "wallet.json"
        identity = ensure_wallet(identity_path)
        first = WORK / "devnet.prior-10"
        second = WORK / "devnet.prior-20"
        first.mkdir()
        second.mkdir()
        (first / "wallet.json").write_bytes(identity_path.read_bytes())
        os.chmod(first / "wallet.json", 0o600)
        ensure_wallet(second / "wallet.json")
        with mock.patch("validator_store.pm2_entries", return_value=[]), \
             mock.patch("validator_store.active_data_owners", return_value=[]), \
             mock.patch("validator_store.data_pids", return_value=[]):
            removed, total, skipped = remove_prior(data, identity)
        self.assertEqual(removed, 1)
        self.assertGreater(total, 0)
        self.assertEqual(skipped, 1)
        self.assertFalse(first.exists())
        self.assertTrue(second.is_dir())

    def test_store_prune_removes_checked_states(self):
        data = WORK / "devnet"
        data.mkdir()
        identity_path = WORK / "wallet.json"
        identity = ensure_wallet(identity_path)
        prior = WORK / "devnet.prior-10"
        prior.mkdir()
        (prior / "wallet.json").write_bytes(identity_path.read_bytes())
        os.chmod(prior / "wallet.json", 0o600)
        with mock.patch("validator_store.pm2_entries", return_value=[]), \
             mock.patch("validator_store.active_data_owners", return_value=[]), \
             mock.patch("validator_store.data_pids", return_value=[]):
            removed, total, skipped = remove_prior(data, identity)
        self.assertEqual(removed, 1)
        self.assertGreater(total, 0)
        self.assertEqual(skipped, 0)
        self.assertFalse(prior.exists())

    def test_store_prune_refuses_active_state(self):
        data = WORK / "devnet"
        data.mkdir()
        identity_path = WORK / "wallet.json"
        identity = ensure_wallet(identity_path)
        prior = WORK / "devnet.prior-10"
        prior.mkdir()
        (prior / "wallet.json").write_bytes(identity_path.read_bytes())
        os.chmod(prior / "wallet.json", 0o600)
        with mock.patch("validator_store.pm2_entries", return_value=[]), \
             mock.patch("validator_store.active_data_owners", return_value=["pm2:node"]), \
             mock.patch("validator_store.data_pids", return_value=[]):
            removed, total, skipped = remove_prior(data, identity)
        self.assertEqual(removed, 0)
        self.assertEqual(total, 0)
        self.assertEqual(skipped, 1)
        self.assertTrue(prior.is_dir())

    def tearDown(self):
        if WORK.exists():
            shutil.rmtree(WORK)
        if RUNTIME_DATA_ROOT.exists():
            try:
                RUNTIME_DATA_ROOT.rmdir()
            except OSError:
                pass

    def test_wallet_is_valid_and_private(self):
        wallet_path = WORK / "data/wallet.json"
        wallet = ensure_wallet(wallet_path)
        self.assertEqual(len(wallet["address"]), 47)
        self.assertEqual(os.stat(wallet_path).st_mode & 0o777, 0o600)
        self.assertEqual(ensure_wallet(wallet_path), wallet)

    def test_upgrade_requires_one_proven_choice(self):
        self.assertEqual(choose("config", ["a", "a"]), "a")
        with self.assertRaisesRegex(ValidatorError, "was not found"):
            choose("config", [])
        with self.assertRaisesRegex(ValidatorError, "is ambiguous"):
            choose("config", ["a", "b"])

    def test_upgrade_reads_systemd_environment_files(self):
        self.assertEqual(
            env_paths("/etc/octra/node.env (ignore_errors=no) -/etc/octra/extra.env"),
            [
                Path("/etc/octra/node.env").resolve(),
                Path("/etc/octra/extra.env").resolve(),
            ],
        )

    def test_upgrade_verifies_pinned_release_signature(self):
        marker = signed_release_value()
        raw = json.dumps(marker).encode("utf-8")
        now = datetime.datetime(2026, 8, 16, 20, tzinfo=datetime.timezone.utc)
        self.assertEqual(release_tool.decode(raw, now=now), marker)
        marker["public_commit"] = "a" * 40
        with self.assertRaisesRegex(ValidatorError, "signature verification failed"):
            release_tool.decode(json.dumps(marker).encode("utf-8"), now=now)

    def test_upgrade_refuses_release_rollback_and_sequence_reuse(self):
        prior = release_value()
        rollback = {**prior, "sequence": 1}
        reused = {**prior, "action": "recommended"}
        for candidate, message in (
            (rollback, "would roll back"),
            (reused, "was reused"),
        ):
            with mock.patch.object(release_tool, "cache_path", return_value=WORK / "release"), mock.patch.object(
                release_tool, "read", return_value=prior
            ), mock.patch.object(
                release_tool, "fetch", return_value=candidate
            ), self.assertRaisesRegex(ValidatorError, message):
                release_tool.trusted(WORK)

    def test_upgrade_uses_cache_only_when_origin_is_unavailable(self):
        cached = release_value()
        with mock.patch.object(release_tool, "cache_path", return_value=WORK / "release"), mock.patch.object(
            release_tool, "read", side_effect=[cached, cached]
        ), mock.patch.object(
            release_tool,
            "fetch",
            side_effect=release_tool.OriginError("offline"),
        ), mock.patch.object(release_tool, "write") as write:
            self.assertEqual(release_tool.trusted(WORK), (cached, "cache"))
        write.assert_not_called()

        with mock.patch.object(release_tool, "cache_path", return_value=WORK / "release"), mock.patch.object(
            release_tool, "read", return_value=cached
        ), mock.patch.object(
            release_tool,
            "fetch",
            side_effect=ValidatorError("signature verification failed"),
        ), self.assertRaisesRegex(ValidatorError, "signature verification failed"):
            release_tool.trusted(WORK)

    def test_upgrade_cache_is_atomic(self):
        path = WORK / "release.json"
        marker = signed_release_value()
        now = datetime.datetime(2026, 8, 16, 20, tzinfo=datetime.timezone.utc)
        release_tool.write(path, marker)
        self.assertEqual(release_tool.read(path, fresh=True, now=now), marker)
        self.assertEqual(os.stat(path).st_mode & 0o777, 0o600)
        self.assertEqual(list(WORK.glob("*.staged")), [])

    def test_upgrade_refuses_linked_cache(self):
        target = WORK / "target.json"
        target.write_text("{}\n", encoding="utf-8")
        path = WORK / "release.json"
        path.symlink_to(target)
        with self.assertRaisesRegex(ValidatorError, "unreadable"):
            release_tool.read(path, fresh=False)

    def test_upgrade_checks_remote_without_fetching(self):
        (WORK / ".git").mkdir()
        head = "a" * 40

        def output(command, **_):
            if command[1:3] == ["rev-parse", "HEAD"]:
                return head
            if command[1] == "symbolic-ref":
                return "main"
            if command[-1] == "branch.main.remote":
                return "origin"
            if command[-1] == "branch.main.merge":
                return "refs/heads/main"
            if command[1] == "ls-remote":
                return head + "\trefs/heads/main"
            self.fail(f"unexpected command: {command}")

        with mock.patch.object(upgrade_tool, "capture", side_effect=output) as capture_git:
            state = upgrade_tool.git_release(WORK)

        self.assertEqual(state["repo_head"], head)
        self.assertEqual(state["upstream_head"], head)
        self.assertEqual(state["upstream"], "origin:refs/heads/main")
        self.assertFalse(any(call.args[0][1] == "fetch" for call in capture_git.call_args_list))

    def test_upgrade_checks_detached_remote_without_fetching(self):
        (WORK / ".git").mkdir()
        head = "a" * 40
        latest = "b" * 40

        def output(command, **_):
            if command[1:3] == ["rev-parse", "HEAD"]:
                return head
            if command[1] == "symbolic-ref":
                raise subprocess.CalledProcessError(1, command)
            if command[1] == "remote":
                return "backup\norigin"
            if command[1] == "ls-remote":
                return latest + "\trefs/heads/main"
            self.fail(f"unexpected command: {command}")

        with mock.patch.object(upgrade_tool, "capture", side_effect=output) as capture_git:
            state = upgrade_tool.git_release(WORK)

        self.assertEqual(state["repo_head"], head)
        self.assertEqual(state["upstream_head"], latest)
        self.assertEqual(state["upstream"], "origin:refs/heads/main")
        self.assertFalse(any(call.args[0][1] == "fetch" for call in capture_git.call_args_list))

    def test_upgrade_uses_signed_release_target(self):
        public = "a" * 40
        source = "b" * 40
        (WORK / "SOURCE_COMMIT").write_text(source + "\n", encoding="utf-8")

        def output(command, **_):
            if command[1:3] == ["status", "--porcelain"]:
                return ""
            if command[1] == "symbolic-ref":
                return "main"
            if command[-1] == "branch.main.remote":
                return "origin"
            if command[-1] == "branch.main.merge":
                return "refs/heads/main"
            if command[1:3] == ["rev-parse", "FETCH_HEAD"]:
                return public
            if command[1:3] == ["rev-parse", "HEAD"]:
                return public
            self.fail(f"unexpected command: {command}")

        with mock.patch.object(upgrade_tool, "capture", side_effect=output), mock.patch.object(
            upgrade_tool, "call"
        ) as run:
            self.assertEqual(
                upgrade_tool.git_update(WORK, public, source),
                (public, source, False),
            )

        self.assertEqual(
            run.call_args_list,
            [
                mock.call(
                    ["git", "fetch", "--no-tags", "origin", "refs/heads/main"],
                    cwd=WORK,
                ),
                mock.call(["git", "merge", "--ff-only", public], cwd=WORK),
            ],
        )

    def test_upgrade_fast_forwards_detached_release(self):
        public = "a" * 40
        source = "b" * 40
        heads = iter(["c" * 40, public])
        (WORK / "SOURCE_COMMIT").write_text(source + "\n", encoding="utf-8")

        def output(command, **_):
            if command[1:3] == ["status", "--porcelain"]:
                return ""
            if command[1] == "symbolic-ref":
                raise subprocess.CalledProcessError(1, command)
            if command[1] == "remote":
                return "backup\norigin"
            if command[1:3] == ["rev-parse", "FETCH_HEAD"]:
                return public
            if command[1:3] == ["rev-parse", "HEAD"]:
                return next(heads)
            self.fail(f"unexpected command: {command}")

        with mock.patch.object(upgrade_tool, "capture", side_effect=output), mock.patch.object(
            upgrade_tool, "call"
        ) as run:
            self.assertEqual(
                upgrade_tool.git_update(WORK, public, source),
                (public, source, True),
            )

        self.assertEqual(
            run.call_args_list,
            [
                mock.call(
                    ["git", "fetch", "--no-tags", "origin", "refs/heads/main"],
                    cwd=WORK,
                ),
                mock.call(["git", "merge", "--ff-only", public], cwd=WORK),
            ],
        )

    def test_upgrade_diagnoses_durable_records_without_deleting_them(self):
        wal = WORK / "wal"
        votes = WORK / "vote_log"
        wal.mkdir()
        votes.mkdir()
        pending = wal / "0000000041_0003.pending"
        pending.write_bytes(b"")
        vote = votes / ("00000000000000000041_00000003_" + "a" * 64 + ".vote")
        vote.write_bytes(b"")
        self.assertEqual(inspect_pending(WORK)[0][0], "pending_store_unreadable")
        self.assertEqual(inspect_votes(WORK)[0][0], "vote_record_unreadable")
        self.assertTrue(pending.is_file())
        self.assertTrue(vote.is_file())

    def test_upgrade_success_is_role_specific(self):
        common = {
            "process": "online",
            "rpc": "ready",
            "binary_match": True,
            "source_match": True,
            "runtime_match": True,
            "head_epoch": 41,
            "peer_epoch": 41,
            "lag": 0,
            "validator_member": True,
            "voting": True,
            "voting_reason": None,
        }
        self.assertTrue(ready("validator", common))
        self.assertFalse(ready("observer", common))
        observer = {
            **common,
            "validator_member": False,
            "voting": False,
            "voting_reason": "role",
        }
        self.assertTrue(ready("observer", observer))
        self.assertFalse(ready("validator", observer))
        self.assertFalse(ready("observer", {**observer, "binary_match": False}))

    def test_upgrade_plans_terminal_range_sync(self):
        values = {
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
            "OCTRA_CATCHUP_MAX_LAG": "5000",
        }
        state = {
            "process": "online",
            "rpc": "ready",
            "head_epoch": 100,
        }
        self.assertIsNone(sync_plan(values, state, 5100))
        self.assertEqual(
            sync_plan(values, state, 5101),
            make_need("octra-devnet-9871-cluster", "range", 101, 100, 5101),
        )
        self.assertIsNone(sync_plan(values, {**state, "rpc": "unavailable"}, 5101))

    def test_upgrade_reads_bounded_sync_head(self):
        payload = json.dumps({
            "version": "octra-state-sync",
            "mode": "live_head",
            "current_epoch": 42,
            "head_epoch": 41,
            "snapshot_epoch": 40,
        }).encode("utf-8")
        certificate = json.dumps({
            "version": "octra-state-sync",
            "checkpoint": {"epoch": "40"},
            "manifest": {"files": [{"path": "ready_roots", "size": "1"}]},
        }).encode("utf-8")
        first = mock.MagicMock()
        first.__enter__.return_value.read.return_value = payload
        second = mock.MagicMock()
        second.__enter__.return_value.read.return_value = certificate
        with mock.patch.object(
            upgrade_tool,
            "urlopen",
            side_effect=[first, second],
        ) as opened:
            self.assertEqual(
                upgrade_tool.sync_epoch("https://seed.example"),
                {"head": 41, "snapshot": 40},
            )
        self.assertEqual(len(opened.call_args_list), 2)
        for item in opened.call_args_list:
            request = item.args[0]
            self.assertEqual(request.get_header("Accept"), "application/json")
            self.assertEqual(request.get_header("User-agent"), "octra-upgrade/1")
        first.__enter__.return_value.read.return_value = b"x" * 65_537
        with mock.patch.object(upgrade_tool, "urlopen", return_value=first):
            with self.assertRaisesRegex(ValidatorError, "byte limit"):
                upgrade_tool.sync_epoch("https://seed.example")

    def test_upgrade_reads_snapshot_epoch_from_certificate(self):
        head = json.dumps({
            "version": "octra-state-sync",
            "mode": "live_head",
            "current_epoch": 42,
            "head_epoch": 41,
        }).encode("utf-8")
        certificate = json.dumps({
            "version": "octra-state-sync",
            "checkpoint": {"epoch": "39"},
            "manifest": {"files": [{"path": "ready_roots", "size": "1"}]},
        }).encode("utf-8")
        first = mock.MagicMock()
        first.__enter__.return_value.read.return_value = head
        second = mock.MagicMock()
        second.__enter__.return_value.read.return_value = certificate
        with mock.patch.object(
            upgrade_tool,
            "urlopen",
            side_effect=[first, second],
        ):
            self.assertEqual(
                upgrade_tool.sync_epoch("https://seed.example"),
                {"head": 41, "snapshot": 39},
            )

    def test_upgrade_rejects_snapshot_without_root_window(self):
        head = json.dumps({
            "version": "octra-state-sync",
            "mode": "live_head",
            "current_epoch": 42,
            "head_epoch": 41,
            "snapshot_epoch": 40,
        }).encode("utf-8")
        certificate = json.dumps({
            "version": "octra-state-sync",
            "checkpoint": {"epoch": "40"},
            "manifest": {"files": []},
        }).encode("utf-8")
        first = mock.MagicMock()
        first.__enter__.return_value.read.return_value = head
        second = mock.MagicMock()
        second.__enter__.return_value.read.return_value = certificate
        with mock.patch.object(
            upgrade_tool,
            "urlopen",
            side_effect=[first, second],
        ):
            with self.assertRaisesRegex(ValidatorError, "root window"):
                upgrade_tool.sync_epoch("https://seed.example")

    def test_upgrade_rejects_unsigned_snapshot_epoch(self):
        head = json.dumps({
            "version": "octra-state-sync",
            "mode": "live_head",
            "current_epoch": 42,
            "head_epoch": 41,
            "snapshot_epoch": 40,
        }).encode("utf-8")
        certificate = json.dumps({
            "version": "octra-state-sync",
            "checkpoint": {"epoch": "39"},
            "manifest": {"files": [{"path": "ready_roots", "size": "1"}]},
        }).encode("utf-8")
        first = mock.MagicMock()
        first.__enter__.return_value.read.return_value = head
        second = mock.MagicMock()
        second.__enter__.return_value.read.return_value = certificate
        with mock.patch.object(
            upgrade_tool,
            "urlopen",
            side_effect=[first, second],
        ):
            with self.assertRaisesRegex(ValidatorError, "fields differ"):
                upgrade_tool.sync_epoch("https://seed.example")

    def test_upgrade_waits_for_snapshot_boundary(self):
        values = {"OCTRA_STATE_SYNC_SOURCES": "https://a.example,https://b.example"}
        need = make_need("octra-test", "root", 101, 100, None)
        with mock.patch.object(
            upgrade_tool,
            "sync_head",
            side_effect=[
                {"head": 200, "snapshot": 100},
                {"head": 201, "snapshot": 150},
            ],
        ), mock.patch.object(upgrade_tool.time, "sleep"):
            self.assertEqual(
                upgrade_tool.sync_wait(values, need, 10.0, 0.1),
                {"head": 201, "snapshot": 150, "required": 101},
            )

    def test_upgrade_distinguishes_unavailable_snapshot_metadata(self):
        values = {"OCTRA_STATE_SYNC_SOURCES": "https://seed.example"}
        need = make_need("octra-test", "root", 101, 100, None)
        with mock.patch.object(
            upgrade_tool, "sync_head", return_value=None
        ), mock.patch.object(
            upgrade_tool.time, "monotonic", side_effect=[0.0, 0.0]
        ):
            with self.assertRaisesRegex(ValidatorError, "metadata is unavailable"):
                upgrade_tool.sync_wait(values, need, 0.0, 0.1)

    def test_upgrade_reports_snapshot_below_boundary(self):
        values = {"OCTRA_STATE_SYNC_SOURCES": "https://seed.example"}
        need = make_need("octra-test", "root", 101, 100, None)
        with mock.patch.object(
            upgrade_tool,
            "sync_head",
            return_value={"head": 110, "snapshot": 100},
        ), mock.patch.object(
            upgrade_tool.time, "monotonic", side_effect=[0.0, 0.0]
        ):
            with self.assertRaisesRegex(ValidatorError, "below recovery boundary"):
                upgrade_tool.sync_wait(values, need, 0.0, 0.1)

    def test_upgrade_rejects_snapshot_beyond_catchup_window(self):
        values = {
            "OCTRA_CATCHUP_MAX_LAG": "5000",
            "OCTRA_STATE_SYNC_SOURCES": "https://a.example,https://b.example",
        }
        need = make_need("octra-test", "range", 1337042, 1337041, 1344077)
        with mock.patch.object(
            upgrade_tool,
            "sync_head",
            side_effect=[
                {"head": 1344077, "snapshot": 1337040},
                {"head": 1344077, "snapshot": 1342801},
            ],
        ), mock.patch.object(upgrade_tool.time, "sleep"):
            self.assertEqual(
                upgrade_tool.sync_wait(values, need, 10.0, 0.1),
                {
                    "head": 1344077,
                    "snapshot": 1342801,
                    "required": 1339077,
                },
            )

    def test_upgrade_prefers_live_peer_head(self):
        self.assertEqual(
            sync_target(
                {"peer_epoch": 1344077},
                {"head": 1342979, "snapshot": 1342801},
            ),
            1344077,
        )

    def test_upgrade_reports_installed_during_consensus_recovery(self):
        state = {
            "process": "online",
            "rpc": "ready",
            "binary_match": True,
            "source_match": True,
            "runtime_match": True,
            "head_epoch": 41,
            "peer_epoch": None,
            "lag": None,
            "validator_member": True,
            "voting": True,
            "voting_reason": None,
        }
        self.assertTrue(recovery_installed("validator", state, release_value()))
        self.assertEqual(
            deadline_result("validator", state, release_value()),
            ("installed", "network_not_finalizing", "leave_running", 0),
        )
        self.assertFalse(
            recovery_installed(
                "validator",
                state,
                release_value(notice="routine_update"),
            )
        )
        self.assertFalse(recovery_installed("validator", {**state, "voting": False}, release_value()))
        self.assertEqual(
            deadline_result(
                "validator",
                {**state, "voting": False},
                release_value(),
            ),
            ("pending", "health_deadline", "do_not_restart", 2),
        )

    def test_upgrade_floor_requires_synced_bootstrap_round(self):
        state = {
            "process": "online",
            "rpc": "ready",
            "head_epoch": 41,
            "peer_epoch": 41,
            "lag": 0,
            "voting": False,
            "voting_reason": "vote_log_bootstrap",
            "round_epoch": 42,
            "round": 7,
        }
        self.assertEqual(upgrade_tool.floor_target(state), (41, 8))
        self.assertIsNone(upgrade_tool.floor_target({**state, "lag": 1}))
        self.assertIsNone(
            upgrade_tool.floor_target({**state, "round_epoch": 43})
        )
        self.assertIsNone(
            upgrade_tool.floor_target({**state, "voting_reason": "vote_log_corrupt"})
        )

    def test_upgrade_installs_recovery_floor_around_one_stop(self):
        events = []
        config = WORK / "node.env"
        data = WORK / "data"
        values = {
            "OCTRA_DATA_DIR": str(data),
            "OCTRA_CHAIN_ID": "octra-devnet-test",
        }
        supervisor = {
            "kind": "systemd",
            "scope": "system",
            "name": "octra-node.service",
            "pid": 42,
            "config": config,
        }
        args = mock.Mock(sudo=True, wait_seconds=10.0, interval=0.1)
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}

        def floor(*_, **kwargs):
            events.append("check" if kwargs.get("check") else "write")

        with mock.patch.object(
            upgrade_tool, "load_wallet", return_value=wallet
        ), mock.patch.object(
            upgrade_tool, "place_floor", side_effect=floor
        ), mock.patch.object(
            upgrade_tool, "stop", side_effect=lambda *_: events.append("stop")
        ), mock.patch.object(
            upgrade_tool, "data_pids", return_value=[]
        ), mock.patch.object(
            upgrade_tool, "start", side_effect=lambda *_: events.append("start")
        ):
            upgrade_tool.install_floor(
                WORK,
                supervisor,
                values,
                args,
                supervisor,
                (41, 8),
            )

        self.assertEqual(events, ["check", "stop", "write", "start"])

    def test_upgrade_floor_check_fails_before_stop(self):
        config = WORK / "node.env"
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-test",
        }
        supervisor = {
            "kind": "pm2",
            "scope": "user",
            "name": "octra-test",
            "pid": 42,
            "config": config,
        }
        args = mock.Mock(sudo=False, wait_seconds=10.0, interval=0.1)
        with mock.patch.object(
            upgrade_tool,
            "load_wallet",
            return_value={"address": identity()[0], "pub": identity()[1]},
        ), mock.patch.object(
            upgrade_tool, "place_floor", side_effect=ValidatorError("unsafe")
        ), mock.patch.object(upgrade_tool, "stop") as stop_node:
            with self.assertRaisesRegex(ValidatorError, "unsafe"):
                upgrade_tool.install_floor(
                    WORK,
                    supervisor,
                    values,
                    args,
                    supervisor,
                    (41, 8),
                )
        stop_node.assert_not_called()

    def test_upgrade_restarts_when_floor_write_fails(self):
        config = WORK / "node.env"
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-test",
        }
        supervisor = {
            "kind": "pm2",
            "scope": "user",
            "name": "octra-test",
            "pid": 42,
            "config": config,
        }
        args = mock.Mock(sudo=False, wait_seconds=10.0, interval=0.1)
        address, public_key = identity()
        calls = []

        def floor(*_, **kwargs):
            if kwargs.get("check"):
                return
            raise ValidatorError("write failed")

        with mock.patch.object(
            upgrade_tool,
            "load_wallet",
            return_value={"address": address, "pub": public_key},
        ), mock.patch.object(
            upgrade_tool, "place_floor", side_effect=floor
        ), mock.patch.object(
            upgrade_tool, "stop"
        ), mock.patch.object(
            upgrade_tool, "data_pids", return_value=[]
        ), mock.patch.object(
            upgrade_tool, "start", side_effect=lambda *_: calls.append("start")
        ):
            with self.assertRaisesRegex(ValidatorError, "write failed"):
                upgrade_tool.install_floor(
                    WORK,
                    supervisor,
                    values,
                    args,
                    supervisor,
                    (41, 8),
                )
        self.assertEqual(calls, ["start"])

    def test_upgrade_restarts_when_floor_owner_remains(self):
        config = WORK / "node.env"
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-test",
        }
        supervisor = {
            "kind": "systemd",
            "scope": "system",
            "name": "octra-node.service",
            "pid": 42,
            "config": config,
        }
        args = mock.Mock(sudo=True)
        address, public_key = identity()
        calls = []
        with mock.patch.object(
            upgrade_tool,
            "load_wallet",
            return_value={"address": address, "pub": public_key},
        ), mock.patch.object(
            upgrade_tool, "place_floor"
        ), mock.patch.object(
            upgrade_tool, "stop"
        ), mock.patch.object(
            upgrade_tool, "data_pids", return_value=[71]
        ), mock.patch.object(
            upgrade_tool, "start", side_effect=lambda *_: calls.append("start")
        ):
            with self.assertRaisesRegex(ValidatorError, "active after stop: 71"):
                upgrade_tool.install_floor(
                    WORK,
                    supervisor,
                    values,
                    args,
                    supervisor,
                    (41, 8),
                )
        self.assertEqual(calls, ["start"])

    def test_upgrade_manual_hashes_are_assertions_only(self):
        release = release_value()
        values = {"OCTRA_CHAIN_ID": release["chain_id"]}
        args = mock.Mock(public_commit=None, source_commit=None)
        self.assertEqual(
            upgrade_tool.release_target(release, args, values),
            (release["public_commit"], release["source_commit"]),
        )
        args.public_commit = "f" * 40
        with self.assertRaisesRegex(ValidatorError, "does not match the signed release"):
            upgrade_tool.release_target(release, args, values)

    def test_upgrade_verifies_signed_release_tree(self):
        network = WORK / "config/network.env"
        network.parent.mkdir()
        network.write_text("OCTRA_CHAIN_ID=octra-devnet-9871-cluster\n", encoding="utf-8")
        source = "b" * 40
        (WORK / "SOURCE_COMMIT").write_text(source + "\n", encoding="utf-8")
        release = {
            **release_value(source=source),
            "network_sha256": hashlib.sha256(network.read_bytes()).hexdigest(),
        }
        upgrade_tool.verify_release_tree(WORK, release)
        network.write_text("OCTRA_CHAIN_ID=wrong\n", encoding="utf-8")
        with self.assertRaisesRegex(ValidatorError, "network configuration"):
            upgrade_tool.verify_release_tree(WORK, release)

    def test_upgrade_uses_signed_snapshot_sources(self):
        network = network_values()
        network["OCTRA_STATE_SYNC_SOURCES"] = "https://fresh.example"
        network["OCTRA_JOIN_RPC"] = "https://join-a.example,https://join-b.example"
        network["OCTRA_CATCHUP_MAX_LAG"] = "7000"
        bundle = WORK / "config/network.env"
        bundle.parent.mkdir()
        write_env(bundle, network)
        release = {
            **release_value(),
            "network_sha256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
        }
        local = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_STATE_SYNC_SOURCES": "https://stale.example",
            "OCTRA_JOIN_RPC": "https://join-stale.example",
            "OCTRA_CATCHUP_MAX_LAG": "5000",
        }

        selected = upgrade_tool.release_sync_values(WORK, local, release)

        self.assertEqual(
            selected["OCTRA_STATE_SYNC_SOURCES"],
            "https://fresh.example",
        )
        self.assertEqual(
            selected["OCTRA_JOIN_RPC"],
            "https://join-a.example,https://join-b.example",
        )
        self.assertEqual(selected["OCTRA_CATCHUP_MAX_LAG"], "7000")
        self.assertEqual(selected["OCTRA_DATA_DIR"], str(WORK / "data"))

    def test_upgrade_updates_release_before_snapshot_wait(self):
        events = []
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_OPERATOR_BINARY": str(WORK / "candidate/octra_node.exe"),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        supervisor = {
            "kind": "pm2",
            "scope": "user",
            "name": "octra-test",
            "pid": 41,
            "active": True,
            "state": "online",
            "config": WORK / "node.env",
        }
        args = mock.Mock(
            sudo=False,
            public_commit="a" * 40,
            source_commit="b" * 40,
            wait_seconds=1.0,
            interval=0.01,
        )
        need = make_need("octra-devnet-9871-cluster", "root", 100, 99, None)

        def update(*_):
            events.append("git")
            return "a" * 40, "b" * 40, False

        def verify(*_):
            events.append("tree")

        def select(*_):
            events.append("network")
            return values

        def wait(*_):
            events.append("wait")
            raise SystemExit(0)

        with mock.patch.object(upgrade_tool, "preflight"), mock.patch.object(
            upgrade_tool, "view", return_value={}
        ), mock.patch.object(
            upgrade_tool, "verify_unit", return_value=None
        ), mock.patch.object(
            upgrade_tool, "git_update", side_effect=update
        ), mock.patch.object(
            upgrade_tool, "verify_release_tree", side_effect=verify
        ), mock.patch.object(
            upgrade_tool, "release_sync_values", side_effect=select
        ), mock.patch.object(
            upgrade_tool, "sync_head", return_value=None
        ), mock.patch.object(
            upgrade_tool, "sync_plan", return_value=None
        ), mock.patch.object(
            upgrade_tool, "read_need", return_value=need
        ), mock.patch.object(
            upgrade_tool, "sync_wait", side_effect=wait
        ), mock.patch.object(
            upgrade_tool, "call"
        ) as run, mock.patch.object(
            upgrade_tool, "stop"
        ) as stop_node:
            with self.assertRaises(SystemExit):
                upgrade_tool.apply(WORK, supervisor, values, args, release_value())

        self.assertEqual(events, ["git", "tree", "network", "wait"])
        run.assert_not_called()
        stop_node.assert_not_called()

    def test_upgrade_builds_and_checks_before_one_stop_start(self):
        events = []
        config = WORK / "node.env"
        binary = WORK / "candidate/octra_node.exe"
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_OPERATOR_BINARY": str(binary),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        supervisor = {
            "kind": "pm2",
            "scope": "user",
            "name": "octra-test",
            "pid": 41,
            "active": True,
            "state": "online",
            "config": config,
        }
        args = mock.Mock(
            sudo=False,
            public_commit="a" * 40,
            source_commit="b" * 40,
            wait_seconds=1.0,
            interval=0.01,
        )
        healthy = {
            "process": "online",
            "rpc": "ready",
            "binary_match": True,
            "source_match": True,
            "runtime_match": True,
            "head_epoch": 41,
            "peer_epoch": 41,
            "lag": 0,
            "validator_member": True,
            "voting": True,
            "voting_reason": None,
        }

        def command(command, **_):
            name = Path(command[1]).name
            if name == "validator_config.py":
                name = "network" if "--adopt-network" in command else "runtime"
            events.append(name)
            return mock.Mock()

        def update(*_):
            events.append("git")
            return "a" * 40, "b" * 40, False

        with mock.patch.object(
            upgrade_tool, "preflight", side_effect=lambda *_: events.append("preflight")
        ), mock.patch.object(
            upgrade_tool, "verify_unit", side_effect=lambda *_: events.append("verify")
        ), mock.patch.object(
            upgrade_tool, "git_update", side_effect=update
        ), mock.patch.object(
            upgrade_tool, "verify_release_tree"
        ), mock.patch.object(
            upgrade_tool, "release_sync_values", return_value=values
        ), mock.patch.object(
            upgrade_tool, "call", side_effect=command
        ), mock.patch.object(
            upgrade_tool, "parse_env", return_value=values
        ), mock.patch.object(
            upgrade_tool, "inspect_pending", return_value=[]
        ), mock.patch.object(
            upgrade_tool, "inspect_votes", return_value=[]
        ), mock.patch.object(
            upgrade_tool, "stop", side_effect=lambda *_: events.append("stop")
        ) as stop_node, mock.patch.object(
            upgrade_tool,
            "disk_state",
            return_value=upgrade_tool.DiskState(
                100, "root", 5, "address", "pubkey", None
            ),
        ), mock.patch.object(
            upgrade_tool,
            "restore_plan",
            side_effect=lambda *_, **__: events.append("recover"),
        ), mock.patch.object(
            upgrade_tool, "start", side_effect=lambda *_: events.append("start")
        ) as start_node, mock.patch.object(
            upgrade_tool, "pm2_entries", return_value=[]
        ), mock.patch.object(
            upgrade_tool, "current", return_value={**supervisor, "pid": 42}
        ), mock.patch.object(
            upgrade_tool, "view", return_value=healthy
        ), mock.patch.object(
            upgrade_tool, "sync_head", return_value=None
        ):
            self.assertEqual(
                upgrade_tool.apply(WORK, supervisor, values, args, release_value()),
                0,
            )

        self.assertEqual(
            events,
            [
                "preflight",
                "verify",
                "git",
                "check.sh",
                "build.sh",
                "network",
                "runtime",
                "validator_guard.py",
                "stop",
                "recover",
                "start",
            ],
        )
        stop_node.assert_called_once()
        start_node.assert_called_once()

    def test_upgrade_never_stops_when_build_fails(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_OPERATOR_BINARY": str(WORK / "candidate/octra_node.exe"),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        supervisor = {
            "kind": "pm2",
            "scope": "user",
            "name": "octra-test",
            "pid": 41,
            "active": True,
            "state": "online",
            "config": WORK / "node.env",
        }
        args = mock.Mock(
            sudo=False,
            public_commit="a" * 40,
            source_commit="b" * 40,
        )

        def command(command, **_):
            if Path(command[1]).name == "build.sh":
                raise subprocess.CalledProcessError(1, command)
            return mock.Mock()

        with mock.patch.object(upgrade_tool, "preflight"), mock.patch.object(
            upgrade_tool, "verify_unit", return_value=None
        ), mock.patch.object(
            upgrade_tool,
            "git_update",
            return_value=("a" * 40, "b" * 40, False),
        ), mock.patch.object(
            upgrade_tool, "verify_release_tree"
        ), mock.patch.object(
            upgrade_tool, "release_sync_values", return_value=values
        ), mock.patch.object(
            upgrade_tool, "call", side_effect=command
        ), mock.patch.object(
            upgrade_tool, "view", return_value={}
        ), mock.patch.object(
            upgrade_tool, "sync_head", return_value=None
        ), mock.patch.object(upgrade_tool, "stop") as stop_node, mock.patch.object(
            upgrade_tool, "start"
        ) as start_node:
            with self.assertRaises(subprocess.CalledProcessError):
                upgrade_tool.apply(WORK, supervisor, values, args, release_value())

        stop_node.assert_not_called()
        start_node.assert_not_called()

    def test_upgrade_reexecs_after_fast_forward_before_build(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_OPERATOR_BINARY": str(WORK / "candidate/octra_node.exe"),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        supervisor = {
            "kind": "pm2",
            "scope": "user",
            "name": "octra-test",
            "pid": 41,
            "active": True,
            "state": "online",
            "config": WORK / "node.env",
        }
        args = mock.Mock(
            sudo=False,
            public_commit="a" * 40,
            source_commit="b" * 40,
        )
        with mock.patch.object(upgrade_tool, "preflight"), mock.patch.object(
            upgrade_tool, "view", return_value={}
        ), mock.patch.object(
            upgrade_tool, "sync_head", return_value=None
        ), mock.patch.object(
            upgrade_tool, "read_need", return_value=None
        ), mock.patch.object(
            upgrade_tool, "verify_unit", return_value=None
        ), mock.patch.object(
            upgrade_tool,
            "git_update",
            return_value=("a" * 40, "b" * 40, True),
        ), mock.patch.object(
            upgrade_tool, "verify_release_tree"
        ), mock.patch.object(
            upgrade_tool, "reexec", side_effect=SystemExit(0)
        ) as restart, mock.patch.object(
            upgrade_tool, "call"
        ) as run, mock.patch.object(
            upgrade_tool, "stop"
        ) as stop_node, mock.patch.object(
            upgrade_tool, "start"
        ) as start_node:
            with self.assertRaises(SystemExit):
                upgrade_tool.apply(WORK, supervisor, values, args, release_value())

        restart.assert_called_once_with(WORK)
        run.assert_not_called()
        stop_node.assert_not_called()
        start_node.assert_not_called()

    def test_upgrade_recovers_marked_state_once(self):
        config = WORK / "node.env"
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        need = make_need("octra-devnet-9871-cluster", "root", 100, 99, None)
        with mock.patch.object(upgrade_tool, "read_need", return_value=need), mock.patch.object(
            upgrade_tool, "recover"
        ) as run:
            self.assertEqual(restore_need(values, config), need)
        run.assert_called_once_with(config, replace_state=True, plan=need)

    def test_upgrade_skips_unmarked_state(self):
        config = WORK / "node.env"
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        with mock.patch.object(upgrade_tool, "read_need", return_value=None), mock.patch.object(
            upgrade_tool, "recover"
        ) as run:
            self.assertIsNone(restore_need(values, config))
        run.assert_not_called()

    def test_restore_plan_refuses_wire_dict_plan(self):
        config = WORK / "node.env"
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        plan = {"cause": "root", "epoch": 100, "head": 99, "target": None}
        with mock.patch.object(
            upgrade_tool, "read_need", return_value=None
        ), mock.patch.object(upgrade_tool, "recover") as run:
            with self.assertRaisesRegex(ValidatorError, "recovery plan type is invalid"):
                upgrade_tool.restore_plan(values, config, plan)
        run.assert_not_called()

    def test_restore_cycle_starts_once_when_state_returns(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        supervisor = {"config": WORK / "node.env"}
        args = mock.Mock(sudo=False)
        before = upgrade_tool.DiskState(100, "3" * 64, 500, "address", "pubkey", None)
        failure = ValidatorError("restore refused")
        with mock.patch.object(
            upgrade_tool, "restore_plan", side_effect=failure
        ), mock.patch.object(
            upgrade_tool, "disk_state", return_value=before
        ), mock.patch.object(upgrade_tool, "start") as start_node:
            with self.assertRaises(ValidatorError) as caught:
                upgrade_tool.restore_cycle(
                    WORK, supervisor, values, args, before, None, None
                )
        self.assertIs(caught.exception, failure)
        start_node.assert_called_once()

    def test_restore_cycle_holds_when_state_differs(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK / "data"),
            "OCTRA_CHAIN_ID": "octra-devnet-9871-cluster",
        }
        supervisor = {"config": WORK / "node.env"}
        args = mock.Mock(sudo=False)
        marker = make_need("octra-devnet-9871-cluster", "root", 100, 99, None)
        before = upgrade_tool.DiskState(100, "3" * 64, 500, "address", "pubkey", None)
        drifted = [
            upgrade_tool.DiskState(101, "3" * 64, 500, "address", "pubkey", None),
            upgrade_tool.DiskState(100, "4" * 64, 500, "address", "pubkey", None),
            upgrade_tool.DiskState(100, "3" * 64, 501, "address", "pubkey", None),
            upgrade_tool.DiskState(100, "3" * 64, 500, "other", "pubkey", None),
            upgrade_tool.DiskState(100, "3" * 64, 500, "address", "other", None),
            upgrade_tool.DiskState(100, "3" * 64, 500, "address", "pubkey", marker),
        ]
        for after in drifted:
            failure = ValidatorError("restore refused")
            with mock.patch.object(
                upgrade_tool, "restore_plan", side_effect=failure
            ), mock.patch.object(
                upgrade_tool, "disk_state", return_value=after
            ), mock.patch.object(upgrade_tool, "start") as start_node:
                with self.assertRaises(ValidatorError) as caught:
                    upgrade_tool.restore_cycle(
                        WORK, supervisor, values, args, before, None, None
                    )
            self.assertIs(caught.exception, failure)
            start_node.assert_not_called()

    def test_validator_reports_resources(self):
        usage = mock.Mock(
            total=900 * 1000 ** 3,
            free=500 * 1000 ** 3,
        )
        with mock.patch("validator_config.os.cpu_count", return_value=8):
            with mock.patch(
                "validator_config.memory_bytes",
                return_value=31 * 1024 ** 3,
            ):
                with mock.patch(
                    "validator_config.shutil.disk_usage",
                    return_value=usage,
                ):
                    resource_report(WORK, "validator")

    def test_validator_accepts_operator_resources(self):
        usage = mock.Mock(
            total=80 * 1000 ** 3,
            free=60 * 1000 ** 3,
        )
        with mock.patch("validator_config.os.cpu_count", return_value=2):
            with mock.patch(
                "validator_config.memory_bytes",
                return_value=4 * 1024 ** 3,
            ):
                with mock.patch(
                    "validator_config.shutil.disk_usage",
                    return_value=usage,
                ):
                    resource_report(WORK, "validator")

    def test_observer_accepts_operator_resources(self):
        usage = mock.Mock(
            total=80 * 1000 ** 3,
            free=60 * 1000 ** 3,
        )
        with mock.patch("validator_config.os.cpu_count", return_value=2):
            with mock.patch(
                "validator_config.memory_bytes",
                return_value=4 * 1024 ** 3,
            ):
                with mock.patch(
                    "validator_config.shutil.disk_usage",
                    return_value=usage,
                ):
                    resource_report(WORK, "observer")

    def test_rejoin_requires_configured_data_directory(self):
        values = {"OCTRA_DATA_DIR": str(WORK)}
        self.assertEqual(configured_data(values, WORK), WORK.resolve())
        with self.assertRaisesRegex(
            ValidatorError,
            "does not match node configuration",
        ):
            configured_data(values, WORK / "other")

    def test_rejoin_defaults_to_configured_data_directory(self):
        values = {"OCTRA_DATA_DIR": str(WORK)}
        self.assertEqual(configured_data(values), WORK.resolve())

    def test_config_build_only_mode(self):
        self.assertTrue(parser().parse_args(["--build-only"]).build_only)

    def test_rejoin_uses_packaged_floor_tool(self):
        root = WORK / "candidate"
        tool = root / "artifacts/vote_floor.exe"
        tool.parent.mkdir(parents=True)
        tool.write_bytes(b"floor")
        self.assertEqual(floor_binary(root), tool)

    def test_rejoin_uses_built_node_binary(self):
        root = WORK / "source"
        binary = root / "_build/default/bin/octra_node.exe"
        binary.parent.mkdir(parents=True)
        binary.write_bytes(b"node")
        self.assertEqual(node_binary(root), binary)

    def test_rejoin_invokes_floor_tool_with_bound_identity(self):
        root = WORK / "candidate"
        tool = root / "artifacts/vote_floor.exe"
        tool.parent.mkdir(parents=True)
        tool.write_bytes(b"floor")
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.subprocess.run") as run:
            place_floor(root, values, wallet, WORK, 41, 7, check=True)
        self.assertEqual(
            run.call_args.args[0],
            [
                str(tool),
                "--data-dir",
                str(WORK),
                "--chain-id",
                "octra-devnet-test",
                "--validator",
                wallet["address"],
                "--validator-pub",
                wallet["pub"],
                "--head-epoch",
                "41",
                "--round",
                "7",
                "--check",
            ],
        )

    def test_rejoin_binds_signed_round_with_local_minimum(self):
        root = WORK / "candidate"
        tool = root / "artifacts/vote_floor.exe"
        tool.parent.mkdir(parents=True)
        tool.write_bytes(b"floor")
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.subprocess.run") as run:
            place_floor(
                root,
                values,
                wallet,
                WORK,
                41,
                7,
                sync="signed",
                check=True,
            )
        self.assertEqual(
            run.call_args.args[0][-5:],
            ["--round-sync", "signed", "--round-min", "7", "--check"],
        )

    def test_rejoin_checks_meet_floor_exactly(self):
        root = WORK / "candidate"
        tool = root / "artifacts/vote_floor.exe"
        tool.parent.mkdir(parents=True)
        tool.write_bytes(b"floor")
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        result = mock.Mock(stdout="status = vote_floor_checked head_epoch = 41 round = 10\n")
        with mock.patch("validator_rejoin.subprocess.run", return_value=result) as run:
            checked_floor(root, values, wallet, WORK, 41, 10)
        self.assertEqual(run.call_args.args[0][-3:], ["--round", "10", "--check"])
        self.assertTrue(run.call_args.kwargs["capture_output"])

    def test_rejoin_refuses_meet_floor_above_target(self):
        root = WORK / "candidate"
        tool = root / "artifacts/vote_floor.exe"
        tool.parent.mkdir(parents=True)
        tool.write_bytes(b"floor")
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        result = mock.Mock(stdout="status = vote_floor_checked head_epoch = 41 round = 11\n")
        with mock.patch("validator_rejoin.subprocess.run", return_value=result):
            with self.assertRaisesRegex(ValidatorError, "does not exceed durable vote"):
                checked_floor(root, values, wallet, WORK, 41, 10)

    def test_rejoin_requires_staged_floor(self):
        root = WORK / "candidate"
        tool = root / "artifacts/vote_floor.exe"
        tool.parent.mkdir(parents=True)
        tool.write_bytes(b"floor")
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        result = mock.Mock(stdout="status = vote_floor_checked head_epoch = 41 round = 10\n")
        with mock.patch("validator_rejoin.subprocess.run", return_value=result) as run:
            staged_floor(root, values, wallet, WORK, 41, 10)
        self.assertEqual(
            run.call_args.args[0][-4:],
            ["--round", "10", "--check", "--staged"],
        )

    def test_rejoin_checks_prepared_floor(self):
        root = WORK / "candidate"
        tool = root / "artifacts/vote_floor.exe"
        tool.parent.mkdir(parents=True)
        tool.write_bytes(b"floor")
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        result = mock.Mock(stdout="status = vote_floor_checked head_epoch = 41 round = 10\n")
        with mock.patch("validator_rejoin.subprocess.run", return_value=result) as run:
            prepared_floor(root, values, wallet, WORK, 41, 10)
        self.assertEqual(
            run.call_args.args[0][-4:],
            ["--round", "10", "--check", "--prepared"],
        )

    def test_rejoin_meet_round_stays_in_sync_window(self):
        snapshot = {"round_epoch": 42, "round": 7, "peer_floor": 7}
        self.assertEqual(meet_round(snapshot, 41, "1030"), 1030)
        with self.assertRaisesRegex(ValidatorError, "outside local window"):
            meet_round(snapshot, 41, "1031")
        with self.assertRaisesRegex(ValidatorError, "outside local window"):
            meet_round(snapshot, 41, "7")

    def test_rejoin_meet_round_respects_peer_window(self):
        snapshot = {"round_epoch": 42, "round": 50, "peer_floor": 7}
        with self.assertRaisesRegex(ValidatorError, "exceeds peer window"):
            meet_round(snapshot, 41, "1031")
        with self.assertRaisesRegex(ValidatorError, "fresh peer evidence"):
            meet_round({"round_epoch": 42, "round": 50}, 41, "100")

    def test_rejoin_meet_cap_matches_consensus_sync_window(self):
        engine = CONFIG_ROOT / "lib/consensus/c_engine.ml"
        text = engine.read_text(encoding="utf-8")
        match = re.search(r"^let max_sync_ahead = ([0-9]+)$", text, re.MULTILINE)
        self.assertIsNotNone(match)
        self.assertEqual(MEET_CAP, int(match.group(1)))

    def test_rejoin_meet_round_next_requires_peer_gap(self):
        snapshot = {
            "round_epoch": 42,
            "round": 7,
            "peer_round": 1031,
            "peer_floor": 7,
        }
        self.assertEqual(meet_round(snapshot, 41, "next"), 1030)
        with self.assertRaisesRegex(ValidatorError, "outside local window"):
            meet_round({**snapshot, "peer_round": 1030}, 41, "next")

    def test_rejoin_meet_round_requires_current_epoch(self):
        snapshot = {"round_epoch": 41, "round": 7}
        with self.assertRaisesRegex(ValidatorError, "does not match finalized head"):
            meet_round(snapshot, 41, "10")

    def test_rejoin_meet_start_requires_explicit_nonnegative_round(self):
        self.assertEqual(meet_value("19"), 19)
        with self.assertRaisesRegex(ValidatorError, "explicit round"):
            meet_value("next")
        with self.assertRaisesRegex(ValidatorError, "meet round is invalid"):
            meet_value("-1")

    def test_rejoin_meet_stage_requires_active_member(self):
        wallet = {"address": identity()[0]}
        with mock.patch(
            "validator_rejoin.membership",
            return_value={"active": False},
        ):
            with self.assertRaisesRegex(ValidatorError, "active validator"):
                active_member({}, wallet)

    def test_rejoin_meet_stage_accepts_active_member(self):
        wallet = {"address": identity()[0]}
        member = {"active": True}
        with mock.patch("validator_rejoin.membership", return_value=member):
            self.assertEqual(active_member({}, wallet), member)

    def test_rejoin_meet_stage_requires_matched_head_and_voting(self):
        with self.assertRaisesRegex(ValidatorError, "matching finalized head"):
            meet_ready({"local_head": 41, "remote_head": 42, "voting": True})
        with self.assertRaisesRegex(ValidatorError, "voting enabled"):
            meet_ready({"local_head": 41, "remote_head": 41, "voting": False})
        self.assertIsNone(
            meet_ready({
                "state": "round_lagging",
                "local_head": 41,
                "remote_head": 41,
                "voting": True,
            })
        )

    def test_rejoin_meet_stage_refuses_inactive_validator(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_OPERATOR_PM2_NAME": "octra-test",
        }
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        entries = [{
            "name": "octra-test",
            "pid": 17,
            "pm2_env": {
                "env": {"OCTRA_DATA_DIR": str(WORK)},
                "status": "online",
            },
        }]
        before = {
            "state": "synced",
            "voting": True,
            "local_head": 41,
            "remote_head": 41,
            "peer_floor": 7,
            "round_epoch": 42,
            "round": 7,
        }
        with mock.patch("validator_rejoin.require_root"):
            with mock.patch("validator_rejoin.private_mode"):
                with mock.patch("validator_rejoin.parse_env", return_value=values):
                    with mock.patch("validator_rejoin.state_ready", return_value=True):
                        with mock.patch(
                            "validator_rejoin.validate_checkpoint",
                            return_value={"epoch": 41},
                        ):
                            with mock.patch(
                                "validator_rejoin.load_wallet",
                                return_value=wallet,
                            ):
                                with mock.patch(
                                    "validator_rejoin.data_pids",
                                    return_value=[17],
                                ):
                                    with mock.patch(
                                        "validator_rejoin.pm2_entries",
                                        return_value=entries,
                                    ):
                                        with mock.patch(
                                            "validator_rejoin.snapshot",
                                            return_value=before,
                                        ):
                                            with mock.patch(
                                                "validator_rejoin.membership",
                                                return_value={"active": False},
                                            ):
                                                with mock.patch(
                                                    "validator_rejoin.meet_stage",
                                                ) as stage:
                                                    with self.assertRaisesRegex(
                                                        ValidatorError,
                                                        "active validator",
                                                    ):
                                                        rejoin(
                                                            WORK,
                                                            WORK / "node.env",
                                                            WORK,
                                                            30,
                                                            meet="10",
                                                            stage=True,
                                                        )
        stage.assert_not_called()

    def test_rejoin_meet_stage_pauses_before_floor_write(self):
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.confirmed_node") as confirmed:
            with mock.patch("validator_rejoin.pause_node") as pause_node:
                with mock.patch("validator_rejoin.checked_floor") as checked:
                    with mock.patch("validator_rejoin.stop") as stop_node:
                        with mock.patch("validator_rejoin.data_pids", return_value=[]):
                            with mock.patch("validator_rejoin.place_floor") as floor:
                                meet_stage(
                                    WORK,
                                    WORK / "node.env",
                                    values,
                                    wallet,
                                    WORK,
                                    41,
                                    17,
                                    10,
                                )
        confirmed.assert_called_once_with(values, WORK, 17)
        pause_node.assert_called_once_with(17)
        checked.assert_called_once_with(WORK, values, wallet, WORK, 41, 10)
        stop_node.assert_called_once_with(WORK, WORK / "node.env")
        floor.assert_called_once_with(WORK, values, wallet, WORK, 41, 10)

    def test_rejoin_meet_start_requires_staged_floor(self):
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.data_pids", return_value=[]):
            with mock.patch("validator_rejoin.prepared_floor") as prepared:
                with mock.patch("validator_rejoin.place_floor") as floor:
                    with mock.patch("validator_rejoin.staged_floor") as staged:
                        with mock.patch("validator_rejoin.launch") as launch_node:
                            meet_start(
                                WORK,
                                WORK / "node.env",
                                values,
                                wallet,
                                WORK,
                                41,
                                10,
                            )
        prepared.assert_called_once_with(WORK, values, wallet, WORK, 41, 10)
        floor.assert_called_once_with(WORK, values, wallet, WORK, 41, 10)
        staged.assert_called_once_with(WORK, values, wallet, WORK, 41, 10)
        launch_node.assert_called_once_with(WORK, WORK / "node.env")

    def test_rejoin_meet_start_refuses_running_node(self):
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.data_pids", return_value=[17]):
            with self.assertRaisesRegex(ValidatorError, "must be stopped"):
                meet_start(
                    WORK,
                    WORK / "node.env",
                    values,
                    wallet,
                    WORK,
                    41,
                    10,
                )

    def test_rejoin_confirms_process_before_meet_stage(self):
        values = {"OCTRA_OPERATOR_PM2_NAME": "octra-test"}
        entries = [{
            "name": "octra-test",
            "pid": 17,
            "pm2_env": {
                "env": {"OCTRA_DATA_DIR": str(WORK)},
                "status": "online",
            },
        }]
        with mock.patch("validator_rejoin.pm2_entries", return_value=entries):
            with mock.patch("validator_rejoin.data_pids", return_value=[17]):
                with mock.patch("validator_rejoin.running_binary") as binary:
                    confirmed_node(values, WORK, 17)
        binary.assert_called_once_with(17)

    def test_rejoin_meet_stage_restores_unmarked_failure(self):
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.confirmed_node"):
            with mock.patch("validator_rejoin.pause_node"):
                with mock.patch("validator_rejoin.checked_floor"):
                    with mock.patch("validator_rejoin.stop"):
                        with mock.patch(
                            "validator_rejoin.data_pids",
                            side_effect=[[], []],
                        ):
                            with mock.patch(
                                "validator_rejoin.place_floor",
                                side_effect=ValidatorError("floor write failed"),
                            ):
                                with mock.patch(
                                    "validator_rejoin.stage_marked",
                                    return_value=False,
                                ):
                                    with mock.patch("validator_rejoin.launch") as launch_node:
                                        with self.assertRaises(MeetError) as raised:
                                            meet_stage(
                                                WORK,
                                                WORK / "node.env",
                                                values,
                                                wallet,
                                                WORK,
                                                41,
                                                17,
                                                10,
                                            )
        self.assertEqual(raised.exception.state, "restored")
        launch_node.assert_called_once_with(WORK, WORK / "node.env")

    def test_rejoin_meet_stage_keeps_prepared_node_stopped(self):
        values = {"OCTRA_CHAIN_ID": "octra-devnet-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.confirmed_node"):
            with mock.patch("validator_rejoin.pause_node"):
                with mock.patch("validator_rejoin.checked_floor"):
                    with mock.patch("validator_rejoin.stop"):
                        with mock.patch("validator_rejoin.data_pids", return_value=[]):
                            with mock.patch(
                                "validator_rejoin.place_floor",
                                side_effect=ValidatorError("floor write failed"),
                            ):
                                with mock.patch(
                                    "validator_rejoin.stage_marked",
                                    return_value=True,
                                ):
                                    with mock.patch("validator_rejoin.launch") as launch_node:
                                        with self.assertRaises(MeetError) as raised:
                                            meet_stage(
                                                WORK,
                                                WORK / "node.env",
                                                values,
                                                wallet,
                                                WORK,
                                                41,
                                                17,
                                                10,
                                            )
        self.assertEqual(raised.exception.state, "stopped")
        self.assertIn("--meet-start --wait-seconds 600", raised.exception.command)
        launch_node.assert_not_called()

    def test_rejoin_requires_running_validator(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK),
            "OCTRA_OPERATOR_ROLE": "validator",
        }
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.require_root"):
            with mock.patch("validator_rejoin.private_mode"):
                with mock.patch("validator_rejoin.parse_env", return_value=values):
                    with mock.patch("validator_rejoin.state_ready", return_value=True):
                        with mock.patch(
                            "validator_rejoin.validate_checkpoint",
                            return_value={"epoch": 41},
                        ):
                            with mock.patch(
                                "validator_rejoin.load_wallet",
                                return_value=wallet,
                            ):
                                with mock.patch(
                                    "validator_rejoin.data_pids",
                                    return_value=[],
                                ):
                                    with self.assertRaisesRegex(
                                        ValidatorError,
                                        "running validator is required",
                                    ):
                                        rejoin(
                                            WORK,
                                            WORK / "node.env",
                                            WORK,
                                            30,
                                        )

    def test_rejoin_captures_round_before_stopping(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_OPERATOR_PM2_NAME": "octra-test",
        }
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        entries = [{
            "name": "octra-test",
            "pid": 17,
            "pm2_env": {
                "env": {"OCTRA_DATA_DIR": str(WORK)},
                "status": "online",
            },
        }]
        before = {"round_epoch": 42, "round": 7, "peer_round": 9}
        after = {
            "state": "synced",
            "pid": 18,
            "local_head": 41,
            "remote_head": 41,
            "voting": True,
            "round": 8,
            "peer_round": 8,
            "round_peers": 4,
            "round_agreed": True,
        }
        with mock.patch("validator_rejoin.require_root"):
            with mock.patch("validator_rejoin.private_mode"):
                with mock.patch("validator_rejoin.parse_env", return_value=values):
                    with mock.patch("validator_rejoin.state_ready", return_value=True):
                        with mock.patch(
                            "validator_rejoin.validate_checkpoint",
                            return_value={"epoch": 41},
                        ):
                            with mock.patch(
                                "validator_rejoin.load_wallet",
                                return_value=wallet,
                            ):
                                with mock.patch(
                                    "validator_rejoin.data_pids",
                                    side_effect=[[17], []],
                                ):
                                    with mock.patch(
                                        "validator_rejoin.pm2_entries",
                                        return_value=entries,
                                    ):
                                        with mock.patch(
                                            "validator_rejoin.snapshot",
                                            side_effect=[before, after],
                                        ):
                                            with mock.patch(
                                                "validator_rejoin.place_floor",
                                            ) as floor:
                                                with mock.patch(
                                                    "validator_rejoin.stop",
                                                ) as stop_node:
                                                    with mock.patch(
                                                        "validator_rejoin.launch",
                                                    ) as launch_node:
                                                        with mock.patch(
                                                            "validator_rejoin.report_ready",
                                                            return_value=0,
                                                        ):
                                                            with mock.patch("validator_rejoin.emit"):
                                                                result = rejoin(
                                                                    WORK,
                                                                    WORK / "node.env",
                                                                    WORK,
                                                                    30,
                                                                )
        self.assertEqual(result, 0)
        self.assertEqual(
            floor.call_args_list[0].args,
            (WORK, values, wallet, WORK, 41, 8),
        )
        self.assertTrue(floor.call_args_list[0].kwargs["check"])
        self.assertEqual(
            floor.call_args_list[1].args,
            (WORK, values, wallet, WORK, 41, 8),
        )
        stop_node.assert_called_once_with(WORK, WORK / "node.env")
        launch_node.assert_called_once_with(WORK, WORK / "node.env")

    def test_rejoin_check_never_stops_validator(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_OPERATOR_PM2_NAME": "octra-test",
        }
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        entries = [{
            "name": "octra-test",
            "pid": 17,
            "pm2_env": {
                "env": {"OCTRA_DATA_DIR": str(WORK)},
                "status": "online",
            },
        }]
        before = {"round_epoch": 42, "round": 7, "peer_round": 9}
        with mock.patch("validator_rejoin.require_root"):
            with mock.patch("validator_rejoin.private_mode"):
                with mock.patch("validator_rejoin.parse_env", return_value=values):
                    with mock.patch("validator_rejoin.state_ready", return_value=True):
                        with mock.patch(
                            "validator_rejoin.validate_checkpoint",
                            return_value={"epoch": 41},
                        ):
                            with mock.patch(
                                "validator_rejoin.load_wallet",
                                return_value=wallet,
                            ):
                                with mock.patch(
                                    "validator_rejoin.data_pids",
                                    return_value=[17],
                                ):
                                    with mock.patch(
                                        "validator_rejoin.pm2_entries",
                                        return_value=entries,
                                    ):
                                        with mock.patch(
                                            "validator_rejoin.snapshot",
                                            return_value=before,
                                        ):
                                            with mock.patch(
                                                "validator_rejoin.place_floor",
                                            ) as floor:
                                                with mock.patch(
                                                    "validator_rejoin.stop",
                                                ) as stop_node:
                                                    with mock.patch(
                                                        "validator_rejoin.launch",
                                                    ) as launch_node:
                                                        with mock.patch("validator_rejoin.emit"):
                                                            result = rejoin(
                                                                WORK,
                                                                WORK / "node.env",
                                                                WORK,
                                                                30,
                                                                check=True,
                                                            )
        self.assertEqual(result, 0)
        floor.assert_called_once()
        self.assertTrue(floor.call_args.kwargs["check"])
        stop_node.assert_not_called()
        launch_node.assert_not_called()

    def test_rejoin_legacy_pauses_before_handoff(self):
        values = {
            "OCTRA_DATA_DIR": str(WORK),
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_OPERATOR_PM2_NAME": "octra-test",
        }
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        entries = [{
            "name": "octra-test",
            "pid": 17,
            "pm2_env": {
                "env": {"OCTRA_DATA_DIR": str(WORK)},
                "status": "online",
            },
        }]
        before = {"pid": 17}
        after = {
            "state": "synced",
            "pid": 18,
            "local_head": 41,
            "remote_head": 41,
            "voting": True,
            "round": 13,
            "peer_round": 13,
            "round_peers": 4,
            "round_agreed": True,
        }
        with mock.patch("validator_rejoin.require_root"):
            with mock.patch("validator_rejoin.private_mode"):
                with mock.patch("validator_rejoin.parse_env", return_value=values):
                    with mock.patch("validator_rejoin.state_ready", return_value=True):
                        with mock.patch(
                            "validator_rejoin.validate_checkpoint",
                            return_value={"epoch": 41},
                        ):
                            with mock.patch(
                                "validator_rejoin.load_wallet",
                                return_value=wallet,
                            ):
                                with mock.patch(
                                    "validator_rejoin.data_pids",
                                    side_effect=[[17], [], []],
                                ):
                                    with mock.patch(
                                        "validator_rejoin.pm2_entries",
                                        return_value=entries,
                                    ):
                                        with mock.patch(
                                            "validator_rejoin.snapshot",
                                            side_effect=[before, after],
                                        ):
                                            with mock.patch(
                                                "validator_rejoin.log_round",
                                                return_value=14,
                                            ):
                                                with mock.patch(
                                                    "validator_rejoin.signed_round",
                                                    return_value="signed",
                                                ):
                                                    with mock.patch(
                                                        "validator_rejoin.place_floor",
                                                    ) as floor:
                                                        with mock.patch(
                                                            "validator_rejoin.stop",
                                                        ):
                                                            with mock.patch(
                                                                "validator_rejoin.pause_node",
                                                            ) as pause_node:
                                                                with mock.patch(
                                                                    "validator_rejoin.resume_node",
                                                                ) as resume_node:
                                                                    with mock.patch(
                                                                        "validator_rejoin.launch",
                                                                    ):
                                                                        with mock.patch(
                                                                            "validator_rejoin.report_ready",
                                                                            return_value=0,
                                                                        ):
                                                                            with mock.patch("validator_rejoin.emit"):
                                                                                result = rejoin(
                                                                                    WORK,
                                                                                    WORK / "node.env",
                                                                                    WORK,
                                                                                    30,
                                                                                    legacy=True,
                                                                                )
        self.assertEqual(result, 0)
        pause_node.assert_called_once_with(17)
        resume_node.assert_not_called()
        self.assertEqual(floor.call_args_list[0].args[5], 14)
        self.assertEqual(floor.call_args_list[1].args[5], 14)

    def test_legacy_handoff_resumes_after_log_refusal(self):
        values = {"OCTRA_OPERATOR_PM2_NAME": "octra-test"}
        address, public_key = identity()
        wallet = {"address": address, "pub": public_key}
        with mock.patch("validator_rejoin.pause_node") as pause_node:
            with mock.patch(
                "validator_rejoin.log_round",
                side_effect=ValidatorError("validator round is unavailable in local log"),
            ):
                with mock.patch("validator_rejoin.resume_node") as resume_node:
                    with mock.patch("validator_rejoin.stop") as stop_node:
                        with self.assertRaisesRegex(ValidatorError, "round is unavailable"):
                            legacy_handoff(
                                WORK,
                                WORK / "node.env",
                                values,
                                wallet,
                                WORK,
                                41,
                                17,
                                "signed",
                            )
        pause_node.assert_called_once_with(17)
        resume_node.assert_called_once_with(17)
        stop_node.assert_not_called()

    def test_rejoin_captured_round_requires_current_epoch(self):
        with self.assertRaisesRegex(ValidatorError, "does not match"):
            captured_round({"round_epoch": 41, "round": 7}, 41)

    def test_rejoin_captured_round_uses_local_round(self):
        self.assertEqual(
            captured_round({"round_epoch": 42, "round": 7, "peer_round": 9}, 41),
            8,
        )

    def test_rejoin_requires_explicit_legacy_handoff(self):
        snapshot = {}
        values = {"OCTRA_OPERATOR_PM2_NAME": "octra-test"}
        wallet = {"address": identity()[0]}
        with self.assertRaisesRegex(ValidatorError, "legacy handoff"):
            floor_source(snapshot, values, wallet, 41, False)

    def test_rejoin_uses_signed_legacy_handoff(self):
        snapshot = {}
        values = {"OCTRA_OPERATOR_PM2_NAME": "octra-test"}
        wallet = {"address": identity()[0]}
        with mock.patch("validator_rejoin.signed_round", return_value="signed"):
            self.assertEqual(
                floor_source(snapshot, values, wallet, 41, True),
                (None, "signed"),
            )

    def test_rejoin_reads_local_round_log(self):
        output = (
            "event = round_skip height = 42 old_round = 9 new_round = 10\n"
            "event = make_proposal epoch = 42 round = 11\n"
        )
        result = mock.Mock(returncode=0, stdout=output)
        with mock.patch("validator_rejoin.subprocess.run", return_value=result):
            self.assertEqual(log_round("octra-test", 42), 11)

    def test_rejoin_refuses_missing_local_round_log(self):
        result = mock.Mock(returncode=0, stdout="")
        with mock.patch("validator_rejoin.subprocess.run", return_value=result):
            with self.assertRaisesRegex(ValidatorError, "local log"):
                log_round("octra-test", 42)

    def test_rejoin_reads_signed_round(self):
        values = {"OCTRA_OPERATOR_RPC_URL": "https://devnet.example/rpc"}
        wallet = {"address": identity()[0]}
        with mock.patch(
            "validator_rejoin.rpc_call",
            return_value={"round_sync": "signed"},
        ):
            self.assertEqual(signed_round(values, wallet), "signed")

    def test_rejoin_witness_sets_operator_agent(self):
        response = mock.Mock()
        response.read.return_value = b'{"result":{"round_sync":"signed"}}'
        context = mock.Mock()
        context.__enter__ = mock.Mock(return_value=response)
        context.__exit__ = mock.Mock(return_value=False)
        with mock.patch("urllib.request.urlopen", return_value=context) as open_rpc:
            self.assertEqual(
                rpc_call(
                    "https://devnet.example/rpc",
                    "octra_roundWitness",
                    [identity()[0]],
                ),
                {"round_sync": "signed"},
            )
        request = open_rpc.call_args.args[0]
        self.assertEqual(request.get_header("User-agent"), "octra-validator-rejoin/1")

    def test_rejoin_requires_one_online_process(self):
        data_dir = WORK.resolve()
        entries = [{
            "name": "octra-test",
            "pid": 17,
            "pm2_env": {
                "OCTRA_DATA_DIR": str(data_dir),
                "status": "online",
            },
        }]
        self.assertEqual(online_entry(entries, "octra-test", data_dir), 17)
        with self.assertRaisesRegex(ValidatorError, "exactly one"):
            online_entry(entries + entries, "octra-test", data_dir)

    def test_pm2_data_reads_both_forms(self):
        data_dir = str(WORK.resolve())
        self.assertEqual(
            entry_data({"pm2_env": {"OCTRA_DATA_DIR": data_dir}}),
            data_dir,
        )
        self.assertEqual(
            entry_data({"pm2_env": {"env": {"OCTRA_DATA_DIR": data_dir}}}),
            data_dir,
        )
        self.assertIsNone(
            entry_data({
                "pm2_env": {
                    "OCTRA_DATA_DIR": data_dir,
                    "env": {"OCTRA_DATA_DIR": data_dir + "/other"},
                },
            }),
        )

    def test_rejoin_peer_head_ignores_invalid_entries(self):
        payload = {
            "peers": [
                {"head_epoch": "41"},
                {"head_epoch": 39},
                {"head_epoch": "bad"},
                {},
            ],
        }
        self.assertEqual(peer_head(payload), 41)
        self.assertIsNone(peer_head({"peers": []}))

    def test_rejoin_reads_vote_state(self):
        self.assertEqual(vote_state({"voting": True}), (True, None))
        self.assertEqual(
            vote_state({"voting": False, "voting_reason": "vote_log_conflict"}),
            (False, "vote_log_conflict"),
        )
        self.assertEqual(vote_state({"voting": "true"}), (None, None))

    def test_status_reports_round_view(self):
        self.assertEqual(
            round_view({
                "round_state": {
                    "epoch_id": "41",
                    "round": 18,
                    "step": "prevote",
                },
                "round_agreed": True,
                "round_peers": [{}, {}],
            }),
            {
                "round_epoch": 41,
                "round": 18,
                "round_step": "prevote",
                "round_agreed": True,
                "round_peers": 2,
            },
        )
        self.assertEqual(round_view({"round_state": {}}), {})

    def test_rejoin_requires_round_alignment(self):
        aligned = round_alignment({
            "round_state": {"epoch_id": "41", "round": 18},
            "round_agreed": True,
            "round_peers": [
                {"epoch_id": "41", "round": 17, "age_sec": 1.0},
                {"epoch_id": "41", "round": 19, "age_sec": 2.0},
            ],
        })
        self.assertEqual(aligned["state"], "round_aligned")
        lagging = round_alignment({
            "round_state": {"epoch_id": "41", "round": 16},
            "round_agreed": False,
            "round_peers": [
                {"epoch_id": "41", "round": 186, "age_sec": 1.0},
            ],
        })
        self.assertEqual(lagging["state"], "round_lagging")
        self.assertEqual(lagging["peer_round"], 186)
        unconfirmed = round_alignment({
            "round_state": {"epoch_id": "41", "round": 186},
            "round_agreed": False,
            "round_peers": [
                {"epoch_id": "41", "round": 186, "age_sec": 1.0},
            ],
        })
        self.assertEqual(unconfirmed["state"], "round_unconfirmed")
        self.assertEqual(
            round_alignment({
                "round_state": {"epoch_id": "41", "round": 18},
                "round_agreed": True,
                "round_peers": [],
            })["state"],
            "round_aligned",
        )
        waiting = round_alignment({
            "round_state": {"epoch_id": "41", "round": 18},
            "round_agreed": False,
            "round_peers": [],
        })
        self.assertEqual(waiting["state"], "waiting_round")

    def test_rejoin_keeps_recent_peer_floor_for_meet(self):
        status = round_alignment({
            "round_state": {"epoch_id": "41", "round": 18},
            "round_agreed": False,
            "round_peers": [
                {"epoch_id": "41", "round": 11, "age_sec": 41.0},
            ],
        })
        self.assertEqual(status["state"], "waiting_round")
        self.assertEqual(status["peer_floor"], 11)

    def test_rejoin_requires_current_round_not_global_agreement(self):
        self.assertEqual(
            sync_state(41, 41, {"state": "round_unconfirmed"}),
            "synced",
        )
        self.assertEqual(
            sync_state(41, 41, {"state": "round_lagging"}),
            "round_lagging",
        )
        self.assertEqual(
            sync_state(41, 41, {"state": "waiting_round"}),
            "waiting_round",
        )

    def test_snapshot_keeps_local_vote_ready_state(self):
        values = {
            "OCTRA_OPERATOR_PM2_NAME": "octra-test",
            "OCTRA_API_PORT": "8080",
        }
        entries = [{
            "name": "octra-test",
            "pid": 17,
            "pm2_env": {
                "env": {"OCTRA_DATA_DIR": str(WORK)},
                "status": "online",
            },
        }]
        peers = {
            "peers": [{"head_epoch": "41"}],
            "round_state": {"epoch_id": "42", "round": 18},
            "round_agreed": False,
            "round_peers": [{"epoch_id": "42", "round": 18, "age_sec": 1.0}],
            "voting": True,
        }
        with mock.patch("validator_rejoin.pm2_entries", return_value=entries):
            with mock.patch("validator_rejoin.data_pids", return_value=[17]):
                with mock.patch("validator_rejoin.running_binary", return_value=WORK):
                    with mock.patch("validator_rejoin.rpc_status", return_value={"head_epoch": "41"}):
                        with mock.patch("validator_rejoin.rpc_method", return_value=peers):
                            self.assertEqual(snapshot(values, WORK)["state"], "synced")

    def test_rejoin_refuses_vote_log_conflict(self):
        values = {"OCTRA_OPERATOR_ROLE": "validator"}
        wallet = {"address": identity()[0]}
        snapshot = {
            "pid": 17,
            "local_head": 41,
            "remote_head": 41,
            "voting": False,
            "voting_reason": "vote_log_conflict",
        }
        member = {
            "active": True,
            "scheduled": False,
            "activate_epoch": None,
        }
        with mock.patch("validator_rejoin.membership", return_value=member):
            with mock.patch("validator_rejoin.emit") as emit:
                self.assertEqual(report_ready(values, wallet, snapshot), 4)
        self.assertEqual(emit.call_args.kwargs["status"], "voting_disabled")
        self.assertEqual(
            emit.call_args.kwargs["reason"],
            "vote_log_conflict",
        )

    def test_rejoin_reports_meet_only_after_finality_advances(self):
        values = {"OCTRA_OPERATOR_ROLE": "validator"}
        wallet = {"address": identity()[0]}
        member = {"active": True}
        current = {
            "pid": 17,
            "local_head": 41,
            "voting": True,
        }
        with mock.patch("validator_rejoin.membership", return_value=member):
            with mock.patch("validator_rejoin.emit") as emit:
                self.assertIsNone(report_meet(values, wallet, current, 41, 10))
                self.assertEqual(
                    report_meet(
                        values,
                        wallet,
                        {**current, "local_head": 42},
                        41,
                        10,
                    ),
                    0,
                )
        self.assertEqual(emit.call_args.kwargs["status"], "meet_finalized")

    def test_rejoin_waits_ready(self):
        values = {"OCTRA_OPERATOR_ROLE": "validator"}
        wallet = {"address": identity()[0]}
        snapshot = {
            "pid": 17,
            "local_head": 41,
            "remote_head": 41,
            "voting": False,
            "voting_reason": "not_ready",
        }
        member = {
            "active": True,
            "scheduled": False,
            "activate_epoch": None,
        }
        with mock.patch("validator_rejoin.membership", return_value=member):
            with mock.patch("validator_rejoin.emit") as emit:
                self.assertIsNone(report_ready(values, wallet, snapshot))
        emit.assert_not_called()

    def test_rejoin_wait_bounds(self):
        self.assertEqual(positive_seconds("30"), 30)
        with self.assertRaisesRegex(ValidatorError, "outside"):
            positive_seconds("29")

    def test_rejoin_launch_rebinds_runtime(self):
        root = WORK / "release"
        config = WORK / "node.env"
        root.mkdir()
        config.write_text("OCTRA_DATA_DIR=/data\n", encoding="utf-8")
        with mock.patch("validator_rejoin.subprocess.run") as run:
            launch(root, config)
        command = run.call_args.args[0]
        environment = run.call_args.kwargs["env"]
        self.assertEqual(command[-1], "--rebind-runtime")
        self.assertEqual(environment["OCTRA_OPERATOR_CONFIG"], str(config))

    def test_validator_accepts_active_on_chain_identity(self):
        require_validator_membership(
            "validator",
            {"active": True, "scheduled": False},
        )

    def test_validator_accepts_scheduled_on_chain_identity(self):
        require_validator_membership(
            "validator",
            {"active": False, "scheduled": True},
        )

    def test_validator_rejects_unselected_identity(self):
        with self.assertRaisesRegex(
            ValidatorError,
            "not active or scheduled",
        ):
            require_validator_membership(
                "validator",
                {"active": False, "scheduled": False},
            )

    def test_network_bundle_round_trip(self):
        bundle = WORK / "network.env"
        values = network_values()
        write_env(bundle, values)
        digest = hashlib.sha256(bundle.read_bytes()).hexdigest()
        _, loaded_digest, loaded = load_network(bundle, digest)
        self.assertEqual(loaded_digest, digest)
        self.assertEqual(loaded["OCTRA_FHE_MAX_PER_EPOCH"], "1")
        self.assertEqual(loaded["OCTRA_BFT_PROPOSE_TIMEOUT_MS"], "180000")
        self.assertEqual(loaded["OCTRA_PEERS"], values["OCTRA_BOOTSTRAP_PEERS"])

    def test_network_rejects_unsafe_override(self):
        values = network_values()
        values["OCTRA_ALLOW_UNSAFE_QUORUM"] = "1"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_address_key_mismatch(self):
        values = network_values()
        entries = values["OCTRA_VALIDATORS"].split(",")
        address, _ = entries[0].split(":", 1)
        _, other_key = identity()
        entries[0] = f"{address}:{other_key}"
        values["OCTRA_VALIDATORS"] = ",".join(entries)
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_activation_drift(self):
        values = network_values()
        values["OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH"] = "101"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_missing_migration_root(self):
        values = network_values()
        del values["OCTRA_PVAC_MIGRATION_ROOT"]
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_invalid_migration_root(self):
        values = network_values()
        values["OCTRA_PVAC_MIGRATION_ROOT"] = "invalid"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_bundle_validator_is_source_build_independent(self):
        bundle = WORK / "network.env"
        values = network_values()
        write_env(bundle, values)
        loaded, digest = validate_bundle(bundle)
        self.assertNotIn("OCTRA_BINARY_HASH", loaded)
        self.assertEqual(digest, hashlib.sha256(bundle.read_bytes()).hexdigest())

    def test_network_requires_permissionless_validator_activation(self):
        values = network_values()
        del values["OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH"]
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_requires_proposal_protocol_activation(self):
        values = network_values()
        del values["OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH"]
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_allows_checkpoint_rotation(self):
        values = network_values()
        values["OCTRA_CHECKPOINT_EPOCH"] = "250"
        values["OCTRA_CHECKPOINT_STATE_ROOT"] = "5" * 64
        values["OCTRA_CHECKPOINT_TXID_HI"] = "900"
        validated = validate_network(values, WORK)
        self.assertEqual(validated["OCTRA_CHECKPOINT_EPOCH"], "250")

    def test_proposal_protocol_activation_cannot_precede_emission(self):
        values = network_values()
        values["OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH"] = "99"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_validator_activation_cannot_precede_emission(self):
        values = network_values()
        values["OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH"] = "99"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_public_plain_http_state_sync(self):
        values = network_values()
        values["OCTRA_STATE_SYNC_SOURCES"] = (
            "http://203.0.113.1:8080,https://seed-b.example"
        )
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_allows_one_signed_state_sync_source(self):
        values = network_values()
        values["OCTRA_STATE_SYNC_SOURCES"] = "https://seed-a.example"
        self.assertEqual(
            validate_network(values, WORK)["OCTRA_STATE_SYNC_SOURCES"],
            "https://seed-a.example",
        )

    def test_network_validates_join_sources(self):
        values = network_values()
        values["OCTRA_JOIN_RPC"] = "https://join-a.example,https://join-b.example"
        self.assertEqual(
            validate_network(values, WORK)["OCTRA_JOIN_RPC"],
            "https://join-a.example,https://join-b.example",
        )
        values["OCTRA_JOIN_RPC"] = "http://203.0.113.1:8080"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_advertise_uses_consensus_port(self):
        self.assertEqual(
            validate_advertise("203.0.113.1:19000", 19000),
            "203.0.113.1:19000",
        )
        with self.assertRaises(ValidatorError):
            validate_advertise("203.0.113.1:9000", 19000)

    def test_rpc_url_requires_secure_public_transport(self):
        self.assertEqual(
            rpc_url("https://devnet.example/rpc"),
            "https://devnet.example/rpc",
        )
        self.assertEqual(
            rpc_url("http://127.0.0.1:8080"),
            "http://127.0.0.1:8080/rpc",
        )
        with self.assertRaises(ValidatorError):
            rpc_url("http://203.0.113.1:8080/rpc")

    def test_membership_requires_exact_identity(self):
        address, pubkey = identity()
        wallet = {"address": address, "pub": pubkey}
        self.assertTrue(
            exact_member([{"address": address, "pubkey": pubkey}], wallet)
        )
        self.assertFalse(exact_member([], wallet))
        with self.assertRaises(ValidatorError):
            exact_member([{"address": address, "pubkey": identity()[1]}], wallet)

    def test_membership_reports_only_own_activation_epoch(self):
        address, pubkey = identity()
        wallet = {"address": address, "pub": pubkey}
        values = {"OCTRA_API_PORT": "8080", "OCTRA_CHAIN_ID": "octra-test"}
        proof = {
            "chain_id": "octra-test",
            "validators": [],
            "scheduled": {
                "activate_epoch": "1372752",
                "validators": [],
            },
            "validator_set_hash": "a" * 64,
        }
        with mock.patch("validator_enroll.call", return_value=proof):
            state = membership(values, wallet)
        self.assertFalse(state["scheduled"])
        self.assertIsNone(state["activate_epoch"])
        self.assertEqual(state["next_set_epoch"], 1372752)
        proof["scheduled"]["validators"] = [
            {"address": address, "pubkey": pubkey},
        ]
        with mock.patch("validator_enroll.call", return_value=proof):
            state = membership(values, wallet)
        self.assertTrue(state["scheduled"])
        self.assertEqual(state["activate_epoch"], 1372752)

    def test_committed_enrollment_requires_exact_identity(self):
        address, pubkey = identity()
        wallet = {"address": address, "pub": pubkey}
        values = {"OCTRA_API_PORT": "8080"}
        payload = {
            "head_epoch": 1372752,
            "address": address,
            "consensus_pubkey": pubkey,
            "identity": "self_reported",
            "state": "ready",
            "bond": "1000000",
            "bonded_epoch": "1300000",
            "ready_epoch": "1300064",
            "exit_epoch": None,
        }
        node = {"head_epoch": 1372752, "state_root": "a" * 64}
        with mock.patch(
            "validator_enroll.call",
            side_effect=[payload, node],
        ):
            enrollment = committed_enrollment(values, wallet)
        self.assertEqual(enrollment.state, EnrollmentState.READY)
        self.assertEqual(enrollment.bond, 1000000)
        with mock.patch(
            "validator_enroll.call",
            return_value={**payload, "consensus_pubkey": identity()[1]},
        ):
            with self.assertRaises(ValidatorError):
                committed_enrollment(values, wallet)
        with mock.patch(
            "validator_enroll.call",
            side_effect=[payload, {**node, "head_epoch": 1372753}],
        ):
            with self.assertRaises(ValidatorError):
                committed_enrollment(values, wallet)
        with mock.patch(
            "validator_enroll.call",
            return_value={**payload, "ready_epoch": 1300064.5},
        ):
            with self.assertRaises(ValidatorError):
                committed_enrollment(values, wallet)

    def test_join_step_uses_committed_enrollment(self):
        member = {"active": False, "scheduled": False}
        absent = Enrollment(EnrollmentState.ABSENT, 9, None, None, None, None)
        bonded = Enrollment(EnrollmentState.BONDED, 9, 1000000, 1, None, None)
        ready = Enrollment(EnrollmentState.READY, 9, 1000000, 1, 8, None)
        exiting = Enrollment(EnrollmentState.EXITING, 9, 1000000, 1, 8, 9)
        self.assertEqual(join_step(member, absent), JoinStep.SUBMIT_BOND)
        self.assertEqual(join_step(member, bonded), JoinStep.SUBMIT_READY)
        self.assertEqual(join_step(member, ready), JoinStep.WAIT_SELECTION)
        self.assertEqual(join_step(member, exiting), JoinStep.REFUSE)
        self.assertEqual(
            join_step({"active": True, "scheduled": False}, exiting),
            JoinStep.ACTIVATE,
        )

    def test_existing_committed_bond_blocks_duplicate(self):
        enrollment = Enrollment(
            EnrollmentState.BONDED,
            9,
            1000000,
            1,
            None,
            None,
        )
        args = mock.Mock()
        with mock.patch(
            "validator_enroll.committed_enrollment",
            return_value=enrollment,
        ), mock.patch("validator_enroll.control_result") as control:
            with self.assertRaises(ValidatorError):
                submit_bond(
                    WORK / "node.env",
                    {},
                    {},
                    WORK / "wallet.json",
                    1000000,
                    args,
                )
        control.assert_not_called()

    def test_enrollment_nonce_comes_from_local_observer(self):
        wallet = {"address": identity()[0]}
        values = {"OCTRA_API_PORT": "8080"}
        with mock.patch(
            "validator_enroll.call",
            return_value={"nonce": 41},
        ) as invoke:
            self.assertEqual(next_nonce(values, wallet), 42)
        self.assertEqual(
            invoke.call_args.args,
            ("http://127.0.0.1:8080/rpc", "octra_account", [wallet["address"], 1]),
        )

    def test_admission_waits_for_activation_epoch(self):
        values = {
            "OCTRA_API_PORT": "8080",
            "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH": "100",
        }
        with mock.patch(
            "validator_enroll.call",
            return_value={"head_epoch": 99, "state_root": "a" * 64},
        ):
            with self.assertRaises(ValidatorError):
                require_admission_active(values)
        with mock.patch(
            "validator_enroll.call",
            return_value={"head_epoch": 100, "state_root": "a" * 64},
        ):
            require_admission_active(values)

    def test_validator_mode_runs_control_through_shell(self):
        config = WORK / "node.env"
        values = {"OCTRA_OPERATOR_ROLE": "observer"}
        state = {
            "active": True,
            "scheduled": False,
            "activate_epoch": 100,
        }
        with mock.patch("validator_enroll.subprocess.run") as invoke:
            set_validator_mode(config, values, state, True)
        invoke.assert_called_once_with(
            ["sh", str(CONFIG_ROOT / "controls/run.sh")],
            cwd=CONFIG_ROOT,
            check=True,
        )
        self.assertEqual(parse_env(config)["OCTRA_OPERATOR_ROLE"], "validator")

    def test_active_validator_waits_for_committed_head(self):
        values = {"OCTRA_API_PORT": "8080"}
        wallet = {"address": identity()[0], "pub": identity()[1]}
        state = {
            "active": True,
            "scheduled": False,
            "activate_epoch": 100,
        }
        args = mock.Mock(wait_seconds=1, poll_seconds=0.1)
        with mock.patch(
            "validator_enroll.node_status",
            side_effect=[(100, "a" * 64), (101, "b" * 64)],
        ), mock.patch(
            "validator_enroll.membership",
            return_value=state,
        ), mock.patch(
            "validator_enroll.time.monotonic",
            side_effect=[0.0, 0.0],
        ), mock.patch("validator_enroll.time.sleep") as pause:
            self.assertEqual(
                wait_active_commit(values, wallet, state, args),
                state,
            )
        pause.assert_not_called()

    def test_selection_timeout_remains_pending(self):
        values = {"OCTRA_API_PORT": "8080"}
        wallet = {"address": identity()[0], "pub": identity()[1]}
        state = {
            "active": False,
            "scheduled": False,
            "activate_epoch": None,
            "next_set_epoch": 128,
        }
        args = mock.Mock(wait_seconds=1, poll_seconds=0.1)
        with mock.patch(
            "validator_enroll.membership",
            return_value=state,
        ), mock.patch(
            "validator_enroll.time.monotonic",
            side_effect=[0.0, 0.0, 1.0],
        ), mock.patch("validator_enroll.time.sleep") as pause:
            self.assertIsNone(wait_scheduled(values, wallet, args))
        pause.assert_called_once_with(0.1)

    def test_join_resumes_confirmed_transaction(self):
        config = WORK / "node.env"
        config.write_text("", encoding="utf-8")
        state = {
            "version": "octra-validator-enrollment",
            "transactions": {
                "bond": {
                    "tx_hash": "a" * 64,
                    "submitted_at": 1,
                },
            },
        }
        (WORK / "enrollment.json").write_text(
            json.dumps(state),
            encoding="utf-8",
        )
        values = {"OCTRA_API_PORT": "8080"}
        args = mock.Mock(no_wait=False, wait_seconds=1, poll_seconds=0.1)
        with mock.patch(
            "validator_enroll.transaction",
            return_value={"status": "confirmed", "epoch": 101},
        ):
            self.assertTrue(
                resume_join_transaction(config, values, "bond", args)
            )
        self.assertFalse(
            resume_join_transaction(config, values, "ready", args)
        )

    def test_missing_ready_transaction_can_be_resubmitted(self):
        config = WORK / "node.env"
        config.write_text("", encoding="utf-8")
        state = {
            "version": "octra-validator-enrollment",
            "transactions": {
                "ready": {
                    "tx_hash": "b" * 64,
                    "submitted_at": 1,
                },
            },
        }
        (WORK / "enrollment.json").write_text(
            json.dumps(state),
            encoding="utf-8",
        )
        values = {"OCTRA_API_PORT": "8080"}
        args = mock.Mock(no_wait=False, wait_seconds=1, poll_seconds=0.1)
        with mock.patch("validator_enroll.transaction", return_value=None):
            self.assertFalse(
                resume_join_transaction(
                    config,
                    values,
                    "ready",
                    args,
                    missing_ok=True,
                )
            )

    def test_operator_pm2_name(self):
        self.assertEqual(operator_pm2_name("val01"), "octra-val01")
        self.assertEqual(operator_pm2_name("octra-val01"), "octra-val01")

    def test_promotion_readiness_tracks_observer_marker(self):
        values = {"OCTRA_OPERATOR_ROLE": "observer"}
        self.assertEqual(promotion_readiness(values, WORK), "pending")
        (WORK / "ready_to_vote.json").write_text("{}\n", encoding="utf-8")
        self.assertEqual(promotion_readiness(values, WORK), "ready")

    def test_active_validator_does_not_require_promotion_marker(self):
        values = {"OCTRA_OPERATOR_ROLE": "validator"}
        self.assertEqual(promotion_readiness(values, WORK), "not_required")

    def test_strict_validator_requires_promotion_marker(self):
        values = {
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_VALIDATOR_READY_STRICT": "1",
        }
        self.assertEqual(promotion_readiness(values, WORK), "pending")
        (WORK / "ready_to_vote.json").write_text("{}\n", encoding="utf-8")
        self.assertEqual(promotion_readiness(values, WORK), "ready")

    def test_permissionless_validator_uses_chain_readiness(self):
        values = {
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH": "100",
            "OCTRA_VALIDATOR_READY_STRICT": "1",
        }
        self.assertEqual(promotion_readiness(values, WORK), "not_required")

    def test_verified_snapshot_installs_atomically(self):
        values = network_values()
        snapshot = WORK / "stage/snapshots/abc"
        data = snapshot / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":101,"state_root":"' + "5" * 64 + '","txid_hi":"510"}\n',
            encoding="utf-8",
        )
        marker = {
            "version": "octra-state-sync-verified",
            "status": "snapshot_verified",
            "voting": False,
            "chain_id": values["OCTRA_CHAIN_ID"],
            "config_hash": values["OCTRA_CONSENSUS_CONFIG_HASH"],
            "manifest_hash": "a" * 64,
            "snapshot_epoch": "101",
        }
        (snapshot / "snapshot_verified.json").write_text(
            json.dumps(marker),
            encoding="utf-8",
        )
        (snapshot / "certificate.json").write_text("{}\n", encoding="utf-8")
        selected = load_verified_snapshot(WORK / "stage", values)
        target = WORK / "installed"
        install_verified_snapshot(selected, target)
        self.assertTrue((target / "HEAD.json").is_file())
        self.assertTrue((target / ".state_sync/snapshot_verified.json").is_file())
        self.assertFalse(data.exists())

    def test_checkpoint_rejects_wrong_root(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":99,"state_root":"' + "4" * 64 + '","txid_hi":"500"}\n',
            encoding="utf-8",
        )
        with self.assertRaises(ValidatorError):
            validate_checkpoint(data, network_values())

    def test_checkpoint_allows_progress_for_restart(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":101,"state_root":"' + "5" * 64 + '","txid_hi":"510"}\n',
            encoding="utf-8",
        )
        values = network_values()
        validate_checkpoint(data, values, allow_progress=True)
        with self.assertRaises(ValidatorError):
            validate_checkpoint(data, values)

    def test_env_parser_never_executes_values(self):
        marker = WORK / "executed"
        bundle = WORK / "hostile.env"
        bundle.write_text(f"OCTRA_CHAIN_ID='$(touch {marker})'\n", encoding="utf-8")
        values = parse_env(bundle)
        self.assertFalse(marker.exists())
        self.assertEqual(values["OCTRA_CHAIN_ID"], f"$(touch {marker})")

    def test_source_commit_rejects_invalid_value(self):
        root = WORK / "candidate-source"
        root.mkdir()
        (root / "SOURCE_COMMIT").write_text("invalid\n", encoding="utf-8")
        with mock.patch("validator_config.ROOT", root):
            with self.assertRaisesRegex(ValidatorError, "SOURCE_COMMIT is invalid"):
                source_commit()

    def test_run_requires_explicit_candidate_binding(self):
        source_path = Path(__file__).resolve().parent / "run.sh"
        exported_path = Path(__file__).resolve().parent.parent / "run.sh"
        script_path = source_path if source_path.is_file() else exported_path
        script = script_path.read_text(encoding="utf-8")
        binding = script.index("--check-runtime")
        recovery = script.index("validator_recover.py")
        guard = script.index("validator_guard.py")
        source = script.index('. "$CONFIG"')
        cleanup = script.index("validator_process.py")
        runtime = script.index('mkdir -p "$ROOT/data"')
        start = script.index('pm2 start "$OCTRA_OPERATOR_BINARY"')
        self.assertLess(binding, recovery)
        self.assertLess(recovery, guard)
        self.assertLess(guard, source)
        self.assertLess(source, cleanup)
        self.assertLess(cleanup, start)
        self.assertLess(runtime, start)
        self.assertNotIn("pm2 restart", script)

    def test_recover_forwards_explicit_options(self):
        source_path = Path(__file__).resolve().parent / "recover.sh"
        exported_path = Path(__file__).resolve().parent.parent / "recover.sh"
        script_path = source_path if source_path.is_file() else exported_path
        script = script_path.read_text(encoding="utf-8")
        self.assertIn('validator_recover.py" --config "$CONFIG" "$@"', script)

    def test_runtime_rebind_preserves_node_state(self):
        root = WORK / "candidate"
        config = WORK / "live/.keys/validator/node.env"
        packaged_network = root / "config/network.env"
        installed_network = config.parent / "network.env"
        binary = root / "artifacts/octra_node.exe"
        worker = root / "artifacts/octra_pvac_worker.exe"
        sync_binary = root / "artifacts/octra_state_sync_client.exe"
        control_binary = root / "artifacts/bft_control_tx.exe"
        floor_binary = root / "artifacts/vote_floor.exe"
        for path, body in (
            (root / "SOURCE_COMMIT", b"a" * 40 + b"\n"),
            (packaged_network, b"network\n"),
            (installed_network, b"network\n"),
            (binary, b"node"),
            (worker, b"worker"),
            (sync_binary, b"sync"),
            (control_binary, b"control"),
            (floor_binary, b"floor"),
        ):
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(body)
        network_hash = hashlib.sha256(b"network\n").hexdigest()
        write_env(config, {
            "OCTRA_DATA_DIR": "/srv/octra/data/node",
            "OCTRA_OPERATOR_BINARY": "/old/artifacts/octra_node.exe",
            "OCTRA_OPERATOR_NETWORK_SHA256": network_hash,
            "OCTRA_OPERATOR_ROLE": "validator",
            "OCTRA_STATE_SYNC_ENABLE": "1",
        })
        paths = {
            "ROOT": root,
            "DEFAULT_BINARY": binary,
            "DEFAULT_WORKER": worker,
            "DEFAULT_SYNC_BINARY": sync_binary,
            "DEFAULT_CONTROL_BINARY": control_binary,
            "DEFAULT_FLOOR_BINARY": floor_binary,
        }
        with mock.patch.multiple("validator_config", **paths):
            with self.assertRaisesRegex(ValidatorError, "candidate paths are stale"):
                require_runtime_binding(config)
            rebind_runtime(config)
            require_runtime_binding(config)
        values = parse_env(config)
        self.assertEqual(values["OCTRA_DATA_DIR"], "/srv/octra/data/node")
        self.assertEqual(values["OCTRA_OPERATOR_ROLE"], "validator")
        self.assertEqual(values["OCTRA_STATE_SYNC_ENABLE"], "1")
        self.assertEqual(values["OCTRA_OPERATOR_BINARY"], str(binary.resolve()))
        self.assertEqual(values["OCTRA_BINARY_HASH"], hashlib.sha256(b"node").hexdigest())
        self.assertEqual(values["OCTRA_SOURCE_COMMIT"], "a" * 40)
        self.assertEqual(
            values["OCTRA_OPERATOR_NETWORK_BUNDLE"],
            str(installed_network.resolve()),
        )

    def test_network_adoption_preserves_local_state(self):
        config = WORK / "live/.keys/validator/node.env"
        installed = config.parent / "network.env"
        candidate = WORK / "candidate/config/network.env"
        current = network_values()
        current["OCTRA_BOOTSTRAP_PEERS"] = "old-a:19000,old-b:19000"
        current["OCTRA_PEERS"] = current["OCTRA_BOOTSTRAP_PEERS"]
        next_values = network_values()
        next_values["OCTRA_BOOTSTRAP_PEERS"] = "new-a:19000,new-b:19000"
        next_values["OCTRA_PEERS"] = next_values["OCTRA_BOOTSTRAP_PEERS"]
        write_env(installed, current)
        write_env(candidate, next_values)
        write_env(config, {
            **current,
            "OCTRA_DATA_DIR": "/srv/octra/data/node",
            "OCTRA_OPERATOR_NETWORK_BUNDLE": str(installed),
            "OCTRA_OPERATOR_NETWORK_SHA256": hashlib.sha256(
                installed.read_bytes()
            ).hexdigest(),
            "OCTRA_OPERATOR_ROLE": "validator",
        })
        candidate_hash = hashlib.sha256(candidate.read_bytes()).hexdigest()
        adopt_network(config, candidate, candidate_hash)
        values = parse_env(config)
        self.assertEqual(values["OCTRA_DATA_DIR"], "/srv/octra/data/node")
        self.assertEqual(values["OCTRA_OPERATOR_ROLE"], "validator")
        self.assertEqual(values["OCTRA_BOOTSTRAP_PEERS"], "new-a:19000,new-b:19000")
        self.assertEqual(values["OCTRA_OPERATOR_NETWORK_SHA256"], candidate_hash)
        self.assertEqual(installed.read_bytes(), candidate.read_bytes())

    def test_install_prepares_operator_data_root(self):
        source_path = Path(__file__).resolve().parent / "install.sh"
        exported_path = Path(__file__).resolve().parent.parent / "install.sh"
        script_path = source_path if source_path.is_file() else exported_path
        script = script_path.read_text(encoding="utf-8")
        self.assertIn("OCTRA_DATA_ROOT", script)
        self.assertIn('run_root install -d -m 700 -o "$OPERATOR_USER"', script)
        self.assertIn('TOOLCHAIN_ROOT="$ROOT/runtime_data/toolchains"', script)
        self.assertIn('CARGO_HOME="$TOOLCHAIN_ROOT/cargo"', script)
        self.assertIn('RUSTUP_HOME="$TOOLCHAIN_ROOT/rustup"', script)
        self.assertIn('OCAML_TOOLCHAIN=4.14.2', script)
        self.assertIn('OCAML_SWITCH="$TOOLCHAIN_ROOT/ocaml"', script)
        self.assertIn(
            'opam init --bare --disable-sandboxing --no-setup --yes',
            script,
        )
        self.assertIn('opam switch create', script)
        self.assertIn(
            'opam switch link "$OCAML_SWITCH" "$ROOT" --yes',
            script,
        )
        self.assertIn('"setenv=CARGO_HOME=\\\"$CARGO_HOME\\\""', script)
        self.assertIn('"setenv+=RUSTUP_HOME=\\\"$RUSTUP_HOME\\\""', script)
        self.assertIn('"setenv+=PATH+=\\\"$CARGO_HOME/bin\\\""', script)
        self.assertIn('env -C "$ROOT"', script)
        self.assertIn('if ! command -v pm2', script)
        self.assertIn('systemctl is-enabled --quiet "$PM2_SERVICE"', script)
        self.assertNotIn("pm2-logrotate", script)
        self.assertNotIn("pm2 install", script)
        self.assertNotIn("pm2 save --force", script)

    def test_pm2_service_enabled_reads_systemd(self):
        probe = subprocess.CompletedProcess([], 0)
        with mock.patch("validator_config.subprocess.run", return_value=probe) as run:
            self.assertTrue(pm2_service_enabled("octra"))
        self.assertEqual(
            run.call_args.args[0],
            ["systemctl", "is-enabled", "--quiet", "pm2-octra.service"],
        )
        self.assertFalse(run.call_args.kwargs["check"])

    def test_install_runtime_preserves_existing_pm2(self):
        account = mock.Mock(pw_dir="/home/octra")
        with mock.patch("validator_config.ubuntu_host", return_value=True):
            with mock.patch("validator_config.install_rust_toolchain"):
                with mock.patch("validator_config.shutil.which", return_value="/usr/local/bin/pm2"):
                    with mock.patch("validator_config.pm2_service_enabled", return_value=True):
                        with mock.patch("validator_config.pwd.getpwnam", return_value=account):
                            with mock.patch("validator_config.os.geteuid", return_value=0):
                                with mock.patch.dict(
                                    "validator_config.os.environ",
                                    {"USER": "octra"},
                                    clear=True,
                                ):
                                    with mock.patch("validator_config.run") as run:
                                        install_runtime()
        commands = [call.args[0] for call in run.call_args_list]
        self.assertEqual(commands[0], ["apt-get", "update"])
        self.assertEqual(commands[1][:3], ["apt-get", "install", "-y"])
        self.assertFalse(any(command[0] == "npm" for command in commands))
        self.assertFalse(any("startup" in command for command in commands))

    def test_install_runtime_initializes_missing_pm2(self):
        account = mock.Mock(pw_dir="/home/octra")
        with mock.patch("validator_config.ubuntu_host", return_value=True):
            with mock.patch("validator_config.install_rust_toolchain"):
                with mock.patch("validator_config.shutil.which", return_value=None):
                    with mock.patch("validator_config.pm2_service_enabled", return_value=False):
                        with mock.patch("validator_config.pwd.getpwnam", return_value=account):
                            with mock.patch("validator_config.os.geteuid", return_value=0):
                                with mock.patch.dict(
                                    "validator_config.os.environ",
                                    {"USER": "octra", "PATH": "/usr/bin"},
                                    clear=True,
                                ):
                                    with mock.patch("validator_config.run") as run:
                                        install_runtime()
        commands = [call.args[0] for call in run.call_args_list]
        self.assertIn(["npm", "install", "-g", "pm2"], commands)
        startup = next(command for command in commands if "startup" in command)
        self.assertEqual(startup[-4:], ["-u", "octra", "--hp", "/home/octra"])

    def test_build_toolchain_bootstraps_old_rust(self):
        old = {"PATH": "old"}
        fresh = {"PATH": "fresh"}

        def locate(command, path=None):
            return f"/{path}/{command}"

        def probe(command, **_):
            version = "1.75.0" if command[0].startswith("/old/") else "1.80.1"
            return subprocess.CompletedProcess(command, 0, f"rustc {version}\n", "")

        with mock.patch(
            "validator_config.rust_environment", side_effect=[old, fresh]
        ):
            with mock.patch("validator_config.shutil.which", side_effect=locate):
                with mock.patch("validator_config.subprocess.run", side_effect=probe):
                    with mock.patch("validator_config.install_rust_toolchain") as install:
                        self.assertEqual(ensure_build_toolchain(), fresh)
        install.assert_called_once_with()

    def test_gate_reports_missing_python_runtime(self):
        source_path = Path(__file__).resolve().parent / "validator_tools_gate.sh"
        exported_path = Path(__file__).resolve().parent.parent / "check.sh"
        script_path = source_path if source_path.is_file() else exported_path
        script = script_path.read_text(encoding="utf-8")
        self.assertIn("python3_missing", script)
        self.assertIn("python3_nacl_missing", script)

    def test_gate_requires_source_commit(self):
        source_path = Path(__file__).resolve().parent / "validator_tools_gate.sh"
        exported_path = Path(__file__).resolve().parent.parent / "check.sh"
        script_path = source_path if source_path.is_file() else exported_path
        script = script_path.read_text(encoding="utf-8")
        self.assertIn("source_commit_missing", script)
        self.assertIn("source_commit_invalid", script)

    def test_sync_budget_matches_seed_capacity(self):
        args = parser().parse_args([])
        self.assertEqual(args.sync_concurrency, 4)
        self.assertEqual(args.sync_source_concurrency, 4)
        self.assertIsNone(args.sync_stage)

    def test_default_sync_stage_is_sibling(self):
        data = WORK / "node"
        self.assertEqual(
            resolve_sync_stage(None, data),
            (WORK / "node.state_sync").resolve(),
        )

    def test_nested_sync_stage_is_rejected(self):
        data = WORK / "node"
        with self.assertRaises(ValidatorError):
            validate_sync_layout(data / "state_sync", data)

    def test_sync_budget_reaches_client(self):
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        args = parser().parse_args([
            "--sync",
            "--sync-binary",
            str(sync_binary),
            "--sync-stage",
            str(WORK / "stage"),
            "--yes",
        ])
        with mock.patch("validator_config.run", side_effect=RuntimeError("captured")) as invoke:
            with self.assertRaisesRegex(RuntimeError, "captured"):
                maybe_sync(args, str(WORK / "data"), network_values())
        command = invoke.call_args.args[0]
        self.assertEqual(command[command.index("--concurrency") + 1], "4")
        self.assertEqual(command[command.index("--source-concurrency") + 1], "4")
        self.assertEqual(
            command[command.index("--migration-root") + 1],
            "2" * 64,
        )

    def test_sync_minimum_epoch_reaches_client(self):
        command = sync_client_command(
            WORK / "state_sync_client",
            WORK / "stage",
            network_values(),
            ["https://seed.example"],
            4,
            1,
            min_epoch=1340000,
        )
        self.assertEqual(command[command.index("--min-epoch") + 1], "1340000")

    def test_sync_source_check_uses_each_published_source(self):
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        sources = ["https://seed-a.example", "https://seed-b.example"]
        result = subprocess.CompletedProcess(
            [],
            1,
            "",
            "error = snapshot exceeds configured byte limit\n",
        )
        with mock.patch(
            "validator_config.subprocess.run",
            return_value=result,
        ) as invoke:
            check_sync_sources(
                sync_binary,
                WORK / "state_sync_check",
                network_values(),
                sources,
            )
        self.assertEqual(invoke.call_count, 2)
        for call, source in zip(invoke.call_args_list, sources):
            command = call.args[0]
            self.assertEqual(command.count("--source"), 1)
            self.assertEqual(command[command.index("--source") + 1], source)
            self.assertEqual(command[command.index("--max-bytes") + 1], "1")

    def test_sync_source_check_rejects_route_mismatch(self):
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        result = subprocess.CompletedProcess(
            [],
            1,
            "",
            "event = manifest_source_rejected source = https://seed-a.example "
            "error = HTTP 404\n"
            "error = no source returned a valid quorum certificate\n",
        )
        with mock.patch(
            "validator_config.subprocess.run",
            return_value=result,
        ):
            with self.assertRaisesRegex(
                ValidatorError,
                "state sync source check failed",
            ):
                check_sync_sources(
                    sync_binary,
                    WORK / "state_sync_check",
                    network_values(),
                    ["https://seed-a.example"],
                )

    def test_unattended_configuration_syncs_by_default(self):
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        args = parser().parse_args([
            "--sync-binary",
            str(sync_binary),
            "--sync-stage",
            str(WORK / "stage"),
            "--yes",
        ])
        with mock.patch("validator_config.run", side_effect=RuntimeError("captured")):
            with self.assertRaisesRegex(RuntimeError, "captured"):
                maybe_sync(args, str(WORK / "data"), network_values())

    def test_recovery_keeps_valid_offline_state(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n',
            encoding="utf-8",
        )
        config = WORK / "node.env"
        write_env(config, {
            "OCTRA_CHAIN_ID": "octra-devnet-bft-v1",
            "OCTRA_CHECKPOINT_EPOCH": "99",
            "OCTRA_CHECKPOINT_STATE_ROOT": "3" * 64,
            "OCTRA_CHECKPOINT_TXID_HI": "500",
            "OCTRA_DATA_DIR": str(data),
        })
        with mock.patch("validator_recover.pm2_entries") as inspect:
            recover(config)
        inspect.assert_not_called()

    def test_recovery_starts_observer_for_journal_marker(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n',
            encoding="utf-8",
        )
        marker = write_need(data, "octra-devnet-bft-v1", cause="journal")
        config = WORK / "node.env"
        write_env(config, {
            "OCTRA_CHAIN_ID": "octra-devnet-bft-v1",
            "OCTRA_CHECKPOINT_EPOCH": "99",
            "OCTRA_CHECKPOINT_STATE_ROOT": "3" * 64,
            "OCTRA_CHECKPOINT_TXID_HI": "500",
            "OCTRA_DATA_DIR": str(data),
        })
        with mock.patch("validator_recover.pm2_entries") as inspect, mock.patch(
            "validator_recover.emit"
        ) as emitted:
            recover(config)
        inspect.assert_not_called()
        self.assertTrue(marker.is_file())
        reported = emitted.call_args.kwargs
        self.assertEqual(reported["status"], "verify")
        self.assertEqual(reported["cause"], "journal")
        self.assertEqual(reported["action"], "start_observer")

    def test_recovery_marker_is_chain_bound(self):
        data = WORK / "data"
        marker = write_need(data, "octra-devnet-bft-v1")
        self.assertEqual(
            read_need(data, "octra-devnet-bft-v1"),
            make_need("octra-devnet-bft-v1", "root", 100, 99, None),
        )
        with self.assertRaisesRegex(ValidatorError, "binding differs"):
            read_need(data, "other-chain")
        marker.unlink()
        marker.mkdir()
        with self.assertRaisesRegex(ValidatorError, "not a regular file"):
            read_need(data, "octra-devnet-bft-v1")

    def test_recovery_marker_fields_are_exact(self):
        value = {
            "schema": "octra_sync_need_v1",
            "chain_id": "octra-devnet-bft-v1",
            "cause": "root",
            "epoch": 100,
            "head": 99,
            "target": None,
            "extra": True,
        }
        with self.assertRaisesRegex(ValidatorError, "fields are invalid"):
            need_of(value, "octra-devnet-bft-v1")

    def test_recovery_marker_cause_type_is_checked(self):
        value = {
            "schema": "octra_sync_need_v1",
            "chain_id": "octra-devnet-bft-v1",
            "cause": [],
            "epoch": 100,
            "head": 99,
            "target": None,
        }
        with self.assertRaisesRegex(ValidatorError, "cause is invalid"):
            need_of(value, "octra-devnet-bft-v1")

    def test_recovery_refuses_systemd_data_owner(self):
        data = WORK / "data"
        data.mkdir()
        config = WORK / "node.env"
        write_env(config, {
            "OCTRA_CHAIN_ID": "octra-devnet-bft-v1",
            "OCTRA_DATA_DIR": str(data),
        })
        with mock.patch("validator_recover.pm2_entries", return_value=[]), mock.patch(
            "validator_recover.data_pids",
            return_value=[41],
        ):
            with self.assertRaisesRegex(ValidatorError, "pid:41"):
                recover(config)

    def test_recovery_refuses_link_name_owner(self):
        target = WORK / "volume/data"
        data = WORK / "home/data"
        (target / "irmin_store").mkdir(parents=True)
        (target / "chaindata").mkdir()
        (target / "HEAD.json").write_text(
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n',
            encoding="utf-8",
        )
        data.parent.mkdir()
        data.symlink_to(target, target_is_directory=True)
        config = WORK / "node.env"
        write_env(config, {
            "OCTRA_CHAIN_ID": "octra-devnet-bft-v1",
            "OCTRA_CHECKPOINT_EPOCH": "99",
            "OCTRA_CHECKPOINT_STATE_ROOT": "3" * 64,
            "OCTRA_CHECKPOINT_TXID_HI": "500",
            "OCTRA_DATA_DIR": str(data),
        })
        entries = [{
            "name": "octra-validator",
            "pm2_env": {
                "OCTRA_DATA_DIR": str(data),
                "status": "online",
            },
        }]
        with mock.patch("validator_recover.pm2_entries", return_value=entries):
            with self.assertRaisesRegex(ValidatorError, "octra-validator"):
                recover(config, replace_state=True)

    def test_recovery_downloads_verified_state(self):
        data = WORK / "data"
        bundle = WORK / "network.env"
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        network = network_values()
        write_env(bundle, network)
        config = WORK / "node.env"
        wallet = ensure_wallet(config.parent / "wallet.json")
        write_env(config, {
            **network,
            "OCTRA_DATA_DIR": str(data),
            "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle),
            "OCTRA_OPERATOR_NETWORK_SHA256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
            "OCTRA_OPERATOR_SYNC_BINARY": str(sync_binary),
            "OCTRA_OPERATOR_SYNC_BINARY_HASH": hashlib.sha256(b"client").hexdigest(),
            "OCTRA_OPERATOR_SYNC_CONCURRENCY": "4",
            "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": "1",
            "OCTRA_OPERATOR_SYNC_STAGE": str(WORK / "stage"),
        })

        def install(*_, **__):
            (data / "irmin_store").mkdir(parents=True)
            (data / "chaindata").mkdir()
            (data / "HEAD.json").write_text(
                '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n',
                encoding="utf-8",
            )

        with mock.patch("validator_recover.pm2_entries", return_value=[]):
            with mock.patch("validator_recover.sync_snapshot", side_effect=install) as sync:
                recover(config)
        sync.assert_called_once()
        self.assertEqual(
            sync.call_args.kwargs["min_epoch"],
            int(network["OCTRA_CHECKPOINT_EPOCH"]),
        )
        self.assertEqual(load_wallet(data / "wallet.json"), wallet)

    def test_recovery_preserves_nonempty_invalid_state(self):
        data = WORK / "data"
        data.mkdir()
        evidence = data / "unknown"
        evidence.write_bytes(b"preserve")
        config = WORK / "node.env"
        write_env(config, {
            "OCTRA_CHAIN_ID": "octra-devnet-bft-v1",
            "OCTRA_DATA_DIR": str(data),
        })
        with self.assertRaisesRegex(ValidatorError, "evidence-preserving"):
            recover(config)
        self.assertEqual(evidence.read_bytes(), b"preserve")

    def test_recovery_replaces_valid_state_and_preserves_identity(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n',
            encoding="utf-8",
        )
        config = WORK / "keys" / "node.env"
        identity_path = config.parent / "wallet.json"
        wallet = ensure_wallet(identity_path)
        (data / "wallet.json").write_bytes(identity_path.read_bytes())
        (data / "wallet.json").chmod(0o600)
        bundle = WORK / "network.env"
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        network = network_values()
        write_env(bundle, network)
        write_env(config, {
            **network,
            "OCTRA_DATA_DIR": str(data),
            "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle),
            "OCTRA_OPERATOR_NETWORK_SHA256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
            "OCTRA_OPERATOR_SYNC_BINARY": str(sync_binary),
            "OCTRA_OPERATOR_SYNC_BINARY_HASH": hashlib.sha256(b"client").hexdigest(),
            "OCTRA_OPERATOR_SYNC_CONCURRENCY": "4",
            "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": "1",
            "OCTRA_OPERATOR_SYNC_STAGE": str(WORK / "stage"),
        })

        def install(*_, **__):
            self.assertFalse(data.exists())
            (data / "irmin_store").mkdir(parents=True)
            (data / "chaindata").mkdir()
            (data / "HEAD.json").write_text(
                '{"epoch_id":100,"state_root":"' + "4" * 64 + '","txid_hi":"501"}\n',
                encoding="utf-8",
            )

        with mock.patch("validator_recover.pm2_entries", return_value=[]):
            with mock.patch("validator_recover.sync_snapshot", side_effect=install):
                recover(config, replace_state=True)
        preserved = preserved_state_path(data, 99)
        self.assertTrue((preserved / "HEAD.json").is_file())
        self.assertEqual(load_wallet(data / "wallet.json"), wallet)

    def test_recovery_preserves_data_link(self):
        home = WORK / "home"
        volume = WORK / "volume"
        target = volume / "data"
        data = home / "data"
        (target / "irmin_store").mkdir(parents=True)
        (target / "chaindata").mkdir()
        (target / "HEAD.json").write_text(
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n',
            encoding="utf-8",
        )
        home.mkdir()
        data.symlink_to(target, target_is_directory=True)
        config = WORK / "keys" / "node.env"
        identity_path = config.parent / "wallet.json"
        wallet = ensure_wallet(identity_path)
        (target / "wallet.json").write_bytes(identity_path.read_bytes())
        (target / "wallet.json").chmod(0o600)
        bundle = WORK / "network.env"
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        network = network_values()
        write_env(bundle, network)
        write_env(config, {
            **network,
            "OCTRA_DATA_DIR": str(data),
            "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle),
            "OCTRA_OPERATOR_NETWORK_SHA256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
            "OCTRA_OPERATOR_SYNC_BINARY": str(sync_binary),
            "OCTRA_OPERATOR_SYNC_BINARY_HASH": hashlib.sha256(b"client").hexdigest(),
            "OCTRA_OPERATOR_SYNC_CONCURRENCY": "4",
            "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": "1",
            "OCTRA_OPERATOR_SYNC_STAGE": str(WORK / "stage"),
        })

        def install(*args, **__):
            selected = args[2]
            (selected / "irmin_store").mkdir(parents=True)
            (selected / "chaindata").mkdir()
            (selected / "HEAD.json").write_text(
                '{"epoch_id":100,"state_root":"' + "4" * 64 + '","txid_hi":"501"}\n',
                encoding="utf-8",
            )

        with mock.patch("validator_recover.pm2_entries", return_value=[]), mock.patch(
            "validator_recover.sync_snapshot", side_effect=install
        ):
            recover(config, replace_state=True)
        prior = target.with_name("data.prior-99")
        self.assertTrue(data.is_symlink())
        self.assertEqual(data.resolve(), target.resolve())
        self.assertTrue((prior / "HEAD.json").is_file())
        self.assertEqual(load_wallet(target / "wallet.json"), wallet)
        self.assertFalse(home.joinpath("data.prior-99").exists())
        head_bytes = (target / "HEAD.json").read_bytes()
        with mock.patch("validator_recover.pm2_entries", return_value=[]), mock.patch(
            "validator_recover.sync_snapshot",
            side_effect=ValidatorError("source unavailable"),
        ):
            with self.assertRaisesRegex(ValidatorError, "source unavailable"):
                recover(config, replace_state=True)
        self.assertTrue(data.is_symlink())
        self.assertEqual((target / "HEAD.json").read_bytes(), head_bytes)
        self.assertFalse(target.with_name("data.prior-100").exists())

    def test_recovery_restores_state_after_sync_failure(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        head_bytes = (
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n'
        ).encode("utf-8")
        (data / "HEAD.json").write_bytes(head_bytes)
        config = WORK / "keys" / "node.env"
        identity_path = config.parent / "wallet.json"
        ensure_wallet(identity_path)
        (data / "wallet.json").write_bytes(identity_path.read_bytes())
        (data / "wallet.json").chmod(0o600)
        bundle = WORK / "network.env"
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        network = network_values()
        write_env(bundle, network)
        write_env(config, {
            **network,
            "OCTRA_DATA_DIR": str(data),
            "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle),
            "OCTRA_OPERATOR_NETWORK_SHA256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
            "OCTRA_OPERATOR_SYNC_BINARY": str(sync_binary),
            "OCTRA_OPERATOR_SYNC_BINARY_HASH": hashlib.sha256(b"client").hexdigest(),
            "OCTRA_OPERATOR_SYNC_CONCURRENCY": "4",
            "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": "1",
            "OCTRA_OPERATOR_SYNC_STAGE": str(WORK / "stage"),
        })
        with mock.patch("validator_recover.pm2_entries", return_value=[]):
            with mock.patch(
                "validator_recover.sync_snapshot",
                side_effect=ValidatorError("source unavailable"),
            ):
                with self.assertRaisesRegex(ValidatorError, "source unavailable"):
                    recover(config, replace_state=True)
        self.assertEqual((data / "HEAD.json").read_bytes(), head_bytes)
        self.assertFalse(preserved_state_path(data, 99).exists())

    def test_recovery_rejects_snapshot_below_marker(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        head_bytes = (
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n'
        ).encode("utf-8")
        (data / "HEAD.json").write_bytes(head_bytes)
        config = WORK / "keys" / "node.env"
        identity_path = config.parent / "wallet.json"
        ensure_wallet(identity_path)
        (data / "wallet.json").write_bytes(identity_path.read_bytes())
        (data / "wallet.json").chmod(0o600)
        network = network_values()
        write_need(data, network["OCTRA_CHAIN_ID"])
        bundle = WORK / "network.env"
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        write_env(bundle, network)
        write_env(config, {
            **network,
            "OCTRA_DATA_DIR": str(data),
            "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle),
            "OCTRA_OPERATOR_NETWORK_SHA256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
            "OCTRA_OPERATOR_SYNC_BINARY": str(sync_binary),
            "OCTRA_OPERATOR_SYNC_BINARY_HASH": hashlib.sha256(b"client").hexdigest(),
            "OCTRA_OPERATOR_SYNC_CONCURRENCY": "4",
            "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": "1",
            "OCTRA_OPERATOR_SYNC_STAGE": str(WORK / "stage"),
        })

        def install(*_, **__):
            (data / "irmin_store").mkdir(parents=True)
            (data / "chaindata").mkdir()
            (data / "HEAD.json").write_bytes(head_bytes)

        with mock.patch("validator_recover.pm2_entries", return_value=[]), mock.patch(
            "validator_recover.sync_snapshot", side_effect=install
        ):
            with self.assertRaisesRegex(ValidatorError, "below recovery boundary"):
                recover(config, replace_state=True)
        self.assertEqual((data / "HEAD.json").read_bytes(), head_bytes)
        self.assertIsNotNone(read_need(data, network["OCTRA_CHAIN_ID"]))
        self.assertTrue(data.with_name(data.name + ".rejected-99").is_dir())

    def test_recovery_accepts_snapshot_at_marker(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":99,"state_root":"' + "3" * 64 + '","txid_hi":"500"}\n',
            encoding="utf-8",
        )
        config = WORK / "keys" / "node.env"
        identity_path = config.parent / "wallet.json"
        wallet = ensure_wallet(identity_path)
        (data / "wallet.json").write_bytes(identity_path.read_bytes())
        (data / "wallet.json").chmod(0o600)
        network = network_values()
        write_need(data, network["OCTRA_CHAIN_ID"])
        bundle = WORK / "network.env"
        sync_binary = WORK / "state_sync_client"
        sync_binary.write_bytes(b"client")
        write_env(bundle, network)
        write_env(config, {
            **network,
            "OCTRA_DATA_DIR": str(data),
            "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle),
            "OCTRA_OPERATOR_NETWORK_SHA256": hashlib.sha256(bundle.read_bytes()).hexdigest(),
            "OCTRA_OPERATOR_SYNC_BINARY": str(sync_binary),
            "OCTRA_OPERATOR_SYNC_BINARY_HASH": hashlib.sha256(b"client").hexdigest(),
            "OCTRA_OPERATOR_SYNC_CONCURRENCY": "4",
            "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": "1",
            "OCTRA_OPERATOR_SYNC_STAGE": str(WORK / "stage"),
        })

        def install(*_, **__):
            (data / "irmin_store").mkdir(parents=True)
            (data / "chaindata").mkdir()
            (data / "HEAD.json").write_text(
                '{"epoch_id":100,"state_root":"' + "4" * 64 + '","txid_hi":"501"}\n',
                encoding="utf-8",
            )

        with mock.patch("validator_recover.pm2_entries", return_value=[]), mock.patch(
            "validator_recover.sync_snapshot", side_effect=install
        ):
            recover(config, replace_state=True)
        prior = preserved_state_path(data, 99)
        self.assertIsNone(read_need(data, network["OCTRA_CHAIN_ID"]))
        self.assertIsNotNone(read_need(prior, network["OCTRA_CHAIN_ID"]))
        self.assertEqual(load_wallet(data / "wallet.json"), wallet)

    def test_config_root_matches_layout(self):
        source = Path(__file__).resolve()
        expected = source.parents[2] if source.parents[1].name == "controls" else source.parents[3]
        self.assertEqual(CONFIG_ROOT, expected)

    def test_tool_version(self):
        self.assertEqual(tool_version("rustc 1.85.1 (test)"), (1, 85, 1))
        self.assertEqual(tool_version("rustc 1.80"), (1, 80, 0))
        with self.assertRaises(ValidatorError):
            tool_version("rustc invalid")

    def test_source_toolchains_are_repo_local(self):
        with mock.patch.object(Path, "mkdir") as mkdir:
            environment = rust_environment()
        mkdir.assert_called_once_with(parents=True, exist_ok=True)
        self.assertEqual(TOOLCHAIN_ROOT, CONFIG_ROOT / "runtime_data/toolchains")
        self.assertEqual(environment["T" + "MPDIR"], str(BUILD_WORK))
        self.assertEqual(environment["CARGO_HOME"], str(CARGO_HOME))
        self.assertEqual(environment["RUSTUP_HOME"], str(RUSTUP_HOME))
        self.assertEqual(OPAM_SWITCH, TOOLCHAIN_ROOT / "ocaml")
        self.assertNotIn("OPAMROOT", environment)
        self.assertEqual(
            environment["PATH"].split(os.pathsep)[0],
            str(CARGO_HOME / "bin"),
        )

    def test_source_build_metadata_is_pinned(self):
        self.assertTrue((CONFIG_ROOT / "octra_node.opam.locked").is_file())
        install_path = Path(__file__).resolve().parent / "install.sh"
        exported_install = Path(__file__).resolve().parent.parent / "install.sh"
        installer = install_path if install_path.is_file() else exported_install
        install_value = installer.read_text(encoding="utf-8")
        self.assertIn("build-essential", install_value)
        self.assertIn("clang", install_value)
        self.assertIn("liblmdb-dev", install_value)
        self.assertIn("liblmdb0", install_value)
        self.assertIn("https://sh.rustup.rs", install_value)
        self.assertIn("OCAML_TOOLCHAIN=4.14.2", install_value)
        self.assertIn("RUST_TOOLCHAIN=1.80.1", install_value)
        config_path = Path(__file__).resolve().with_name("validator_config.py")
        config_value = config_path.read_text(encoding="utf-8")
        self.assertIn("switch = str(OPAM_SWITCH)", config_value)
        self.assertIn('"--locked"', config_value)
        self.assertIn('"--require-checksums"', config_value)

    def test_source_build_exports_all_runtime_artifacts(self):
        (WORK / "octra_node.opam.locked").write_text(
            'opam-version: "2.0"\n',
            encoding="utf-8",
        )
        commands = []
        names = [
            "octra_node.exe",
            "octra_pvac_worker.exe",
            "octra_state_sync_client.exe",
            "octra_state_sync_manifest.exe",
            "bft_control_tx.exe",
            "vote_floor.exe",
        ]

        def run_build(command, cwd=WORK, env=None):
            commands.append((command, cwd, env))
            if "dune" in command:
                target = WORK / "_build/default/bin"
                target.mkdir(parents=True)
                for name in names:
                    (target / name).write_bytes(name.encode("ascii"))

        def run_probe(command, **_):
            if command == ["opam", "switch", "list", "--short"]:
                return subprocess.CompletedProcess(
                    command,
                    0,
                    str(OPAM_SWITCH) + "\n",
                    "",
                )
            if command[-2:] == ["ocamlc", "-version"]:
                return subprocess.CompletedProcess(command, 0, "4.14.2\n", "")
            raise AssertionError(command)

        with mock.patch("validator_config.ROOT", WORK):
            with mock.patch("validator_config.sys.platform", "linux"):
                with mock.patch(
                    "validator_config.os.uname",
                    return_value=mock.Mock(machine="aarch64"),
                ):
                    with mock.patch(
                        "validator_config.ensure_build_toolchain",
                        return_value={"PATH": "test"},
                    ):
                        with mock.patch(
                            "validator_config.subprocess.run",
                            side_effect=run_probe,
                        ):
                            with mock.patch(
                                "validator_config.run",
                                side_effect=run_build,
                            ):
                                build_candidate()
        command_values = [command for command, _, _ in commands]
        install = next(command for command in command_values if command[:2] == ["opam", "install"])
        build = next(command for command in command_values if "dune" in command)
        build_env = next(env for command, _, env in commands if command == build)
        self.assertIn("--locked", install)
        self.assertIn("--require-checksums", install)
        self.assertEqual(build_env["OCTRA_SRC_ROOT"], str(WORK))
        self.assertTrue((WORK / "mcl/obj").is_dir())
        self.assertTrue((WORK / "mcl/lib").is_dir())
        for name in names:
            self.assertIn(f"bin/{name}", build)
            self.assertEqual(
                (WORK / "_build/default/bin" / name).read_bytes(),
                name.encode("ascii"),
            )

    def test_source_build_reuses_existing_local_switch(self):
        (WORK / "octra_node.opam.locked").write_text(
            'opam-version: "2.0"\n',
            encoding="utf-8",
        )
        switch = WORK / "runtime_data/toolchains/ocaml"
        (switch / "_opam").mkdir(parents=True)
        names = [
            "octra_node.exe",
            "octra_pvac_worker.exe",
            "octra_state_sync_client.exe",
            "octra_state_sync_manifest.exe",
            "bft_control_tx.exe",
            "vote_floor.exe",
        ]
        commands = []

        def run_build(command, cwd=WORK, env=None):
            commands.append((command, cwd, env))
            if "dune" in command:
                target = WORK / "_build/default/bin"
                target.mkdir(parents=True)
                for name in names:
                    (target / name).write_bytes(name.encode("ascii"))

        def run_probe(command, **_):
            if command == ["opam", "switch", "list", "--short"]:
                return subprocess.CompletedProcess(command, 0, "", "")
            if command[-2:] == ["ocamlc", "-version"]:
                return subprocess.CompletedProcess(command, 0, "4.14.2\n", "")
            raise AssertionError(command)

        with mock.patch.multiple(
            "validator_config",
            ROOT=WORK,
            OPAM_SWITCH=switch,
        ):
            with mock.patch("validator_config.sys.platform", "linux"):
                with mock.patch(
                    "validator_config.os.uname",
                    return_value=mock.Mock(machine="aarch64"),
                ):
                    with mock.patch(
                        "validator_config.ensure_build_toolchain",
                        return_value={"PATH": "test"},
                    ):
                        with mock.patch(
                            "validator_config.subprocess.run",
                            side_effect=run_probe,
                        ):
                            with mock.patch(
                                "validator_config.run",
                                side_effect=run_build,
                            ):
                                build_candidate()
        values = [command for command, _, _ in commands]
        self.assertNotIn(
            ["opam", "switch", "create", str(switch), "4.14.2", "-y"],
            values,
        )
        self.assertIn(
            ["opam", "switch", "link", str(switch), str(WORK), "-y"],
            values,
        )

    def test_hashed_file_rejects_tampering(self):
        worker = WORK / "octra_pvac_worker.exe"
        worker.write_bytes(b"worker")
        values = {
            "OCTRA_PVAC_VERIFY_WORKER": str(worker),
            "OCTRA_PVAC_VERIFY_WORKER_HASH": hashlib.sha256(b"worker").hexdigest(),
        }
        self.assertEqual(
            require_hashed_file(
                values,
                "OCTRA_PVAC_VERIFY_WORKER",
                "OCTRA_PVAC_VERIFY_WORKER_HASH",
            ),
            worker,
        )
        worker.write_bytes(b"tampered")
        with self.assertRaises(ValidatorError):
            require_hashed_file(
                values,
                "OCTRA_PVAC_VERIFY_WORKER",
                "OCTRA_PVAC_VERIFY_WORKER_HASH",
            )

    def test_control_builder_creates_deterministic_bond_proof(self):
        built = CONFIG_ROOT / "_build/default/bin/bft_control_tx.exe"
        packaged = CONFIG_ROOT / "artifacts/bft_control_tx.exe"
        binary = built if built.is_file() else packaged
        if not binary.is_file():
            self.skipTest("bft_control_tx is not built")
        if binary.read_bytes()[:4] == b"\x7fELF" and sys.platform != "linux":
            self.skipTest("packaged bft_control_tx requires Linux")
        wallet_path = WORK / "wallet.json"
        wallet = ensure_wallet(wallet_path)
        result = subprocess.run(
            [
                str(binary),
                "--wallet",
                str(wallet_path),
                "--rpc",
                "http://127.0.0.1:1/rpc",
                "--chain-id",
                "octra-devnet-test",
                "--op",
                "validator_bond",
                "--amount",
                "1000000",
                "--nonce",
                "7",
                "--print-only",
            ],
            check=True,
            capture_output=True,
            text=True,
        )
        tx = json.loads(result.stdout)
        payload = json.loads(tx["message"])
        public_key = base64.b64decode(wallet["pub"])
        fields = [
            b"octra:validator_bond:v1",
            b"octra-devnet-test",
            wallet["address"].encode("ascii"),
            b"1000000",
            b"7",
            public_key,
        ]
        message = b"".join(
            str(len(field)).encode("ascii") + b":" + field
            for field in fields
        )
        SigningKey(base64.b64decode(wallet["priv"])).verify_key.verify(
            message,
            base64.b64decode(payload["proof"]),
        )
        escrow = address_from_pubkey(
            hashlib.sha256(b"octra:validator_bond:escrow").digest()
        )
        self.assertEqual(tx["to_"], escrow)
        self.assertEqual(tx["nonce"], 7)
        self.assertEqual(payload["consensus_pubkey"], wallet["pub"])

    def test_process_plan_removes_stale_owner(self):
        entries = [
            {
                "name": "octra-old",
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/octra",
                    "status": "stopped",
                },
            },
        ]
        self.assertEqual(
            process_plan(entries, "octra-new", "/data/octra"),
            ["octra-old"],
        )

    def test_process_plan_accepts_fresh_install(self):
        self.assertEqual(
            process_plan([], "octra-new", "/data/octra"),
            [],
        )

    def test_process_plan_rejects_active_owner(self):
        entries = [
            {
                "name": "octra-old",
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/octra",
                    "status": "online",
                },
            },
        ]
        with self.assertRaises(ValidatorError):
            process_plan(entries, "octra-new", "/data/octra")

    def test_process_pids_tracks_deleted_entries(self):
        entries = [
            {
                "name": "octra-new",
                "pid": 201,
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/octra",
                    "status": "online",
                },
            },
            {
                "name": "octra-old",
                "pid": 199,
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/octra",
                    "status": "stopped",
                },
            },
            {
                "name": "other",
                "pid": 301,
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/other",
                    "status": "online",
                },
            },
        ]
        self.assertEqual(
            process_pids(entries, ["octra-new", "octra-old"]),
            [199, 201],
        )

    def test_active_data_owners_block_recovery(self):
        entries = [
            {
                "name": "octra-validator",
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/octra",
                    "status": "online",
                },
            },
            {
                "name": "octra-observer",
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/other",
                    "status": "online",
                },
            },
        ]
        self.assertEqual(
            active_data_owners(entries, "/data/octra"),
            ["octra-validator"],
        )

    def test_data_pids_find_systemd_owner(self):
        root = WORK / "proc"
        process = root / "41"
        process.mkdir(parents=True)
        data = WORK / "data"
        (process / "environ").write_bytes(
            b"PATH=/usr/bin\0OCTRA_DATA_DIR=" + os.fsencode(str(data.resolve())) + b"\0"
        )
        self.assertEqual(data_pids(data, root=root), [41])

    def test_optional_pm2_inspection_allows_systemd_host(self):
        with mock.patch("validator_process.shutil.which", return_value=None):
            self.assertEqual(pm2_entries(required=False), [])
            with self.assertRaisesRegex(ValidatorError, "PM2 process table"):
                pm2_entries()

    def test_optional_pm2_inspection_fails_closed(self):
        with mock.patch(
            "validator_process.shutil.which",
            return_value="/usr/bin/pm2",
        ):
            with mock.patch("validator_process.subprocess.run", side_effect=OSError):
                with self.assertRaisesRegex(ValidatorError, "PM2 process table"):
                    pm2_entries(required=False)

    def test_remaining_owners_detects_name_and_data_dir(self):
        entries = [
            {
                "name": "octra-new",
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/new",
                },
            },
            {
                "name": "octra-renamed",
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/octra",
                },
            },
            {
                "name": "other",
                "pm2_env": {
                    "OCTRA_DATA_DIR": "/data/other",
                },
            },
        ]
        self.assertEqual(
            remaining_owners(entries, ["octra-new"], "/data/octra"),
            ["octra-new", "octra-renamed"],
        )

    def test_wait_stopped_observes_process_exit(self):
        with mock.patch(
            "validator_process.process_alive",
            side_effect=[True, False],
        ) as alive:
            with mock.patch("validator_process.time.sleep"):
                wait_stopped([41], timeout=5.0, poll=0.0)
        self.assertEqual(alive.call_count, 2)

    def test_wait_stopped_refuses_lingering_process(self):
        with mock.patch(
            "validator_process.process_alive",
            return_value=True,
        ):
            with mock.patch(
                "validator_process.time.monotonic",
                side_effect=[0.0, 2.0],
            ):
                with self.assertRaises(ValidatorError):
                    wait_stopped([41], timeout=1.0, poll=0.0)

if __name__ == "__main__":
    unittest.main()