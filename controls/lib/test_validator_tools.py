# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import base64
import hashlib
import json
import os
import shutil
import subprocess
import sys
import unittest
from pathlib import Path
from unittest import mock

from nacl.signing import SigningKey

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
from validator_config import BUILD_WORK
from validator_config import build_candidate
from validator_config import CARGO_HOME
from validator_config import check_sync_sources
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
from validator_config import validate_advertise
from validator_config import validate_sync_layout
from validator_bundle import validate_bundle
from validator_enroll import exact_member
from validator_enroll import next_nonce
from validator_enroll import require_admission_active
from validator_enroll import resume_join_transaction
from validator_enroll import set_validator_mode
from validator_enroll import wait_active_commit
from validator_guard import require_hashed_file
from validator_process import process_plan
from validator_process import process_pids
from validator_process import remaining_owners
from validator_process import active_data_owners
from validator_process import wait_stopped
from validator_recover import recover
from validator_recover import preserved_state_path
from validator_status import promotion_readiness

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

class ValidatorToolsTest(unittest.TestCase):
    def setUp(self):
        if WORK.exists():
            shutil.rmtree(WORK)
        WORK.mkdir(parents=True)

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
        config = root / ".keys/validator/node.env"
        packaged_network = root / "config/network.env"
        installed_network = root / ".keys/validator/network.env"
        binary = root / "artifacts/octra_node.exe"
        worker = root / "artifacts/octra_pvac_worker.exe"
        sync_binary = root / "artifacts/octra_state_sync_client.exe"
        control_binary = root / "artifacts/bft_control_tx.exe"
        for path, body in (
            (root / "SOURCE_COMMIT", b"a" * 40 + b"\n"),
            (packaged_network, b"network\n"),
            (installed_network, b"network\n"),
            (binary, b"node"),
            (worker, b"worker"),
            (sync_binary, b"sync"),
            (control_binary, b"control"),
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
            "OCTRA_CHECKPOINT_EPOCH": "99",
            "OCTRA_CHECKPOINT_STATE_ROOT": "3" * 64,
            "OCTRA_CHECKPOINT_TXID_HI": "500",
            "OCTRA_DATA_DIR": str(data),
        })
        with mock.patch("validator_recover.pm2_entries") as inspect:
            recover(config)
        inspect.assert_not_called()

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

        def install(*_):
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
        self.assertEqual(load_wallet(data / "wallet.json"), wallet)

    def test_recovery_preserves_nonempty_invalid_state(self):
        data = WORK / "data"
        data.mkdir()
        evidence = data / "unknown"
        evidence.write_bytes(b"preserve")
        config = WORK / "node.env"
        write_env(config, {"OCTRA_DATA_DIR": str(data)})
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

        def install(*_):
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
        self.assertIn("--locked", install)
        self.assertIn("--require-checksums", install)
        self.assertTrue((WORK / "mcl/obj").is_dir())
        self.assertTrue((WORK / "mcl/lib").is_dir())
        for name in names:
            self.assertIn(f"bin/{name}", build)
            self.assertEqual(
                (WORK / "_build/default/bin" / name).read_bytes(),
                name.encode("ascii"),
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
