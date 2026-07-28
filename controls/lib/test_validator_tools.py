import base64
import hashlib
import json
import os
import shutil
import unittest
from pathlib import Path

from nacl.signing import SigningKey

from validator_common import ValidatorError
from validator_common import address_from_pubkey
from validator_common import ensure_wallet
from validator_common import load_network
from validator_common import parse_env
from validator_common import validate_checkpoint
from validator_common import validate_network
from validator_common import write_env
from validator_config import canonical_pm2_name
from validator_config import ROOT as CONFIG_ROOT
from validator_config import tool_version
from validator_config import install_verified_snapshot
from validator_config import load_verified_snapshot
from validator_config import validate_advertise
from validator_process import process_plan

ROOT = Path(__file__).resolve().parent.parent
WORK = ROOT / "tmp/validator_tools_test"

def identity():
    key = SigningKey.generate()
    public_key = bytes(key.verify_key)
    encoded = base64.b64encode(public_key).decode("ascii")
    return address_from_pubkey(public_key), encoded

def network_values(entitlement):
    validators = ",".join(":".join(identity()) for _ in range(5))
    exporter = ":".join(identity())
    program_key = base64.b64encode(bytes(SigningKey.generate().verify_key)).decode("ascii")
    return {
        "OCTRA_BFT_RELEASE_PROFILE": "devnet_full_v1",
        "OCTRA_BINARY_HASH": "1" * 64,
        "OCTRA_BOOTSTRAP_PEERS": "10.0.0.1:19000,10.0.0.2:19000",
        "OCTRA_CHAIN_ID": "octra-devnet-v1",
        "OCTRA_CHECKPOINT_EPOCH": "99",
        "OCTRA_CHECKPOINT_STATE_ROOT": "3" * 64,
        "OCTRA_CHECKPOINT_TXID_HI": "500",
        "OCTRA_CONSENSUS_CONFIG_HASH": "4" * 64,
        "OCTRA_EMISSION_ACTIVATION_EPOCH": "100",
        "OCTRA_EPOCH_DURATION": "10",
        "OCTRA_FHE_MAX_PER_EPOCH": "1",
        "OCTRA_P2P_REQUIRE_BINARY_HASH": "1",
        "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH": "100",
        "OCTRA_PROGRAM_RELEASE_KEYS": f"release={program_key}",
        "OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH": "100",
        "OCTRA_PVAC_MIGRATION_ENTITLEMENTS": str(entitlement),
        "OCTRA_PVAC_MIGRATION_ROOT": "2" * 64,
        "OCTRA_STEALTH_MAX_PER_EPOCH": "1",
        "OCTRA_STATE_SYNC_EXPORTERS": exporter,
        "OCTRA_STATE_SYNC_SOURCES": "https://seed-a.example,https://seed-b.example",
        "OCTRA_VALIDATORS": validators,
    }

class ValidatorToolsTest(unittest.TestCase):
    def setUp(self):
        if WORK.exists():
            shutil.rmtree(WORK)
        WORK.mkdir(parents=True)
        self.entitlement = WORK / "entitlements.json"
        self.entitlement.write_text("{}\n", encoding="utf-8")

    def tearDown(self):
        if WORK.exists():
            shutil.rmtree(WORK)

    def test_wallet_is_canonical_and_private(self):
        wallet_path = WORK / "data/wallet.json"
        wallet = ensure_wallet(wallet_path)
        self.assertEqual(len(wallet["address"]), 47)
        self.assertEqual(os.stat(wallet_path).st_mode & 0o777, 0o600)
        self.assertEqual(ensure_wallet(wallet_path), wallet)

    def test_network_bundle_round_trip(self):
        bundle = WORK / "network.env"
        values = network_values(self.entitlement)
        write_env(bundle, values)
        digest = hashlib.sha256(bundle.read_bytes()).hexdigest()
        _, loaded_digest, loaded = load_network(bundle, digest)
        self.assertEqual(loaded_digest, digest)
        self.assertEqual(loaded["OCTRA_FHE_MAX_PER_EPOCH"], "1")
        self.assertEqual(loaded["OCTRA_PEERS"], values["OCTRA_BOOTSTRAP_PEERS"])

    def test_network_rejects_unsafe_override(self):
        values = network_values(self.entitlement)
        values["OCTRA_ALLOW_UNSAFE_QUORUM"] = "1"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_address_key_mismatch(self):
        values = network_values(self.entitlement)
        entries = values["OCTRA_VALIDATORS"].split(",")
        address, _ = entries[0].split(":", 1)
        _, other_key = identity()
        entries[0] = f"{address}:{other_key}"
        values["OCTRA_VALIDATORS"] = ",".join(entries)
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_activation_drift(self):
        values = network_values(self.entitlement)
        values["OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH"] = "101"
        with self.assertRaises(ValidatorError):
            validate_network(values, WORK)

    def test_network_rejects_public_plain_http_state_sync(self):
        values = network_values(self.entitlement)
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

    def test_pm2_name_is_canonical(self):
        self.assertEqual(canonical_pm2_name("val01"), "octra-val01")
        self.assertEqual(canonical_pm2_name("octra-val01"), "octra-val01")

    def test_verified_snapshot_installs_atomically(self):
        values = network_values(self.entitlement)
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
            validate_checkpoint(data, network_values(self.entitlement))

    def test_checkpoint_allows_progress_for_restart(self):
        data = WORK / "data"
        (data / "irmin_store").mkdir(parents=True)
        (data / "chaindata").mkdir()
        (data / "HEAD.json").write_text(
            '{"epoch_id":101,"state_root":"' + "5" * 64 + '","txid_hi":"510"}\n',
            encoding="utf-8",
        )
        values = network_values(self.entitlement)
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

    def test_run_rebinds_candidate_path(self):
        source_path = Path(__file__).resolve().parent / "run.sh"
        exported_path = Path(__file__).resolve().parent.parent / "run.sh"
        script_path = source_path if source_path.is_file() else exported_path
        script = script_path.read_text(encoding="utf-8")
        cleanup = script.index("validator_process.py")
        runtime = script.index('mkdir -p "$ROOT/data"')
        start = script.index('pm2 start "$OCTRA_OPERATOR_BINARY"')
        self.assertLess(cleanup, start)
        self.assertLess(runtime, start)
        self.assertNotIn("pm2 restart", script)

    def test_config_root_matches_layout(self):
        source = Path(__file__).resolve()
        expected = source.parents[2] if source.parents[1].name == "controls" else source.parents[3]
        self.assertEqual(CONFIG_ROOT, expected)

    def test_tool_version(self):
        self.assertEqual(tool_version("rustc 1.85.1 (test)"), (1, 85, 1))
        self.assertEqual(tool_version("rustc 1.80"), (1, 80, 0))
        with self.assertRaises(ValidatorError):
            tool_version("rustc invalid")

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
            ["octra-new", "octra-old"],
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

if __name__ == "__main__":
    unittest.main()
