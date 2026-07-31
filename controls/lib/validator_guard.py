# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import base64
import json
import os
import sys
from pathlib import Path

from nacl.signing import VerifyKey

from validator_common import FORBIDDEN_KEYS
from validator_common import ValidatorError
from validator_common import load_network
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import private_mode
from validator_common import sha256_file
from validator_common import state_ready
from validator_common import validate_checkpoint
from validator_common import validate_network_binding
from validator_common import validator_entries

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def require_file(values, key):
    path = Path(values.get(key, ""))
    if not path.is_file():
        raise ValidatorError(f"required file is missing: {key}")
    return path

def require_hashed_file(values, path_key, hash_key):
    path = require_file(values, path_key)
    if sha256_file(path) != values.get(hash_key):
        raise ValidatorError(f"file hash mismatch: {path_key}")
    return path

def validate_ready_marker(data_dir, values, wallet):
    path = data_dir / "ready_to_vote.json"
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
        ready_epoch = int(payload["ready_epoch"])
        records = int(payload["catchup_records_verified"])
        state_root = payload["state_root"]
        signer = payload["validator"]
        pubkey = payload["validator_pubkey"]
        expected = (
            f"octra:observer-ready:v1|{values['OCTRA_CHAIN_ID']}|{signer}|"
            f"{ready_epoch}|{state_root}|{records}"
        )
        signature = base64.b64decode(payload["signature"], validate=True)
        public = base64.b64decode(pubkey, validate=True)
        VerifyKey(public).verify(expected.encode("utf-8"), signature)
    except Exception as error:
        raise ValidatorError("ready-to-vote marker is invalid") from error
    if payload.get("version") != "octra-observer-ready":
        raise ValidatorError("ready-to-vote marker version mismatch")
    if payload.get("status") != "observer_ready":
        raise ValidatorError("ready-to-vote marker status mismatch")
    if payload.get("chain_id") != values["OCTRA_CHAIN_ID"]:
        raise ValidatorError("ready-to-vote marker chain mismatch")
    if signer != wallet["address"] or pubkey != wallet["pub"]:
        raise ValidatorError("ready-to-vote marker identity mismatch")
    if payload.get("sign_payload") != expected:
        raise ValidatorError("ready-to-vote marker payload mismatch")
    if ready_epoch < int(values["OCTRA_CHECKPOINT_EPOCH"]):
        raise ValidatorError("ready-to-vote marker predates checkpoint")

def validate(values):
    inherited = sorted(key for key in FORBIDDEN_KEYS if key in os.environ)
    if inherited:
        raise ValidatorError("forbidden inherited env: " + ",".join(inherited))
    role = values.get("OCTRA_OPERATOR_ROLE")
    mode = values.get("OCTRA_CONSENSUS_MODE")
    if role not in {"observer", "validator"}:
        raise ValidatorError("invalid operator role")
    if mode != ("bft" if role == "validator" else "observer"):
        raise ValidatorError("operator role and consensus mode mismatch")
    data_dir = Path(values.get("OCTRA_DATA_DIR", ""))
    if not state_ready(data_dir):
        raise ValidatorError("checkpoint is not ready")
    validate_checkpoint(data_dir, values, allow_progress=True)
    wallet_path = data_dir / "wallet.json"
    wallet = load_wallet(wallet_path)
    require_hashed_file(values, "OCTRA_OPERATOR_BINARY", "OCTRA_BINARY_HASH")
    worker = require_hashed_file(
        values,
        "OCTRA_PVAC_VERIFY_WORKER",
        "OCTRA_PVAC_VERIFY_WORKER_HASH",
    )
    control = require_hashed_file(
        values,
        "OCTRA_OPERATOR_CONTROL_BINARY",
        "OCTRA_OPERATOR_CONTROL_BINARY_HASH",
    )
    if not os.access(worker, os.X_OK):
        raise ValidatorError("PVAC worker is not executable")
    if not os.access(control, os.X_OK):
        raise ValidatorError("validator control binary is not executable")
    bundle = require_file(values, "OCTRA_OPERATOR_NETWORK_BUNDLE")
    bundle_hash = values.get("OCTRA_OPERATOR_NETWORK_SHA256", "")
    _, _, network = load_network(bundle, bundle_hash)
    validate_network_binding(values, network)
    members = dict(validator_entries(values.get("OCTRA_VALIDATORS", "")))
    permissionless = values.get("OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH", "").isdigit()
    if (
        role == "validator"
        and not permissionless
        and members.get(wallet["address"]) != wallet["pub"]
    ):
        raise ValidatorError("validator identity is not active")
    if (
        role == "validator"
        and not permissionless
        and values.get("OCTRA_VALIDATOR_READY_STRICT") == "1"
    ):
        validate_ready_marker(data_dir, values, wallet)
    if values.get("OCTRA_P2P_REQUIRE_BINARY_HASH") != "0":
        raise ValidatorError("binary hash admission must be disabled")
    if values.get("OCTRA_BFT_RELEASE_PROFILE") != "devnet_full_v1":
        raise ValidatorError("invalid release profile")
    if values.get("OCTRA_FHE_MAX_PER_EPOCH") != "1":
        raise ValidatorError("invalid first-run FHE limit")
    for key in [
        "OCTRA_EMISSION_ACTIVATION_EPOCH",
        "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH",
        "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH",
        "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH",
        "OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH",
        "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH",
    ]:
        if not values.get(key, "").isdigit():
            raise ValidatorError(f"invalid activation epoch: {key}")
    epochs = {
        values["OCTRA_EMISSION_ACTIVATION_EPOCH"],
        values["OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH"],
        values["OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH"],
    }
    if len(epochs) != 1:
        raise ValidatorError("activation epochs do not match")
    return wallet

def main():
    parser = argparse.ArgumentParser(prog="validator_guard.py")
    parser.add_argument("--config", required=True)
    args = parser.parse_args()
    private_mode(args.config)
    values = parse_env(args.config)
    wallet = validate(values)
    emit(
        status="ready",
        role=values["OCTRA_OPERATOR_ROLE"],
        address=wallet["address"],
        chain=values["OCTRA_CHAIN_ID"],
    )

if __name__ == "__main__":
    try:
        main()
    except (ValidatorError, OSError, KeyError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)