# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import sys
from pathlib import Path

from validator_common import ValidatorError
from validator_common import copy_private
from validator_common import load_network
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import private_mode
from validator_common import sha256_file
from validator_common import state_ready
from validator_common import state_sync_sources
from validator_common import validate_checkpoint
from validator_common import validate_network_binding
from validator_config import sync_snapshot
from validator_process import active_data_owners
from validator_process import pm2_entries

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def positive_int(values, key):
    try:
        value = int(values[key])
    except (KeyError, TypeError, ValueError) as error:
        raise ValidatorError(f"invalid integer: {key}") from error
    if value < 1:
        raise ValidatorError(f"nonpositive integer: {key}")
    return value

def preserved_state_path(data_path, epoch):
    return data_path.with_name(data_path.name + f".prior-{epoch}")

def preserve_state(identity_path, data_path, head):
    data_wallet = data_path / "wallet.json"
    if load_wallet(identity_path) != load_wallet(data_wallet):
        raise ValidatorError("operator identity and state wallet mismatch")
    preserved = preserved_state_path(data_path, head["epoch"])
    if preserved.exists():
        raise ValidatorError(f"preserved state already exists: {preserved}")
    data_path.replace(preserved)
    return preserved

def restore_preserved_state(data_path, preserved):
    if data_path.exists():
        return False
    preserved.replace(data_path)
    return True

def recover(config, replace_state=False):
    private_mode(config)
    values = parse_env(config)
    data_path = Path(values["OCTRA_DATA_DIR"])
    ready = state_ready(data_path)
    head = (
        validate_checkpoint(data_path, values, allow_progress=True)
        if ready
        else None
    )
    if ready and not replace_state:
        emit(event="recovery", status="ready", epoch=head["epoch"])
        return
    if not ready and data_path.exists() and any(data_path.iterdir()):
        raise ValidatorError("nonempty state requires evidence-preserving recovery")
    owners = active_data_owners(pm2_entries(), str(data_path))
    if owners:
        raise ValidatorError("state directory is active: " + ",".join(owners))
    bundle = Path(values["OCTRA_OPERATOR_NETWORK_BUNDLE"])
    _, _, network = load_network(
        bundle,
        values["OCTRA_OPERATOR_NETWORK_SHA256"],
    )
    validate_network_binding(values, network)
    sync_binary = Path(values["OCTRA_OPERATOR_SYNC_BINARY"])
    if not sync_binary.is_file():
        raise ValidatorError("state sync client is missing")
    if sha256_file(sync_binary) != values["OCTRA_OPERATOR_SYNC_BINARY_HASH"]:
        raise ValidatorError("state sync client hash mismatch")
    stage = Path(values["OCTRA_OPERATOR_SYNC_STAGE"])
    sources = state_sync_sources(values["OCTRA_STATE_SYNC_SOURCES"])
    identity_path = Path(config).parent / "wallet.json"
    load_wallet(identity_path)
    preserved = None
    if ready:
        preserved = preserve_state(identity_path, data_path, head)
    try:
        sync_snapshot(
            sync_binary,
            stage,
            data_path,
            values,
            sources,
            positive_int(values, "OCTRA_OPERATOR_SYNC_CONCURRENCY"),
            positive_int(values, "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY"),
        )
    except Exception:
        if preserved is not None:
            restore_preserved_state(data_path, preserved)
        raise
    copy_private(identity_path, data_path / "wallet.json")
    head = validate_checkpoint(data_path, values, allow_progress=True)
    emit(
        event="recovery",
        status="restored",
        epoch=head["epoch"],
        preserved=preserved or "none",
    )

def main():
    parser = argparse.ArgumentParser(prog="recover.sh")
    parser.add_argument("--config", required=True)
    parser.add_argument("--replace-state", action="store_true")
    args = parser.parse_args()
    recover(args.config, replace_state=args.replace_state)

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)