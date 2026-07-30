# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import sys
from pathlib import Path

from validator_common import ValidatorError
from validator_common import load_network
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

def recover(config):
    private_mode(config)
    values = parse_env(config)
    data_path = Path(values["OCTRA_DATA_DIR"])
    if state_ready(data_path):
        head = validate_checkpoint(data_path, values, allow_progress=True)
        emit(event="recovery", status="ready", epoch=head["epoch"])
        return
    if data_path.exists() and any(data_path.iterdir()):
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
    sync_snapshot(
        sync_binary,
        stage,
        data_path,
        values,
        sources,
        positive_int(values, "OCTRA_OPERATOR_SYNC_CONCURRENCY"),
        positive_int(values, "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY"),
    )
    head = validate_checkpoint(data_path, values, allow_progress=True)
    emit(event="recovery", status="restored", epoch=head["epoch"])

def main():
    parser = argparse.ArgumentParser(prog="recover.sh")
    parser.add_argument("--config", required=True)
    args = parser.parse_args()
    recover(args.config)

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)