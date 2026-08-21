# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import stat
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
from validator_process import data_pids
from validator_process import pm2_entries

NEED_SCHEMA = "octra_sync_need_v1"
NEED_CAUSES = frozenset({"root", "journal", "range"})
NEED_FIELDS = frozenset({"schema", "chain_id", "cause", "epoch", "head", "target"})
NEED_BYTES = 4096

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

def free_state_path(data_path, label, epoch):
    base = data_path.with_name(data_path.name + f".{label}-{epoch}")
    if not os.path.lexists(base):
        return base
    for serial in range(1, 1000):
        candidate = base.with_name(base.name + f"-{serial}")
        if not os.path.lexists(candidate):
            return candidate
    raise ValidatorError(f"{label} state path limit exceeded")

def sync_dir(path):
    descriptor = os.open(path, os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)

def move_state(source, target):
    source.replace(target)
    sync_dir(target.parent)

def need_of(value, chain):
    if not isinstance(value, dict) or set(value) != NEED_FIELDS:
        raise ValidatorError("recovery marker fields are invalid")
    if value.get("schema") != NEED_SCHEMA or value.get("chain_id") != chain:
        raise ValidatorError("recovery marker binding differs")
    cause = value["cause"]
    epoch = value["epoch"]
    head = value["head"]
    target = value["target"]
    if cause not in NEED_CAUSES:
        raise ValidatorError("recovery marker cause is invalid")
    if type(epoch) is not int or type(head) is not int:
        raise ValidatorError("recovery marker height is invalid")
    if head < 0 or epoch != head + 1:
        raise ValidatorError("recovery marker boundary is invalid")
    if cause == "range":
        if type(target) is not int or target <= head:
            raise ValidatorError("recovery marker target is invalid")
    elif target is not None:
        raise ValidatorError("recovery marker target is invalid")
    return {"cause": cause, "epoch": epoch, "head": head, "target": target}

def read_need(data_path, chain):
    path = data_path / "recovery/sync_need.json"
    try:
        descriptor = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    except FileNotFoundError:
        return None
    except OSError as error:
        raise ValidatorError("recovery marker is unreadable") from error
    try:
        meta = os.fstat(descriptor)
        if not stat.S_ISREG(meta.st_mode):
            raise ValidatorError("recovery marker is not a regular file")
        if meta.st_size <= 0 or meta.st_size > NEED_BYTES:
            raise ValidatorError("recovery marker size is invalid")
        raw = bytearray()
        while len(raw) <= NEED_BYTES:
            part = os.read(descriptor, NEED_BYTES + 1 - len(raw))
            if not part:
                break
            raw.extend(part)
        if len(raw) > NEED_BYTES:
            raise ValidatorError("recovery marker size is invalid")
        if len(raw) != meta.st_size:
            raise ValidatorError("recovery marker changed during read")
        try:
            value = json.loads(bytes(raw).decode("utf-8"))
        except (UnicodeDecodeError, json.JSONDecodeError) as error:
            raise ValidatorError("recovery marker is invalid") from error
        return need_of(value, chain)
    except OSError as error:
        raise ValidatorError("recovery marker is unreadable") from error
    finally:
        os.close(descriptor)

def preserve_state(identity_path, data_path, head):
    data_wallet = data_path / "wallet.json"
    if load_wallet(identity_path) != load_wallet(data_wallet):
        raise ValidatorError("operator identity and state wallet mismatch")
    preserved = free_state_path(data_path, "prior", head["epoch"])
    move_state(data_path, preserved)
    return preserved

def restore_preserved_state(data_path, preserved):
    if os.path.lexists(data_path):
        return False
    move_state(preserved, data_path)
    return True

def rollback_state(data_path, preserved, epoch):
    rejected = None
    if os.path.lexists(data_path):
        rejected = free_state_path(data_path, "rejected", epoch)
        move_state(data_path, rejected)
    if not restore_preserved_state(data_path, preserved):
        raise ValidatorError("preserved state rollback was blocked")
    return rejected

def recover(config, replace_state=False, plan=None, min_epoch=None):
    private_mode(config)
    values = parse_env(config)
    data_path = Path(values["OCTRA_DATA_DIR"])
    marked = read_need(data_path, values["OCTRA_CHAIN_ID"])
    planned = need_of(plan, values["OCTRA_CHAIN_ID"]) if plan is not None else None
    need = marked or planned
    ready = state_ready(data_path)
    head = (
        validate_checkpoint(data_path, values, allow_progress=True)
        if ready
        else None
    )
    if ready and need is not None and not replace_state:
        raise ValidatorError("terminal recovery requires --replace-state")
    if ready and not replace_state:
        emit(event="recovery", status="ready", epoch=head["epoch"])
        return
    if not ready and data_path.exists() and any(data_path.iterdir()):
        raise ValidatorError("nonempty state requires evidence-preserving recovery")
    owners = active_data_owners(pm2_entries(required=False), str(data_path))
    if owners:
        raise ValidatorError("state directory is active: " + ",".join(owners))
    pids = data_pids(data_path)
    if pids:
        raise ValidatorError(
            "state directory is active: " + ",".join(f"pid:{pid}" for pid in pids)
        )
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
    floor = int(values["OCTRA_CHECKPOINT_EPOCH"])
    if need is not None:
        floor = max(floor, need["epoch"])
    if min_epoch is not None:
        floor = max(floor, int(min_epoch))
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
            min_epoch=floor,
        )
        head = validate_checkpoint(data_path, values, allow_progress=True)
        if need is not None and head["epoch"] < need["epoch"]:
            raise ValidatorError("signed snapshot is below recovery boundary")
        copy_private(identity_path, data_path / "wallet.json")
    except Exception:
        if preserved is not None:
            rejected = rollback_state(data_path, preserved, head["epoch"])
            emit(
                event="recovery",
                status="rolled_back",
                restored=preserved,
                rejected=rejected or "none",
            )
        raise
    emit(
        event="recovery",
        status="restored",
        epoch=head["epoch"],
        cause=need["cause"] if need is not None else "manual",
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