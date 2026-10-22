# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import re
import signal
import stat
import subprocess
import sys
from contextlib import contextmanager
from pathlib import Path
import validator_config

from validator_common import ValidatorError
from validator_common import load_network
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import private_mode
from validator_common import sha256_file
from validator_common import state_ready
from validator_common import state_sync_sources
from validator_common import validate_checkpoint
from validator_common import validate_network_binding
from validator_common import write_private_json
from validator_config import sync_snapshot
from validator_process import active_data_owners
from validator_process import data_pids
from validator_process import pm2_entries
from sync_need import MAX_BYTES as NEED_BYTES
from sync_need import choose
from sync_need import carry_guard
from sync_need import decode
from sync_need import history_limit, snapshot_lag
from sync_tip import sync_head, sync_issue
from validator_exit import command_lock, read_outbox

PLAN_SCHEMA = "octra_upgrade_sync"

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

@contextmanager
def interrupts():
    signals = (signal.SIGINT, signal.SIGTERM, signal.SIGHUP)
    saved = {item: signal.getsignal(item) for item in signals}

    def stop(number, _frame):
        for item in signals:
            signal.signal(item, signal.SIG_IGN)
        raise ValidatorError(f"recovery interrupted signal = {number}")

    try:
        for item in signals:
            signal.signal(item, stop)
        yield
    finally:
        for item, handler in saved.items():
            signal.signal(item, handler)

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
        path = base.with_name(base.name + f"-{serial}")
        if not os.path.lexists(path):
            return path
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
    return decode(value, chain)

def read_marker(path, chain):
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

def read_need(data_path, chain):
    marked = read_marker(data_path / "recovery/sync_conflict.json", chain)
    if marked is not None:
        if marked.cause != "conflict":
            raise ValidatorError("conflict marker cause is invalid")
        return marked
    return read_marker(data_path / "recovery/sync_need.json", chain)

def read_plan(config, values):
    path = Path(config).with_name(Path(config).name + ".sync.json")
    record = read_outbox(path)
    if record is None:
        return None
    data = str(Path(values["OCTRA_DATA_DIR"]).expanduser().resolve())
    if (set(record) != {"schema", "data_dir", "need"}
            or record["schema"] != PLAN_SCHEMA or record["data_dir"] != data):
        raise ValidatorError("upgrade recovery plan identity differs")
    plan = need_of(record["need"], values["OCTRA_CHAIN_ID"])
    if plan.cause != "range":
        raise ValidatorError("upgrade recovery plan cause differs")
    return plan

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

def require_idle(paths):
    entries = pm2_entries(required = False)
    owners = sorted({
        owner for path in paths for owner in active_data_owners(entries, str(path))
    })
    pids = sorted({pid for path in paths for pid in data_pids(path)})
    if owners or pids:
        raise ValidatorError("state directory is active: " + ",".join(
            owners + [f"pid:{pid}" for pid in pids]
        ))

def prior_epoch(data, name):
    pattern = re.escape(data.name) + r"\.prior-([0-9]+)(?:-[0-9]+)?"
    match = re.fullmatch(pattern, name)
    return int(match[1]) if match is not None else None

def restore_prior(config, values, prior):
    data_ref = Path(values["OCTRA_DATA_DIR"]).expanduser()
    data = data_ref.resolve()
    saved = Path(prior).expanduser().absolute()
    epoch = prior_epoch(data, saved.name)
    if saved.parent != data.parent or epoch is None or saved.is_symlink():
        raise ValidatorError("invalid preserved state path")
    with command_lock({"OCTRA_DATA_DIR": str(saved)}):
        require_idle([data_ref, data, saved])
        if state_ready(data) and os.path.lexists(data / "wallet.json"):
            raise ValidatorError("current state has an identity; preserved restore refused")
        info = saved.stat()
        if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o022:
            raise ValidatorError("preserved state directory is not owned safely")
        if not state_ready(saved):
            raise ValidatorError("preserved state is incomplete")
        _, _, network = load_network(
            Path(values["OCTRA_OPERATOR_NETWORK_BUNDLE"]),
            values["OCTRA_OPERATOR_NETWORK_SHA256"],
        )
        validate_network_binding(values, network)
        head = validate_checkpoint(saved, values, allow_progress = True)
        if head["epoch"] != epoch:
            raise ValidatorError("preserved state epoch differs from its path")
        if validator_config.operator_wallet(config)[1] != load_wallet(saved / "wallet.json"):
            raise ValidatorError("operator identity and preserved wallet mismatch")
        rejected = rollback_state(data, saved, head["epoch"])
        emit(event = "recovery", status = "resumed", epoch = head["epoch"],
             rejected = rejected or "none")

def recover(config, replace_state = False, plan = None, min_epoch = None, prior = None):
    if min_epoch is not None and (type(min_epoch) is not int or min_epoch < 0):
        raise ValidatorError("minimum snapshot epoch is invalid")
    private_mode(config)
    values = parse_env(config)
    with interrupts(), command_lock(values):
        if prior is not None:
            if replace_state or plan is not None or min_epoch is not None:
                raise ValidatorError("preserved restore cannot replace a snapshot")
            restore_prior(config, values, prior)
            return
        recover_state(config, values, replace_state, plan, min_epoch)

def recover_state(config, values, replace_state, plan, min_epoch):
    data_ref = Path(values["OCTRA_DATA_DIR"]).expanduser()
    data_path = data_ref.resolve()
    marked = read_need(data_path, values["OCTRA_CHAIN_ID"])
    saved = read_plan(config, values)
    if saved is not None and (plan is None or (
            plan.cause == "range" and saved.target > plan.target)):
        plan = saved
    ready = state_ready(data_path)
    if ready:
        wallet = data_path / "wallet.json"
        if not os.path.lexists(wallet):
            raise ValidatorError("state identity is missing; restore preserved state with --prior")
        identity_path, identity = validator_config.operator_wallet(config)
        if identity != load_wallet(wallet):
            raise ValidatorError("operator identity and state wallet mismatch")
    head = (
        validate_checkpoint(data_path, values, allow_progress=True)
        if ready
        else None
    )
    if (ready and plan is not None and plan.cause == "range"
            and plan.target - head["epoch"] <= history_limit(values)):
        plan = None
    need = choose(marked, plan, values["OCTRA_CHAIN_ID"])
    pending = read_marker(data_path / "recovery/sync_need.json", values["OCTRA_CHAIN_ID"])
    guard = carry_guard(pending, plan, values["OCTRA_CHAIN_ID"], saved = saved)
    if ready and not replace_state and min_epoch is not None and head["epoch"] < min_epoch:
        raise ValidatorError("state is below requested epoch; use --replace-state")
    held = ready and need is not None and (need.cause == "conflict"
        or (need.cause in ("root", "range") and (head["epoch"] < need.epoch
            or (need.target is not None and need.target - head["epoch"] > history_limit(values))))
        or head["epoch"] < need.head)
    tip = sync_head(values) if held or replace_state or not ready else None
    floor = max(int(values.get("OCTRA_CHECKPOINT_EPOCH", "0")), min_epoch or 0)
    if need is not None:
        floor = max(floor, need.epoch, (need.target or 0) - snapshot_lag(values))
    if guard is not None:
        floor = max(floor, guard.epoch, (guard.target or 0) - snapshot_lag(values))
    if plan is not None and plan.target is not None:
        floor = max(floor, plan.target - snapshot_lag(values))
    if head is not None:
        floor = max(floor, head["epoch"] + 1)
    if tip is not None and tip["head"] is not None:
        floor = max(floor, tip["head"] - snapshot_lag(values))
    if ready and need is not None and not replace_state:
        action = "start_observer"
        if held:
            issue = sync_issue(tip, floor)
            action = issue or f"recover.sh --replace-state --min-epoch {floor}"
        emit(
            event="recovery",
            status="held" if held else "verify",
            cause=need.cause,
            epoch=need.epoch,
            action = action,
        )
        return
    if ready and not replace_state:
        emit(event="recovery", status="ready", epoch=head["epoch"])
        return
    if not ready and data_path.exists() and any(data_path.iterdir()):
        raise ValidatorError("nonempty state requires evidence-preserving recovery")
    if not ready and any(
        prior_epoch(data_path, path.name) is not None
        for path in data_path.parent.iterdir()
    ):
        raise ValidatorError("preserved state requires recovery with --prior")
    require_idle([data_ref, data_path])
    if not ready:
        identity_path, identity = validator_config.operator_wallet(config)
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
    issue = sync_issue(tip, floor)
    if issue is not None:
        raise ValidatorError(issue)
    preserved = free_state_path(data_path, "prior", head["epoch"]) if ready else None
    try:
        if preserved is not None:
            move_state(data_path, preserved)
        sync_snapshot(
            sync_binary,
            stage,
            data_path,
            values,
            sources,
            positive_int(values, "OCTRA_OPERATOR_SYNC_CONCURRENCY"),
            positive_int(values, "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY"),
            min_epoch=floor,
            guard = guard,
            prior = preserved,
            identity = (identity_path, identity),
        )
        head = validate_checkpoint(data_path, values, allow_progress=True)
        if head["epoch"] < floor:
            raise ValidatorError("signed snapshot is below recovery boundary")
        if load_wallet(data_path / "wallet.json") != identity:
            raise ValidatorError("installed state identity differs")
    except BaseException:
        if preserved is not None and os.path.lexists(preserved):
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
        cause=need.cause if need is not None else "manual",
        preserved=preserved or "none",
    )

def main():
    parser = argparse.ArgumentParser(prog="recover.sh")
    parser.add_argument("--config", required=True)
    parser.add_argument("--replace-state", action="store_true")
    parser.add_argument("--prior")
    parser.add_argument("--min-epoch", type = int)
    args = parser.parse_args()
    recover(args.config, replace_state = args.replace_state, prior = args.prior,
        min_epoch = args.min_epoch)

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, subprocess.CalledProcessError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)