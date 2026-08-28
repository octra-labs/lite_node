# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import re
import shutil
import stat
import sys
import urllib.error
import urllib.request
from pathlib import Path

from validator_common import ValidatorError
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import validate_checkpoint
from validator_process import active_data_owners
from validator_process import data_pids
from validator_process import pm2_entries

GC_RESERVE = 8 * 1024 * 1024 * 1024

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def tree_bytes(path, device=None, live=False):
    try:
        meta = path.lstat()
    except FileNotFoundError:
        if live:
            return 0
        raise
    if stat.S_ISLNK(meta.st_mode):
        raise ValidatorError(f"symbolic link is not allowed: {path}")
    if device is not None and (meta.st_dev != device or os.path.ismount(path)):
        raise ValidatorError(f"mounted path is not allowed: {path}")
    if stat.S_ISDIR(meta.st_mode):
        device = meta.st_dev if device is None else device
        try:
            children = tuple(path.iterdir())
        except FileNotFoundError:
            if live:
                return 0
            raise
        return sum(tree_bytes(child, device, live) for child in children)
    return meta.st_size

def prior_scan(data_path):
    pattern = re.compile(
        re.escape(data_path.name) + r"\.prior-[0-9]+(?:-[0-9]+)?$"
    )
    items = []
    for child in data_path.parent.iterdir():
        if not pattern.fullmatch(child.name):
            continue
        try:
            meta = child.lstat()
        except OSError:
            items.append((child, "stat"))
            continue
        if stat.S_ISLNK(meta.st_mode):
            items.append((child, "link"))
        elif not stat.S_ISDIR(meta.st_mode):
            items.append((child, "type"))
        else:
            items.append((child, None))
    return sorted(items, key=lambda item: item[0].name)

def suffix_bytes(data_path):
    store = data_path / "irmin_store"
    if not store.is_dir():
        return 0
    total = 0
    for child in store.iterdir():
        if not re.fullmatch(r"store\.[0-9]+\.suffix", child.name):
            continue
        meta = child.lstat()
        if stat.S_ISLNK(meta.st_mode) or not stat.S_ISREG(meta.st_mode):
            raise ValidatorError(f"store suffix is not a file: {child}")
        total += meta.st_size
    return total

def pack_bytes(data_path):
    store = data_path / "irmin_store"
    if not store.is_dir():
        return 0
    total = 0
    for child in store.iterdir():
        meta = child.lstat()
        if stat.S_ISLNK(meta.st_mode):
            raise ValidatorError(f"store entry is a symbolic link: {child}")
        if stat.S_ISREG(meta.st_mode):
            total += meta.st_size
    return total

def snapshot_stats(values, data_path):
    selected = values.get("OCTRA_STATE_SYNC_SNAPSHOT_DIR", "").strip()
    root = (
        Path(selected).expanduser().resolve()
        if selected
        else data_path / "state_sync_snapshots"
    )
    if not root.is_dir():
        return {"count": 0, "payload": 0, "leased": 0, "skipped": 0}
    count = 0
    payload = 0
    leased = 0
    skipped = 0
    for path in root.iterdir():
        try:
            meta = path.lstat()
            if stat.S_ISLNK(meta.st_mode) or not stat.S_ISDIR(meta.st_mode):
                skipped += 1
                continue
            body = json.loads((path / ".certificate.json").read_text(encoding="utf-8"))
            size = int(body["manifest"]["total_size"])
            if size < 0:
                raise ValueError("negative snapshot size")
            count += 1
            payload += size
            leased += int((path / "download.lease").is_file())
        except (KeyError, OSError, TypeError, ValueError):
            skipped += 1
    return {
        "count": count,
        "payload": payload,
        "leased": leased,
        "skipped": skipped,
    }

def data_dir(values):
    return Path(values["OCTRA_DATA_DIR"]).expanduser().resolve()

def rpc_status(port):
    request = urllib.request.Request(
        f"http://127.0.0.1:{port}/status",
        headers={"Accept": "application/json"},
    )
    try:
        with urllib.request.urlopen(request, timeout=3) as response:
            return json.loads(response.read())
    except (OSError, ValueError, urllib.error.URLError):
        return None

def rpc_method(port, method):
    body = json.dumps({
        "jsonrpc": "2.0",
        "id": 1,
        "method": method,
        "params": [],
    }).encode("utf-8")
    request = urllib.request.Request(
        f"http://127.0.0.1:{port}/rpc",
        data=body,
        headers={"Accept": "application/json", "Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=3) as response:
            payload = json.loads(response.read())
            return payload.get("result")
    except (OSError, ValueError, urllib.error.URLError):
        return None

def report(values):
    data_path = data_dir(values)
    states = prior_scan(data_path)
    prior = []
    skipped = []
    for path, reason in states:
        if reason:
            skipped.append((path, reason))
            continue
        try:
            prior.append((path, tree_bytes(path)))
        except (OSError, ValidatorError):
            skipped.append((path, "tree"))
    store_path = data_path / "irmin_store"
    usage = shutil.disk_usage(store_path if store_path.exists() else data_path)
    suffix = suffix_bytes(data_path)
    pack = pack_bytes(data_path)
    need = pack + GC_RESERVE
    snapshots = snapshot_stats(values, data_path)
    emit(
        event="storage",
        data_bytes=tree_bytes(data_path, live=True) if data_path.is_dir() else 0,
        pack_bytes=pack,
        suffix_bytes=suffix,
        prior_count=len(prior),
        prior_bytes=sum(size for _, size in prior),
        prior_skipped=len(skipped),
        free_bytes=usage.free,
        gc_need_bytes=need,
        gc_ready=str(usage.free >= need).lower(),
    )
    emit(
        event="sync_store",
        snapshot_count=snapshots["count"],
        snapshot_payload_bytes=snapshots["payload"],
        lease_files=snapshots["leased"],
        snapshot_skipped=snapshots["skipped"],
    )
    for path, size in prior:
        emit(event="prior_state", path=path, bytes=size)
    for path, reason in skipped:
        emit(event="prior_skipped", path=path, reason=reason)
    gc = rpc_method(values["OCTRA_API_PORT"], "octra_epochTags")
    if isinstance(gc, dict):
        emit(
            event="pack_gc",
            enabled=str(gc.get("pack_gc_enabled", False)).lower(),
            running=str(gc.get("pack_gc_running", False)).lower(),
            split_epoch=gc.get("split_epoch", "none"),
            keep_epochs=gc.get("keep_epochs", "unknown"),
            min_epoch=gc.get("min_epoch", "unknown"),
            max_epoch=gc.get("max_epoch", "unknown"),
        )

def live_state(values, config):
    data_path = data_dir(values)
    if data_path.parent == data_path or not data_path.is_dir():
        raise ValidatorError("data directory is invalid")
    head = validate_checkpoint(data_path, values, allow_progress=True)
    if (data_path / "recovery/sync_need.json").exists():
        raise ValidatorError("recovery marker is present")
    status = rpc_status(values["OCTRA_API_PORT"])
    if not isinstance(status, dict):
        raise ValidatorError("local RPC is unavailable")
    try:
        rpc_epoch = int(status.get("head_epoch") or status.get("current_epoch"))
    except (TypeError, ValueError) as error:
        raise ValidatorError("local RPC head is invalid") from error
    if rpc_epoch < int(head["epoch"]):
        raise ValidatorError("local RPC is behind the stored head")
    identity = load_wallet(Path(config).parent / "wallet.json")
    if load_wallet(data_path / "wallet.json") != identity:
        raise ValidatorError("operator identity and state wallet mismatch")
    return data_path, identity

def prior_check(path, identity, entries):
    if os.path.ismount(path):
        return None, "mount"
    try:
        wallet = load_wallet(path / "wallet.json")
    except (OSError, ValidatorError):
        return None, "wallet"
    if wallet != identity:
        return None, "wallet"
    owners = active_data_owners(entries, str(path))
    pids = data_pids(path)
    if owners or pids:
        return None, "active"
    try:
        return tree_bytes(path), None
    except (OSError, ValidatorError):
        return None, "tree"

def prior_plan(data_path, identity):
    entries = pm2_entries(required=False)
    plan = []
    skipped = []
    for path, reason in prior_scan(data_path):
        size, reason = (
            (None, reason)
            if reason
            else prior_check(path, identity, entries)
        )
        if reason:
            skipped.append((path, reason))
        else:
            plan.append((path, size))
    return plan, skipped

def remove_prior(data_path, identity):
    plan, skipped = prior_plan(data_path, identity)
    for path, reason in skipped:
        emit(event="prior_skipped", path=path, reason=reason)
    removed = 0
    total = 0
    for path, size in plan:
        staged = path.with_name(path.name + f".removing-{os.getpid()}")
        path.replace(staged)
        shutil.rmtree(staged)
        removed += 1
        total += size
        emit(event="prior_removed", path=path, bytes=size)
    return removed, total, len(skipped)

def main():
    parser = argparse.ArgumentParser(prog="storage.sh")
    parser.add_argument("--config", required=True)
    parser.add_argument("--prune-prior", action="store_true")
    parser.add_argument("--yes", action="store_true")
    args = parser.parse_args()
    values = parse_env(args.config)
    if not args.prune_prior:
        report(values)
        return
    if not args.yes:
        raise ValidatorError("prior removal requires --yes")
    data_path, identity = live_state(values, args.config)
    removed, total, skipped = remove_prior(data_path, identity)
    status = "partial" if skipped else "complete"
    emit(
        event="prior_prune",
        status=status,
        removed=removed,
        skipped=skipped,
        bytes=total,
    )

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)