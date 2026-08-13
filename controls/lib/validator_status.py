# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import shutil
import subprocess
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

from validator_common import ValidatorError
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import sha256_file
from validator_common import state_ready
from validator_enroll import membership

def emit(key, value):
    print(f"{key} = {value}")

def pm2_process(name):
    try:
        payload = subprocess.run(
            ["pm2", "jlist"],
            check=True,
            capture_output=True,
            text=True,
        ).stdout
        entries = json.loads(payload)
    except Exception:
        return None
    return next((entry for entry in entries if entry.get("name") == name), None)

def process_memory(pid):
    path = Path(f"/proc/{pid}/status")
    if not path.is_file():
        return {}
    wanted = {"VmRSS", "RssAnon", "RssFile", "VmSwap"}
    values = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        name = line.split(":", 1)[0]
        if name in wanted:
            values[name] = " ".join(line.split()[1:])
    return values

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

def head_status(data_dir):
    path = Path(data_dir) / "HEAD.json"
    if not path.is_file():
        return None
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except Exception:
        return None

def disk_status(data_dir):
    parent = Path(data_dir)
    while not parent.exists() and parent != parent.parent:
        parent = parent.parent
    return shutil.disk_usage(parent)

def age(value):
    if not value:
        return "unknown"
    seconds = max(0, int(time.time() - value / 1000))
    days, remainder = divmod(seconds, 86400)
    hours, remainder = divmod(remainder, 3600)
    minutes, seconds = divmod(remainder, 60)
    if days:
        return f"{days}d{hours}h"
    if hours:
        return f"{hours}h{minutes}m"
    return f"{minutes}m{seconds}s"

def promotion_readiness(values, data_dir):
    permissionless = values.get(
        "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH",
        "",
    ).isdigit()
    if (
        values["OCTRA_OPERATOR_ROLE"] == "validator"
        and (
            permissionless
            or values.get("OCTRA_VALIDATOR_READY_STRICT") != "1"
        )
    ):
        return "not_required"
    marker = Path(data_dir) / "ready_to_vote.json"
    return "ready" if marker.is_file() else "pending"

def round_view(payload):
    if not isinstance(payload, dict):
        return {}
    state = payload.get("round_state")
    if not isinstance(state, dict):
        return {}
    try:
        epoch = int(state["epoch_id"])
        round_id = int(state["round"])
    except (KeyError, TypeError, ValueError):
        return {}
    step = state.get("step")
    agreed = payload.get("round_agreed")
    peers = payload.get("round_peers")
    if epoch < 0 or round_id < 0 or not isinstance(step, str):
        return {}
    return {
        "round_epoch": epoch,
        "round": round_id,
        "round_step": step,
        "round_agreed": agreed if isinstance(agreed, bool) else "unknown",
        "round_peers": len(peers) if isinstance(peers, list) else 0,
    }


def main():
    parser = argparse.ArgumentParser(prog="stat.sh")
    parser.add_argument("--config", required=True)
    parser.add_argument("--logs", action="store_true")
    args = parser.parse_args()
    values = parse_env(args.config)
    data_dir = values["OCTRA_DATA_DIR"]
    wallet = load_wallet(Path(data_dir) / "wallet.json")
    process = pm2_process(values["OCTRA_OPERATOR_PM2_NAME"])
    emit("node", values["OCTRA_OPERATOR_PM2_NAME"])
    emit("role", values["OCTRA_OPERATOR_ROLE"])
    emit("address", wallet["address"])
    emit("chain", values["OCTRA_CHAIN_ID"])
    emit("checkpoint", "ready" if state_ready(data_dir) else "missing")
    emit("checkpoint_epoch", values["OCTRA_CHECKPOINT_EPOCH"])
    emit("checkpoint_root", values["OCTRA_CHECKPOINT_STATE_ROOT"])
    sync_marker = Path(data_dir) / ".state_sync/snapshot_verified.json"
    emit("state_sync", "verified" if sync_marker.is_file() else "direct")
    emit("promotion_readiness", promotion_readiness(values, data_dir))
    emit(
        "validator_admission_epoch",
        values["OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH"],
    )
    binary = Path(values["OCTRA_OPERATOR_BINARY"])
    emit("binary", sha256_file(binary) if binary.is_file() else "missing")
    worker = Path(values["OCTRA_PVAC_VERIFY_WORKER"])
    worker_hash = sha256_file(worker) if worker.is_file() else "missing"
    emit("pvac_worker", worker_hash)
    emit(
        "pvac_worker_integrity",
        "ready"
        if worker_hash == values["OCTRA_PVAC_VERIFY_WORKER_HASH"]
        else "mismatch",
    )
    if process is None:
        emit("process", "offline")
    else:
        environment = process.get("pm2_env", {})
        monitor = process.get("monit", {})
        pid = process.get("pid", 0)
        emit("process", environment.get("status", "unknown"))
        emit("pid", pid)
        emit("uptime", age(environment.get("pm_uptime")))
        emit("restarts", environment.get("restart_time", 0))
        emit("cpu", f"{monitor.get('cpu', 0)}%")
        emit("rss", f"{round(monitor.get('memory', 0) / 1024 ** 2, 1)} MiB")
        for key, value in process_memory(pid).items():
            emit(key.lower(), value)
    usage = disk_status(data_dir)
    emit("disk_used", f"{round(usage.used / 1024 ** 3, 1)} GiB")
    emit("disk_free", f"{round(usage.free / 1024 ** 3, 1)} GiB")
    status = rpc_status(values["OCTRA_API_PORT"])
    if status is None:
        emit("rpc", "unavailable")
        head = head_status(data_dir)
        if head:
            emit("head_epoch", head.get("epoch_id", head.get("epoch", "unknown")))
            emit("state_root", head.get("state_root", "unknown"))
    else:
        emit("rpc", "ready")
        emit("epoch", status.get("current_epoch", "unknown"))
        emit("head_epoch", status.get("head_epoch", "unknown"))
        emit("state_root", status.get("state_root", "unknown"))
        emit("txid_hi", status.get("txid_hi", "unknown"))
        emit("accounts", status.get("total_accounts", "unknown"))
    peers = rpc_method(values["OCTRA_API_PORT"], "octra_consensusPeerStates")
    if peers is None:
        emit("peer_rpc", "unavailable")
    else:
        diagnostics = peers.get("p2p_diagnostics") or {}
        records = peers.get("peers") or []
        emit("peer_rpc", "ready")
        voting = peers.get("voting")
        if isinstance(voting, bool):
            emit("voting", "enabled" if voting else "disabled")
            reason = peers.get("voting_reason")
            if isinstance(reason, str) and reason:
                emit("voting_reason", reason)
        else:
            emit("voting", "unavailable")
        for key, value in round_view(peers).items():
            emit(key, value)
        emit("p2p_connected", diagnostics.get("connected", 0))
        emit("p2p_known", diagnostics.get("known", 0))
        emit("consensus_peers", len(records))
        if status and records:
            local_epoch = int(status.get("head_epoch") or status.get("current_epoch") or 0)
            lags = [max(0, local_epoch - int(record.get("head_epoch", 0))) for record in records]
            emit("peer_max_lag", max(lags))
    try:
        validator_state = membership(values, wallet)
        emit("validator_active", str(validator_state["active"]).lower())
        emit("validator_scheduled", str(validator_state["scheduled"]).lower())
        emit("validator_activation_epoch", validator_state["activate_epoch"])
    except ValidatorError:
        emit("validator_membership", "unavailable")
    if args.logs:
        subprocess.run([
            "pm2",
            "logs",
            values["OCTRA_OPERATOR_PM2_NAME"],
            "--lines",
            "40",
            "--nostream",
        ], check=False)

if __name__ == "__main__":
    try:
        main()
    except (ValidatorError, OSError, KeyError) as error:
        emit("status", f"unavailable reason = {error}")
        sys.exit(1)