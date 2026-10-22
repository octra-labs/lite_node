# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import base64
import json
import os
import re
import signal
import shutil
import stat
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional
from sync_tip import sync_head, sync_epoch, sync_fields, sync_issue

from release import trusted as trusted_release
from validator_common import ValidatorError
from validator_common import load_network
from validator_common import load_head
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import sha256_file
from validator_common import state_ready
from validator_common import validate_checkpoint
from validator_common import write_private_json
from validator_enroll import membership
from validator_process import entry_data
from validator_process import node_owners
from validator_process import process_alive
from validator_process import wait_stopped
from validator_recover import read_need
from validator_recover import read_plan
from validator_recover import PLAN_SCHEMA
from validator_recover import recover
from validator_recover import sync_dir
from validator_exit import command_lock
from validator_rejoin import place_floor
from validator_status import rpc_method
from validator_status import rpc_status
from validator_status import recent_heads
from sync_need import Need
from sync_need import choose as choose_need
from sync_need import make
from sync_need import SCHEMA as NEED_SCHEMA
from sync_need import history_limit, snapshot_lag
from validator_config import operator_wallet
from wal_check import inspect as inspect_wal

HEX40 = re.compile(r"^[0-9a-f]{40}$")
ACTIVE = frozenset({"launching", "online", "stopping"})
VOTE = re.compile(r"^[0-9]{20}_[0-9]{8}_[0-9a-f]{64}\.vote$")
RESTART_CODE = 75

@dataclass(frozen=True)
class DiskState:
    epoch: int
    root: str
    txid: int
    address: str
    pubkey: str
    need: Optional[Need]

@dataclass(frozen=True)
class Resume:
    allowed: bool
    reason: str

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()), flush=True)

def call(command, cwd=None, check=True, quiet=False, timeout=None):
    if timeout is not None and not quiet:
        with subprocess.Popen(command, cwd=cwd, start_new_session=True) as process:
            try:
                code = process.wait(timeout=timeout)
            except subprocess.TimeoutExpired as error:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                process.wait()
                raise ValidatorError(f"command timed out: {command[0]}") from error
            if check and code:
                raise subprocess.CalledProcessError(code, command)
            return subprocess.CompletedProcess(command, code)
    return subprocess.run(
        command,
        cwd=cwd,
        check=check,
        capture_output=quiet,
        text=True,
        timeout=timeout,
    )

def capture(command, cwd=None, timeout=None):
    return call(command, cwd=cwd, quiet=True, timeout=timeout).stdout.strip()

def git_release(root):
    unknown = {
        "repo_head": "unknown",
        "upstream_head": "unknown",
        "upstream": "unknown",
    }
    if not (root / ".git").exists():
        return unknown
    try:
        head = capture(["git", "rev-parse", "HEAD"], cwd=root)
    except (OSError, subprocess.CalledProcessError):
        return unknown
    result = {**unknown, "repo_head": head if HEX40.fullmatch(head) else "unknown"}
    try:
        remote, merge = update_ref(root)
        result["upstream"] = f"{remote}:{merge}"
    except (OSError, subprocess.CalledProcessError, ValidatorError):
        return result
    try:
        raw = capture(
            ["git", "ls-remote", "--exit-code", remote, merge],
            cwd=root,
            timeout=15.0,
        )
        latest = raw.split()[0] if raw.split() else "unknown"
        result["upstream_head"] = latest if HEX40.fullmatch(latest) else "unknown"
        return result
    except (OSError, subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return result

def update_ref(root):
    try:
        branch = capture(
            ["git", "symbolic-ref", "--quiet", "--short", "HEAD"],
            cwd=root,
        )
        remote = capture(
            ["git", "config", "--get", f"branch.{branch}.remote"],
            cwd=root,
        )
        merge = capture(
            ["git", "config", "--get", f"branch.{branch}.merge"],
            cwd=root,
        )
        if remote and merge:
            return remote, merge
    except (OSError, subprocess.CalledProcessError):
        pass
    remotes = capture(["git", "remote"], cwd=root).splitlines()
    choices = ["origin"] if "origin" in remotes else remotes
    return choose("git release remote", choices), "refs/heads/main"

def choose(label, values):
    unique = []
    for value in values:
        if value not in unique:
            unique.append(value)
    if not unique:
        raise ValidatorError(f"{label} was not found")
    if len(unique) != 1:
        raise ValidatorError(f"{label} is ambiguous: " + ",".join(map(str, unique)))
    return unique[0]

def pm2_entries():
    if shutil.which("pm2") is None:
        return []
    try:
        payload = capture(["pm2", "jlist"])
        entries = json.loads(payload)
    except Exception:
        return []
    return entries if isinstance(entries, list) else []

def entry_env(entry):
    meta = entry.get("pm2_env")
    if not isinstance(meta, dict):
        return {}
    nested = meta.get("env")
    nested = nested if isinstance(nested, dict) else {}
    return {**nested, **meta}

def ctl(scope, *args):
    command = ["systemctl"]
    if scope == "user":
        command.append("--user")
    return command + list(args)

def unit_names(scope):
    if shutil.which("systemctl") is None:
        return []
    try:
        raw = capture(ctl(
            scope,
            "list-units",
            "--type=service",
            "--all",
            "--no-legend",
            "--plain",
        ))
    except Exception:
        return []
    names = [line.split()[0] for line in raw.splitlines() if line.split()]
    return sorted(name for name in names if "octra" in name.lower())

def props(scope, unit):
    raw = capture(ctl(
        scope,
        "show",
        unit,
        "--no-pager",
        "--property=Id,LoadState,ActiveState,SubState,MainPID,User,ExecStart,EnvironmentFiles,WorkingDirectory,Restart,SuccessExitStatus,RestartPreventExitStatus,RestartForceExitStatus",
    ))
    values = {}
    for line in raw.splitlines():
        if "=" in line:
            key, value = line.split("=", 1)
            values[key] = value
    if values.get("LoadState") != "loaded":
        raise ValidatorError(f"systemd unit is not loaded: {unit}")
    return values

def env_paths(raw):
    found = re.findall(r"(?:^|\s)-?(/[^\s;()]+)", raw or "")
    return [Path(value).expanduser().resolve() for value in found]

def exec_path(raw):
    match = re.search(r"(?:path|argv\[\])=([^ ;}]+)", raw or "")
    return Path(match.group(1)).resolve() if match else None

def proc_env(pid):
    if not isinstance(pid, int) or pid <= 0:
        return {}
    try:
        raw = Path(f"/proc/{pid}/environ").read_bytes()
    except OSError:
        return {}
    values = {}
    for item in raw.split(b"\0"):
        if b"=" in item:
            key, value = item.split(b"=", 1)
            values[key.decode("ascii", "ignore")] = value.decode("utf-8", "replace")
    return values

def proc_exe(pid):
    try:
        return Path(os.readlink(f"/proc/{pid}/exe")).resolve()
    except OSError:
        return None

def proc_hash(pid):
    path = Path(f"/proc/{pid}/exe")
    try:
        return sha256_file(path)
    except OSError:
        return None

def data_pids(data_dir):
    root = Path("/proc")
    if not root.is_dir():
        return []
    expected = os.fsencode(str(Path(data_dir).resolve()))
    result = []
    for item in root.iterdir():
        if not item.name.isdigit():
            continue
        try:
            values = (item / "environ").read_bytes().split(b"\0")
        except OSError:
            continue
        if b"OCTRA_DATA_DIR=" + expected in values:
            result.append(int(item.name))
    return sorted(result)

def valid_config(path):
    try:
        values = parse_env(path)
    except (OSError, ValidatorError):
        return False
    needed = {
        "OCTRA_API_PORT",
        "OCTRA_DATA_DIR",
        "OCTRA_OPERATOR_BINARY",
        "OCTRA_OPERATOR_ROLE",
    }
    return needed <= set(values) and values["OCTRA_OPERATOR_ROLE"] in {
        "observer",
        "validator",
    }

def unit_rows(scope, names):
    rows = []
    for name in names:
        try:
            rows.append((scope, name, props(scope, name)))
        except (OSError, subprocess.CalledProcessError, ValidatorError):
            pass
    return rows

def config_path(root, explicit, rows, entries):
    if explicit:
        path = Path(explicit).expanduser().resolve()
        if not valid_config(path):
            raise ValidatorError(f"node configuration is invalid: {path}")
        return path
    paths = []
    local = root / ".keys/validator/node.env"
    if valid_config(local):
        paths.append(local.resolve())
    for _, _, info in rows:
        paths.extend(
            path for path in env_paths(info.get("EnvironmentFiles"))
            if valid_config(path)
        )
    for entry in entries:
        value = entry_env(entry).get("OCTRA_OPERATOR_CONFIG")
        if isinstance(value, str) and valid_config(Path(value)):
            paths.append(Path(value).expanduser().resolve())
    return choose("node configuration", paths)

def pm2_sup(config, values, entries):
    data = str(Path(values["OCTRA_DATA_DIR"]).resolve())
    name = values.get("OCTRA_OPERATOR_PM2_NAME")
    matches = []
    for entry in entries:
        env = entry_env(entry)
        bound = env.get("OCTRA_OPERATOR_CONFIG")
        same_config = isinstance(bound, str) and Path(bound).expanduser().resolve() == config
        same_data = entry_data(entry) == data
        same_name = name and entry.get("name") == name
        if same_config or (same_data and same_name):
            meta = entry.get("pm2_env") or {}
            status = meta.get("status", "unknown")
            matches.append({
                "kind": "pm2",
                "scope": "user",
                "name": entry.get("name", name),
                "pid": entry.get("pid", 0),
                "active": status in ACTIVE,
                "state": status,
                "config": config,
                "exec": proc_exe(entry.get("pid", 0)),
                "env_files": [],
            })
    return matches

def unit_sup(config, values, rows, explicit=None):
    data = str(Path(values["OCTRA_DATA_DIR"]).resolve())
    binary = Path(values["OCTRA_OPERATOR_BINARY"]).expanduser().resolve()
    matches = []
    for scope, name, info in rows:
        files = env_paths(info.get("EnvironmentFiles"))
        pid = int(info.get("MainPID") or 0)
        environment = proc_env(pid)
        command = exec_path(info.get("ExecStart"))
        bound = environment.get("OCTRA_OPERATOR_CONFIG")
        same_config = config in files or (
            isinstance(bound, str)
            and Path(bound).expanduser().resolve() == config
        )
        same_data = environment.get("OCTRA_DATA_DIR") == data
        same_binary = command == binary or proc_exe(pid) == binary
        if name == explicit or same_config or (same_data and same_binary):
            matches.append({
                "kind": "systemd",
                "scope": scope,
                "name": name,
                "pid": pid,
                "active": info.get("ActiveState") == "active",
                "state": info.get("ActiveState", "unknown"),
                "config": config,
                "exec": command,
                "env_files": files,
                "restart": {
                    key: info.get(key, "") for key in (
                        "Restart", "SuccessExitStatus",
                        "RestartPreventExitStatus", "RestartForceExitStatus",
                    )
                },
            })
    return matches

def restart_ready(policy):
    codes = {str(RESTART_CODE), "TEMPFAIL", "EX_TEMPFAIL"}
    has = lambda key: bool(codes.intersection(policy.get(key, "").split()))
    if has("RestartPreventExitStatus"):
        return False
    if has("RestartForceExitStatus"):
        return True
    mode = policy.get("Restart", "")
    success = has("SuccessExitStatus")
    return mode == "always" or (mode == "on-success" if success else mode == "on-failure")

def restart_notice(sup):
    if sup["kind"] != "systemd":
        return
    policy = sup.get("restart", {})
    ready = restart_ready(policy)
    emit(
        event="recovery_restart",
        status="automatic" if ready else "warning",
        unit=sup["name"],
        policy=policy.get("Restart") or "unknown",
        exit_code=RESTART_CODE,
        action="none" if ready else "review_unit_restart_policy",
    )

def storage_stop_ready(policy):
    codes = {"78", "CONFIG", "EX_CONFIG"}
    has = lambda key: bool(codes.intersection(policy.get(key, "").split()))
    if has("RestartForceExitStatus"):
        return False
    if has("RestartPreventExitStatus"):
        return True
    mode = policy.get("Restart", "")
    if mode not in {"no", "always", "on-success", "on-failure", "on-abnormal", "on-abort", "on-watchdog"}:
        return False
    if has("SuccessExitStatus"):
        return mode not in {"always", "on-success"}
    return mode not in {"always", "on-failure"}

def supervisor(config, values, rows, entries, unit=None):
    supervisors = pm2_sup(config, values, entries)
    supervisors.extend(unit_sup(config, values, rows, explicit = unit))
    return choose("node supervisor", supervisors)

def inspect_pending(data_dir):
    return inspect_wal(data_dir)

def inspect_votes(data_dir):
    vote_log = Path(data_dir) / "vote_log"
    if not vote_log.is_dir():
        return []
    faults = []
    for path in sorted(vote_log.glob("*.vote")):
        if not VOTE.fullmatch(path.name):
            faults.append(("vote_name_invalid", path))
            continue
        try:
            meta = path.lstat()
            if not stat.S_ISREG(meta.st_mode) or meta.st_size <= 0 or meta.st_size > 512:
                faults.append(("vote_record_unreadable", path))
        except FileNotFoundError:
            continue
        except OSError as error:
            faults.append(("vote_record_unreadable", path, str(error)))
    return faults

def durable_faults(data_dir, head=None):
    if not isinstance(head, int) or isinstance(head, bool) or head < 0:
        head = None
        try:
            head = load_head(data_dir)["epoch"]
        except ValidatorError:
            pass
    votes = inspect_votes(data_dir)
    return inspect_pending(data_dir) + [fault for fault in votes
        if head is None or not VOTE.fullmatch(Path(fault[1]).name)
        or int(Path(fault[1]).name.split("_", 1)[0]) >= head]

def check_built(root, values):
    entries = (
        ("octra_node.exe", "OCTRA_OPERATOR_BINARY", "OCTRA_BINARY_HASH"),
        ("octra_pvac_worker.exe", "OCTRA_PVAC_VERIFY_WORKER", "OCTRA_PVAC_VERIFY_WORKER_HASH"),
        ("octra_state_sync_client.exe", "OCTRA_OPERATOR_SYNC_BINARY", "OCTRA_OPERATOR_SYNC_BINARY_HASH"),
    )
    for name, path_key, hash_key in entries:
        built = Path(root) / "_build/default/bin" / name
        selected = Path(values.get(path_key, "")).expanduser()
        if not built.is_file() or not selected.is_file():
            raise ValidatorError(f"built binary is missing: {name}; node was not stopped")
        digest = sha256_file(built)
        if sha256_file(selected) != digest or values.get(hash_key) != digest:
            raise ValidatorError(f"built binary differs from selected runtime: {name}; node was not stopped")

def peer_head(payload):
    return max(recent_heads(payload), default=None)

def round_value(payload, key):
    if not isinstance(payload, dict):
        return None
    try:
        value = int(payload[key])
    except (KeyError, TypeError, ValueError):
        return None
    return value if value >= 0 else None

def sync_target(state, tip):
    values = [
        value
        for value in [
            tip["head"] if tip is not None else None,
            state.get("peer_epoch"),
        ]
        if isinstance(value, int)
    ]
    return max(values) if values else None

def sync_wait(values, need, wait, interval, *, min_epoch = 0):
    deadline = time.monotonic() + wait
    seen = None
    while True:
        tip = sync_head(values)
        head = tip["head"] if tip is not None else None
        required = max(need.epoch, min_epoch)
        target = need.target
        if isinstance(target, int):
            required = max(required, target - snapshot_lag(values))
        if head is not None:
            required = max(required, head - snapshot_lag(values))
        issue = sync_issue(tip, required)
        current = sync_fields(tip, required)
        if current != seen:
            emit(
                event = "sync_snapshot",
                status = "ready" if issue is None else
                    "refused" if current["head_status"] == "independent_missing" else "wait",
                **current,
            )
            seen = current
        if issue is None:
            return {**tip, "required": required}
        if current["head_status"] == "independent_missing" or time.monotonic() >= deadline:
            raise ValidatorError(issue)
        time.sleep(interval)

def sync_limit(values):
    return history_limit(values)

def need_reached(need, head, limit):
    return (need is not None and need.cause in ("root", "range")
        and type(head) is int and head >= max(need.epoch, (need.target or 0) - limit))

def stored_epoch(values):
    try:
        return load_head(Path(values["OCTRA_DATA_DIR"]))["epoch"]
    except (OSError, ValueError, KeyError, ValidatorError):
        return None

def remember_plan(config, values, state, planned):
    path = Path(config).with_name(Path(config).name + ".sync.json")
    data = str(Path(values["OCTRA_DATA_DIR"]).expanduser().resolve())
    with command_lock(values):
        stored = read_plan(config, values)
        head = state.get("head_epoch")
        if state.get("rpc") != "ready" or type(head) is not int:
            head = stored_epoch(values)
        if need_reached(stored, head, sync_limit(values)):
            path.unlink()
            sync_dir(path.parent)
            stored = None
        if need_reached(planned, head, sync_limit(values)):
            planned = None
        plan = planned if stored is None else stored
        if planned is not None and (plan is None or planned.target > plan.target):
            plan = planned
        if plan is not None and plan != stored:
            write_private_json(path, {
                "schema": PLAN_SCHEMA, "data_dir": data, "need": {
                    "schema": NEED_SCHEMA, "chain_id": plan.chain, "cause": plan.cause,
                    "epoch": plan.epoch, "head": plan.head, "target": plan.target,
                },
            })
            sync_dir(path.parent)
        return plan

def sync_plan(values, state, target):
    head = state.get("head_epoch")
    if (
        state.get("process") != "online"
        or state.get("rpc") != "ready"
        or not isinstance(head, int)
        or not isinstance(target, int)
        or target - head <= sync_limit(values)
    ):
        return None
    return make(values["OCTRA_CHAIN_ID"], "range", head + 1, head, target)

def profile_match(version, release):
    if not isinstance(version, dict):
        return False
    direct = version.get("runtime_profile_hash") == release["runtime_profile_hash"]
    rows = version.get("runtime_profiles")
    if rows is None:
        return direct
    epoch = version.get("profile_epoch")
    if (type(epoch) is not int or epoch < 0
            or version.get("chain_id") != release["chain_id"]
            or not isinstance(rows, list) or not 1 <= len(rows) <= 128):
        return False
    previous = -1
    selected = None
    hashes = set()
    for row in rows:
        if not isinstance(row, dict) or set(row) != {"from_epoch", "hash"}:
            return False
        start, digest = row["from_epoch"], row["hash"]
        if (type(start) is not int or start < 0 or start <= previous
                or not isinstance(digest, str) or re.fullmatch(r"[0-9a-f]{64}", digest) is None):
            return False
        if previous == -1 and start != 0:
            return False
        previous = start
        hashes.add(digest)
        if start <= epoch:
            selected = digest
    return (release["runtime_profile_hash"] in hashes
        and selected == version.get("runtime_profile_hash"))

def view(values, pid, release):
    status = rpc_status(values["OCTRA_API_PORT"])
    peers = rpc_method(values["OCTRA_API_PORT"], "octra_consensusPeerStates")
    version = rpc_method(values["OCTRA_API_PORT"], "octra_runtimeVersion")
    voting = peers.get("voting") if isinstance(peers, dict) else None
    reason = peers.get("voting_reason") if isinstance(peers, dict) else None
    round_state = peers.get("round_state") if isinstance(peers, dict) else None
    round_epoch = round_value(round_state, "epoch_id")
    round_id = round_value(round_state, "round")
    local = status.get("head_epoch") if isinstance(status, dict) else None
    remote = peer_head(peers)
    wallet = load_wallet(Path(values["OCTRA_DATA_DIR"]) / "wallet.json")
    try:
        member = membership(values, wallet)
    except ValidatorError:
        member = {"active": None, "scheduled": None, "activate_epoch": None}
    expected = values.get("OCTRA_BINARY_HASH")
    live = proc_hash(pid)
    source_match = (
        isinstance(version, dict)
        and version.get("source_commit") == release["source_commit"]
    )
    runtime_match = (
        source_match
        and profile_match(version, release)
        and version.get("consensus_profile") == release["consensus_profile"]
        and version.get("consensus_rules_id") == release["consensus_rules_id"]
    )
    return {
        "pid": pid,
        "process": "online" if process_alive(pid) else "offline",
        "live_binary": live or "unknown",
        "binary_match": (live is not None and live == expected
            and isinstance(version, dict) and version.get("binary_hash") == live),
        "live_source": version.get("source_commit", "unknown")
        if isinstance(version, dict) else "unknown",
        "source_match": source_match,
        "runtime_match": runtime_match,
        "live_profile": version.get("runtime_profile_hash")
        if isinstance(version, dict) else None,
        "profile_epoch": version.get("profile_epoch") if isinstance(version, dict) else None,
        "live_consensus_profile": version.get("consensus_profile")
        if isinstance(version, dict) else None,
        "live_rules_id": version.get("consensus_rules_id")
        if isinstance(version, dict) else None,
        "rpc": "ready" if isinstance(status, dict) else "unavailable",
        "head_epoch": local,
        "peer_epoch": remote,
        "lag": max(0, remote - local) if isinstance(local, int) and isinstance(remote, int) else None,
        "voting": voting,
        "voting_reason": reason,
        "round_epoch": round_epoch,
        "round": round_id,
        "validator_member": member["active"],
        "validator_scheduled": member["scheduled"],
        "activation_epoch": member["activate_epoch"],
    }

def require_profile(state, release, *, allow_missing = False):
    missing = state.get("live_profile") is None and all(
        state.get(key) in (None, release[field]) for key, field in (
            ("live_consensus_profile", "consensus_profile"),
            ("live_rules_id", "consensus_rules_id"),
        )
    )
    if allow_missing and missing:
        return
    if state.get("source_match") is True and state.get("runtime_match") is not True:
        raise ValidatorError(
            "release runtime mismatch"
            f"; source = {release['source_commit']}"
            f"; head = {state.get('head_epoch')}"
            f"; profile_epoch = {state.get('profile_epoch')}"
            f"; marker_profile = {release['runtime_profile_hash']}"
            f"; live_profile = {state.get('live_profile')}"
            f"; marker_consensus_profile = {release['consensus_profile']}"
            f"; live_consensus_profile = {state.get('live_consensus_profile')}"
            f"; marker_rules = {release['consensus_rules_id']}"
            f"; live_rules = {state.get('live_rules_id')}"
            "; action = review_signed_release"
        )

def matches(state):
    return (
        state.get("process") == "online"
        and state.get("rpc") == "ready"
        and state.get("binary_match") is True
        and state.get("source_match") is True
        and state.get("runtime_match") is True
        and isinstance(state.get("head_epoch"), int)
    )

def startup_running(sup, values, state, release, public):
    expected = values.get("OCTRA_BINARY_HASH")
    return (sup.get("active") is True and type(sup.get("pid")) is int and sup["pid"] > 0
        and state.get("process") == "online" and state.get("rpc") == "unavailable"
        and public == release["public_commit"]
        and values.get("OCTRA_SOURCE_COMMIT") == release["source_commit"]
        and isinstance(expected, str) and re.fullmatch(r"[0-9a-f]{64}", expected) is not None
        and state.get("live_binary") == expected)

def snapshot_running(values, need, plan):
    head = stored_epoch(values)
    return (need_reached(need, head, snapshot_lag(values))
        and (plan is None or need_reached(plan, head, snapshot_lag(values))))

def installed(role, state):
    if role == "validator":
        return matches(state) and state.get("validator_member") is True and state.get("voting") is True
    return matches(state) and state.get("voting") is False and state.get("voting_reason") == "role"

def ready(role, state):
    return installed(role, state) and state.get("lag") == 0

def join_pending(role, state):
    return (
        role == "validator"
        and matches(state)
        and state.get("lag") == 0
        and state.get("validator_member") is False
        and (
            state.get("voting") is True
            or state.get("voting_reason") in ("not_ready", "not_member")
        )
    )

def recovery_installed(role, state, release):
    return (
        release["notice_code"] == "consensus_recovery"
        and installed(role, state)
        and state.get("peer_epoch") is None
    )

def deadline_result(role, state, release):
    if recovery_installed(role, state, release):
        return "installed", "peer_head_unavailable", "leave_running", 0
    return "pending", "health_deadline", "do_not_restart", 2

def current(sup, rows, entries):
    if sup["kind"] == "pm2":
        fresh = pm2_sup(sup["config"], parse_env(sup["config"]), entries)
        match = next((item for item in fresh if item["name"] == sup["name"]), None)
        return match or {**sup, "pid": 0, "active": False, "state": "offline"}
    fresh = unit_sup(
        sup["config"],
        parse_env(sup["config"]),
        rows,
        explicit=sup["name"],
    )
    return fresh[0] if fresh else {**sup, "pid": 0, "active": False, "state": "offline"}

def floor_target(state):
    if state.get("voting") is True:
        return None
    head = state.get("head_epoch")
    epoch = state.get("round_epoch")
    round_id = state.get("round")
    if (
        state.get("process") != "online"
        or state.get("rpc") != "ready"
        or state.get("lag") != 0
        or state.get("voting_reason") != "vote_log_bootstrap"
        or not isinstance(head, int)
        or not isinstance(epoch, int)
        or not isinstance(round_id, int)
        or epoch != head + 1
    ):
        return None
    return head, round_id + 1

def install_floor(root, sup, values, args, live, target):
    head, round_id = target
    data_dir = Path(values["OCTRA_DATA_DIR"])
    wallet = load_wallet(data_dir / "wallet.json")
    place_floor(root, values, wallet, data_dir, head, round_id, check=True)
    stop(live, args.sudo)
    owners = data_pids(data_dir)
    if owners:
        raise ValidatorError(
            "state directory is active after stop: "
            + ",".join(map(str, owners))
        )
    try:
        place_floor(root, values, wallet, data_dir, head, round_id)
    except Exception:
        start(root, sup, sup["config"], args.sudo)
        raise
    start(root, sup, sup["config"], args.sudo)
    emit(event="vote_floor", status="restored", head_epoch=head, round=round_id)

def unit_command(sup, action, use_sudo):
    command = ctl(sup["scope"], action, sup["name"])
    return ["sudo", "-n", *command] if use_sudo else command

def stop(sup, use_sudo):
    pid = sup["pid"]
    if sup["kind"] == "pm2":
        call(["pm2", "stop", sup["name"]])
    else:
        call(unit_command(sup, "stop", use_sudo))
    if pid:
        wait_stopped([pid], timeout=90.0, poll=0.2)

def start(root, sup, config, use_sudo):
    if sup["kind"] == "pm2":
        environment = os.environ.copy()
        environment["OCTRA_OPERATOR_CONFIG"] = str(config)
        subprocess.run(
            ["sh", str(root / "controls/run.sh"), "--rebind-runtime"],
            cwd=root,
            env=environment,
            check=True,
        )
    else:
        call(unit_command(sup, "start", use_sudo))

def disk_state(values, config):
    data = Path(values["OCTRA_DATA_DIR"])
    if not state_ready(data):
        raise ValidatorError("restart state is incomplete")
    head = validate_checkpoint(data, values, allow_progress=True)
    _, identity = operator_wallet(config)
    wallet = load_wallet(data / "wallet.json")
    if wallet != identity:
        raise ValidatorError("restart state identity differs")
    need = read_need(data, values["OCTRA_CHAIN_ID"])
    return DiskState(
        head["epoch"],
        head["state_root"],
        head["txid_hi"],
        wallet["address"],
        wallet["pub"],
        need,
    )

def resume_plan(before, after):
    if before.epoch != after.epoch:
        return Resume(False, "head_changed")
    if before.root != after.root:
        return Resume(False, "root_changed")
    if before.txid != after.txid:
        return Resume(False, "txid_changed")
    if before.address != after.address or before.pubkey != after.pubkey:
        return Resume(False, "identity_changed")
    if before.need != after.need:
        return Resume(False, "marker_changed")
    return Resume(True, "state_unchanged")

def restore_cycle(root, sup, values, args, before, plan, min_epoch):
    try:
        restored = restore_plan(
            values,
            sup["config"],
            plan,
            min_epoch=min_epoch,
        )
    except Exception as error:
        emit(event="restore_restart", status="check", detail=str(error))
        try:
            after = disk_state(values, sup["config"])
        except Exception as state_error:
            emit(
                event="restore_restart",
                status="held",
                reason="state_invalid",
                action="remain_stopped",
            )
            raise error from state_error
        decision = resume_plan(before, after)
        if not decision.allowed:
            emit(
                event="restore_restart",
                status="held",
                reason=decision.reason,
                action="remain_stopped",
            )
            raise
        try:
            start(root, sup, sup["config"], args.sudo)
        except Exception as start_error:
            emit(
                event="restore_restart",
                status="held",
                reason="start_failed",
                action="operator_start",
            )
            raise error from start_error
        emit(
            event="restore_restart",
            status="started",
            reason=decision.reason,
        )
        raise
    start(root, sup, sup["config"], args.sudo)
    return restored

def preflight(root, sup, values, use_sudo, head=None):
    for name in ("octra_node.exe", "octra_pvac_worker.exe", "octra_state_sync_client.exe",
                 "bft_control_tx.exe", "vote_floor.exe"):
        path = Path(root) / "artifacts" / name
        if os.path.lexists(path):
            raise ValidatorError(
                f"runtime_layout_artifacts: {path}; use a separate source checkout; no changes performed"
            )
    owners = data_pids(values["OCTRA_DATA_DIR"])
    if owners and not node_owners(owners, sup["pid"], values):
        raise ValidatorError(
            "data directory is used by unexpected processes: "
            + ",".join(map(str, owners))
        )
    faults = durable_faults(values["OCTRA_DATA_DIR"], head)
    if faults:
        for fault in faults:
            emit(fault = fault[0], path = fault[1],
                detail = fault[2] if len(fault) > 2 else "invalid", action = "do_not_delete")
        raise ValidatorError("durable record requires review; no build, stop or data changes performed")
    if sup["kind"] == "systemd" and not storage_stop_ready(sup.get("restart", {})):
        raise ValidatorError(
            "storage restart policy does not stop on exit 78; review RestartPreventExitStatus "
            "and RestartForceExitStatus before upgrading; no unit changes performed"
        )
    if sup["kind"] == "systemd" and use_sudo:
        call(["sudo", "-n", "true"], quiet=True)
    free = shutil.disk_usage(root).free
    emit(disk_free_gib=round(free / 1024 ** 3, 1))
    if free < 4 * 1024 ** 3:
        raise ValidatorError("less than 4 GiB is free; expand the volume before building")

def git_update(root, public, source):
    dirty = capture(["git", "status", "--porcelain", "--untracked-files=no"], cwd=root)
    if dirty:
        raise ValidatorError("tracked source tree is dirty")
    prior = capture(["git", "rev-parse", "HEAD"], cwd=root)
    if not HEX40.fullmatch(prior):
        raise ValidatorError("local release commit is invalid")
    remote, merge = update_ref(root)
    call(["git", "fetch", "--no-tags", remote, merge], cwd=root, timeout=120)
    target = capture(["git", "rev-parse", "FETCH_HEAD"], cwd=root)
    if not HEX40.fullmatch(target):
        raise ValidatorError("fetched release commit is invalid")
    if target != public:
        raise ValidatorError(f"public commit mismatch: {target}")
    call(["git", "merge", "--ff-only", target], cwd=root)
    head = capture(["git", "rev-parse", "HEAD"], cwd=root)
    if not HEX40.fullmatch(head) or head != target:
        raise ValidatorError("local HEAD does not equal the fetched release")
    if head != public:
        raise ValidatorError(f"public commit mismatch: {head}")
    source_path = root / "SOURCE_COMMIT"
    actual = source_path.read_text(encoding="utf-8").strip() if source_path.is_file() else ""
    if not HEX40.fullmatch(actual):
        raise ValidatorError("SOURCE_COMMIT is missing or invalid")
    if actual != source:
        raise ValidatorError(f"source commit mismatch: {actual or 'missing'}")
    emit(event="upgrade_target", public_commit=head, source_commit=actual)
    return head, actual, prior != head

def release_target(release, args, values):
    if release["action"] == "hold":
        raise ValidatorError("release channel is on hold")
    if values.get("OCTRA_CHAIN_ID") != release["chain_id"]:
        raise ValidatorError("node chain does not match the release channel")
    assertions = (
        ("--public-commit", args.public_commit, release["public_commit"]),
        ("--source-commit", args.source_commit, release["source_commit"]),
    )
    for label, supplied, trusted in assertions:
        if supplied is not None and supplied != trusted:
            raise ValidatorError(f"{label} does not match the signed release marker")
    return release["public_commit"], release["source_commit"]

def verify_release_tree(root, release):
    network = root / "config/network.env"
    if not network.is_file() or sha256_file(network) != release["network_sha256"]:
        raise ValidatorError("network configuration does not match the signed release")
    source = root / "SOURCE_COMMIT"
    actual = source.read_text(encoding="utf-8").strip() if source.is_file() else ""
    if actual != release["source_commit"]:
        raise ValidatorError("source commit does not match the signed release")

def release_sync_values(root, values, release):
    _, _, network = load_network(
        root / "config/network.env",
        release["network_sha256"],
    )
    selected = dict(values)
    for key in (
        "OCTRA_STATE_SYNC_SOURCES",
        "OCTRA_JOIN_RPC",
        "OCTRA_CATCHUP_MAX_LAG",
    ):
        if key in network:
            selected[key] = network[key]
    return selected

def reexec(root):
    script = root / "controls/lib/upgrade.py"
    if not script.is_file():
        raise ValidatorError("updated upgrade control is missing")
    emit(event="upgrade_reexec", script=script)
    os.execv(sys.executable, [sys.executable, str(script), *sys.argv[1:]])
    raise ValidatorError("upgrade re-exec returned")

def verify_unit(sup, values):
    if sup["kind"] != "systemd":
        return None
    binary = Path(values["OCTRA_OPERATOR_BINARY"]).expanduser().resolve()
    if sup["config"] not in sup["env_files"] and proc_env(sup["pid"]).get(
        "OCTRA_OPERATOR_CONFIG"
    ) != str(sup["config"]):
        raise ValidatorError("systemd unit is not bound to the selected config")
    if sup["exec"] != binary and proc_exe(sup["pid"]) != binary:
        raise ValidatorError(
            "systemd ExecStart does not select the node binary: " + str(sup["exec"])
        )
    return binary

def journal_restart(need, head, epoch):
    return (need is not None and need.cause == "root" and need.target is None
        and type(head) is int and type(epoch) is int
        and need.head == head and need.epoch == epoch == head + 1)

def local_journal(values, need):
    if need is None or need.chain != values["OCTRA_CHAIN_ID"]:
        return False
    try:
        data = Path(values["OCTRA_DATA_DIR"])
        head = load_head(data)["epoch"]
        path = data / "finality" / "pending_finalized.json"
        meta = path.lstat()
        if not stat.S_ISREG(meta.st_mode) or not 0 < meta.st_size <= 128 * 1024 * 1024:
            return False
        saved = json.loads(path.read_text(encoding = "utf-8"))
        if not isinstance(saved, dict) or saved.get("schema") != "octra_finality_journal":
            return False
        raw = base64.b64decode(saved["finalize"], validate = True)
        size = int.from_bytes(raw[:4], "big")
        if not 0 < size <= 128 or len(raw) < size + 12:
            return False
        if raw[4:4 + size] != need.chain.encode("utf-8"):
            return False
        epoch = int.from_bytes(raw[4 + size:12 + size], "big")
        return journal_restart(need, head, epoch)
    except (OSError, ValueError, TypeError, KeyError, UnicodeError, RecursionError, ValidatorError):
        return False

def diagnose(root, sup, values, release):
    pid = sup["pid"] if sup["active"] else 0
    emit(
        status="diagnostic",
        root=root,
        supervisor=sup["kind"],
        name=sup["name"],
        state=sup["state"],
        config=sup["config"],
        data_dir=values["OCTRA_DATA_DIR"],
        role=values["OCTRA_OPERATOR_ROLE"],
        pid=pid,
    )
    if pid:
        state = view(values, pid, release)
        emit(**state)
    else:
        state = {"process": "offline", "source_match": False, "binary_match": False}
    marked = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
    if marked is not None and marked.cause == "range":
        emit(event = "binary_rollback", status = "hold", target_source = "f7637a7e",
            reason = "range_marker", action = "use_upgrade",
            path = Path(values["OCTRA_DATA_DIR"]) / "recovery" / "sync_need.json")
    profile_error = None
    try:
        require_profile(state, release, allow_missing = marked is not None)
    except ValidatorError as error:
        profile_error = str(error)
    git = git_release(root)
    remote_known = git["upstream_head"] != "unknown"
    published = remote_known and git["upstream_head"] == release["public_commit"]
    tip = sync_head(values)
    planned = remember_plan(sup["config"], values, state,
        sync_plan(values, state, sync_target(state, tip)))
    need = choose_need(marked, planned, values["OCTRA_CHAIN_ID"])
    replay = planned is None and local_journal(values, marked)
    resumed = need is not None and snapshot_running(values, need, planned)
    starting = (need is None or replay or resumed) and (startup_running(
        sup, values, state, release, git["repo_head"]) or ((replay or resumed) and matches(state)))
    restart_error = (need is not None and need.cause in ("root", "range")
        and sup["kind"] == "systemd" and not restart_ready(sup.get("restart", {})))
    upgrade = (
        git["repo_head"] != release["public_commit"]
        or state.get("source_match") is not True
        or state.get("binary_match") is not True
        or state.get("runtime_match") is not True
        or need is not None
    )
    faults = durable_faults(values["OCTRA_DATA_DIR"], state.get("head_epoch"))
    action = "leave_running" if starting else "upgrade_local_journal" if replay else (
        "recover" if need is not None else ("upgrade" if upgrade else "none"))
    if faults:
        action = "do_not_delete"
    elif profile_error is not None:
        action = "review_signed_release"
    elif restart_error:
        action = "review_unit_restart_policy"
    emit(
        event="upgrade_check",
        **git,
        release_sequence=release["sequence"],
        release_action=release["action"],
        release_public=release["public_commit"],
        release_source=release["source_commit"],
        release_published=published,
        upgrade_available=upgrade and not starting,
        action=action,
    )
    for fault in faults:
        emit(fault=fault[0], path=fault[1], detail=fault[2] if len(fault) > 2 else "invalid")
    if profile_error is not None:
        emit(status = "hold", reason = "release_runtime_mismatch",
            detail = profile_error, action = "do_not_apply")
        return 2
    if release["action"] == "hold":
        emit(status="hold", reason="release_channel_hold", action="do_not_apply")
        return 2
    if not published:
        emit(status="hold", reason="release_target_not_published", action="do_not_apply")
        return 2
    if faults:
        emit(status="hold", reason="durable_record_requires_review", action="do_not_delete")
        return 2
    if restart_error:
        emit(status = "hold", reason = "unit_restart_75_required",
            action = "review_unit_restart_policy", unit = sup["name"])
        return 2
    if starting:
        emit(status = "recovering", reason = ("snapshot_verification" if resumed else "journal_verification")
            if state.get("rpc") == "ready"
            else "startup_verification", action = "leave_running")
        return 2
    if need is not None:
        emit(
            event="sync_recovery",
            status="required",
            cause=need.cause,
            epoch=need.epoch,
            head=need.head,
            action="apply",
        )
        emit(status="hold", reason="local_journal_check" if replay else "signed_snapshot_required",
            action="apply_upgrade")
        return 2
    if join_pending(values["OCTRA_OPERATOR_ROLE"], state):
        emit(event = "validator_entry", status = "pending", action = "leave_running")
    emit(status="pass", gate="upgrade_diagnostic")
    return 0

def restore_need(values, config):
    return restore_plan(values, config, None)

def restore_plan(values, config, plan, min_epoch=None):
    marked = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
    need = choose_need(marked, plan, values["OCTRA_CHAIN_ID"])
    if need is None:
        return None
    if min_epoch is None:
        recover(config, replace_state=True, plan=need)
    else:
        recover(config, replace_state=True, plan=need, min_epoch=min_epoch)
    return need

def apply(root, sup, values, args, release):
    public, source = release_target(release, args, values)
    state = view(values, sup["pid"], release) if sup["pid"] else {}
    preflight(root, sup, values, args.sudo, head=state.get("head_epoch"))
    marked = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
    require_profile(state, release, allow_missing = marked is not None)
    prior_binary = Path(values["OCTRA_OPERATOR_BINARY"]).expanduser().resolve()
    unit_binary = verify_unit(sup, values)
    repo_head, _, changed = git_update(root, public, source)
    verify_release_tree(root, release)
    if changed:
        reexec(root)
    sync_values = release_sync_values(root, values, release)
    tip = sync_head(sync_values)
    plan = remember_plan(sup["config"], values, state,
        sync_plan(sync_values, state, sync_target(state, tip)))
    marked = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
    need = choose_need(marked, plan, values["OCTRA_CHAIN_ID"])
    replay = plan is None and local_journal(values, marked)
    resumed = need is not None and snapshot_running(values, need, plan)
    if (need is not None and need.cause in ("root", "range")
            and sup["kind"] == "systemd" and not restart_ready(sup.get("restart", {}))):
        raise ValidatorError("unit does not restart on exit 75; node was not stopped; review unit restart policy")
    if (need is None or replay or resumed) and (startup_running(sup, values, state, release, repo_head)
            or ((replay or resumed) and matches(state))):
        emit(event = "upgrade_resume", status = "recovering",
            reason = ("snapshot_verification" if resumed else "journal_verification")
                if state.get("rpc") == "ready" else "startup_verification",
            action = "leave_running", restart = False, build = False)
        return wait_node(root, sup, values, args, release, prior_binary,
            journal = need if replay else None, guard = need if resumed and not replay else None)
    if (need is None and (tip is None or tip["head"] is None) and state.get("process") == "online"
            and state.get("lag") is None):
        raise ValidatorError("network head is unavailable; node was not stopped; retry upgrade")
    if need is None and matches(state):
        worker = Path(values.get("OCTRA_PVAC_VERIFY_WORKER", "")).expanduser()
        if worker.is_file() and sha256_file(worker) == values.get("OCTRA_PVAC_VERIFY_WORKER_HASH"):
            faults = durable_faults(values["OCTRA_DATA_DIR"], state.get("head_epoch"))
            if faults:
                raise ValidatorError("durable record requires review; node was not stopped")
            emit(event="upgrade_resume", action="wait", restart=False, build=False)
            return wait_node(root, sup, values, args, release, prior_binary)
    fresh = (
        sync_wait(sync_values, need, args.wait_seconds, args.interval,
            min_epoch = max(0, (plan.target or 0) - snapshot_lag(sync_values)) if plan is not None else 0)
        if need is not None and not replay
        else None
    )
    call(["sh", str(root / "controls/check.sh")], cwd=root, timeout=300)
    call(["sh", str(root / "controls/build.sh")], cwd=root, timeout=3600)
    if sup["pid"]:
        marked = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
        state = view(values, sup["pid"], release)
        require_profile(state, release,
            allow_missing = marked is not None)
    call([
        "python3",
        str(root / "controls/lib/validator_config.py"),
        "--adopt-network",
        "--config",
        str(sup["config"]),
        "--network",
        str(root / "config/network.env"),
        "--network-sha",
        release["network_sha256"],
    ], cwd=root)
    call([
        "python3",
        str(root / "controls/lib/validator_config.py"),
        "--rebind-runtime",
        "--config",
        str(sup["config"]),
    ], cwd=root)
    values = parse_env(sup["config"])
    call([
        "python3",
        str(root / "controls/lib/validator_guard.py"),
        "--config",
        str(sup["config"]),
    ], cwd=root)
    next_binary = Path(values["OCTRA_OPERATOR_BINARY"]).expanduser().resolve()
    check_built(root, values)
    if unit_binary is not None and next_binary != unit_binary:
        raise ValidatorError("systemd binary path changed during rebind")
    if sup["kind"] == "pm2" and prior_binary.parent != next_binary.parent:
        emit(event = "runtime_path", prior = prior_binary, current = next_binary)
    if sup["pid"]:
        state = view(values, sup["pid"], release)
    faults = durable_faults(values["OCTRA_DATA_DIR"], state.get("head_epoch"))
    if faults:
        for fault in faults:
            emit(fault=fault[0], path=fault[1], action="do_not_delete")
        raise ValidatorError("durable record requires review; node was not stopped")
    if sup["pid"]:
        marked = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
        require_profile(state, release,
            allow_missing = marked is not None)
    stop(sup, args.sudo)
    owners = data_pids(values["OCTRA_DATA_DIR"])
    if owners:
        raise ValidatorError(
            "state directory is active after stop: " + ",".join(map(str, owners))
        )
    before = disk_state(values, sup["config"])
    if replay:
        current = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
        if current != need or before.need != need or not local_journal(values, current):
            raise ValidatorError("local journal changed; state retained; retry upgrade")
        emit(event = "sync_recovery", status = "checking_local_journal",
            epoch = need.epoch, head = need.head, action = "start")
        start(root, sup, sup["config"], args.sudo)
        restored = None
    else:
        restored = restore_cycle(
            root,
            sup,
            values,
            args,
            before,
            plan,
            fresh["required"] if fresh is not None else None,
        )
    if restored is not None:
        emit(
            event="sync_recovery",
            status="restored",
            cause=restored.cause,
            head=restored.head,
            target=restored.target or "none",
        )
    return wait_node(root, sup, values, args, release, prior_binary,
        journal = need if replay else None,
        guard = (read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"]) or restored)
            if restored is not None else None)

def wait_node(root, sup, values, args, release, prior_binary, *, journal = None, guard = None):
    deadline = time.monotonic() + args.wait_seconds
    recovering = journal is not None or guard is not None
    offline_since = None
    phase = None
    phase_started = time.monotonic()
    marker = None
    floor_restored = False
    probe_at = 0.0
    while True:
        entries = pm2_entries()
        names = [(sup["scope"], sup["name"], props(sup["scope"], sup["name"]))] \
            if sup["kind"] == "systemd" else []
        live = current(sup, names, entries)
        state = view(values, live["pid"], release) if live["pid"] else {
            "process": "offline",
            "pid": 0,
        }
        now = tuple(sorted(state.items()))
        if now != marker:
            emit(event="upgrade_wait", **state)
            marker = now
        checking = journal if journal is not None else guard
        if checking is not None:
            current_need = read_need(Path(values["OCTRA_DATA_DIR"]), values["OCTRA_CHAIN_ID"])
            if current_need is None:
                journal = None
                guard = None
                deadline = time.monotonic() + args.wait_seconds
            elif current_need != checking:
                emit(status = "hold", reason = "local_journal_changed" if journal is not None else "sync_marker_changed",
                    action = "keep_records_report_node_log",
                    epoch = current_need.epoch, head = current_need.head, config = sup["config"])
                return 2
            else:
                now = time.monotonic()
                observed = "process_offline" if state.get("process") != "online" else (
                    ("journal_verification" if journal is not None else "snapshot_verification")
                    if state.get("rpc") == "ready" else "startup_verification")
                if observed != phase:
                    phase, phase_started = observed, now
                    emit(status = "recovering", reason = phase,
                        action = "await_supervisor" if phase == "process_offline" else "leave_running")
                until = deadline if phase in ("startup_verification", "snapshot_verification") \
                    else min(deadline, phase_started + 120.0)
                if now >= until:
                    reason = {"process_offline": "node_offline", "startup_verification": "startup_deadline",
                        "journal_verification": "local_journal_unconfirmed",
                        "snapshot_verification": "snapshot_unconfirmed"}[phase]
                    emit(status = "hold", reason = reason,
                        action = "operator_start" if phase == "process_offline" else "keep_records_report_node_log",
                        epoch = checking.epoch, head = checking.head, config = sup["config"])
                    return 2
                time.sleep(min(args.interval, max(0.0, until - now)))
                continue
        if recovering and state.get("process") != "online":
            now = time.monotonic()
            if offline_since is None:
                offline_since = now
            until = min(deadline, offline_since + 120.0)
            if now >= until:
                emit(status = "hold", reason = "node_offline", action = "operator_start", config = sup["config"])
                return 2
            time.sleep(min(args.interval, max(0.0, until - now)))
            continue
        offline_since = None
        if (state.get("lag") is None and state.get("rpc") == "ready"
                and values.get("OCTRA_STATE_SYNC_SOURCES") and time.monotonic() >= probe_at):
            probe_at = time.monotonic() + 30.0
            tip = sync_head(values)
            plan = remember_plan(sup["config"], values, state,
                sync_plan(values, state, sync_target(state, tip)))
            if plan is not None:
                emit(status = "hold", reason = "signed_snapshot_required", action = "apply_upgrade",
                    head = state.get("head_epoch"), target = plan.target, config = sup["config"])
                return 2
        if state.get("lag") == 0:
            try:
                require_profile(state, release, allow_missing = True)
            except ValidatorError as error:
                emit(status = "hold", reason = "release_runtime_mismatch",
                    detail = str(error), action = "do_not_restart",
                    prior_binary = prior_binary, config = sup["config"], **state)
                return 2
        target = (
            floor_target(state)
            if values["OCTRA_OPERATOR_ROLE"] == "validator" and not floor_restored
            and state.get("runtime_match") is True
            else None
        )
        if target is not None:
            install_floor(root, sup, values, args, live, target)
            floor_restored = True
            deadline = time.monotonic() + args.wait_seconds
            marker = None
            time.sleep(args.interval)
            continue
        if ready(values["OCTRA_OPERATOR_ROLE"], state):
            result = "validator_active" if values["OCTRA_OPERATOR_ROLE"] == "validator" else "observer_synced"
            emit(status=result, **state)
            return 0
        if join_pending(values["OCTRA_OPERATOR_ROLE"], state):
            emit(
                status="installed",
                reason = "validator_entry_pending",
                action="leave_running",
                **state,
            )
            return 0
        if time.monotonic() >= deadline:
            status, reason, action, code = deadline_result(
                values["OCTRA_OPERATOR_ROLE"],
                state,
                release,
            )
            emit(
                status=status,
                reason=reason,
                action=action,
                prior_binary=prior_binary,
                config=sup["config"],
                **state,
            )
            return code
        time.sleep(args.interval)

def parser():
    value = argparse.ArgumentParser(prog="upgrade.sh")
    mode = value.add_mutually_exclusive_group()
    mode.add_argument("--apply", action="store_true")
    mode.add_argument("--diagnose", action="store_true")
    mode.add_argument("--repair-cache", action="store_true")
    value.add_argument("--config")
    value.add_argument("--interval", type=float, default=15.0)
    value.add_argument("--public-commit")
    value.add_argument("--root", required=True)
    value.add_argument("--source-commit")
    value.add_argument("--sudo", action="store_true")
    value.add_argument("--unit")
    value.add_argument("--user-unit", action="store_true")
    value.add_argument("--wait-seconds", type=float, default=3600.0)
    return value

def main():
    args = parser().parse_args()
    root = Path(args.root).expanduser().resolve()
    if not (root / "controls").is_dir():
        raise ValidatorError(f"node root is invalid: {root}")
    if args.wait_seconds <= 0 or args.interval <= 0:
        raise ValidatorError("wait values must be positive")
    if args.apply:
        if args.public_commit is not None and not HEX40.fullmatch(args.public_commit):
            raise ValidatorError("--public-commit must be a full commit hash")
        if args.source_commit is not None and not HEX40.fullmatch(args.source_commit):
            raise ValidatorError("--source-commit must be a full commit hash")
    if args.repair_cache:
        marker, marker_source = trusted_release(root, repair=True)
        emit(status="pass", event="release_cache", source=marker_source, sequence=marker["sequence"])
        return 0
    marker, marker_source = trusted_release(root)
    emit(
        event="release_marker",
        source=marker_source,
        sequence=marker["sequence"],
        action=marker["action"],
        public_commit=marker["public_commit"],
        source_commit=marker["source_commit"],
        expires_at=marker["expires_at"],
    )
    scope = "user" if args.user_unit else "system"
    names = [args.unit] if args.unit else unit_names(scope)
    rows = unit_rows(scope, names)
    entries = pm2_entries()
    config = config_path(root, args.config, rows, entries)
    values = parse_env(config)
    sup = supervisor(config, values, rows, entries, unit=args.unit)
    restart_notice(sup)
    if args.apply:
        return apply(root, sup, values, args, marker)
    if values.get("OCTRA_CHAIN_ID") != marker["chain_id"]:
        raise ValidatorError("node chain does not match the release channel")
    return diagnose(root, sup, values, marker)

if __name__ == "__main__":
    try:
        sys.exit(main())
    except (KeyError, OSError, subprocess.CalledProcessError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)