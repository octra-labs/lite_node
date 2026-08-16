# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import re
import shutil
import stat
import subprocess
import sys
import time
from pathlib import Path

from validator_common import ValidatorError
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import sha256_file
from validator_enroll import membership
from validator_process import entry_data
from validator_process import process_alive
from validator_process import wait_stopped
from validator_status import rpc_method
from validator_status import rpc_status

HEX40 = re.compile(r"^[0-9a-f]{40}$")
ACTIVE = frozenset({"launching", "online", "stopping"})
PENDING = re.compile(r"^[0-9]{10}_[0-9]{4}\.pending$")
VOTE = re.compile(r"^[0-9]{20}_[0-9]{8}_[0-9a-f]{64}\.vote$")


def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()), flush=True)


def call(command, cwd=None, check=True, quiet=False, timeout=None):
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
        if not remote or not merge:
            return result
        result["upstream"] = f"{remote}:{merge}"
    except (OSError, subprocess.CalledProcessError):
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
        "--property=Id,LoadState,ActiveState,SubState,MainPID,User,ExecStart,EnvironmentFiles,WorkingDirectory",
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
    candidates = []
    local = root / ".keys/validator/node.env"
    if valid_config(local):
        candidates.append(local.resolve())
    for _, _, info in rows:
        candidates.extend(
            path for path in env_paths(info.get("EnvironmentFiles"))
            if valid_config(path)
        )
    for entry in entries:
        value = entry_env(entry).get("OCTRA_OPERATOR_CONFIG")
        if isinstance(value, str) and valid_config(Path(value)):
            candidates.append(Path(value).expanduser().resolve())
    return choose("node configuration", candidates)


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
            })
    return matches


def supervisor(config, values, rows, entries, unit=None):
    candidates = pm2_sup(config, values, entries)
    candidates.extend(unit_sup(config, values, rows, explicit=unit))
    return choose("node supervisor", candidates)


def inspect_pending(data_dir):
    wal = Path(data_dir) / "wal"
    if not wal.is_dir():
        return []
    faults = []
    for path in sorted(wal.glob("*.pending")):
        if not PENDING.fullmatch(path.name):
            faults.append(("pending_name_invalid", path))
            continue
        try:
            meta = path.lstat()
            if not stat.S_ISREG(meta.st_mode) or meta.st_size <= 0:
                raise ValueError("pending record size is invalid")
            if meta.st_size > 128 * 1024 * 1024:
                raise ValueError("pending record exceeds size limit")
            value = json.loads(path.read_text(encoding="utf-8"))
            if not isinstance(value, dict):
                raise ValueError("pending record is not an object")
            for key in ("epoch_id", "round", "proposal_id", "validator_addr"):
                if key not in value:
                    raise ValueError(f"pending field is missing: {key}")
        except (OSError, UnicodeError, ValueError, json.JSONDecodeError) as error:
            faults.append(("pending_store_unreadable", path, str(error)))
    return faults


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
        except OSError as error:
            faults.append(("vote_record_unreadable", path, str(error)))
    return faults


def peer_head(payload):
    if not isinstance(payload, dict):
        return None
    values = []
    for record in payload.get("peers") or []:
        try:
            values.append(int(record["head_epoch"]))
        except (KeyError, TypeError, ValueError):
            pass
    return max(values) if values else None


def view(values, pid, source):
    status = rpc_status(values["OCTRA_API_PORT"])
    peers = rpc_method(values["OCTRA_API_PORT"], "octra_consensusPeerStates")
    version = rpc_method(values["OCTRA_API_PORT"], "octra_runtimeVersion")
    voting = peers.get("voting") if isinstance(peers, dict) else None
    reason = peers.get("voting_reason") if isinstance(peers, dict) else None
    local = status.get("head_epoch") if isinstance(status, dict) else None
    remote = peer_head(peers)
    wallet = load_wallet(Path(values["OCTRA_DATA_DIR"]) / "wallet.json")
    try:
        member = membership(values, wallet)
    except ValidatorError:
        member = {"active": None, "scheduled": None, "activate_epoch": None}
    expected = values.get("OCTRA_BINARY_HASH")
    live = proc_hash(pid)
    return {
        "pid": pid,
        "process": "online" if process_alive(pid) else "offline",
        "live_binary": live or "unknown",
        "binary_match": live is not None and live == expected,
        "live_source": version.get("source_commit", "unknown")
        if isinstance(version, dict) else "unknown",
        "source_match": isinstance(version, dict) and version.get("source_commit") == source,
        "rpc": "ready" if isinstance(status, dict) else "unavailable",
        "head_epoch": local,
        "peer_epoch": remote,
        "lag": max(0, remote - local) if isinstance(local, int) and isinstance(remote, int) else None,
        "voting": voting,
        "voting_reason": reason,
        "validator_member": member["active"],
        "validator_scheduled": member["scheduled"],
        "activation_epoch": member["activate_epoch"],
    }


def ready(role, state):
    common = (
        state.get("process") == "online"
        and state.get("rpc") == "ready"
        and state.get("binary_match") is True
        and state.get("source_match") is True
        and state.get("lag") == 0
    )
    if role == "validator":
        return common and state.get("validator_member") is True and state.get("voting") is True
    return common and state.get("voting") is False and state.get("voting_reason") == "role"


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


def preflight(root, sup, values, use_sudo):
    owners = data_pids(values["OCTRA_DATA_DIR"])
    if owners and owners != ([sup["pid"]] if sup["pid"] else []):
        raise ValidatorError(
            "data directory is used by unexpected processes: "
            + ",".join(map(str, owners))
        )
    if sup["kind"] == "systemd" and use_sudo:
        call(["sudo", "-n", "true"], quiet=True)
    free = shutil.disk_usage(root).free
    emit(disk_free_gib=round(free / 1024 ** 3, 1))
    if free < 4 * 1024 ** 3:
        raise ValidatorError("less than 4 GiB is free; expand the volume before building")


def git_update(root, public=None, source=None):
    dirty = capture(["git", "status", "--porcelain", "--untracked-files=no"], cwd=root)
    if dirty:
        raise ValidatorError("tracked source tree is dirty")
    call(["git", "pull", "--ff-only"], cwd=root)
    head = capture(["git", "rev-parse", "HEAD"], cwd=root)
    upstream = capture(["git", "rev-parse", "@{u}"], cwd=root)
    if not HEX40.fullmatch(head) or head != upstream:
        raise ValidatorError("local HEAD does not equal the tracked release")
    if public is not None and head != public:
        raise ValidatorError(f"public commit mismatch: {head}")
    source_path = root / "SOURCE_COMMIT"
    actual = source_path.read_text(encoding="utf-8").strip() if source_path.is_file() else ""
    if not HEX40.fullmatch(actual):
        raise ValidatorError("SOURCE_COMMIT is missing or invalid")
    if source is not None and actual != source:
        raise ValidatorError(f"source commit mismatch: {actual or 'missing'}")
    emit(event="upgrade_target", public_commit=head, source_commit=actual)
    return head, actual


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
            "systemd ExecStart does not select the candidate binary: " + str(sup["exec"])
        )
    return binary


def diagnose(root, sup, values, source):
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
        state = view(values, pid, source)
        emit(**state)
    else:
        state = {"process": "offline", "source_match": False, "binary_match": False}
    release = git_release(root)
    remote_known = release["upstream_head"] != "unknown"
    upgrade = (
        (remote_known and release["repo_head"] != release["upstream_head"])
        or state.get("source_match") is not True
        or state.get("binary_match") is not True
    )
    action = "upgrade" if upgrade else ("none" if remote_known else "verify_remote")
    emit(
        event="upgrade_check",
        **release,
        release_source=source,
        upgrade_available=upgrade,
        action=action,
    )
    faults = inspect_pending(values["OCTRA_DATA_DIR"]) + inspect_votes(values["OCTRA_DATA_DIR"])
    for fault in faults:
        emit(fault=fault[0], path=fault[1], detail=fault[2] if len(fault) > 2 else "invalid")
    if faults:
        emit(status="hold", reason="durable_record_requires_review", action="do_not_delete")
        return 2
    emit(status="pass", gate="upgrade_diagnostic")
    return 0


def apply(root, sup, values, args):
    preflight(root, sup, values, args.sudo)
    prior_binary = Path(values["OCTRA_OPERATOR_BINARY"]).expanduser().resolve()
    unit_binary = verify_unit(sup, values)
    _, source = git_update(root, args.public_commit, args.source_commit)
    call(["sh", str(root / "controls/check.sh")], cwd=root)
    call(["sh", str(root / "controls/build.sh")], cwd=root)
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
    rebound_binary = Path(values["OCTRA_OPERATOR_BINARY"]).expanduser().resolve()
    if unit_binary is not None and rebound_binary != unit_binary:
        raise ValidatorError("systemd candidate binary path changed during rebind")
    if sup["kind"] == "pm2" and prior_binary.parent != rebound_binary.parent:
        emit(event="candidate_path", prior=prior_binary, current=rebound_binary)
    faults = inspect_pending(values["OCTRA_DATA_DIR"]) + inspect_votes(values["OCTRA_DATA_DIR"])
    unsafe = [fault for fault in faults if fault[0].startswith("pending_")]
    if unsafe:
        for fault in unsafe:
            emit(fault=fault[0], path=fault[1], action="do_not_delete")
        raise ValidatorError("pending WAL is unreadable; node was not stopped")
    stop(sup, args.sudo)
    start(root, sup, sup["config"], args.sudo)
    deadline = time.monotonic() + args.wait_seconds
    marker = None
    while True:
        entries = pm2_entries()
        names = [(sup["scope"], sup["name"], props(sup["scope"], sup["name"]))] \
            if sup["kind"] == "systemd" else []
        live = current(sup, names, entries)
        state = view(values, live["pid"], source) if live["pid"] else {
            "process": "offline",
            "pid": 0,
        }
        now = tuple(sorted(state.items()))
        if now != marker:
            emit(event="upgrade_wait", **state)
            marker = now
        if ready(values["OCTRA_OPERATOR_ROLE"], state):
            result = "validator_active" if values["OCTRA_OPERATOR_ROLE"] == "validator" else "observer_synced"
            emit(status=result, **state)
            return 0
        if time.monotonic() >= deadline:
            emit(
                status="pending",
                reason="health_deadline",
                action="do_not_restart",
                prior_binary=prior_binary,
                config=sup["config"],
                **state,
            )
            return 2
        time.sleep(args.interval)


def parser():
    value = argparse.ArgumentParser(prog="upgrade.sh")
    mode = value.add_mutually_exclusive_group()
    mode.add_argument("--apply", action="store_true")
    mode.add_argument("--diagnose", action="store_true")
    value.add_argument("--config")
    value.add_argument("--interval", type=float, default=15.0)
    value.add_argument("--public-commit")
    value.add_argument("--root", required=True)
    value.add_argument("--source-commit")
    value.add_argument("--sudo", action="store_true")
    value.add_argument("--unit")
    value.add_argument("--user-unit", action="store_true")
    value.add_argument("--wait-seconds", type=float, default=600.0)
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
    scope = "user" if args.user_unit else "system"
    names = [args.unit] if args.unit else unit_names(scope)
    rows = unit_rows(scope, names)
    entries = pm2_entries()
    config = config_path(root, args.config, rows, entries)
    values = parse_env(config)
    sup = supervisor(config, values, rows, entries, unit=args.unit)
    source_path = root / "SOURCE_COMMIT"
    source = source_path.read_text(encoding="utf-8").strip() if source_path.is_file() else "unknown"
    if args.apply:
        return apply(root, sup, values, args)
    return diagnose(root, sup, values, source)


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (KeyError, OSError, subprocess.CalledProcessError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)