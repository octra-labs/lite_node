# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import re
import signal
import subprocess
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

from validator_common import ValidatorError
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import private_mode
from validator_common import rpc_url
from validator_common import state_ready
from validator_common import validate_checkpoint
from validator_enroll import membership
from validator_process import entry_data
from validator_process import pm2_entries
from validator_status import rpc_method
from validator_status import rpc_status

ROUND_AGE_SECONDS = 30.0
ROUND_GAP = 2
ROUND_LOG_LIMIT = 320
WITNESS_BYTES = 2048


def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))


def resolved(value):
    return Path(value).expanduser().resolve()


def positive_seconds(value):
    try:
        seconds = int(value)
    except (TypeError, ValueError) as error:
        raise ValidatorError("wait seconds is invalid") from error
    if seconds < 30 or seconds > 3600:
        raise ValidatorError("wait seconds is outside 30..3600")
    return seconds


def configured_data(values, supplied=None):
    data_dir = resolved(supplied or values["OCTRA_DATA_DIR"])
    configured = resolved(values["OCTRA_DATA_DIR"])
    if data_dir != configured:
        raise ValidatorError("data directory does not match node configuration")
    return data_dir


def require_root(root):
    wanted = [
        root / "controls/run.sh",
        root / "controls/lib/validator_config.py",
        root / "config/network.env",
    ]
    missing = [str(path) for path in wanted if not path.is_file()]
    if missing:
        raise ValidatorError("release package is incomplete: " + ",".join(missing))
    node_binary(root)


def node_binary(root):
    paths = [
        root / "artifacts/octra_node.exe",
        root / "_build/default/bin/octra_node.exe",
    ]
    for path in paths:
        if path.is_file():
            return path
    raise ValidatorError("node binary is missing")


def floor_binary(root):
    paths = [
        root / "artifacts/vote_floor.exe",
        root / "_build/default/bin/vote_floor.exe",
    ]
    for path in paths:
        if path.is_file():
            return path
    raise ValidatorError("vote floor tool is missing")


def place_floor(root, values, wallet, data_dir, head_epoch, round_id, sync=None, check=False):
    command = [
        str(floor_binary(root)),
        "--data-dir",
        str(data_dir),
        "--chain-id",
        values["OCTRA_CHAIN_ID"],
        "--validator",
        wallet["address"],
        "--validator-pub",
        wallet["pub"],
        "--head-epoch",
        str(head_epoch),
    ]
    if sync is None:
        command.extend(["--round", str(round_id)])
    else:
        command.extend(["--round-sync", sync, "--round-min", str(round_id)])
    if check:
        command.append("--check")
    subprocess.run(command, check=True)


def data_pids(data_dir):
    root = Path("/proc")
    if not root.is_dir():
        raise ValidatorError("Linux process inspection is unavailable")
    expected = b"OCTRA_DATA_DIR=" + os.fsencode(str(data_dir))
    found = []
    for entry in root.iterdir():
        if not entry.name.isdigit():
            continue
        try:
            raw = (entry / "environ").read_bytes()
        except OSError:
            continue
        if expected in raw.split(b"\x00"):
            found.append(int(entry.name))
    return sorted(found)


def node_paused(pid):
    try:
        lines = (Path("/proc") / str(pid) / "status").read_text(
            encoding="utf-8"
        ).splitlines()
    except OSError as error:
        raise ValidatorError("cannot inspect paused node") from error
    return any(line.startswith("State:") and "\tT" in line for line in lines)


def pause_node(pid):
    try:
        os.kill(pid, signal.SIGSTOP)
    except OSError as error:
        raise ValidatorError("cannot pause validator node") from error
    deadline = time.monotonic() + 5.0
    while time.monotonic() < deadline:
        if node_paused(pid):
            return
        time.sleep(0.05)
    resume_node(pid)
    raise ValidatorError("validator node did not pause")


def resume_node(pid):
    try:
        os.kill(pid, signal.SIGCONT)
    except ProcessLookupError:
        return
    except OSError as error:
        raise ValidatorError("cannot resume validator node") from error


def online_entry(entries, name, data_dir):
    matched = [
        entry
        for entry in entries
        if entry.get("name") == name
        and entry_data(entry) == str(data_dir)
    ]
    if len(matched) != 1:
        raise ValidatorError("expected exactly one PM2 node process")
    entry = matched[0]
    if entry.get("pm2_env", {}).get("status") != "online":
        raise ValidatorError("node process is not online")
    pid = entry.get("pid")
    if not isinstance(pid, int) or pid < 1:
        raise ValidatorError("node process has no valid pid")
    return pid


def running_binary(pid):
    path = Path("/proc") / str(pid) / "exe"
    try:
        raw = os.readlink(path)
    except OSError as error:
        raise ValidatorError("cannot inspect running node binary") from error
    if raw.endswith(" (deleted)"):
        raw = raw[: -len(" (deleted)")]
    actual = Path(raw)
    if actual.name != "octra_node.exe":
        raise ValidatorError("running process is not an octra node")
    return actual


def number(payload, key):
    try:
        value = int(payload[key])
    except (KeyError, TypeError, ValueError) as error:
        raise ValidatorError("node status has no valid " + key) from error
    if value < 0:
        raise ValidatorError("node status has negative " + key)
    return value


def peer_head(payload):
    if not isinstance(payload, dict):
        return None
    records = payload.get("peers")
    if not isinstance(records, list):
        return None
    heads = []
    for record in records:
        if not isinstance(record, dict):
            continue
        try:
            head = int(record["head_epoch"])
        except (KeyError, TypeError, ValueError):
            continue
        if head >= 0:
            heads.append(head)
    return max(heads) if heads else None


def round_value(row, key):
    if not isinstance(row, dict):
        return None
    try:
        value = int(row[key])
    except (KeyError, TypeError, ValueError):
        return None
    return value if value >= 0 else None


def round_alignment(payload):
    if not isinstance(payload, dict):
        return {"state": "waiting_round"}
    local = payload.get("round_state")
    epoch = round_value(local, "epoch_id")
    local_round = round_value(local, "round")
    rows = payload.get("round_peers")
    agreed = payload.get("round_agreed")
    if (
        epoch is None
        or local_round is None
        or not isinstance(rows, list)
        or not isinstance(agreed, bool)
    ):
        return {"state": "waiting_round"}
    peer_rounds = []
    for row in rows:
        peer_epoch = round_value(row, "epoch_id")
        peer_round = round_value(row, "round")
        try:
            age = float(row.get("age_sec"))
        except (AttributeError, TypeError, ValueError):
            continue
        if peer_epoch == epoch and peer_round is not None and 0 <= age <= ROUND_AGE_SECONDS:
            peer_rounds.append(peer_round)
    if not peer_rounds:
        if agreed:
            return {
                "state": "round_aligned",
                "round": local_round,
                "peer_round": local_round,
                "round_epoch": epoch,
                "round_peers": 0,
                "round_agreed": True,
            }
        return {
            "state": "waiting_round",
            "round": local_round,
            "round_epoch": epoch,
        }
    peer_round = max(peer_rounds)
    if peer_round > local_round + ROUND_GAP:
        return {
            "state": "round_lagging",
            "round": local_round,
            "peer_round": peer_round,
            "round_epoch": epoch,
            "round_peers": len(peer_rounds),
        }
    if not agreed:
        return {
            "state": "round_unconfirmed",
            "round": local_round,
            "peer_round": peer_round,
            "round_epoch": epoch,
            "round_peers": len(peer_rounds),
            "round_agreed": False,
        }
    return {
        "state": "round_aligned",
        "round": local_round,
        "peer_round": peer_round,
        "round_epoch": epoch,
        "round_peers": len(peer_rounds),
        "round_agreed": True,
    }


def sync_state(local_head, remote_head, round_status):
    if remote_head is None:
        return "waiting_peers"
    if remote_head > local_head + 1:
        return "catching_up"
    if round_status["state"] in {"round_lagging", "waiting_round"}:
        return round_status["state"]
    return "synced"


def vote_state(payload):
    if not isinstance(payload, dict):
        return None, None
    voting = payload.get("voting")
    reason = payload.get("voting_reason")
    if not isinstance(voting, bool):
        return None, None
    return voting, reason if isinstance(reason, str) else None


def launch(root, config):
    env = os.environ.copy()
    env["OCTRA_OPERATOR_CONFIG"] = str(config)
    subprocess.run(
        ["sh", str(root / "controls/run.sh"), "--rebind-runtime"],
        cwd=root,
        env=env,
        check=True,
    )


def stop(root, config):
    subprocess.run(
        [
            "python3",
            str(root / "controls/lib/validator_process.py"),
            "--config",
            str(config),
        ],
        cwd=root,
        check=True,
    )


def snapshot(values, data_dir):
    pid = online_entry(
        pm2_entries(),
        values["OCTRA_OPERATOR_PM2_NAME"],
        data_dir,
    )
    pids = data_pids(data_dir)
    if pids != [pid]:
        raise ValidatorError(
            "data directory is used by unexpected processes: "
            + ",".join(str(value) for value in pids)
        )
    running = running_binary(pid)
    status = rpc_status(values["OCTRA_API_PORT"])
    if not isinstance(status, dict):
        return {"state": "waiting_rpc", "pid": pid, "running_binary": running}
    local_head = number(status, "head_epoch")
    peers = rpc_method(values["OCTRA_API_PORT"], "octra_consensusPeerStates")
    remote_head = peer_head(peers)
    voting, voting_reason = vote_state(peers)
    round_status = round_alignment(peers)
    state = sync_state(local_head, remote_head, round_status)
    if state == "waiting_peers":
        return {
            "pid": pid,
            "running_binary": running,
            "local_head": local_head,
            "voting": voting,
            "voting_reason": voting_reason,
            **round_status,
            "state": "waiting_peers",
        }
    if state != "synced":
        return {
            "pid": pid,
            "running_binary": running,
            "local_head": local_head,
            "remote_head": remote_head,
            "lag": max(0, remote_head - local_head),
            "voting": voting,
            "voting_reason": voting_reason,
            **round_status,
            "state": state,
        }
    return {
        "pid": pid,
        "running_binary": running,
        "local_head": local_head,
        "remote_head": remote_head,
        "lag": max(0, remote_head - local_head),
        "voting": voting,
        "voting_reason": voting_reason,
        **round_status,
        "state": "synced",
    }


def report_wait(snapshot):
    fields = {"event": "rejoin_wait", **snapshot}
    emit(**fields)


def report_ready(values, wallet, snapshot):
    try:
        member = membership(values, wallet)
    except ValidatorError as error:
        emit(
            status="online_membership_unknown",
            role=values["OCTRA_OPERATOR_ROLE"],
            address=wallet["address"],
            pid=snapshot["pid"],
            head_epoch=snapshot["local_head"],
            peer_epoch=snapshot["remote_head"],
            detail=str(error),
        )
        return 2
    common = {
        "role": values["OCTRA_OPERATOR_ROLE"],
        "address": wallet["address"],
        "pid": snapshot["pid"],
        "head_epoch": snapshot["local_head"],
        "peer_epoch": snapshot["remote_head"],
        "round": snapshot.get("round"),
        "peer_round": snapshot.get("peer_round"),
        "round_peers": snapshot.get("round_peers"),
        "round_agreed": snapshot.get("round_agreed"),
        "validator_active": str(member["active"]).lower(),
        "validator_scheduled": str(member["scheduled"]).lower(),
        "activation_epoch": member["activate_epoch"],
        "voting": (
            "enabled"
            if snapshot.get("voting") is True
            else "disabled"
            if snapshot.get("voting") is False
            else "unknown"
        ),
    }
    if values["OCTRA_OPERATOR_ROLE"] == "observer":
        emit(status="observer_synced", **common)
        return 0
    if member["active"] and snapshot.get("voting") is True:
        emit(status="validator_active", **common)
        return 0
    if member["active"]:
        reason = snapshot.get("voting_reason")
        if isinstance(reason, str):
            emit(status="voting_disabled", reason=reason, **common)
            return 4
        return None
    if member["scheduled"]:
        emit(status="validator_scheduled", **common)
        return 2
    emit(status="online_not_active", **common)
    return 2


def captured_round(snapshot, head_epoch):
    epoch = snapshot.get("round_epoch")
    round_id = snapshot.get("round")
    if not isinstance(epoch, int) or not isinstance(round_id, int):
        raise ValidatorError("running node did not report a consensus round")
    if epoch != head_epoch + 1:
        raise ValidatorError("running node round does not match finalized head")
    peer_round = snapshot.get("peer_round")
    if isinstance(peer_round, int) and peer_round >= 0:
        return max(round_id, peer_round) + 1
    return round_id + 1


def log_round(name, epoch):
    result = subprocess.run(
        ["pm2", "logs", name, "--lines", str(ROUND_LOG_LIMIT), "--nostream"],
        check=False,
        capture_output=True,
        text=True,
        timeout=15,
    )
    if result.returncode != 0:
        raise ValidatorError("cannot read validator round log")
    escaped = re.escape(str(epoch))
    patterns = [
        rf"event = round_skip height = {escaped} old_round = [0-9]+ new_round = ([0-9]+)",
        rf"event = request_proposal epoch = {escaped} round = ([0-9]+)",
        rf"event = replay_deferred_proposal epoch = {escaped} round = ([0-9]+)",
        rf"event = make_proposal epoch = {escaped} round = ([0-9]+)",
    ]
    rounds = []
    for pattern in patterns:
        rounds.extend(int(value) for value in re.findall(pattern, result.stdout))
    if not rounds:
        raise ValidatorError("validator round is unavailable in local log")
    return max(rounds)


def rpc_call(url, method, params):
    body = json.dumps({
        "jsonrpc": "2.0",
        "id": 1,
        "method": method,
        "params": params,
    }).encode("utf-8")
    request = urllib.request.Request(
        rpc_url(url),
        data=body,
        headers={
            "Accept": "application/json",
            "Content-Type": "application/json",
            "User-Agent": "octra-validator-rejoin/1",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=5) as response:
            payload = json.loads(response.read())
    except (OSError, ValueError, urllib.error.URLError) as error:
        raise ValidatorError("signed round witness is unavailable") from error
    result = payload.get("result") if isinstance(payload, dict) else None
    if not isinstance(result, dict):
        raise ValidatorError("signed round witness is unavailable")
    return result


def signed_round(values, wallet):
    try:
        url = values["OCTRA_OPERATOR_RPC_URL"]
    except KeyError as error:
        raise ValidatorError("validator RPC URL is unavailable") from error
    result = rpc_call(url, "octra_roundWitness", [wallet["address"]])
    wire = result.get("round_sync")
    if not isinstance(wire, str) or not wire or len(wire) > WITNESS_BYTES:
        raise ValidatorError("signed round witness is invalid")
    return wire


def floor_source(snapshot, values, wallet, head_epoch, legacy):
    try:
        return captured_round(snapshot, head_epoch), None
    except ValidatorError as error:
        if str(error) != "running node did not report a consensus round":
            raise
        if not legacy:
            raise ValidatorError("legacy handoff requires --legacy-handoff") from error
        return None, signed_round(values, wallet)


def legacy_handoff(root, config, values, wallet, data_dir, head_epoch, pid, sync):
    pause_node(pid)
    try:
        round_id = log_round(
            values["OCTRA_OPERATOR_PM2_NAME"],
            head_epoch + 1,
        )
        place_floor(
            root,
            values,
            wallet,
            data_dir,
            head_epoch,
            round_id,
            sync=sync,
            check=True,
        )
        stop(root, config)
        if data_pids(data_dir):
            raise ValidatorError("node process remained after vote floor preparation")
        return round_id
    except BaseException:
        resume_node(pid)
        raise


def rejoin(root, config, supplied_data, wait_seconds, legacy=False, check=False):
    require_root(root)
    private_mode(config)
    values = parse_env(config)
    data_dir = configured_data(values, supplied_data)
    if not state_ready(data_dir):
        emit(
            status="recovery_required",
            data_dir=data_dir,
            command=f"sh {root}/controls/recover.sh --replace-state",
        )
        return 3
    try:
        head = validate_checkpoint(data_dir, values, allow_progress=True)
        wallet = load_wallet(data_dir / "wallet.json")
    except ValidatorError as error:
        emit(
            status="recovery_required",
            data_dir=data_dir,
            detail=str(error),
            command=f"sh {root}/controls/recover.sh --replace-state",
        )
        return 3
    prior = data_pids(data_dir)
    if prior:
        entries = pm2_entries()
        known = {
            entry.get("pid")
            for entry in entries
            if entry_data(entry) == str(data_dir)
        }
        if any(pid not in known for pid in prior):
            raise ValidatorError("data directory is used outside PM2")
    if values["OCTRA_OPERATOR_ROLE"] == "validator":
        if not prior:
            raise ValidatorError("running validator is required to prepare the vote journal")
        before = snapshot(values, data_dir)
        round_id, sync = floor_source(
            before,
            values,
            wallet,
            head["epoch"],
            legacy,
        )
        if sync is None:
            place_floor(
                root,
                values,
                wallet,
                data_dir,
                head["epoch"],
                round_id,
                check=True,
            )
        elif check:
            round_id = log_round(
                values["OCTRA_OPERATOR_PM2_NAME"],
                head["epoch"] + 1,
            )
            place_floor(
                root,
                values,
                wallet,
                data_dir,
                head["epoch"],
                round_id,
                sync=sync,
                check=True,
            )
        if check:
            emit(
                status="rejoin_ready",
                role=values["OCTRA_OPERATOR_ROLE"],
                address=wallet["address"],
                data_dir=data_dir,
                head_epoch=head["epoch"],
                round=round_id,
                handoff="legacy" if sync is not None else "current",
            )
            return 0
        if sync is None:
            stop(root, config)
            if data_pids(data_dir):
                raise ValidatorError("node process remained after vote floor preparation")
        else:
            round_id = legacy_handoff(
                root,
                config,
                values,
                wallet,
                data_dir,
                head["epoch"],
                before["pid"],
                sync,
            )
        place_floor(
            root,
            values,
            wallet,
            data_dir,
            head["epoch"],
            round_id,
            sync=sync,
        )
    elif check:
        emit(
            status="rejoin_ready",
            role=values["OCTRA_OPERATOR_ROLE"],
            address=wallet["address"],
            data_dir=data_dir,
            head_epoch=head["epoch"],
            handoff="observer",
        )
        return 0
    elif prior:
        stop(root, config)
    emit(
        event="rejoin",
        status="starting",
        role=values["OCTRA_OPERATOR_ROLE"],
        address=wallet["address"],
        data_dir=data_dir,
    )
    launch(root, config)
    values = parse_env(config)
    deadline = time.monotonic() + wait_seconds
    prior_report = None
    while True:
        try:
            current = snapshot(values, data_dir)
        except ValidatorError as error:
            current = {"state": "waiting_process", "detail": str(error)}
        marker = tuple(sorted(current.items()))
        if marker != prior_report:
            report_wait(current)
            prior_report = marker
        if current.get("state") == "synced":
            result = report_ready(values, wallet, current)
            if result is not None:
                return result
        if time.monotonic() >= deadline:
            state = current.get("state")
            if state == "round_lagging":
                status = "round_lagging"
            elif state == "round_unconfirmed":
                status = "round_unconfirmed"
            elif state == "waiting_round":
                status = "round_unobserved"
            elif state == "synced":
                status = "not_ready_to_vote"
            else:
                status = "not_synced"
            emit(
                status=status,
                role=values["OCTRA_OPERATOR_ROLE"],
                address=wallet["address"],
                data_dir=data_dir,
                detail=current.get("voting_reason") or current.get("state", "unknown"),
            )
            return 2
        time.sleep(2.0)


def main():
    parser = argparse.ArgumentParser(prog="rejoin.sh")
    parser.add_argument("--root", required=True)
    parser.add_argument("--config")
    parser.add_argument("--data-dir")
    parser.add_argument("--wait-seconds", default="600")
    parser.add_argument("--legacy-handoff", action="store_true")
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    root = resolved(args.root)
    config = resolved(args.config or root / ".keys/validator/node.env")
    if not config.is_file():
        raise ValidatorError("node configuration is missing: " + str(config))
    return rejoin(
        root,
        config,
        args.data_dir,
        positive_seconds(args.wait_seconds),
        args.legacy_handoff,
        args.check,
    )


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, subprocess.CalledProcessError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)