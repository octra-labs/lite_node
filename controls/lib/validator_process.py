# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import subprocess
import sys
import time

from validator_common import ValidatorError
from validator_common import parse_env

ACTIVE = frozenset({"launching", "online", "stopping"})

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def process_plan(entries, name, data_dir):
    owned = [
        entry
        for entry in entries
        if entry.get("pm2_env", {}).get("OCTRA_DATA_DIR") == data_dir
    ]
    conflicts = [
        entry.get("name", "unknown")
        for entry in owned
        if entry.get("name") != name
        and entry.get("pm2_env", {}).get("status") in ACTIVE
    ]
    if conflicts:
        raise ValidatorError("data directory is owned by " + ",".join(sorted(conflicts)))
    return sorted({
        entry.get("name")
        for entry in entries
        if entry.get("name")
        and (
            entry.get("name") == name
            or entry.get("pm2_env", {}).get("OCTRA_DATA_DIR") == data_dir
        )
    })

def process_pids(entries, names):
    selected = set(names)
    return sorted({
        entry.get("pid")
        for entry in entries
        if entry.get("name") in selected
        and isinstance(entry.get("pid"), int)
        and entry.get("pid") > 0
    })

def active_data_owners(entries, data_dir):
    return sorted({
        entry.get("name")
        for entry in entries
        if entry.get("name")
        and entry.get("pm2_env", {}).get("OCTRA_DATA_DIR") == data_dir
        and entry.get("pm2_env", {}).get("status") in ACTIVE
    })

def process_alive(pid):
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True

def wait_stopped(pids, timeout=90.0, poll=0.1):
    deadline = time.monotonic() + timeout
    remaining = [pid for pid in pids if process_alive(pid)]
    while remaining and time.monotonic() < deadline:
        time.sleep(poll)
        remaining = [pid for pid in remaining if process_alive(pid)]
    if remaining:
        raise ValidatorError(
            "previous node process did not stop: "
            + ",".join(str(pid) for pid in remaining)
        )

def remaining_owners(entries, names, data_dir):
    selected = set(names)
    return sorted({
        entry.get("name")
        for entry in entries
        if entry.get("name") in selected
        or entry.get("pm2_env", {}).get("OCTRA_DATA_DIR") == data_dir
    } - {None})

def pm2_entries():
    try:
        result = subprocess.run(
            ["pm2", "jlist"],
            check=True,
            capture_output=True,
            text=True,
        )
        entries = json.loads(result.stdout)
    except Exception as error:
        raise ValidatorError("cannot inspect PM2 process table") from error
    if not isinstance(entries, list):
        raise ValidatorError("invalid PM2 process table")
    return entries

def main():
    parser = argparse.ArgumentParser(prog="validator_process.py")
    parser.add_argument("--config", required=True)
    args = parser.parse_args()
    values = parse_env(args.config)
    name = values["OCTRA_OPERATOR_PM2_NAME"]
    data_dir = values["OCTRA_DATA_DIR"]
    entries = pm2_entries()
    names = process_plan(entries, name, data_dir)
    pids = process_pids(entries, names)
    for stale in names:
        subprocess.run(
            ["pm2", "delete", stale],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    wait_stopped(pids)
    remaining = remaining_owners(pm2_entries(), names, data_dir)
    if remaining:
        raise ValidatorError(
            "PM2 still owns data directory: " + ",".join(remaining)
        )
    emit(status="ready", removed=len(names), name=name)

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)