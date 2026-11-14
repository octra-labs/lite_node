# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

from validator_common import ValidatorError
from validator_common import parse_env
from validator_common import sha256_file

ACTIVE = frozenset({"launching", "online", "stopping"})

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def entry_data(entry):
    meta = entry.get("pm2_env")
    if not isinstance(meta, dict):
        return None
    direct = meta.get("OCTRA_DATA_DIR")
    nested = meta.get("env")
    inherited = nested.get("OCTRA_DATA_DIR") if isinstance(nested, dict) else None
    if isinstance(direct, str) and isinstance(inherited, str):
        return direct if direct == inherited else None
    if isinstance(direct, str):
        return direct
    return inherited if isinstance(inherited, str) else None

def process_plan(entries, name, data_dir):
    owned = [
        entry
        for entry in entries
        if entry_data(entry) == data_dir
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
            or entry_data(entry) == data_dir
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
        and entry_data(entry) == data_dir
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
        or entry_data(entry) == data_dir
    } - {None})

def data_pids(data_dir, root=Path("/proc")):
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

def worker_owned(pid, parent, values, root=Path("/proc")):
    try:
        worker = Path(values["OCTRA_PVAC_VERIFY_WORKER"]).expanduser()
        digest = values["OCTRA_PVAC_VERIFY_WORKER_HASH"]
        if not worker.is_absolute() or len(digest) != 64:
            return False
        if any(char not in "0123456789abcdef" for char in digest):
            return False
        process = root / str(pid)
        before = (process / "stat").read_text().rsplit(")", 1)[1].split()
        if int(before[1]) != parent:
            return False
        expected = worker.resolve(strict=True)
        executable = process / "exe"
        if executable.resolve(strict=True) != expected:
            return False
        actual = sha256_file(executable)
        after = (process / "stat").read_text().rsplit(")", 1)[1].split()
        return (
            actual == digest
            and (before[1], before[19]) == (after[1], after[19])
            and executable.resolve(strict=True) == expected
        )
    except (KeyError, IndexError, OSError, ValueError, TypeError, RuntimeError):
        return False

def node_owners(pids, pid, values, root=Path("/proc")):
    return isinstance(pid, int) and pid > 0 and pid in pids and all(
        owner == pid or worker_owned(owner, pid, values, root=root)
        or reader_owned(owner, pid, values, root = root)
        for owner in pids
    )

def reader_owned(pid, parent, values, root = Path("/proc")):
    try:
        process, node = root / str(pid), root / str(parent)
        before = (process / "stat").read_text().rsplit(")", 1)[1].split()
        parent_before = (node / "stat").read_text().rsplit(")", 1)[1].split()
        if int(before[1]) != parent or int(before[19]) < int(parent_before[19]):
            return False
        if process.stat().st_uid != node.stat().st_uid:
            return False
        raw = (process / "cmdline").read_bytes()
        if not raw.endswith(b"\0"):
            return False
        args = raw[:-1].decode("utf-8").split("\0")
        data = Path(values["OCTRA_DATA_DIR"]).resolve()
        if len(args) != 8 or args[:6] != ["/proc/self/exe", "--sync-head", str(parent),
                values["OCTRA_CHAIN_ID"], str(data), str(data / "chaindata")]:
            return False
        epoch, offset = int(args[6]), int(args[7])
        if args[6:] != [str(epoch), str(offset)] or not (
                (epoch, offset) == (-1, -1) or 0 <= epoch < 2 ** 62 and 0 <= offset < 2 ** 62):
            return False
        executable, parent_exe = process / "exe", node / "exe"
        child_file, parent_file = executable.stat(), parent_exe.stat()
        file_id = child_file.st_dev, child_file.st_ino
        if file_id != (parent_file.st_dev, parent_file.st_ino):
            return False
        if sha256_file(executable) != values.get("OCTRA_BINARY_HASH"):
            return False
        after = (process / "stat").read_text().rsplit(")", 1)[1].split()
        parent_after = (node / "stat").read_text().rsplit(")", 1)[1].split()
        child_file, parent_file = executable.stat(), parent_exe.stat()
        return ((before[1], before[19]) == (after[1], after[19])
            and parent_before[19] == parent_after[19]
            and file_id == (child_file.st_dev, child_file.st_ino)
            and file_id == (parent_file.st_dev, parent_file.st_ino))
    except (KeyError, IndexError, OSError, ValueError, TypeError, RuntimeError):
        return False

def pm2_entries(required=True):
    if shutil.which("pm2") is None:
        if required:
            raise ValidatorError("cannot inspect PM2 process table")
        return []
    try:
        result = subprocess.run(
            ["pm2", "jlist", "--silent"],
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
    for owner in names:
        subprocess.run(
            ["pm2", "delete", owner],
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