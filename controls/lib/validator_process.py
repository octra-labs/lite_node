import argparse
import json
import subprocess
import sys

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
        for entry in owned
        if entry.get("name")
    } | {name})

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
    names = process_plan(pm2_entries(), name, data_dir)
    for stale in names:
        subprocess.run(
            ["pm2", "delete", stale],
            check=False,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
    emit(status="ready", removed=len(names), name=name)

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)
