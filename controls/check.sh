# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

if [ "$#" -eq 0 ]; then
  TESTS=0
elif [ "$#" -eq 1 ] && [ "$1" = --tests ]; then
  TESTS=1
else
  printf 'status = refused reason = arguments usage = check.sh_[--tests]\n' >&2
  exit 2
fi

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cd "$ROOT"

if [ ! -f SOURCE_COMMIT ]; then
  printf 'status = refused reason = source_commit_missing\n' >&2
  exit 1
fi

SOURCE_COMMIT=$(sed -n '1p' SOURCE_COMMIT)

if ! printf '%s\n' "$SOURCE_COMMIT" | LC_ALL=C grep -Eq '^[0-9a-f]{40}$'; then
  printf 'status = refused reason = source_commit_invalid\n' >&2
  exit 1
fi

if ! command -v python3 >/dev/null 2>&1; then
  printf 'status = refused reason = python3_missing\n' >&2
  exit 1
fi

PYTHONDONTWRITEBYTECODE=1 python3 - <<'PY'
import hashlib
import re
import sys
from pathlib import Path

def refuse(reason, path):
    print(f"status = refused reason = {reason} path = {path}", file = sys.stderr)
    raise SystemExit(1)

manifest = Path("MANIFEST.sha256")
try:
    lines = manifest.read_text(encoding = "utf-8").splitlines()
except FileNotFoundError:
    refuse("manifest_missing", manifest)
except (OSError, UnicodeError):
    refuse("manifest_unreadable", manifest)
if not lines:
    refuse("manifest_invalid", manifest)
for line in lines:
    entry = re.fullmatch(r"([0-9a-f]{64})  (.+)", line)
    if entry is None:
        refuse("manifest_invalid", manifest)
    expected, name = entry.groups()
    path = Path(name)
    if path.is_absolute() or ".." in path.parts:
        refuse("manifest_invalid", manifest)
    digest = hashlib.sha256()
    try:
        with path.open("rb") as source:
            for chunk in iter(lambda: source.read(1024 * 1024), b""):
                digest.update(chunk)
    except FileNotFoundError:
        refuse("manifest_missing", path)
    except OSError:
        refuse("manifest_unreadable", path)
    if digest.hexdigest() != expected:
        refuse("manifest_mismatch", path)
for name in ("nodes.config", "octra_node.opam.locked"):
    if not Path(name).is_file():
        refuse("file_missing", name)
PY

sh -n controls/check.sh
sh -n controls/config_val.sh
sh -n controls/enroll.sh
sh -n controls/install.sh
sh -n controls/upgrade.sh
sh -n controls/recover.sh
sh -n controls/rejoin.sh
sh -n controls/run.sh
sh -n controls/build.sh
sh -n controls/stat.sh
sh -n controls/storage.sh
sh -n controls/stop.sh
sh -n controls/proof_gate.sh

PYTHONDONTWRITEBYTECODE=1 python3 test/python_check.py

if ! PYTHONDONTWRITEBYTECODE=1 python3 -c 'import nacl' >/dev/null 2>&1; then
  printf 'status = refused reason = python3_nacl_missing next = controls/install.sh\n' >&2
  exit 1
fi

PYTHONDONTWRITEBYTECODE=1 python3 controls/lib/surface.py "$ROOT"
PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 -c 'import sync_need, upgrade, validator_bundle, validator_config, validator_enroll, validator_guard, validator_process, validator_recover, validator_rejoin, validator_rpc, validator_status, validator_store'
if [ "$TESTS" -eq 1 ]; then
  PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 -m unittest controls/lib/test_validator_tools.py
  sh controls/print_style_gate.sh
  sh controls/proof_gate.sh
fi
if [ -f config/network.env ]; then
  PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 controls/lib/validator_bundle.py \
    --network config/network.env
fi

printf 'status = pass gate = validator_tools\n'