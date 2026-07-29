# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

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

if command -v sha256sum >/dev/null 2>&1; then
  sha256sum -c MANIFEST.sha256 >/dev/null
elif command -v shasum >/dev/null 2>&1; then
  shasum -a 256 -c MANIFEST.sha256 >/dev/null
else
  printf 'status = refused reason = sha256_tool_missing\n' >&2
  exit 1
fi

test -f nodes.config
sh -n controls/check.sh
sh -n controls/config_val.sh
sh -n controls/enroll.sh
sh -n controls/install.sh
sh -n controls/run.sh
sh -n controls/stat.sh
sh -n controls/stop.sh

if ! command -v python3 >/dev/null 2>&1; then
  printf 'status = refused reason = python3_missing\n' >&2
  exit 1
fi

if ! PYTHONDONTWRITEBYTECODE=1 python3 -c 'import nacl' >/dev/null 2>&1; then
  printf 'status = refused reason = python3_nacl_missing next = controls/install.sh\n' >&2
  exit 1
fi

PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 -c 'import validator_bundle, validator_config, validator_enroll, validator_guard, validator_process, validator_rpc, validator_status'
PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 -m unittest controls/lib/test_validator_tools.py
if [ -f config/network.env ]; then
  if [ -f artifacts/octra_node.exe ]; then
    PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 controls/lib/validator_bundle.py \
      --network config/network.env \
      --binary artifacts/octra_node.exe
  else
    PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 controls/lib/validator_bundle.py \
      --network config/network.env
  fi
fi

printf 'status = pass gate = validator_tools\n'