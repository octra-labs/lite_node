# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CONFIG=${OCTRA_OPERATOR_CONFIG:-"$ROOT/.keys/validator/node.env"}

if [ ! -f "$CONFIG" ]; then
  printf 'status = refused reason = configuration_missing path = %s\n' "$CONFIG" >&2
  exit 1
fi

exec python3 "$ROOT/controls/lib/validator_recover.py" --config "$CONFIG"