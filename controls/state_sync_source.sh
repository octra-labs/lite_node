# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CONFIG=${OCTRA_OPERATOR_CONFIG:-"$ROOT/.keys/validator/node.env"}

python3 "$ROOT/controls/lib/validator_state_sync_source.py" --config "$CONFIG" "$@"
OCTRA_OPERATOR_CONFIG="$CONFIG" sh "$ROOT/controls/run.sh"