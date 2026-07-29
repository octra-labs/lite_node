# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CONFIG=${OCTRA_OPERATOR_CONFIG:-"$ROOT/.keys/validator/node.env"}

if [ ! -f "$CONFIG" ]; then
  printf 'status = refused reason = configuration_missing path = %s\n' "$CONFIG" >&2
  exit 1
fi

python3 "$ROOT/controls/lib/validator_guard.py" --config "$CONFIG"

set -a
. "$CONFIG"
set +a

python3 "$ROOT/controls/lib/validator_process.py" --config "$CONFIG"

mkdir -p "$ROOT/data" "$OCTRA_OPERATOR_LOG_DIR"
chmod 700 "$ROOT/data"
chmod 700 "$OCTRA_OPERATOR_LOG_DIR"

pm2 start "$OCTRA_OPERATOR_BINARY" \
  --name "$OCTRA_OPERATOR_PM2_NAME" \
  --cwd "$ROOT" \
  --interpreter none \
  --merge-logs \
  --output "$OCTRA_OPERATOR_LOG_DIR/node.log" \
  --error "$OCTRA_OPERATOR_LOG_DIR/node.log"

pm2 save --force
printf 'status = started name = %s role = %s\n' "$OCTRA_OPERATOR_PM2_NAME" "$OCTRA_CONSENSUS_MODE"