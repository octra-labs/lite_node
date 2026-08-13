# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

exec python3 "$ROOT/controls/lib/validator_rejoin.py" \
  --root "$ROOT" \
  "$@"