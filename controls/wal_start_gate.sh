# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
if [ "$#" -ne 2 ]; then
  printf 'event = gate name = wal_start status = refused reason = pm2_and_output_required\n' >&2
  exit 1
fi
cd "$ROOT"
printf 'event = gate name = wal_start phase = test\n'
python3 -B test/test_pm2_ipc.py
python3 -B test/test_wal_start.py --root "$ROOT" --output "$2" --pm2 "$1" \
  --binary "$BUILD_DIR/default/bin/octra_node.exe" \
  --worker "$BUILD_DIR/default/bin/octra_pvac_worker.exe"
printf 'event = gate name = wal_start status = passed\n'