# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
cd "$ROOT"
PYTHONDONTWRITEBYTECODE=1 python3 test/drop_link.py --source-only
opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" test/test_tx_drop.exe test/test_drop_sink.exe test/test_node_runtime_history.exe bin/octra_node.exe
opam exec -- "$BUILD_DIR/default/test/test_tx_drop.exe"
opam exec -- "$BUILD_DIR/default/test/test_drop_sink.exe"
opam exec -- "$BUILD_DIR/default/test/test_node_runtime_history.exe"
PYTHONDONTWRITEBYTECODE=1 python3 test/drop_link.py "$BUILD_DIR/default/bin/octra_node.exe"
printf 'event = gate name = drop_store status = passed\n'