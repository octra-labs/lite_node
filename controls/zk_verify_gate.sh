# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
export OCTRA_SRC_ROOT="$ROOT"
cd "$ROOT"
opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" \
  test/test_zk_start.exe test/test_zk_vm.exe test/test_zk_ffi.exe test/test_zk_e2e.exe test/test_zk_golden.exe test/zk_convert.py
opam exec -- "$BUILD_DIR/default/test/test_zk_start.exe"
opam exec -- "$BUILD_DIR/default/test/test_zk_vm.exe"
opam exec -- "$BUILD_DIR/default/test/test_zk_ffi.exe"
opam exec -- "$BUILD_DIR/default/test/test_zk_e2e.exe" "$BUILD_DIR/default/test/zk_convert.py"
opam exec -- "$BUILD_DIR/default/test/test_zk_golden.exe" "$BUILD_DIR/default/test/zk_convert.py" \
  "$ROOT/test/cases/zk/multiplier"
printf 'event = gate name = zk_verify status = passed\n'