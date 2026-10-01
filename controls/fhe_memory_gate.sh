# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
export OCTRA_SRC_ROOT="$ROOT"
cd "$ROOT"
opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" \
  test/test_fhe_memory.exe test/test_vm_effects.exe test/native_math.exe
"$BUILD_DIR/default/test/native_math.exe"
opam exec -- "$BUILD_DIR/default/test/test_fhe_memory.exe"
opam exec -- "$BUILD_DIR/default/test/test_vm_effects.exe"
sh controls/proof_gate.sh formal/coq/fhe_work.v
printf 'event = gate name = fhe_memory status = passed\n'