# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
export OCTRA_SRC_ROOT="$ROOT"
cd "$ROOT"
sh controls/queue_gate.sh
opam exec -- dune build --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" -j1 \
  test/test_fhe_task.exe test/test_fhe_memory.exe test/test_fhe_work.exe test/test_vm_effects.exe \
  test/native_math.exe test/test_cipher_views.exe test/test_circle_program.exe \
  test/test_native_alloc.exe test/test_proof_wait.exe test/test_pvac_verify_worker.exe \
  test/test_contract_rpc_guard.exe test/test_view_root.exe test/test_vm_transition.exe \
  test/test_private_transition_receipt.exe test/test_epoch_exec_reward_atomic.exe \
  test/test_run_exit.exe test/test_journal_hash.exe test/test_catchup_shell.exe \
  bin/octra_pvac_worker.exe
opam exec -- "$BUILD_DIR/default/test/test_vm_transition.exe" --circle
opam exec -- "$BUILD_DIR/default/test/test_vm_effects.exe"
opam exec -- "$BUILD_DIR/default/test/test_run_exit.exe"
opam exec -- "$BUILD_DIR/default/test/test_view_root.exe"
opam exec -- "$BUILD_DIR/default/test/test_circle_program.exe"
opam exec -- "$BUILD_DIR/default/test/test_epoch_exec_reward_atomic.exe"
opam exec -- "$BUILD_DIR/default/test/test_journal_hash.exe"
opam exec -- "$BUILD_DIR/default/test/test_catchup_shell.exe"
opam exec -- "$BUILD_DIR/default/test/test_fhe_memory.exe"
opam exec -- "$BUILD_DIR/default/test/test_fhe_task.exe"
opam exec -- "$BUILD_DIR/default/test/test_fhe_work.exe"
opam exec -- "$BUILD_DIR/default/test/test_native_alloc.exe" --preview
opam exec -- "$BUILD_DIR/default/test/test_pvac_verify_worker.exe"
opam exec -- "$BUILD_DIR/default/test/test_private_transition_receipt.exe" migration_retry
opam exec -- "$BUILD_DIR/default/test/test_proof_wait.exe"
opam exec -- "$BUILD_DIR/default/test/test_vm_transition.exe"
opam exec -- "$BUILD_DIR/default/test/test_contract_rpc_guard.exe"
opam exec -- dune runtest --cache=disabled --root "$ROOT" --build-dir "$BUILD_DIR" \
  --force -j1 test/test_circle_view_cap
"$BUILD_DIR/default/test/native_math.exe"
opam exec -- "$BUILD_DIR/default/test/test_cipher_views.exe"
sh controls/proof_gate.sh formal/coq/aux_restore.v formal/coq/fhe_work.v formal/coq/r1cs_build.v \
  formal/coq/fhe_queue.v formal/coq/proof_wait.v
printf 'event = gate name = fhe_memory status = passed\n'