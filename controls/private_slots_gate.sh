# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
cd "$ROOT"
opam exec -- dune build --root . --build-dir "$BUILD_DIR" \
  test/test_tx_staging_selection.exe test/test_proposal.exe test/test_tx_queue.exe \
  test/test_rule_graph.exe test/test_consensus_profile_golden.exe \
  test/test_private_transition_receipt.exe bin/octra_pvac_worker.exe
opam exec -- "$BUILD_DIR/default/test/test_rule_graph.exe"
opam exec -- "$BUILD_DIR/default/test/test_consensus_profile_golden.exe"
opam exec -- "$BUILD_DIR/default/test/test_tx_staging_selection.exe"
opam exec -- "$BUILD_DIR/default/test/test_proposal.exe"
opam exec -- "$BUILD_DIR/default/test/test_tx_queue.exe"
WORKER=$(CDPATH= cd -- "$BUILD_DIR/default/bin" && pwd)/octra_pvac_worker.exe
OCTRA_PVAC_VERIFY_WORKER="$WORKER" \
  opam exec -- "$BUILD_DIR/default/test/test_private_transition_receipt.exe" batch
opam exec -- coqc -Q formal/coq '' formal/coq/private_slots.v
opam exec -- coqc -Q formal/coq '' formal/coq/circle_refill.v
opam exec -- coqc -Q formal/coq '' formal/coq/fhe_work.v
context=$(opam exec -- coqchk -silent -o -Q formal/coq '' private_slots circle_refill fhe_work 2>&1)
for claim in \
  '* Axioms: <none>' \
  '* Constants/Inductives relying on type-in-type: <none>' \
  '* Constants/Inductives relying on unsafe (co)fixpoints: <none>' \
  '* Inductives whose positivity is assumed: <none>'
do
  if ! printf '%s\n' "$context" | grep -Fqx "$claim"; then
    printf 'status = fail proof = private_slots reason = proof_context\n' >&2
    printf '%s\n' "$context" >&2
    exit 1
  fi
done
printf '%s\n' "$context"
printf 'event = gate name = private_slots status = passed\n'