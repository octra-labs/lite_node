# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
cd "$ROOT"
opam exec -- dune build --root . --build-dir "$BUILD_DIR" test/test_tx_staging_selection.exe test/test_proposal.exe
opam exec -- "$BUILD_DIR/default/test/test_tx_staging_selection.exe"
opam exec -- "$BUILD_DIR/default/test/test_proposal.exe"
opam exec -- coqc -Q formal/coq '' formal/coq/private_slots.v
context=$(opam exec -- coqchk -silent -o -Q formal/coq '' private_slots 2>&1)
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