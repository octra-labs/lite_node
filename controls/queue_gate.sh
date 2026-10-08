# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
work="$root/runtime_data/queue-$$"
mkdir -p "$root/runtime_data"
mkdir "$work"
trap 'rm -rf "$work"' EXIT
cp "$root/formal/coq/fhe_queue.v" "$work"
cp "$root/test/queue_extract.v" "$work"
cp "$root/test/queue_compare.ml" "$work"
cp "$root/lib/vm/runtime/proof_wait.ml" "$work"
cp "$root/lib/vm/runtime/fhe_queue.ml" "$work"
cd "$work"
opam exec -- coqc -q fhe_queue.v
opam exec -- coqc -q queue_extract.v
opam exec -- ocamlfind ocamlopt -package zarith -linkpkg \
  proof_wait.ml fhe_queue.ml queue_model.mli queue_model.ml queue_compare.ml \
  -o queue_compare
./queue_compare