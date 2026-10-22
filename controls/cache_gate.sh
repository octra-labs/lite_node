# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"
cargo test --offline --locked --release \
  --manifest-path lib/core/circle_wasm_native/Cargo.toml \
  --target-dir "${OCTRA_WASM_TARGET:-_cargo_target/circle_wasm_native}" \
  code_cases -- --test-threads=1
opam exec -- dune exec --root . --build-dir "${OCTRA_BUILD_DIR:-_build}" test/test_polc_cache.exe
opam exec -- dune exec --root . --build-dir "${OCTRA_BUILD_DIR:-_build}" test/test_circle_program.exe -- --code-seed
opam exec -- dune exec --root . --build-dir "${OCTRA_BUILD_DIR:-_build}" test/test_table_route.exe
cargo test --offline --locked --release --manifest-path lib/core/circle_wasm_native/Cargo.toml \
  --target-dir "${OCTRA_WASM_TARGET:-_cargo_target/circle_wasm_native}" shape_cases -- --test-threads=1
sh controls/rss_gate.sh
sh controls/proof_gate.sh formal/coq/code_cache.v formal/coq/polc_cache.v formal/coq/wasm_shape.v