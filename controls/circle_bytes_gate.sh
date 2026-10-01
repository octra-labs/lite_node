# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
OUT="$ROOT/runtime_data/circle_bytes"
mkdir -p "$OUT"
cargo tree --locked --offline \
  --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
  --prefix none -e features -i wasmparser@0.221.3 > "$OUT/parser-features.txt"
sed -n 's/^wasmparser feature "\([^"]*\)".*/\1/p' "$OUT/parser-features.txt" \
  | sort -u > "$OUT/parser-features.actual"
printf 'features\nstd\nvalidate\n' > "$OUT/parser-features.expected"
diff -u "$OUT/parser-features.expected" "$OUT/parser-features.actual"
rustc --edition=2021 --test "$ROOT/lib/core/circle_wasm_native/src/native_bytes.rs" \
  -o "$OUT/check"
"$OUT/check" --test-threads=1
cargo test --locked --offline \
  --manifest-path "$ROOT/lib/core/circle_wasm_native/Cargo.toml" \
  --target-dir "$ROOT/runtime_data/circle_bytes_cargo" -- --test-threads=1
printf 'event = gate name = circle_bytes status = passed\n'