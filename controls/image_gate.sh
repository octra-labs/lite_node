# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
cd "$ROOT"
opam exec -- dune build --root . --build-dir "$BUILD_DIR" --profile release \
  test/test_image_count.exe test/test_image.exe test/test_state_sync_client.exe \
  test/test_state_sync_manifest.exe test/test_state_sync_reliability.exe
for name in image_count image state_sync_client state_sync_manifest state_sync_reliability; do
  opam exec -- "$BUILD_DIR/default/test/test_$name.exe"
done
printf 'event = gate name = image status = pass\n'