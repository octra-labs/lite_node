# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-_build}
cd "$ROOT"
opam exec -- dune build --root . --build-dir "$BUILD_DIR" --profile release \
  bin/octra_state_sync_manifest.exe \
  test/test_sync_files.exe test/test_image_count.exe test/test_image.exe test/test_state_sync_client.exe \
  test/test_state_sync_manifest.exe test/test_state_sync_reliability.exe \
  test/test_sync_chain.exe test/test_sync_range.exe test/test_range_actor.exe test/test_join_rpc.exe \
  test/test_driver_boot.exe test/test_journal_hash.exe
for name in driver_boot journal_hash sync_files image_count image state_sync_client state_sync_manifest state_sync_reliability sync_chain sync_range range_actor join_rpc; do
  set -- "$BUILD_DIR/default/test/test_$name.exe"
  if [ "$name" = sync_chain ]; then
    set -- "$@" "$BUILD_DIR/default/bin/octra_state_sync_manifest.exe"
  fi
  opam exec -- "$@"
done
sh controls/proof_gate.sh formal/coq/sync_chain.v
printf 'event = gate name = image status = pass\n'