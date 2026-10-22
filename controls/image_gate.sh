# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BUILD_DIR=${OCTRA_BUILD_DIR:-$ROOT/runtime_data/_build_image}
MODE=${1:-}
if [ "$#" -gt 1 ] || { [ -n "$MODE" ] && [ "$MODE" != --flat ]; }; then
  printf 'event = gate name = image status = fail reason = arguments\n' >&2
  exit 2
fi
cd "$ROOT"
opam exec -- dune build --root . --build-dir "$BUILD_DIR" --profile release \
  bin/octra_state_sync_manifest.exe \
  test/test_sync_files.exe test/test_image_count.exe test/test_image.exe test/test_state_sync_client.exe \
  test/test_state_sync_manifest.exe test/test_state_sync_reliability.exe \
  test/test_sync_chain.exe test/test_sync_range.exe test/test_range_actor.exe test/test_join_rpc.exe \
  test/test_driver_boot.exe test/test_journal_io.exe test/test_journal_hash.exe \
  test/test_catchup_shell.exe test/test_stake_sync.exe
for name in stake_sync journal_io catchup_shell driver_boot journal_hash sync_files image_count image state_sync_client state_sync_manifest state_sync_reliability sync_chain sync_range range_actor join_rpc; do
  set -- "$BUILD_DIR/default/test/test_$name.exe"
  if [ "$name" = sync_chain ]; then
    set -- "$@" "$BUILD_DIR/default/bin/octra_state_sync_manifest.exe"
  fi
  if [ "$name" = image ] && [ "$MODE" = --flat ]; then
    set -- "$@" --flat
  fi
  opam exec -- "$@"
done
PROOF="$ROOT/runtime_data/image-proof-$$"
mkdir -p "$PROOF/controls" "$PROOF/formal/coq"
cp controls/proof_gate.sh "$PROOF/controls/proof_gate.sh"
cp formal/coq/sync_chain.v "$PROOF/formal/coq/sync_chain.v"
cp formal/coq/sync_compact.v "$PROOF/formal/coq/sync_compact.v"
sh "$PROOF/controls/proof_gate.sh" formal/coq/sync_chain.v formal/coq/sync_compact.v
printf 'event = gate name = image status = pass\n'