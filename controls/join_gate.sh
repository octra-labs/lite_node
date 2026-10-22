# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
build=${OCTRA_BUILD_DIR:-_build}
cd "$root"
opam exec -- dune build --root . --build-dir "$build" \
  test/test_peer_limits.exe test/test_peer_diag.exe test/test_p2p_preauth.exe test/test_sync_mark.exe test/test_driver_boot.exe
for name in peer_limits peer_diag p2p_preauth sync_mark driver_boot
do
  "$build/default/test/test_$name.exe"
done
PYTHONPATH=controls/lib python3 -B -m unittest test_upgrade_profile test_sync_tip test_sync_http test_sync_start
sh controls/compat_gate.sh
opam exec -- dune build --root . --build-dir "$build" @test/join_timing @test/pool_timing --force
sh controls/proof_gate.sh formal/coq/peer_accept.v formal/coq/peer_hold.v formal/coq/sync_window.v