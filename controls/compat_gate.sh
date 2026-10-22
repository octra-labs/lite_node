# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$root"
if [ "$#" -eq 0 ]; then
  old=${OCTRA_COMPAT_SOURCE:-git}
  work=runtime_data/compat/$(date -u +%Y%m%dT%H%M%S)-$$
elif [ "$#" -eq 2 ]; then
  old=$1
  work=$2
else
  printf 'status = refused reason = arguments usage = compat_gate.sh_[old_source_work_path]\n' >&2
  exit 2
fi
build=${OCTRA_BUILD_DIR:-_build}
python3 -B test/test_compat_source.py
opam exec -- dune build --root . --build-dir "$build" test/test_sync_mark.exe
python3 -B test/compat_role.py "$old" "$work" "$build/default/test/test_sync_mark.exe"