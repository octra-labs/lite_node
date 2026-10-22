# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"
build=${OCTRA_BUILD_DIR:-_build}
OCTRA_RSS_CYCLES=${OCTRA_RSS_CYCLES:-100}
export OCTRA_RSS_CYCLES
opam exec -- dune build --root . --build-dir "$build" test/test_code_rss.exe
python3 test/code_rss_run.py "$build/default/test/test_code_rss.exe"