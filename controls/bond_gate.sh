#!/bin/sh
# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"
sh -n bond_inc.sh
opam exec -- dune build --root "$ROOT" bin/bft_control_tx.exe test/test_tx_hash.exe
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH="$ROOT/controls/lib" python3 -m unittest controls/lib/test_bond_inc.py