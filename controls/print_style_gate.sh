# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
build=${OCTRA_BUILD_DIR:-_build}
cd "$root"
term=fixt''ure
paths=$(find test controls -print)
names=$(printf '%s\n' "$paths" | LC_ALL=C grep -i "$term" || [ "$?" -eq 1 ])
refs=$(LC_ALL=C grep -R -i "$term" test controls MANIFEST.sha256 || [ "$?" -eq 1 ])
if [ -n "$names$refs" ]; then
  printf 'status = fail check = print_style reason = test_layout\n' >&2
  exit 1
fi
opam exec -- dune build --root "$root" --build-dir "$build" test/test_print_style.exe
if [ "$#" -gt 0 ]; then
  opam exec -- "$build/default/test/test_print_style.exe" "$@"
  exit 0
fi
paths=$(find test -name '*.ml' -type f)
opam exec -- "$build/default/test/test_print_style.exe" $paths