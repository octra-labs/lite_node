# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$ROOT"
if [ "$#" -eq 0 ]; then
  set -- formal/coq/*.v
fi
for path do
  name=${path##*/}
  name=${name%.v}
  if [ ! -f "$path" ]; then
    printf 'status = fail proof = %s reason = missing path = %s\n' "$name" "$path" >&2
    exit 1
  fi
  if context=$(opam exec -- coqc -Q formal/coq '' "$path" 2>&1); then
    :
  else
    code=$?
    printf 'status = fail proof = %s reason = coqc exit = %s\n%s\n' "$name" "$code" "$context" >&2
    exit "$code"
  fi
  if context=$(opam exec -- coqchk -silent -o -Q formal/coq '' "$name" 2>&1); then
    :
  else
    code=$?
    printf 'status = fail proof = %s reason = coqchk exit = %s\n%s\n' "$name" "$code" "$context" >&2
    exit "$code"
  fi
  for claim in \
    '* Axioms: <none>' \
    '* Constants/Inductives relying on type-in-type: <none>' \
    '* Constants/Inductives relying on unsafe (co)fixpoints: <none>' \
    '* Inductives whose positivity is assumed: <none>'
  do
    if ! printf '%s\n' "$context" | grep -Fqx "$claim"; then
      printf 'status = fail proof = %s reason = proof_context\n%s\n' "$name" "$context" >&2
      exit 1
    fi
  done
  printf 'status = pass proof = %s axioms = none\n' "$name"
done