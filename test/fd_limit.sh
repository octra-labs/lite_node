# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

if [ "$#" -eq 0 ]; then
  printf 'status = refused reason = test_command_missing\n' >&2
  exit 2
fi
limit=$(ulimit -S -n)
case "$limit" in
  unlimited) ;;
  *)
    if [ "$limit" -lt 2048 ]; then
      if ! ulimit -S -n 2048; then
        printf 'status = refused reason = open_file_limit required = 2048 actual = %s\n' "$limit" >&2
        exit 2
      fi
    fi
    ;;
esac
exec "$@"