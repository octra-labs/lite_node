# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu
launcher=${1:-test/fd_limit.sh}
maximum=$(ulimit -H -n)
(
  ulimit -S -n 1024
  sh "$launcher" sh -c 'test "$(ulimit -S -n)" -ge 2048'
)
probe=4096
if [ "$maximum" != unlimited ] && [ "$maximum" -lt "$probe" ]; then
  probe=$maximum
fi
if [ "$probe" -gt 2048 ]; then
  (
    ulimit -S -n "$probe"
    sh "$launcher" sh -c 'test "$(ulimit -S -n)" -eq "$1"' sh "$probe"
  )
else
  printf 'status = skip test = fd_preserve reason = hard_limit maximum = %s\n' "$maximum"
fi
if (
  ulimit -S -n 1024
  ulimit -H -n 1024
  sh "$launcher" true
); then
  exit 1
else
  test "$?" -eq 2
fi
if sh "$launcher"; then
  exit 1
else
  test "$?" -eq 2
fi
if sh "$launcher" sh -c 'exit 7'; then
  exit 1
else
  test "$?" -eq 7
fi
printf 'status = pass test = fd_limit\n'