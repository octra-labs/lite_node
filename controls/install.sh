# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
SOURCE_BUILD=0
DATA_ROOT=${OCTRA_DATA_ROOT:-/var/lib/octra}

case "$DATA_ROOT" in
  /*) ;;
  *)
    printf 'status = refused reason = data_root_not_absolute path = %s\n' "$DATA_ROOT" >&2
    exit 1
    ;;
esac

if [ "${1:-}" = "--source-build" ]; then
  SOURCE_BUILD=1
elif [ "$#" -ne 0 ]; then
  printf 'status = refused reason = unsupported_argument value = %s\n' "$1" >&2
  exit 1
fi

if [ "$(uname -s)" != "Linux" ] || ! command -v apt-get >/dev/null 2>&1; then
  printf 'status = refused reason = unsupported_operating_system\n' >&2
  exit 1
fi

CURRENT_USER=$(id -un)

if [ "$(id -u)" -eq 0 ]; then
  ROOT_CMD=
else
  if ! command -v sudo >/dev/null 2>&1; then
    printf 'status = refused reason = sudo_missing\n' >&2
    exit 1
  fi
  ROOT_CMD=sudo
fi

run_root() {
  if [ -n "$ROOT_CMD" ]; then
    "$ROOT_CMD" "$@"
  else
    "$@"
  fi
}

if [ -n "${OCTRA_OPERATOR_USER:-}" ]; then
  OPERATOR_USER=$OCTRA_OPERATOR_USER
elif [ -n "${SUDO_USER:-}" ] && [ "$SUDO_USER" != "root" ]; then
  OPERATOR_USER=$SUDO_USER
elif [ "$(id -u)" -eq 0 ]; then
  OPERATOR_USER=octra
else
  OPERATOR_USER=$CURRENT_USER
fi

if ! id "$OPERATOR_USER" >/dev/null 2>&1; then
  run_root useradd --create-home --shell /bin/bash "$OPERATOR_USER"
fi

OPERATOR_HOME=$(getent passwd "$OPERATOR_USER" | awk -F: '{print $6}')

if [ -z "$OPERATOR_HOME" ]; then
  printf 'status = refused reason = operator_home_missing user = %s\n' "$OPERATOR_USER" >&2
  exit 1
fi

run_operator() {
  if [ "$CURRENT_USER" = "$OPERATOR_USER" ]; then
    env HOME="$OPERATOR_HOME" "$@"
  elif [ "$(id -u)" -eq 0 ]; then
    runuser -u "$OPERATOR_USER" -- env HOME="$OPERATOR_HOME" PATH="$PATH" "$@"
  else
    sudo -H -u "$OPERATOR_USER" env HOME="$OPERATOR_HOME" PATH="$PATH" "$@"
  fi
}

PACKAGES='ca-certificates curl libev4 libgmp10 liblmdb0 libsqlite3-0 nodejs npm python3 python3-nacl'

if [ "$SOURCE_BUILD" -eq 1 ]; then
  PACKAGES="$PACKAGES docker-buildx docker.io git"
fi

printf 'event = install phase = packages\n'
run_root apt-get update
run_root apt-get install -y $PACKAGES
if [ "$SOURCE_BUILD" -eq 1 ]; then
  run_root systemctl enable --now docker
  run_root usermod -aG docker "$OPERATOR_USER"
fi
run_root npm install -g pm2
run_root chown -R "$OPERATOR_USER:$OPERATOR_USER" "$ROOT"
run_root install -d -m 700 -o "$OPERATOR_USER" -g "$OPERATOR_USER" "$DATA_ROOT"

printf 'event = install phase = process_manager user = %s\n' "$OPERATOR_USER"
run_operator pm2 install pm2-logrotate
run_operator pm2 set pm2-logrotate:max_size 100M
run_operator pm2 set pm2-logrotate:retain 10
run_operator pm2 set pm2-logrotate:compress true
run_root env "PATH=$PATH" pm2 startup systemd -u "$OPERATOR_USER" --hp "$OPERATOR_HOME"
run_operator pm2 save --force

printf 'status = ready source_build = %s user = %s data_root = %s\n' "$SOURCE_BUILD" "$OPERATOR_USER" "$DATA_ROOT"
printf 'next = configure user = %s script = %s/controls/config_val.sh\n' "$OPERATOR_USER" "$ROOT"