# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
SOURCE_BUILD=0
DATA_ROOT=${OCTRA_DATA_ROOT:-/var/lib/octra}
OCAML_TOOLCHAIN=4.14.2
RUST_TOOLCHAIN=1.80.1
TOOLCHAIN_ROOT="$ROOT/runtime_data/toolchains"
CARGO_HOME="$TOOLCHAIN_ROOT/cargo"
RUSTUP_HOME="$TOOLCHAIN_ROOT/rustup"
OCAML_SWITCH="$TOOLCHAIN_ROOT/ocaml"

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

PM2_HOME="$OPERATOR_HOME/.pm2"
PM2_SERVICE="pm2-$OPERATOR_USER.service"

run_operator() {
  if [ "$CURRENT_USER" = "$OPERATOR_USER" ]; then
    env -C "$ROOT" HOME="$OPERATOR_HOME" PM2_HOME="$PM2_HOME" "$@"
  elif [ "$(id -u)" -eq 0 ]; then
    runuser -u "$OPERATOR_USER" -- env -C "$ROOT" HOME="$OPERATOR_HOME" PM2_HOME="$PM2_HOME" PATH="$PATH" "$@"
  else
    sudo -H -u "$OPERATOR_USER" env -C "$ROOT" HOME="$OPERATOR_HOME" PM2_HOME="$PM2_HOME" PATH="$PATH" "$@"
  fi
}

install_rust_toolchain() {
  RUSTUP="$CARGO_HOME/bin/rustup"
  if [ ! -x "$RUSTUP" ]; then
    run_operator mkdir -p "$TOOLCHAIN_ROOT"
    run_operator curl \
      --proto '=https' \
      --tlsv1.2 \
      --fail \
      --location \
      --silent \
      --show-error \
      https://sh.rustup.rs \
      --output "$TOOLCHAIN_ROOT/rustup-init.sh"
    run_operator env CARGO_HOME="$CARGO_HOME" RUSTUP_HOME="$RUSTUP_HOME" \
      sh "$TOOLCHAIN_ROOT/rustup-init.sh" \
      -y \
      --no-modify-path \
      --profile minimal \
      --default-toolchain "$RUST_TOOLCHAIN"
  fi
  run_operator env CARGO_HOME="$CARGO_HOME" RUSTUP_HOME="$RUSTUP_HOME" \
    "$RUSTUP" set profile minimal
  run_operator env CARGO_HOME="$CARGO_HOME" RUSTUP_HOME="$RUSTUP_HOME" \
    "$RUSTUP" toolchain install "$RUST_TOOLCHAIN"
  run_operator env CARGO_HOME="$CARGO_HOME" RUSTUP_HOME="$RUSTUP_HOME" \
    "$RUSTUP" default "$RUST_TOOLCHAIN"
}

install_ocaml_toolchain() {
  run_operator mkdir -p "$TOOLCHAIN_ROOT"
  if [ ! -f "$OPERATOR_HOME/.opam/config" ]; then
    run_operator opam init --bare --disable-sandboxing --no-setup --yes
  fi
  if ! run_operator opam exec --switch "$OCAML_SWITCH" \
    -- ocamlc -version >/dev/null 2>&1; then
    run_operator opam switch create \
      "$OCAML_SWITCH" "$OCAML_TOOLCHAIN" --yes
  fi
  run_operator opam switch link "$OCAML_SWITCH" "$ROOT" --yes
  INSTALLED_OCAML=$(run_operator opam exec --switch "$OCAML_SWITCH" \
    -- ocamlc -version)
  if [ "$INSTALLED_OCAML" != "$OCAML_TOOLCHAIN" ]; then
    printf 'status = refused reason = ocaml_toolchain_mismatch expected = %s actual = %s\n' \
      "$OCAML_TOOLCHAIN" "$INSTALLED_OCAML" >&2
    exit 1
  fi
}

bind_build_toolchains() {
  run_operator opam option --switch "$OCAML_SWITCH" \
    "setenv=CARGO_HOME=\"$CARGO_HOME\""
  run_operator opam option --switch "$OCAML_SWITCH" \
    "setenv+=RUSTUP_HOME=\"$RUSTUP_HOME\""
  run_operator opam option --switch "$OCAML_SWITCH" \
    "setenv+=PATH+=\"$CARGO_HOME/bin\""
  OPAM_ENVIRONMENT="$OCAML_SWITCH/_opam/.opam-switch/environment"
  if [ -f "$OPAM_ENVIRONMENT" ]; then
    run_operator unlink "$OPAM_ENVIRONMENT"
  fi
}

PACKAGES='ca-certificates curl libev4 libgmp10 liblmdb0 libsqlite3-0 nodejs npm python3 python3-nacl'

if [ "$SOURCE_BUILD" -eq 1 ]; then
  PACKAGES="$PACKAGES build-essential git libev-dev libgmp-dev liblmdb-dev libsqlite3-dev m4 opam pkg-config"
fi

printf 'event = install phase = packages\n'
run_root apt-get update
run_root apt-get install -y $PACKAGES
if ! command -v pm2 >/dev/null 2>&1; then
  run_root npm install -g pm2
fi
run_root chown -R "$OPERATOR_USER:$OPERATOR_USER" "$ROOT"
run_root install -d -m 700 -o "$OPERATOR_USER" -g "$OPERATOR_USER" "$DATA_ROOT"
if [ "$SOURCE_BUILD" -eq 1 ]; then
  install_ocaml_toolchain
  install_rust_toolchain
  bind_build_toolchains
fi

printf 'event = install phase = process_manager user = %s\n' "$OPERATOR_USER"
if ! systemctl is-enabled --quiet "$PM2_SERVICE" >/dev/null 2>&1; then
  run_root env "PATH=$PATH" pm2 startup systemd -u "$OPERATOR_USER" --hp "$OPERATOR_HOME"
fi

printf 'status = ready source_build = %s user = %s data_root = %s\n' "$SOURCE_BUILD" "$OPERATOR_USER" "$DATA_ROOT"
printf 'next = configure user = %s script = %s/controls/config_val.sh\n' "$OPERATOR_USER" "$ROOT"
