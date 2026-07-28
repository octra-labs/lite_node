set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CONFIG=${OCTRA_OPERATOR_CONFIG:-"$ROOT/.keys/validator/node.env"}

if [ ! -f "$CONFIG" ]; then
  printf 'status = refused reason = configuration_missing path = %s\n' "$CONFIG" >&2
  exit 1
fi

set -a
. "$CONFIG"
set +a

pm2 stop "$OCTRA_OPERATOR_PM2_NAME"
pm2 save --force
printf 'status = stopped name = %s\n' "$OCTRA_OPERATOR_PM2_NAME"
