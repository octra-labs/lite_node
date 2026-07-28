set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
CONFIG=${OCTRA_OPERATOR_CONFIG:-"$ROOT/.keys/validator/node.env"}

if [ ! -f "$CONFIG" ]; then
  printf 'status = unavailable reason = configuration_missing path = %s\n' "$CONFIG" >&2
  exit 1
fi

exec python3 "$ROOT/controls/lib/validator_status.py" --config "$CONFIG" "$@"
