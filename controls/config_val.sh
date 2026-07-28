set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

exec python3 "$ROOT/controls/lib/validator_config.py" "$@"
