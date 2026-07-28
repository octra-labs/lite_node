set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

cd "$ROOT"

test -f nodes.config
sh -n controls/check.sh
sh -n controls/config_val.sh
sh -n controls/install.sh
sh -n controls/run.sh
sh -n controls/stat.sh
sh -n controls/stop.sh

PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 -c 'import validator_config, validator_guard, validator_process, validator_status'
PYTHONPATH="$ROOT/controls/lib" PYTHONDONTWRITEBYTECODE=1 python3 -m unittest controls/lib/test_validator_tools.py

printf 'status = pass gate = validator_tools\n'
