# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import os
import subprocess
import sys
from pathlib import Path

sys.dont_write_bytecode = True
sys.path.insert(0, sys.argv[1])

import validator_exit
from validator_common import parse_env

config = Path(sys.argv[2])
values = parse_env(config)
operation = sys.argv[3]
expected = sys.argv[4]

if operation == "restore":
    with validator_exit.command_lock(values):
        validator_exit.restore_control(Path(expected), Path(values["OCTRA_DATA_DIR"]))
    print("status = pass test = exit_restore")
    sys.exit(0)

tools = Path(sys.argv[1])
shell = tools.parent / "enroll.sh"
exported = tools.parent.name == "controls"
if exported and not shell.is_file():
    raise AssertionError("exported enrollment command missing")
command = ["sh", str(shell)] if exported else [
    sys.executable, "-B", str(tools / "validator_enroll.py"),
]
action = {"validator_bond": "bond", "validator_exit": "exit", "validator_withdraw": "withdraw",
          "repair-pointer": "repair-pointer"}[operation]
result = subprocess.run(
    [*command, "--config", str(config), action, "--no-wait", "--no-restart",
     *(["--renew"] if len(sys.argv) == 6 and sys.argv[5] == "renew" else []),
     *(["--from", sys.argv[5]] if action == "repair-pointer" else [])],
    env = {**os.environ, "OCTRA_OPERATOR_CONFIG": str(config),
           "PYTHONDONTWRITEBYTECODE": "1"},
    capture_output = True, text = True, timeout = 15,
)
output = result.stdout + result.stderr
print(output, end = "")
if expected == "pass":
    if result.returncode != 0:
        raise AssertionError(f"operator exit code = {result.returncode}")
elif result.returncode != 1 or expected not in output:
    raise AssertionError(f"operator refusal missing: {expected}")
print(f"status = pass test = exit_client route = {'shell' if exported else 'cli'}")