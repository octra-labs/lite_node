# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import subprocess
import sys

def main():
    if len(sys.argv) != 2:
        raise SystemExit("expected RSS executable")
    failed = False
    for route in ("view", "apply", "replay"):
        try:
            result = subprocess.run([sys.argv[1], route], timeout=600, check=False)
            status = result.returncode
        except subprocess.TimeoutExpired:
            status = 124
        print(f"event = code_rss_route route = {route} exit = {status}", flush=True)
        failed = failed or status != 0
    return int(failed)

if __name__ == "__main__":
    raise SystemExit(main())