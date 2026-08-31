# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import sys
from pathlib import Path

def modules(root):
    skip = {".git", "_build", "runtime_data"}
    return {
        str(path.relative_to(root))
        for path in root.rglob("*")
        if path.is_file()
        and path.suffix in {".ml", ".mli"}
        and not any(part in skip for part in path.relative_to(root).parts)
    }

def main():
    root = Path(sys.argv[1]).resolve()
    base = {
        line.strip()
        for line in (root / "controls" / "modules").read_text().splitlines()
        if line.strip()
    }
    have = modules(root)
    miss = sorted(base - have)
    added = sorted(have - base)
    print(f"baseline = {len(base)}")
    print(f"present = {len(base) - len(miss)}")
    print(f"added = {len(added)}")
    if miss:
        for path in miss:
            print(f"missing = {path}")
        print("status = fail reason = baseline_module_removed")
        return 1
    print("status = pass gate = surface")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())