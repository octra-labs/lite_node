# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

from pathlib import Path
import re
import subprocess
import sys

root = Path.cwd()
paths = [root / name for name in ("octra_node.opam", "octra_node.opam.locked")]
for folder in ("lib", "bin"):
    base = root / folder
    if not base.is_dir():
        raise SystemExit("status = refused reason = source_directory_missing path = " + folder)
    paths.extend(path for path in base.rglob("*")
        if path.is_file() and (path.suffix in (".ml", ".mli") or path.name == "dune"))
for folder in ("controls",):
    base = root / folder
    if base.is_dir():
        paths.extend(path for path in base.rglob("*")
            if path.name in ("install.sh", "validator_config.py"))
matches = [str(path.relative_to(root)) for path in paths
    if re.search(r"sqlite3", path.read_text(encoding="utf-8"), re.IGNORECASE)]
if matches:
    raise SystemExit("status = refused reason = sqlite_source paths = " + ",".join(matches))
node = (root / "bin/octra_node.ml").read_text(encoding="utf-8")
if "Lwt_main.at_exit" in node:
    raise SystemExit("status = refused reason = nested_lwt_exit_hook")
if any(text not in node for text in ("Drop_sink.finish", "Startup_run_shell.exit_store store",
        "Startup_run_shell.require_sync\n      ~data_dir ~chain:startup_network.chain_id ~store")):
    raise SystemExit("status = refused reason = shutdown_wiring_missing")
if sys.argv[1:] == ["--source-only"]:
    print("status = pass test = drop_source")
    raise SystemExit(0)

binary = Path(sys.argv[1])
if not binary.is_file():
    raise SystemExit("status = refused reason = node_binary_missing")
result = subprocess.run(["nm", str(binary)], capture_output=True, text=True, check=True)
matches = [line for line in result.stdout.splitlines() if "sqlite3" in line.lower()]
if matches:
    raise SystemExit("status = refused reason = sqlite_linked count = " + str(len(matches)))
print("status = pass test = drop_link")