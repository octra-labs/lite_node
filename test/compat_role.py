# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import hashlib
import pathlib
import shutil
import subprocess
import sys

def old_source(old, source, name, digest):
    raw = None
    try:
        if old is None:
            for revision, prefix in [
                    ("7f9be4489870645cfcc95439638cde40f5da9f50", ""),
                    ("f7637a7eda10b78702ad56844707f7fd308db0e3", "libv_litecore/")]:
                result = subprocess.run(["git", "show", revision + ":" + prefix + name],
                    cwd = source, capture_output = True, check = False)
                if result.returncode == 0:
                    raw = result.stdout
                    break
        else:
            raw = (old / name).read_bytes()
    except OSError:
        pass
    if raw is None:
        raise SystemExit("status = refused reason = old_source_missing next = set_OCTRA_COMPAT_SOURCE")
    if hashlib.sha256(raw).hexdigest() != digest:
        if not raw.endswith(b"\n") and hashlib.sha256(raw + b"\n").hexdigest() == digest:
            raw += b"\n"
        else:
            raise SystemExit(f"status = refused reason = old_source_hash file = {name}")
    return raw

def main():
    old = None if sys.argv[1] == "git" else pathlib.Path(sys.argv[1]).resolve()
    work = pathlib.Path(sys.argv[2]).resolve()
    writer = pathlib.Path(sys.argv[3]).resolve()
    source = pathlib.Path(__file__).resolve().parent
    work.mkdir(parents = True, exist_ok = False)
    for line in (source / "cases/old_role.sha256").read_text().splitlines():
        digest, name = line.split("  ", 1)
        raw = old_source(old, source, name, digest)
        (work / pathlib.Path(name).name).write_bytes(raw)
    run_reader(work, source, writer)

def run_reader(work, source, writer):
    boot = (work / "consensus_driver_boot_shell.ml").read_text()
    boot = boot[boot.index("let sync_plan ~head state ="):boot.index("let start (deps : deps) =")]
    (work / "boot_check.ml").write_text(boot)
    node = (work / "octra_node.ml").read_text()
    begin = node.index("    let mark_state_attested =")
    end = node.index("    let set_state_attested ~head ~root =", begin)
    (work / "attest_check.ml").write_text(
        "type network = { chain_id : string }\n"
        "let create ~data_dir ~chain ~set_state_attested ~exit ~exit_error recovery_need =\n"
        "let startup_network = { chain_id = chain } in\n" + node[begin:end] + "mark_state_attested\n")
    (work / "octra_consensus.ml").write_text("module C_role = C_role\n")
    shutil.copyfile(source / "compat_role.ml", work / "compat_role.ml")
    subprocess.run(["opam", "exec", "--", "ocamlfind", "ocamlopt", "-package", "zarith,yojson,unix", "-linkpkg",
        "octra_log_format.ml", "octra_log.ml", "log.ml", "c_role.ml", "octra_consensus.ml",
        "consensus_mode.ml", "sync_need.ml", "sync_mark.ml", "boot_check.ml", "attest_check.ml",
        "compat_role.ml", "-o", "check"], cwd = work, check = True)
    for name in ["range", "root"]:
        (work / name).mkdir()
    subprocess.run([str(writer), "--compat-write", str(work / "range")], check = True)
    subprocess.run([str(work / "check"), str(work / "range"), str(work / "root")], check = True)

if __name__ == "__main__":
    try:
        main()
    except (OSError, subprocess.CalledProcessError) as error:
        raise SystemExit(f"status = refused reason = old_reader error = {type(error).__name__}")