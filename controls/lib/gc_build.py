# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import fcntl
import hashlib
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

from gc_source import CONTROL, digest, prepare

ROOT = CONTROL.parent

def output(args, env):
    return subprocess.check_output(args, env = env, text = True).strip()

def files(root):
    return {str(path.relative_to(root)): digest(path)
        for path in sorted(root.rglob("*")) if path.is_file()}

def check_prefix(prefix, expected):
    if not expected or files(prefix) != expected:
        raise ValueError("storage installed files differ")

def check_metadata(prefix, source):
    for name in ("irmin", "irmin-pack", "index"):
        text = (prefix / "lib" / name / "dune-package").read_text()
        if str(source) in text:
            raise ValueError("storage metadata references build source")

def archives(manifest, root, env, lease):
    cache = Path(output(["opam", "var", "root"], env)) / "download-cache/sha256"
    result = {}
    for name, value in manifest.items():
        sha = value["archive_sha256"]
        cached = cache / sha[:2] / sha
        path = cached if cached.is_file() else root / (name + ".tbz")
        if not path.exists():
            subprocess.run(["curl", "--fail", "--location", "--max-time", "300",
                "--proto", "=https", "--proto-redir", "=https",
                "--output", str(path),
                "https://opam.ocaml.org/cache/sha256/" + sha[:2] + "/" + sha],
                env = env, pass_fds = (lease,), check = True)
        if digest(path) != sha:
            raise ValueError("storage archive digest differs: " + name)
        result[name] = path
    return result

def acquire(lock, timeout = 300):
    deadline = time.monotonic() + timeout
    while True:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
            return
        except BlockingIOError:
            if time.monotonic() >= deadline:
                raise TimeoutError("storage build lease busy: " + str(lock.name))
            time.sleep(min(0.1, max(0, deadline - time.monotonic())))

def preserve(root):
    for attempt in range(1, 10001):
        target = root.with_name(root.name + ".failed-" + str(attempt))
        if not target.exists():
            root.rename(target)
            print("event = storage_build_recovery preserved = " + str(target), file = sys.stderr)
            return
    raise ValueError("storage build recovery limit: " + str(root))

def environment():
    env = dict(os.environ)
    for key in ("OCAMLPATH", "OCAMLFIND_TOOLCHAIN", "OCAMLFIND_COMMANDS", "DESTDIR",
        "DUNE_BUILD_DIR", "DUNE_INSTALL_PREFIX", "DUNE_ROOT", "DUNE_WORKSPACE"):
        env.pop(key, None)
    manifest_path = CONTROL / "gc_sources.json"
    manifest = json.loads(manifest_path.read_text())
    for value in manifest.values():
        if digest(CONTROL / value["patch"]) != value["patch_sha256"]:
            raise ValueError("storage patch digest differs")
    switch = Path(env.get("OPAM_SWITCH_PREFIX") or output(["opam", "var", "prefix"], env))
    env["PATH"] = str(switch / "bin") + os.pathsep + env.get("PATH", os.defpath)
    env["OCAMLFIND_CONF"] = str(switch / "lib/findlib.conf")
    env["OCAMLLIB"] = str(switch / "lib/ocaml")
    identity = [digest(manifest_path), digest(Path(__file__)),
        digest(Path(__file__).with_name("gc_source.py")), str(switch),
        output(["ocamlc", "-config"], env),
        digest(switch / ".opam-switch/switch-state"),
        digest(switch / "bin/ocamlopt"), digest(switch / "bin/dune")]
    key = hashlib.sha256(json.dumps(identity).encode()).hexdigest()
    root = ROOT / "runtime_data/gc_build" / key
    root.parent.mkdir(parents = True, exist_ok = True)
    prefix = root / "prefix"
    with root.with_suffix(".lock").open("a") as lock:
        acquire(lock)
        record = root / "installed.json"
        if record.exists():
            check_prefix(prefix, json.loads(record.read_text()))
        else:
            if shutil.disk_usage(root.parent).free < 9 * 1024 ** 3:
                raise ValueError("storage build disk reserve insufficient: " + str(root))
            if root.exists():
                preserve(root)
            root.mkdir()
            with (root / "started").open("x") as stream:
                stream.write(json.dumps(identity) + "\n")
            inputs = archives(manifest, root, env, lock.fileno())
            source = prepare(inputs["irmin"], inputs["index"], root / "source")
            build = "_build"
            work = root / "work"
            work.mkdir()
            env["TMPDIR"] = str(work)
            env["DUNE_CACHE"] = "disabled"
            with (root / "build.log").open("x") as log:
                for args in (["dune", "build", "--root", str(source), "--build-dir", str(build),
                        "--profile", "release", "-j2", "irmin.install", "irmin-pack.install", "vendors/index/index.install"],
                    ["dune", "install", "--root", str(source), "--build-dir", str(build),
                        "--prefix", str(prefix), "irmin", "irmin-pack", "index"]):
                    subprocess.run(args, cwd = source, env = env, stdout = log,
                        stderr = subprocess.STDOUT, pass_fds = (lock.fileno(),), check = True)
            check_metadata(prefix, source)
            installed = files(prefix)
            check_prefix(prefix, installed)
            staged = root / "installed.next"
            with staged.open("x") as stream:
                json.dump(installed, stream, sort_keys = True)
                stream.flush()
                os.fsync(stream.fileno())
            staged.replace(record)
        env["OCAMLPATH"] = os.pathsep.join([str(prefix / "lib"), str(switch / "lib")])
        env["CAML_LD_LIBRARY_PATH"] = os.pathsep.join([
            str(prefix / "lib/stublibs"), str(switch / "lib/stublibs"),
            str(switch / "lib/ocaml/stublibs")])
        for name in ("irmin", "irmin-pack", "index"):
            found = Path(output(["ocamlfind", "query", name], env)).resolve()
            if found != (prefix / "lib" / name).resolve():
                raise ValueError("storage library path differs: " + name)
    return env

def main():
    args = sys.argv[1:]
    if args[:1] == ["--"]:
        args = args[1:]
    if not args:
        raise ValueError("storage build command missing")
    env = environment()
    os.execvpe(args[0], args, env)

if __name__ == "__main__":
    main()