# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import os
import shutil
import subprocess
import sys
import unittest
from pathlib import Path
from unittest import mock

import gc_build
from gc_source import digest

class BuildCases(unittest.TestCase):
    def setUp(self):
        self.root = gc_build.ROOT / "runtime_data/gc_build_checks" / f"{os.getpid()}-{self._testMethodName}"
        self.root.mkdir(parents = True)
        self.control = self.root / "controls"
        self.control.mkdir()
        self.switch = self.root / "switch"
        (self.switch / "bin").mkdir(parents = True)
        (self.switch / ".opam-switch").mkdir()
        (self.switch / ".opam-switch/switch-state").write_bytes(b"checked")
        for name in ("ocamlopt", "dune"):
            (self.switch / "bin" / name).write_bytes(name.encode())
        patch = self.control / "irmin.patch"
        patch.write_bytes(b"checked")
        (self.control / "gc_sources.json").write_text(json.dumps({
            "irmin": {"patch": patch.name, "patch_sha256": digest(patch)}}))
        self.calls = []
        self.prefix = None

    def tearDown(self):
        shutil.rmtree(self.root)

    def output(self, args, env):
        if args == ["opam", "var", "prefix"]:
            raise AssertionError("selected compiler prefix was ignored")
        if args[:2] == ["ocamlfind", "query"]:
            return str(self.prefix / "lib" / args[2])
        return "checked"

    def build_command(self, args, **kwargs):
        self.assertEqual(len(kwargs.get("pass_fds", ())), 1)
        os.fstat(kwargs["pass_fds"][0])
        self.calls.append(args)
        if args[1] == "install":
            self.prefix = Path(args[args.index("--prefix") + 1])
            for name in ("irmin", "irmin-pack", "index"):
                path = self.prefix / "lib" / name
                path.mkdir(parents = True)
                (path / "META").write_bytes(name.encode())
                (path / "dune-package").write_text("(archives (native checked.cmxa))")

    def environment(self, free = 20 * 1024 ** 3):
        with mock.patch.dict(os.environ, {"OPAM_SWITCH_PREFIX": str(self.switch)}), \
             mock.patch.multiple(gc_build, ROOT = self.root, CONTROL = self.control), \
             mock.patch.object(gc_build, "output", side_effect = self.output), \
             mock.patch.object(gc_build, "archives", return_value = {"irmin": "a", "index": "b"}), \
             mock.patch.object(gc_build, "prepare", return_value = self.root), \
             mock.patch.object(gc_build.subprocess, "run", side_effect = self.build_command), \
             mock.patch.object(gc_build.shutil, "disk_usage", return_value = mock.Mock(free = free)):
            return gc_build.environment()

    def test_build_paths(self):
        with mock.patch.dict(os.environ, {"OCAMLPATH": "/untrusted"}):
            env = self.environment()
        self.assertEqual(len(self.calls), 2)
        self.assertEqual(env["OCAMLPATH"].split(os.pathsep)[0], str(self.prefix / "lib"))
        self.assertNotIn("/untrusted", env["OCAMLPATH"])
        self.assertEqual(self.calls[0][-3:], ["irmin.install", "irmin-pack.install", "vendors/index/index.install"])
        self.assertEqual(self.calls[0][self.calls[0].index("--build-dir") + 1], "_build")
        self.environment()
        self.assertEqual(len(self.calls), 2)

    def test_changed_install(self):
        self.environment()
        (self.prefix / "lib/irmin-pack/META").write_bytes(b"changed")
        with self.assertRaisesRegex(ValueError, "installed files differ"):
            self.environment()

    def test_missing_install(self):
        self.environment()
        (self.prefix / "lib/index/META").unlink()
        with self.assertRaisesRegex(ValueError, "installed files differ"):
            self.environment()

    def test_changed_patch(self):
        (self.control / "irmin.patch").write_bytes(b"changed")
        with self.assertRaisesRegex(ValueError, "patch digest differs"):
            self.environment()
        self.assertEqual(self.calls, [])

    def test_interrupted_build(self):
        def interrupted(args, **kwargs):
            kwargs["stdout"].write("compiler interrupted\n")
            raise subprocess.CalledProcessError(19, args)
        with mock.patch.object(self, "build_command", side_effect = interrupted):
            with self.assertRaises(subprocess.CalledProcessError):
                self.environment()
        cache = self.root / "runtime_data/gc_build"
        failed = next(path for path in cache.iterdir() if path.is_dir())
        before = gc_build.files(failed)
        try:
            self.environment()
        except ValueError:
            self.fail("interrupted attempt did not resume")
        preserved = failed.with_name(failed.name + ".failed-1")
        self.assertEqual(gc_build.files(preserved), before)
        self.assertTrue((failed / "installed.json").is_file())
        self.assertEqual(len(self.calls), 2)

    def test_build_reserve(self):
        with self.assertRaisesRegex(ValueError, "disk reserve insufficient"):
            self.environment(free = 9 * 1024 ** 3 - 1)
        self.assertEqual(self.calls, [])
        self.environment(free = 9 * 1024 ** 3)
        self.assertEqual(len(self.calls), 2)

    def test_child_lease(self):
        path = self.root / "lease"
        owner = path.open("a")
        gc_build.acquire(owner)
        child = subprocess.Popen([sys.executable, "-c",
            "import os,sys; os.fstat(int(sys.argv[1])); print('ready', flush = True); sys.stdin.read()",
            str(owner.fileno())], pass_fds = (owner.fileno(),),
            stdin = subprocess.PIPE, stdout = subprocess.PIPE, text = True)
        try:
            self.assertEqual(child.stdout.readline().strip(), "ready")
            owner.close()
            with path.open("a") as other:
                with self.assertRaisesRegex(TimeoutError, "lease busy"):
                    gc_build.acquire(other, timeout = 0.03)
            child.communicate("", timeout = 5)
            self.assertEqual(child.returncode, 0)
            with path.open("a") as other:
                gc_build.acquire(other, timeout = 0.03)
        finally:
            owner.close()
            if child.poll() is None:
                child.kill()
            child.communicate(timeout = 5)

    def test_empty_install(self):
        with mock.patch.object(gc_build, "files", return_value = {}):
            with self.assertRaisesRegex(ValueError, "installed files differ"):
                gc_build.check_prefix(self.root, {})

    def test_metadata_source(self):
        self.environment()
        gc_build.check_metadata(self.prefix, self.root)
        (self.prefix / "lib/irmin-pack/dune-package").write_text(str(self.root / "_build"))
        with self.assertRaisesRegex(ValueError, "metadata references build source"):
            gc_build.check_metadata(self.prefix, self.root)

    def test_install_entry(self):
        for name in ("octra_node.opam", "octra_node.opam.locked"):
            text = (gc_build.ROOT / name).read_text()
            self.assertIn('"controls/lib/gc_build.py"', text)
            self.assertLess(text.index('"controls/lib/gc_build.py"'), text.index('"dune"\n    "build"'))

if __name__ == "__main__":
    unittest.main()