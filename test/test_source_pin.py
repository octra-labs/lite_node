# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import os
import shutil
import subprocess
import sys
import unittest
import uuid
from pathlib import Path
from unittest.mock import patch

class SourcePin(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = Path(sys.argv[1]).resolve().parent
        root = next(path for path in cls.source.parents
            if (path / "SOURCE_COMMIT").is_file() and (path / "dune-project").is_file())
        cls.work = root / "runtime_data" / ("source-pin-" + uuid.uuid4().hex)
        cls.work.mkdir(parents = True)

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.work)

    def case(self, name):
        root = self.work / name
        target = root / "lib" / "vm"
        shutil.copytree(self.source, target,
            copy_function = shutil.copyfile,
            ignore = shutil.ignore_patterns("source_stamp.ml", ".octra_vm.objs", "*.a", "*.cma", "*.cmxa", "*.cmxs"))
        (root / "dune-project").write_text("(lang dune 3.0)\n", encoding = "ascii")
        rule = (self.source / "dune").read_text(encoding = "utf-8").split("\n(library\n", 1)[0]
        (target / "dune").write_text(rule, encoding = "utf-8")
        return root, target

    def build(self, root):
        result = subprocess.run(["dune", "build", "--root", str(root), "--build-dir", "_build",
            "lib/vm/source_stamp.ml"], cwd = root, capture_output = True,
            text = True, timeout = 45)
        return result, root / "_build" / "default" / "lib" / "vm" / "source_stamp.ml"

    def refused(self, root):
        result, _ = self.build(root)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("source compiler content differs", result.stderr)

    def test_export(self):
        root, target = self.case("export")
        result, stamp = self.build(root)
        self.assertEqual(result.returncode, 0, result.stderr)
        expected = stamp.read_bytes()
        header = b"(* SPDX-License-Identifier: BSD-3-Clause *)\n(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)\n\n"
        for name in ("compiler", "aml", "policy", "wire", "runtime"):
            for path in (target / name).rglob("*"):
                if path.is_file() and path.suffix in (".ml", ".mli"):
                    path.write_bytes(header + path.read_bytes().removeprefix(header).rstrip(b"\n"))
        result, stamp = self.build(root)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(stamp.read_bytes(), expected)

    def test_build_dir(self):
        root, _ = self.case("build-dir")
        outside = root / "_build_external"
        with patch.dict(os.environ, {"DUNE_BUILD_DIR": str(outside)}):
            result, stamp = self.build(root)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(stamp.is_file())
        self.assertFalse(outside.exists())

    def test_edit(self):
        root, target = self.case("edit")
        path = target / "runtime" / "program_record.ml"
        body = path.read_bytes()
        self.assertIn(b"base_200000", body)
        path.write_bytes(body.replace(b"base_200000", b"base_200001", 1))
        self.refused(root)

    def test_removed(self):
        root, target = self.case("removed")
        (target / "runtime" / "program_record.ml").unlink()
        self.refused(root)

    def test_added(self):
        root, target = self.case("added")
        (target / "runtime" / "extra.ml").write_text("let value = 1\n", encoding = "ascii")
        self.refused(root)

    def test_renamed(self):
        root, target = self.case("renamed")
        path = target / "runtime" / "program_record.ml"
        path.rename(path.with_name("record_source.ml"))
        self.refused(root)

if __name__ == "__main__":
    if len(sys.argv) != 2 or not Path(sys.argv[1]).is_file():
        raise SystemExit("source pin input required")
    unittest.main(argv = [sys.argv[0]])