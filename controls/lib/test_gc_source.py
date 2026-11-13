# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import hashlib
import io
import json
import os
import shutil
import tarfile
import unittest
from pathlib import Path
from unittest import mock

from gc_source import prepare

class SourceCases(unittest.TestCase):
    def setUp(self):
        self.root = Path(__file__).resolve().parents[2] / "runtime_data/gc_source_checks"
        self.root = self.root / f"{os.getpid()}-{self._testMethodName}"
        self.root.mkdir(parents = True)
        self.output = self.root / "source"
        self.manifest = {}
        for name in ("irmin", "index"):
            self.archive(name, name + "/a.ml")
            patch = self.root / (name + ".patch")
            patch.write_text("--- a/a.ml\n+++ b/a.ml\n@@ -1 +1 @@\n-let x = 1\n+let x = 2\n")
            self.manifest[name] = {
                "archive_root": name,
                "archive_sha256": self.hash(self.root / (name + ".tbz")),
                "patch": patch.name,
                "patch_sha256": self.hash(patch),
                "files": {"a.ml": hashlib.sha256(b"let x = 2\n").hexdigest()},
            }
        self.save()

    def tearDown(self):
        shutil.rmtree(self.root)

    def hash(self, path):
        return hashlib.sha256(path.read_bytes()).hexdigest()

    def archive(self, name, member, link = False):
        with tarfile.open(self.root / (name + ".tbz"), "w:bz2") as stream:
            item = tarfile.TarInfo(member)
            if link:
                item.type = tarfile.SYMTYPE
                item.linkname = "../outside"
                stream.addfile(item)
            else:
                data = b"let x = 1\n"
                item.size = len(data)
                stream.addfile(item, io.BytesIO(data))

    def save(self):
        (self.root / "gc_sources.json").write_text(json.dumps(self.manifest))

    def run_source(self):
        return prepare(self.root / "irmin.tbz", self.root / "index.tbz",
            self.output, self.root)

    def test_apply(self):
        self.run_source()
        self.assertEqual((self.output / "a.ml").read_bytes(), b"let x = 2\n")
        self.assertEqual((self.output / "vendors/index/a.ml").read_bytes(), b"let x = 2\n")

    def test_archive_digest(self):
        (self.root / "index.tbz").write_bytes(b"wrong")
        with self.assertRaisesRegex(ValueError, "archive digest differs"):
            self.run_source()
        self.assertFalse(self.output.exists())

    def test_patch_digest(self):
        (self.root / "irmin.patch").write_text("wrong")
        with self.assertRaisesRegex(ValueError, "patch digest differs"):
            self.run_source()
        self.assertFalse(self.output.exists())

    def test_preserve_output(self):
        self.output.mkdir()
        (self.output / "keep").write_bytes(b"evidence")
        with self.assertRaises(FileExistsError):
            self.run_source()
        self.assertEqual((self.output / "keep").read_bytes(), b"evidence")

    def test_member_path(self):
        self.archive("irmin", "irmin/../outside")
        self.manifest["irmin"]["archive_sha256"] = self.hash(self.root / "irmin.tbz")
        self.save()
        with self.assertRaisesRegex(ValueError, "archive path differs"):
            self.run_source()
        self.assertFalse((self.root / "outside").exists())

    def test_member_link(self):
        self.archive("irmin", "irmin/a.ml", link = True)
        self.manifest["irmin"]["archive_sha256"] = self.hash(self.root / "irmin.tbz")
        self.save()
        with self.assertRaisesRegex(ValueError, "archive type differs"):
            self.run_source()

    def test_skipped_patch(self):
        with mock.patch("gc_source.subprocess.run"):
            with self.assertRaisesRegex(ValueError, "source digest differs"):
                self.run_source()

if __name__ == "__main__":
    unittest.main()