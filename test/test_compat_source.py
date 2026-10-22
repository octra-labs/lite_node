# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import hashlib
import pathlib
import subprocess
import unittest
from unittest import mock

from compat_role import old_source

class SourceCase(unittest.TestCase):
    def test_files(self):
        raw = b"let value = 1\n"
        digest = hashlib.sha256(raw).hexdigest()
        for data in [raw, raw[:-1]]:
            with mock.patch.object(pathlib.Path, "read_bytes", return_value = data):
                self.assertEqual(old_source(pathlib.Path("."), None, "code.ml", digest), raw)
        for data in [raw + b"\n", raw.replace(b"1", b"2"), b""]:
            with mock.patch.object(pathlib.Path, "read_bytes", return_value = data):
                with self.assertRaisesRegex(SystemExit, "old_source_hash"):
                    old_source(pathlib.Path("."), None, "code.ml", digest)

    def test_missing(self):
        for old in [None, pathlib.Path(".")]:
            with mock.patch("compat_role.subprocess.run", side_effect = FileNotFoundError), \
                    mock.patch.object(pathlib.Path, "read_bytes", side_effect = FileNotFoundError):
                with self.assertRaisesRegex(SystemExit, "old_source_missing"):
                    old_source(old, pathlib.Path("."), "code.ml", "0" * 64)

    def test_public_git(self):
        raw = b"let value = 1"
        digest = hashlib.sha256(raw).hexdigest()
        with mock.patch("compat_role.subprocess.run",
                return_value = subprocess.CompletedProcess([], 0, raw)) as run:
            self.assertEqual(old_source(None, pathlib.Path("."), "code.ml", digest), raw)
        self.assertEqual(run.call_args.args[0],
            ["git", "show", "7f9be4489870645cfcc95439638cde40f5da9f50:code.ml"])

if __name__ == "__main__":
    unittest.main()