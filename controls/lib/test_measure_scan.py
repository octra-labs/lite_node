# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import unittest
import uuid

ROOT = Path(__file__).resolve().parents[2]
AREAS = ("lib", "node_runtime", "bin", "net", "circle_runtime")

class MeasureTests(unittest.TestCase):
    def setUp(self):
        self.work = ROOT / "runtime_data/measure_gate" / uuid.uuid4().hex
        (self.work / "controls/lib").mkdir(parents = True)
        for area in AREAS:
            (self.work / area).mkdir()
        for name in ("measure_gate.sh", "lib/measure_scan.py"):
            shutil.copyfile(ROOT / "controls" / name, self.work / "controls" / name)
        (self.work / "controls/measure_sites.txt").write_text("", encoding = "utf-8")
        self.index = 0

    def run_case(self, args, code, reason):
        self.index += 1
        result = subprocess.run(args, cwd = self.work, capture_output = True, text = True,
            env = {**os.environ, "PYTHONDONTWRITEBYTECODE": "1"}, check = False)
        (self.work / f"run_{self.index}.json").write_text(json.dumps({
            "args": args, "exit": result.returncode, "stdout": result.stdout,
            "stderr": result.stderr}, indent = 2) + "\n", encoding = "utf-8")
        self.assertEqual(result.returncode, code, result.stdout + result.stderr)
        self.assertIn(reason, result.stdout + result.stderr)

    def gate(self, code, reason):
        self.run_case(["sh", "controls/measure_gate.sh"], code, reason)

    def test_empty(self):
        self.gate(0, "status = pass")

    def test_seatless(self):
        for source in ("let active = C_quorum_policy.seatless policy epoch\n",
            "let active = Octra_consensus.C_quorum_policy.seatless policy epoch\n"):
            with self.subTest(source = source):
                (self.work / "node_runtime/entry.ml").write_text(source, encoding = "utf-8")
                self.gate(1, "rule = rule")

    def test_count_fields(self):
        for source in ("let enough = count >= vs.n\n",
            "let enough = count >= validator_count\n",
            "let enough = validator_count <= count\n"):
            with self.subTest(source = source):
                (self.work / "node_runtime/entry.ml").write_text(source, encoding = "utf-8")
                self.gate(1, "reason = site")

    def test_packed(self):
        for number, source in enumerate((
            "let held = (module C_quorum_policy)\n",
            "let held = (module Octra_consensus.C_quorum_policy)\n",
            "let held = (\nmodule\nC_quorum_policy)\n",
            "let held = (module C_measure)\n",
        )):
            with self.subTest(number = number):
                (self.work / "node_runtime/entry.ml").write_text(source, encoding = "utf-8")
                self.gate(1, "rule = packed")

    def test_length_limits(self):
        for source in (
            "let enough = List.length members >= required\n",
            "let enough = required <= List.length members\n",
            "if List.length members < 4 then refuse ()\n",
            "if List.length witnesses > cfg.max_members then refuse ()\n",
            "if List.length (members state) >= seat_cap then refuse ()\n",
            "if (List.length members) <> committee_size then refuse ()\n",
            "if List.length votes >= signer_floor then refuse ()\n",
            "if 4 < List.length members then refuse ()\n",
            "if List.length members\n  < 4 then refuse ()\n",
            "if List.length members < (4) then refuse ()\n",
            "if (4) > (List.length members) then refuse ()\n",
            "if List.length members >= (required) then accept ()\n",
            "if List.length (List.filter\n  is_live members) < 4 then refuse ()\n",
        ):
            with self.subTest(source = source):
                (self.work / "node_runtime/entry.ml").write_text(source, encoding = "utf-8")
                self.gate(1, "rule = length")

    def test_weight_scale(self):
        for source in (
            "let excess = Z.sub (Z.mul (Z.of_int 3) member.weight) total\n",
            "let excess = Z.(sub (mul (of_int 3) member.weight) total)\n",
            "let excess = Z.mul\n  (Z.of_int 3) member.weight\n",
            "let excess = Z.sub (Z.mul member.weight (Z.of_int 3)) total\n",
            "let excess = Z.(sub (mul weight (of_int 3)) total)\n",
        ):
            with self.subTest(source = source):
                (self.work / "lib/entry.ml").write_text(source, encoding = "utf-8")
                self.gate(1, "rule = weight_scale")

    def test_entry_rules(self):
        for source, rule in (
            ("if List.length after < 4 then Ok ()\n", "length"),
            ("(List.length before < 4 ||\n", "length"),
            ("if List.length validators < 4 then\n", "length"),
            ("let excess = Z.sub (Z.mul (Z.of_int 3) member.weight) after_total in\n", "weight_scale"),
            ("| Some old -> Z.gt excess (Z.sub (Z.mul (Z.of_int 3) old.weight) before_total))\n", "weight_scale"),
        ):
            with self.subTest(source = source):
                (self.work / "lib/core").mkdir(exist_ok = True)
                (self.work / "lib/core/validator_entry.ml").write_text(source, encoding = "utf-8")
                self.gate(1, "rule = " + rule)

    def test_length_inventory(self):
        source = "let accepted = List.length members >= 4\n"
        path = self.work / "node_runtime/entry.ml"
        path.write_text(source, encoding = "utf-8")
        (self.work / "controls/measure_sites.txt").write_text(
            "set_count node_runtime/entry.ml " + source, encoding = "utf-8")
        self.gate(0, "sites = 1")
        path.write_text(source.replace(">= 4", ">= 3"), encoding = "utf-8")
        self.gate(1, "reason = site")

    def test_scale_inventory(self):
        source = "let excess = Z.sub (Z.mul (Z.of_int 3) member.weight) total\n"
        path = self.work / "lib/entry.ml"
        path.write_text(source, encoding = "utf-8")
        (self.work / "controls/measure_sites.txt").write_text(
            "seat_weight lib/entry.ml " + source, encoding = "utf-8")
        self.gate(0, "sites = 1")
        path.write_text(source.replace("of_int 3", "of_int 4"), encoding = "utf-8")
        self.gate(1, "reason = unused")

    def test_length_values(self):
        (self.work / "node_runtime/entry.ml").write_text(
            "let count = List.length members\n"
            "let same = List.length actual = List.length expected\n", encoding = "utf-8")
        self.gate(0, "sites = 0")

    def test_site_scope(self):
        source = "if List.length members < 4 then refuse ()\n"
        path = self.work / "lib/entry.ml"
        path.write_text(source, encoding = "utf-8")
        (self.work / "controls/measure_sites.txt").write_text(
            "set_count lib/entry.ml " + source, encoding = "utf-8")
        self.gate(0, "sites = 1")
        path.write_text(source * 2, encoding = "utf-8")
        self.gate(1, "reason = site")
        path.write_text(source, encoding = "utf-8")
        (self.work / "node_runtime/entry.ml").write_text(source, encoding = "utf-8")
        self.gate(1, "file = node_runtime/entry.ml")

    def test_list_missing(self):
        (self.work / "net").rmdir()
        self.run_case([sys.executable, "controls/lib/measure_scan.py", "."], 1, "reason = area")

    def test_gate_missing(self):
        (self.work / "net").rmdir()
        self.gate(1, "reason = area")

    def test_arguments(self):
        self.run_case([sys.executable, "controls/lib/measure_scan.py"], 2, "reason = arguments")

    def test_list_format(self):
        (self.work / "node_runtime/entry.ml").write_text(
            "let count = vs.quorum\n", encoding = "utf-8")
        for row in (
            "printed",
            "printed node_runtime/entry.ml",
            "printed  node_runtime/entry.ml let count = vs.quorum",
            " printed node_runtime/entry.ml let count = vs.quorum",
            "printed\tnode_runtime/entry.ml\tlet count = vs.quorum",
            "printed node_runtime/entry.ml let count\t= vs.quorum",
        ):
            with self.subTest(row = row):
                (self.work / "controls/measure_sites.txt").write_text(
                    row + "\n", encoding = "utf-8")
                self.gate(1, "reason = list")

    def test_output_format(self):
        (self.work / "node_runtime/entry.ml").write_text(
            "let count = vs.quorum\n", encoding = "utf-8")
        self.run_case(
            [sys.executable, "controls/lib/measure_scan.py", "."],
            0, "field node_runtime/entry.ml let count = vs.quorum\n"
        )

    def test_allowed(self):
        (self.work / "node_runtime/entry.ml").write_text("let count = vs.quorum\n", encoding = "utf-8")
        (self.work / "controls/measure_sites.txt").write_text(
            "printed node_runtime/entry.ml let count = vs.quorum\n", encoding = "utf-8")
        self.gate(0, "sites = 1")
        (self.work / "node_runtime/entry.ml").write_text("", encoding = "utf-8")
        self.gate(1, "reason = unused")

if __name__ == "__main__":
    unittest.main()