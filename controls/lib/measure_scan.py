# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import re
import sys
from collections import Counter
from pathlib import Path

AREAS = ("lib", "node_runtime", "bin", "net", "circle_runtime")
MODULE = ("lib/consensus/c_measure.ml", "lib/consensus/c_measure.mli")
POLICY = ("lib/consensus/c_quorum_policy.ml", "lib/consensus/c_quorum_policy.mli")
PENDING = ("node_runtime/consensus_pending_commit_recovery.ml",)
EVERY = ()
ASKERS = MODULE + ("lib/consensus/c_types.ml", "lib/consensus/c_driver.ml")
PACKED = re.compile(r"\(\s*module\s+(?:Octra_consensus\s*\.\s*)?C_(?:quorum_policy|measure)\b")
TERM = r"(?:[A-Za-z_][\w'.]*|\((?:[^()]|\([^()]*\))*\))"
LENGTH = rf"\bList\s*\.\s*length\s+(?:!\s*)?{TERM}"
THREE = r"\(\s*(?:Z\s*\.\s*)?of_int\s+3\s*\)"
THRESHOLD = (r"(?:[34]\b|(?:[A-Za-z_][\w']*\.)*"
    r"(?:quorum(?:_weight)?|required(?:_[A-Za-z0-9_]+)?|threshold|cap|committee_size"
    r"|(?:max|min)_(?:members|validators|signers|votes|seats)"
    r"|[A-Za-z_][\w']*_(?:threshold|quorum|floor|cap))\b)")
COMPARE = r"(?:>=|<=|<>|=|>|<)"
RULES = (
    ("seats", MODULE, EVERY, re.compile(
        r"(>=|<=|[^-]>|<)[^\"-]*\bquorum(_weight)?\b|\bquorum(_weight)?\b[ )]*(>=|<=|>|<[^-])")),
    ("weight", MODULE, EVERY, re.compile(
        r"\b(geq|gt|leq|lt)\b.*\bquorum_weight\b|\bquorum_weight\b.*\b(geq|gt|leq|lt)\b")),
    ("field", MODULE, EVERY, re.compile(r"\.(f|n|quorum|quorum_weight)\b|\{[^}]*\b(f|quorum) *[;}]")),
    ("count", MODULE, EVERY, re.compile(r"\bvalidator_count\b")),
    ("formula", MODULE, EVERY, re.compile(
        r"(?<!\+ 2\) )/ *(of_int +)?3\b|\(of_int 2\)\) *\(of_int 3\)|\* *3 *>"
        r"|\bf *\+ *[12]\b|quorum *- *1\b")),
    ("length", MODULE, EVERY, re.compile(
        rf"{LENGTH}\s*\)*\s*{COMPARE}\s*\(*\s*{THRESHOLD}"
        rf"|{THRESHOLD}\s*\)*\s*{COMPARE}\s*\(*\s*{LENGTH}")),
    ("weight_scale", MODULE, EVERY, re.compile(
        rf"\b(?:Z\s*\.\s*)?mul\b\s*(?:{THREE}|{TERM}\s+{THREE})")),
    ("rule", MODULE + POLICY, EVERY, re.compile(
        r"C_quorum_policy *\. *(select|rule|rule_at|active|linear_weight|seatless|Prior|Capped|Stake)\b")),
    ("alias", (), EVERY, re.compile(
        r"(module +[A-Z][A-Za-z0-9_]* *= *|open +|include +)(Octra_consensus\.)?"
        r"C_(quorum_policy|measure)\b|\bC_(quorum_policy|measure) *(\. *\(|\.? *$)")),
    ("ask", ASKERS, EVERY, re.compile(r"\bC_measure\b")),
    ("text", MODULE, EVERY, re.compile(r"C_measure *\. *(label|start_text|promise_text)\b")),
    ("name", (), EVERY, re.compile(
        r"\b(has_quorum_values|has_weight_quorum|has_quorum_for_rule|has_quorum|count_floor_required"
        r"|count_cap_required|required_attesters|peer_quorum|min_peers|weight_required"
        r"|count_required|count_limit|catchup_source_agreement_reached)\b")),
    ("parse", (), PENDING, re.compile(
        r"\b(int_of_string|of_string|sscanf)|String\.(equal|compare)[^;]*\blevel\b"
        r"|\blevel\b *(<>|<|>)")),
)

def refuse(reason, **fields):
    rest = " ".join(f"{key} = {value}" for key, value in fields.items())
    print(f"status = fail check = measure reason = {reason} {rest}", file = sys.stderr)

def sources(root):
    return sorted(
        path for area in AREAS for path in (root / area).rglob("*.ml*")
        if path.suffix in (".ml", ".mli"))

def found(root):
    for path in sources(root):
        name = path.relative_to(root).as_posix()
        source = path.read_text(encoding = "utf-8")
        for match in PACKED.finditer(source):
            number = source.count("\n", 0, match.start()) + 1
            yield name, " ".join(match.group().split()), number, "packed"
        for kind, owners, _, pattern in RULES:
            if kind not in ("length", "weight_scale") or name in owners:
                continue
            for match in pattern.finditer(source):
                if "\n" in match.group():
                    number = source.count("\n", 0, match.start()) + 1
                    yield name, " ".join(match.group().split()), number, kind
        for number, line in enumerate(source.splitlines(), 1):
            kinds = [
                kind for kind, owners, only, pattern in RULES
                if name not in owners and (not only or name in only) and pattern.search(line)]
            if kinds:
                yield name, " ".join(line.split()), number, ",".join(kinds)

def areas_present(root):
    absent = [area for area in AREAS if not (root / area).is_dir()]
    for area in absent:
        refuse("area", path = area)
    return not absent

def main(root, sites):
    if not areas_present(root):
        return 1
    rows = [line.split(" ", 2) for line in sites.read_text(encoding = "utf-8").splitlines()]
    invalid = any(
        len(row) != 3 or not all(row) or any("\t" in part for part in row)
        for row in rows
    )
    if invalid:
        refuse("list", file = sites.name)
        return 1
    allowed = Counter((name, text) for _, name, text in rows)
    hits = list(found(root))
    seen = Counter((name, text) for name, text, _, _ in hits)
    extra = [hit for hit in hits if seen[hit[:2]] > allowed[hit[:2]]]
    unused = [row for row in rows if allowed[tuple(row[1:])] > seen[tuple(row[1:])]]
    for name, text, number, kinds in extra:
        refuse("site", file = name, line = number, rule = kinds, text = text)
    for label, name, text in unused:
        refuse("unused", name = label, file = name, text = text)
    if extra or unused:
        return 1
    print(f"status = pass check = measure sites = {len(hits)}")
    return 0

def command(args):
    if len(args) not in (1, 2):
        refuse("arguments")
        return 2
    root = Path(args[0])
    if len(args) == 1:
        if not areas_present(root):
            return 1
        for name, text, _, kinds in found(root):
            print(f"{kinds} {name} {text}")
        return 0
    return main(root, Path(args[1]))

if __name__ == "__main__":
    sys.exit(command(sys.argv[1:]))