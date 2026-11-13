# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import hashlib
import json
import shutil
import subprocess
import tarfile
from pathlib import Path, PurePosixPath

CONTROL = Path(__file__).resolve().parent.parent

def digest(path):
    with Path(path).open("rb") as stream:
        value = hashlib.sha256()
        for part in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(part)
        return value.hexdigest()

def extract(archive, root, name):
    with tarfile.open(archive, "r:bz2") as stream:
        members = stream.getmembers()
        for item in members:
            parts = PurePosixPath(item.name).parts
            if not parts or parts[0] != name or ".." in parts:
                raise ValueError("storage archive path differs")
            if not item.isfile() and not item.isdir():
                raise ValueError("storage archive type differs")
        for item in members:
            path = root.joinpath(*PurePosixPath(item.name).parts[1:])
            if item.isdir():
                path.mkdir(parents = True, exist_ok = True)
            else:
                path.parent.mkdir(parents = True, exist_ok = True)
                with stream.extractfile(item) as source, path.open("xb") as target:
                    shutil.copyfileobj(source, target)
                path.chmod(item.mode & 0o755)

def prepare(irmin, index, output, control = CONTROL):
    manifest = json.loads((control / "gc_sources.json").read_text())
    inputs = {"irmin": Path(irmin), "index": Path(index)}
    for name, path in inputs.items():
        record = manifest[name]
        if digest(path) != record["archive_sha256"]:
            raise ValueError("storage archive digest differs: " + name)
        if digest(control / record["patch"]) != record["patch_sha256"]:
            raise ValueError("storage patch digest differs: " + name)
    output = Path(output)
    output.mkdir()
    for name, path in inputs.items():
        record = manifest[name]
        root = output if name == "irmin" else output / "vendors/index"
        extract(path, root, record["archive_root"])
        subprocess.run(["patch", "--batch", "--forward", "-p1", "-i",
            str((control / record["patch"]).resolve())], cwd = root, check = True)
        for file, expected in record["files"].items():
            if digest(root / file) != expected:
                raise ValueError("storage source digest differs: " + file)
    return output

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--irmin", required = True)
    parser.add_argument("--index", required = True)
    parser.add_argument("--output", required = True)
    args = parser.parse_args()
    output = prepare(args.irmin, args.index, args.output)
    print(f"event = storage_source status = checked path = {output}")

if __name__ == "__main__":
    main()