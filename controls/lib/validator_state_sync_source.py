# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import re
import subprocess
import sys
from pathlib import Path

from validator_common import ValidatorError
from validator_common import exporter_entries
from validator_common import parse_env
from validator_common import validator_entries
from validator_common import write_env

SOURCE = Path(__file__).resolve()
ROOT = SOURCE.parents[2] if SOURCE.parents[1].name == "controls" else SOURCE.parents[3]
DEFAULT_CONFIG = ROOT / ".keys/validator/node.env"
ARTIFACT_MANIFEST = ROOT / "artifacts/octra_state_sync_manifest.exe"
DEFAULT_MANIFEST = (
    ARTIFACT_MANIFEST
    if ARTIFACT_MANIFEST.is_file()
    else ROOT / "_build/default/bin/octra_state_sync_manifest.exe"
)
HEX64 = re.compile(r"[0-9a-f]{64}")
SOURCE_KEYS = (
    "OCTRA_STATE_SYNC_ENABLE",
    "OCTRA_STATE_SYNC_CERT",
    "OCTRA_STATE_SYNC_SNAPSHOT_DIR",
)

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def read_json(path, limit):
    target = Path(path)
    if not target.is_file():
        raise ValidatorError(f"JSON file is missing: {target}")
    if target.stat().st_size > limit:
        raise ValidatorError(f"JSON file exceeds size limit: {target}")
    try:
        return json.loads(target.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValidatorError(f"invalid JSON file: {target}") from error

def object_field(value, name):
    field = value.get(name) if isinstance(value, dict) else None
    if not isinstance(field, dict):
        raise ValidatorError(f"certificate has invalid {name}")
    return field

def string_field(value, name):
    field = value.get(name) if isinstance(value, dict) else None
    if not isinstance(field, str) or not field:
        raise ValidatorError(f"certificate has invalid {name}")
    return field

def integer_field(value, name):
    field = value.get(name) if isinstance(value, dict) else None
    try:
        result = int(field)
    except (TypeError, ValueError) as error:
        raise ValidatorError(f"certificate has invalid {name}") from error
    if result < 0:
        raise ValidatorError(f"certificate has invalid {name}")
    return result

def verify_ready_snapshot(certificate, snapshot_root, values):
    checkpoint = object_field(certificate, "checkpoint")
    manifest = object_field(certificate, "manifest")
    checkpoint_hash = string_field(certificate, "checkpoint_hash")
    manifest_hash = string_field(certificate, "manifest_hash")
    snapshot_id = string_field(manifest, "snapshot_id")
    if not HEX64.fullmatch(checkpoint_hash):
        raise ValidatorError("certificate checkpoint hash is invalid")
    if not HEX64.fullmatch(manifest_hash):
        raise ValidatorError("certificate manifest hash is invalid")
    if snapshot_id != checkpoint_hash:
        raise ValidatorError("snapshot id does not match checkpoint hash")
    if string_field(manifest, "checkpoint_hash") != checkpoint_hash:
        raise ValidatorError("manifest does not match checkpoint hash")
    if string_field(checkpoint, "chain_id") != values["OCTRA_CHAIN_ID"]:
        raise ValidatorError("checkpoint chain does not match node configuration")
    if string_field(checkpoint, "config_hash") != values["OCTRA_CONSENSUS_CONFIG_HASH"]:
        raise ValidatorError("checkpoint configuration does not match node configuration")
    epoch = integer_field(checkpoint, "epoch")
    if epoch < int(values["OCTRA_CHECKPOINT_EPOCH"]):
        raise ValidatorError("checkpoint is older than the network minimum")
    state_root = string_field(checkpoint, "state_root")
    txid_hi = string_field(checkpoint, "txid_hi")
    root = Path(snapshot_root).expanduser().resolve()
    if not root.is_dir():
        raise ValidatorError(f"snapshot root is missing: {root}")
    snapshot = root / snapshot_id
    if not snapshot.is_dir():
        raise ValidatorError(f"snapshot is missing: {snapshot}")
    ready = read_json(snapshot / ".ready.json", 16_384)
    head = read_json(snapshot / "HEAD.json", 16_384)
    if ready.get("version") != "octra-state-sync-snapshot-ready":
        raise ValidatorError("snapshot ready marker version is invalid")
    if integer_field(ready, "head_epoch") != epoch:
        raise ValidatorError("snapshot ready marker epoch does not match certificate")
    if string_field(ready, "state_root") != state_root:
        raise ValidatorError("snapshot ready marker root does not match certificate")
    if integer_field(head, "epoch_id") != epoch:
        raise ValidatorError("snapshot head epoch does not match certificate")
    if string_field(head, "state_root") != state_root:
        raise ValidatorError("snapshot head root does not match certificate")
    if string_field(head, "txid_hi") != txid_hi:
        raise ValidatorError("snapshot head transaction id does not match certificate")
    if string_field(head, "commit_id") != string_field(ready, "commit_id"):
        raise ValidatorError("snapshot ready marker commit does not match head")
    return {
        "epoch": epoch,
        "manifest": manifest_hash,
        "snapshot": snapshot,
    }

def verify_certificate(binary, certificate, values):
    target = Path(binary).expanduser().resolve()
    if not target.is_file():
        raise ValidatorError(f"state sync manifest binary is missing: {target}")
    command = [
        str(target),
        "--command",
        "verify",
        "--certificate",
        str(certificate),
    ]
    for address, pubkey in validator_entries(values["OCTRA_VALIDATORS"]):
        command.extend(["--validator", f"{address}:{pubkey}"])
    for address, pubkey in exporter_entries(values["OCTRA_STATE_SYNC_EXPORTERS"]):
        command.extend(["--exporter", f"{address}:{pubkey}"])
    subprocess.run(command, cwd=ROOT, check=True)

def enable_source(config, certificate, snapshot_root, manifest_binary):
    config_path = Path(config).expanduser().resolve()
    values = parse_env(config_path)
    certificate_path = Path(certificate).expanduser().resolve()
    payload = read_json(certificate_path, 64 * 1024 * 1024)
    result = verify_ready_snapshot(payload, snapshot_root, values)
    verify_certificate(manifest_binary, certificate_path, values)
    values["OCTRA_STATE_SYNC_ENABLE"] = "1"
    values["OCTRA_STATE_SYNC_CERT"] = str(certificate_path)
    values["OCTRA_STATE_SYNC_SNAPSHOT_DIR"] = str(
        Path(snapshot_root).expanduser().resolve()
    )
    write_env(config_path, values)
    emit(
        event="state_sync_source_configured",
        epoch=result["epoch"],
        manifest=result["manifest"],
        snapshot=result["snapshot"],
    )

def disable_source(config):
    config_path = Path(config).expanduser().resolve()
    values = parse_env(config_path)
    for key in SOURCE_KEYS:
        values.pop(key, None)
    write_env(config_path, values)
    emit(event="state_sync_source_disabled")

def parser():
    value = argparse.ArgumentParser(prog="state_sync_source.sh")
    value.add_argument("--config", default=str(DEFAULT_CONFIG))
    commands = value.add_subparsers(dest="command", required=True)
    enable = commands.add_parser("enable")
    enable.add_argument("--certificate", required=True)
    enable.add_argument("--manifest-binary", default=str(DEFAULT_MANIFEST))
    enable.add_argument("--snapshot-dir", required=True)
    commands.add_parser("disable")
    return value

def main():
    args = parser().parse_args()
    if args.command == "enable":
        enable_source(
            args.config,
            args.certificate,
            args.snapshot_dir,
            args.manifest_binary,
        )
    else:
        disable_source(args.config)

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, subprocess.CalledProcessError, ValidatorError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)