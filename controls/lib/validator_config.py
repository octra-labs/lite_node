# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import importlib.util
import json
import os
import pwd
import shutil
import socket
import subprocess
import sys
from pathlib import Path

from validator_common import ENDPOINT
from validator_common import NAME
from validator_common import ValidatorError
from validator_common import copy_private
from validator_common import ensure_wallet
from validator_common import exporter_entries
from validator_common import load_wallet
from validator_common import load_network
from validator_common import parse_env
from validator_common import read_digest
from validator_common import rpc_url
from validator_common import sha256_file
from validator_common import state_ready
from validator_common import state_sync_sources
from validator_common import validate_checkpoint
from validator_common import validator_entries
from validator_common import write_env
from validator_enroll import membership

SOURCE = Path(__file__).resolve()
ROOT = SOURCE.parents[2] if SOURCE.parents[1].name == "controls" else SOURCE.parents[3]
ARTIFACT_BINARY = ROOT / "artifacts/octra_node.exe"
ARTIFACT_WORKER = ROOT / "artifacts/octra_pvac_worker.exe"
ARTIFACT_SYNC_BINARY = ROOT / "artifacts/octra_state_sync_client.exe"
ARTIFACT_CONTROL_BINARY = ROOT / "artifacts/bft_control_tx.exe"
DEFAULT_BINARY = (
    ARTIFACT_BINARY
    if ARTIFACT_BINARY.is_file()
    else ROOT / "_build/default/bin/octra_node.exe"
)
DEFAULT_SYNC_BINARY = (
    ARTIFACT_SYNC_BINARY
    if ARTIFACT_SYNC_BINARY.is_file()
    else ROOT / "_build/default/bin/octra_state_sync_client.exe"
)
DEFAULT_WORKER = (
    ARTIFACT_WORKER
    if ARTIFACT_WORKER.is_file()
    else ROOT / "_build/default/bin/octra_pvac_worker.exe"
)
DEFAULT_CONTROL_BINARY = (
    ARTIFACT_CONTROL_BINARY
    if ARTIFACT_CONTROL_BINARY.is_file()
    else ROOT / "_build/default/bin/bft_control_tx.exe"
)
DEFAULT_CONFIG = ROOT / ".keys/validator/node.env"
DEFAULT_DATA = ROOT / "data"
IDENTITY_WALLET = ROOT / ".keys/validator/wallet.json"
RUST_TOOLCHAIN = "1.80.1"
TOOLCHAIN_ROOT = ROOT / "runtime_data/toolchains"
BUILD_WORK = TOOLCHAIN_ROOT / "build"
CARGO_HOME = TOOLCHAIN_ROOT / "cargo"
RUSTUP_HOME = TOOLCHAIN_ROOT / "rustup"
OPAM_SWITCH = TOOLCHAIN_ROOT / "ocaml"

def operator_pm2_name(name):
    return name if name.startswith("octra-") else f"octra-{name}"

def runtime_binding(config):
    key_dir = ROOT / ".keys/validator"
    return {
        "OCTRA_BINARY_HASH": sha256_file(DEFAULT_BINARY),
        "OCTRA_OPERATOR_BINARY": str(DEFAULT_BINARY.resolve()),
        "OCTRA_OPERATOR_CONFIG": str(Path(config).resolve()),
        "OCTRA_OPERATOR_CONTROL_BINARY": str(DEFAULT_CONTROL_BINARY.resolve()),
        "OCTRA_OPERATOR_CONTROL_BINARY_HASH": sha256_file(DEFAULT_CONTROL_BINARY),
        "OCTRA_OPERATOR_LOG_DIR": str((ROOT / "data/operator_logs").resolve()),
        "OCTRA_OPERATOR_NETWORK_BUNDLE": str((key_dir / "network.env").resolve()),
        "OCTRA_OPERATOR_SYNC_BINARY": str(DEFAULT_SYNC_BINARY.resolve()),
        "OCTRA_OPERATOR_SYNC_BINARY_HASH": sha256_file(DEFAULT_SYNC_BINARY),
        "OCTRA_PVAC_VERIFY_WORKER": str(DEFAULT_WORKER.resolve()),
        "OCTRA_PVAC_VERIFY_WORKER_HASH": sha256_file(DEFAULT_WORKER),
    }

def require_runtime_files():
    files = [
        DEFAULT_BINARY,
        DEFAULT_CONTROL_BINARY,
        DEFAULT_SYNC_BINARY,
        DEFAULT_WORKER,
    ]
    missing = [str(path) for path in files if not path.is_file()]
    if missing:
        raise ValidatorError("candidate files are missing: " + ",".join(missing))

def require_network_binding(values):
    expected = values.get("OCTRA_OPERATOR_NETWORK_SHA256", "")
    packaged = ROOT / "config/network.env"
    installed = ROOT / ".keys/validator/network.env"
    if not expected:
        raise ValidatorError("network hash is missing from node config")
    for path in (packaged, installed):
        if not path.is_file() or sha256_file(path) != expected:
            raise ValidatorError(f"candidate network bundle mismatch: {path}")

def require_runtime_binding(config):
    values = parse_env(config)
    require_runtime_files()
    require_network_binding(values)
    expected = runtime_binding(config)
    stale = [key for key, value in expected.items() if values.get(key) != value]
    if stale:
        raise ValidatorError(
            "candidate paths are stale; run controls/run.sh --rebind-runtime: "
            + ",".join(stale)
        )

def rebind_runtime(config):
    values = parse_env(config)
    require_runtime_files()
    require_network_binding(values)
    write_env(config, {**values, **runtime_binding(config)})
    require_runtime_binding(config)
    emit(event="runtime_binding", status="ready", root=ROOT)

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def ask(label, default=None):
    suffix = f" [{default}]" if default is not None else ""
    value = input(f"{label}{suffix}: ").strip()
    return value if value else default

def ask_bool(label, default):
    marker = "Y/n" if default else "y/N"
    value = input(f"{label} [{marker}]: ").strip().lower()
    if not value:
        return default
    if value in {"y", "yes"}:
        return True
    if value in {"n", "no"}:
        return False
    raise ValidatorError(f"invalid yes or no answer: {value}")

def run(command, cwd=ROOT, env=None):
    emit(event="run", command=command[0])
    subprocess.run(command, cwd=cwd, check=True, env=env)

def elevated(command):
    if os.geteuid() == 0:
        return command
    if shutil.which("sudo") is None:
        raise ValidatorError("sudo is required for package installation")
    return ["sudo", *command]

def ubuntu_host():
    path = Path("/etc/os-release")
    if not path.is_file():
        return False
    text = path.read_text(encoding="utf-8")
    return "ID=ubuntu" in text or "ID=debian" in text

def pm2_service_enabled(user):
    result = subprocess.run(
        ["systemctl", "is-enabled", "--quiet", f"pm2-{user}.service"],
        cwd=ROOT,
        check=False,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return result.returncode == 0

def install_runtime():
    if not ubuntu_host():
        raise ValidatorError("automatic installation supports Ubuntu and Debian")
    packages = [
        "build-essential",
        "ca-certificates",
        "cargo",
        "curl",
        "git",
        "libev-dev",
        "libgmp-dev",
        "liblmdb-dev",
        "libsqlite3-dev",
        "m4",
        "nodejs",
        "npm",
        "opam",
        "pkg-config",
        "python3",
        "python3-nacl",
        "rustc",
    ]
    run(elevated(["apt-get", "update"]), cwd=Path("/"))
    run(elevated(["apt-get", "install", "-y", *packages]), cwd=Path("/"))
    install_rust_toolchain()
    if shutil.which("pm2") is None:
        run(elevated(["npm", "install", "-g", "pm2"]), cwd=ROOT)
    user = os.environ.get("SUDO_USER") or os.environ.get("USER")
    if not user:
        raise ValidatorError("cannot determine PM2 service user")
    home = pwd.getpwnam(user).pw_dir
    if not pm2_service_enabled(user):
        run(elevated([
            "env",
            f"PATH={os.environ.get('PATH', '')}",
            "pm2",
            "startup",
            "systemd",
            "-u",
            user,
            "--hp",
            home,
        ]), cwd=ROOT)

def missing_runtime():
    commands = ["curl", "pm2", "python3"]
    missing = [command for command in commands if shutil.which(command) is None]
    if importlib.util.find_spec("nacl") is None:
        missing.append("python3-nacl")
    return missing

def tool_version(raw):
    fields = raw.strip().split()
    if len(fields) < 2:
        raise ValidatorError("invalid tool version")
    try:
        parts = tuple(int(value) for value in fields[1].split("."))
    except ValueError as error:
        raise ValidatorError("invalid tool version") from error
    if len(parts) < 2:
        raise ValidatorError("invalid tool version")
    return (parts + (0, 0, 0))[:3]

def rust_environment():
    BUILD_WORK.mkdir(parents=True, exist_ok=True)
    cargo_bin = str(CARGO_HOME / "bin")
    environment = {
        **os.environ,
        "CARGO_HOME": str(CARGO_HOME),
        "PATH": os.pathsep.join([cargo_bin, os.environ.get("PATH", "")]),
        "RUSTUP_HOME": str(RUSTUP_HOME),
    }
    environment["T" + "MPDIR"] = str(BUILD_WORK)
    return environment

def install_rust_toolchain():
    environment = rust_environment()
    rustc = shutil.which("rustc", path=environment["PATH"])
    if rustc is not None:
        version = subprocess.run(
            [rustc, "--version"],
            check=True,
            capture_output=True,
            text=True,
            env=environment,
        ).stdout
        if tool_version(version) >= (1, 80, 0):
            return
    stage = ROOT / "runtime_data/rustup"
    stage.mkdir(parents=True, exist_ok=True)
    installer = stage / "rustup-init.sh"
    run([
        "curl",
        "--proto",
        "=https",
        "--tlsv1.2",
        "--fail",
        "--location",
        "--silent",
        "--show-error",
        "https://sh.rustup.rs",
        "--output",
        str(installer),
    ], env=environment)
    run([
        "sh",
        str(installer),
        "-y",
        "--no-modify-path",
        "--profile",
        "minimal",
        "--default-toolchain",
        RUST_TOOLCHAIN,
    ], env=environment)

def ensure_build_toolchain():
    environment = rust_environment()
    commands = ["cargo", "g++", "make", "opam", "rustc"]
    missing = [
        command
        for command in commands
        if shutil.which(command, path=environment["PATH"]) is None
    ]
    if missing:
        raise ValidatorError("missing build tools: " + ",".join(missing))
    rustc = subprocess.run(
        ["rustc", "--version"],
        check=True,
        capture_output=True,
        text=True,
        env=environment,
    ).stdout
    if tool_version(rustc) < (1, 80, 0):
        raise ValidatorError("source build requires rustc 1.80 or newer")
    return environment

def bind_build_toolchains(environment, switch):
    updates = [
        "setenv=" + f'CARGO_HOME="{CARGO_HOME}"',
        "setenv+=" + f'RUSTUP_HOME="{RUSTUP_HOME}"',
        "setenv+=" + f'PATH+="{CARGO_HOME / "bin"}"',
    ]
    for update in updates:
        run(
            ["opam", "option", "--switch", switch, update],
            env=environment,
        )
    environment_cache = OPAM_SWITCH / "_opam/.opam-switch/environment"
    environment_cache.unlink(missing_ok=True)

def build_candidate():
    if sys.platform != "linux" or os.uname().machine not in {"amd64", "x86_64"}:
        raise ValidatorError("source build requires Linux x86_64")
    locked = ROOT / "octra_node.opam.locked"
    if not locked.is_file():
        raise ValidatorError("source build lock is missing")
    environment = ensure_build_toolchain()
    switch = str(OPAM_SWITCH)
    run(
        [
            "opam",
            "init",
            "--bare",
            "--disable-sandboxing",
            "--no-setup",
            "-y",
        ],
        env=environment,
    )
    switches = subprocess.run(
        ["opam", "switch", "list", "--short"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
        env=environment,
    ).stdout.splitlines()
    if switch not in switches:
        run(
            ["opam", "switch", "create", switch, "4.14.2", "-y"],
            env=environment,
        )
    run(
        ["opam", "switch", "link", switch, str(ROOT), "-y"],
        env=environment,
    )
    bind_build_toolchains(environment, switch)
    compiler = subprocess.run(
        ["opam", "exec", "--switch", switch, "--", "ocamlc", "-version"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
        env=environment,
    ).stdout.strip()
    if compiler != "4.14.2":
        raise ValidatorError(f"source build compiler mismatch: {compiler}")
    run([
        "opam",
        "install",
        "--switch",
        switch,
        ".",
        "--deps-only",
        "--with-test",
        "--locked",
        "--require-checksums",
        "-y",
    ], env=environment)
    run([
        "make",
        "-C",
        "mcl",
        "MCL_FP_BIT=256",
        "MCL_FR_BIT=256",
        "lib/libmcl.a",
    ], env=environment)
    run([
        "opam",
        "exec",
        "--switch",
        switch,
        "--",
        "dune",
        "build",
        "--root",
        str(ROOT),
        "--profile",
        "release",
        "bin/octra_node.exe",
        "bin/octra_pvac_worker.exe",
        "bin/octra_state_sync_client.exe",
        "bin/octra_state_sync_manifest.exe",
        "bin/bft_control_tx.exe",
    ], env=environment)
    for name in [
        "octra_node.exe",
        "octra_pvac_worker.exe",
        "octra_state_sync_client.exe",
        "octra_state_sync_manifest.exe",
        "bft_control_tx.exe",
    ]:
        if not (ROOT / "_build/default/bin" / name).is_file():
            raise ValidatorError(f"source build artifact is missing: {name}")

def memory_bytes():
    path = Path("/proc/meminfo")
    if not path.is_file():
        return 0
    for line in path.read_text(encoding="utf-8").splitlines():
        if line.startswith("MemTotal:"):
            return int(line.split()[1]) * 1024
    return 0

def resource_report(data_dir, role):
    parent = Path(data_dir)
    while not parent.exists() and parent != parent.parent:
        parent = parent.parent
    usage = shutil.disk_usage(parent)
    cpu = os.cpu_count() or 0
    memory = memory_bytes()
    emit(
        event="resources",
        cpu=cpu,
        ram_gib=round(memory / 1024 ** 3, 1),
        disk_free_gib=round(usage.free / 1024 ** 3, 1),
        role=role,
    )

def valid_port(value):
    try:
        port = int(value)
    except ValueError as error:
        raise ValidatorError(f"invalid port: {value}") from error
    if port < 1 or port > 65535:
        raise ValidatorError(f"invalid port: {value}")
    return port

def validate_advertise(value, consensus_port):
    if not value or not ENDPOINT.fullmatch(value):
        raise ValidatorError("invalid public consensus endpoint")
    if int(value.rsplit(":", 1)[1]) != consensus_port:
        raise ValidatorError("advertised and consensus ports must match")
    return value

def require_validator_membership(role, state):
    if role == "validator" and not state["active"] and not state["scheduled"]:
        raise ValidatorError("validator identity is not active or scheduled")

def resolve_digest(args, network):
    if args.network_sha:
        return args.network_sha
    digest_path = Path(str(network) + ".sha256")
    if digest_path.is_file():
        return read_digest(digest_path)
    if args.yes:
        raise ValidatorError("network bundle digest is required")
    return ask("Expected network bundle SHA-256")

def copy_network(bundle, target_dir):
    target_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
    bundle_copy = target_dir / "network.env"
    copy_private(bundle, bundle_copy)
    return bundle_copy

def bind_wallet(data_dir):
    data_wallet = Path(data_dir) / "wallet.json"
    if IDENTITY_WALLET.is_file() and not data_wallet.is_file():
        copy_private(IDENTITY_WALLET, data_wallet)
    wallet = ensure_wallet(data_wallet)
    if IDENTITY_WALLET.is_file():
        identity = load_wallet(IDENTITY_WALLET)
        if identity != wallet:
            raise ValidatorError("operator identity and data wallet mismatch")
    else:
        copy_private(data_wallet, IDENTITY_WALLET)
    return wallet

def create_identity(data_dir, role):
    resource_report(data_dir, role)
    wallet = ensure_wallet(IDENTITY_WALLET)
    emit(event="identity", status="ready", role=role, address=wallet["address"])
    emit(event="identity", pubkey=wallet["pub"])
    emit(event="next", command="configure_node")

def load_verified_snapshot(stage, values):
    candidates = []
    for marker in sorted((Path(stage) / "snapshots").glob("*/snapshot_verified.json")):
        try:
            payload = json.loads(marker.read_text(encoding="utf-8"))
            epoch = int(payload["snapshot_epoch"])
        except Exception:
            continue
        if payload.get("version") != "octra-state-sync-verified":
            continue
        if payload.get("status") != "snapshot_verified" or payload.get("voting") is not False:
            continue
        if payload.get("chain_id") != values["OCTRA_CHAIN_ID"]:
            continue
        if payload.get("config_hash") != values["OCTRA_CONSENSUS_CONFIG_HASH"]:
            continue
        data = marker.parent / "data"
        if not state_ready(data):
            continue
        candidates.append((epoch, payload["manifest_hash"], marker.parent))
    if not candidates:
        raise ValidatorError("state sync completed without a verified snapshot")
    return max(candidates)[2]

def resolve_sync_stage(value, data_dir):
    target = Path(data_dir).expanduser().resolve()
    if value:
        return Path(value).expanduser().resolve()
    return target.with_name(target.name + ".state_sync")

def validate_sync_layout(stage, data_dir):
    target = Path(data_dir).expanduser().resolve()
    if stage == target or target in stage.parents or stage in target.parents:
        raise ValidatorError("state sync stage and data directory must be siblings")
    target.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    stage.mkdir(parents=True, exist_ok=True, mode=0o700)
    if stage.stat().st_dev != target.parent.stat().st_dev:
        raise ValidatorError("state sync stage and data directory must share a filesystem")

def install_verified_snapshot(snapshot, data_dir):
    source = snapshot / "data"
    target = Path(data_dir)
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.exists():
        if not target.is_dir():
            raise ValidatorError("data path is not a directory")
        if any(target.iterdir()):
            raise ValidatorError("data directory changed during state sync")
        target.rmdir()
    if source.stat().st_dev != target.parent.stat().st_dev:
        raise ValidatorError("state sync stage and data directory must share a filesystem")
    evidence = source / ".state_sync"
    evidence.mkdir(mode=0o700)
    copy_private(snapshot / "snapshot_verified.json", evidence / "snapshot_verified.json")
    copy_private(snapshot / "certificate.json", evidence / "certificate.json")
    os.replace(source, target)
    descriptor = os.open(target.parent, os.O_RDONLY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)

def sync_snapshot(
    sync_binary,
    stage,
    data_path,
    values,
    sources,
    concurrency,
    source_concurrency,
):
    validate_sync_layout(stage, data_path)
    command = sync_client_command(
        sync_binary,
        stage,
        values,
        sources,
        concurrency,
        source_concurrency,
    )
    run(command)
    snapshot = load_verified_snapshot(stage, values)
    install_verified_snapshot(snapshot, data_path)
    if not state_ready(data_path):
        raise ValidatorError("state sync completed without a valid checkpoint")
    validate_checkpoint(data_path, values, allow_progress=True)

def sync_client_command(
    sync_binary,
    stage,
    values,
    sources,
    concurrency,
    source_concurrency,
):
    validators = validator_entries(values["OCTRA_VALIDATORS"])
    exporters = exporter_entries(values["OCTRA_STATE_SYNC_EXPORTERS"])
    command = [
        str(sync_binary),
        "--stage",
        str(stage),
        "--chain-id",
        values["OCTRA_CHAIN_ID"],
        "--config-hash",
        values["OCTRA_CONSENSUS_CONFIG_HASH"],
        "--concurrency",
        str(concurrency),
        "--source-concurrency",
        str(source_concurrency),
        "--min-epoch",
        values["OCTRA_CHECKPOINT_EPOCH"],
    ]
    for source in sources:
        command.extend(["--source", source])
    for address, pubkey in validators:
        command.extend(["--validator", f"{address}:{pubkey}"])
    for address, pubkey in exporters:
        command.extend(["--exporter", f"{address}:{pubkey}"])
    if any(source.startswith("http://") for source in sources):
        command.append("--allow-private-http")
    return command

def check_sync_sources(sync_binary, stage, values, sources):
    expected = "error = snapshot exceeds configured byte limit"
    for position, source in enumerate(sources, 1):
        command = sync_client_command(
            sync_binary,
            Path(stage) / f"source-{position}",
            values,
            [source],
            1,
            1,
        )
        command.extend([
            "--retries",
            "1",
            "--timeout",
            "10",
            "--max-bytes",
            "1",
        ])
        try:
            result = subprocess.run(
                command,
                cwd=ROOT,
                check=False,
                capture_output=True,
                text=True,
                timeout=40,
            )
        except subprocess.TimeoutExpired as error:
            raise ValidatorError(
                f"state sync source check timed out: {source}"
            ) from error
        lines = [
            line.strip()
            for line in (result.stdout + "\n" + result.stderr).splitlines()
            if line.strip()
        ]
        if (
            result.returncode != 1
            or expected not in lines
            or any(
                line.startswith("event = manifest_source_rejected")
                for line in lines
            )
        ):
            detail = " | ".join(lines[-3:]) or f"exit {result.returncode}"
            raise ValidatorError(
                f"state sync source check failed: {source}: {detail}"
            )
        emit(event="state_sync_source", status="ready", source=source)

def maybe_sync(args, data_dir, values):
    if state_ready(data_dir):
        validate_checkpoint(data_dir, values, allow_progress=True)
        emit(event="checkpoint", status="ready", path=data_dir)
        return
    data_path = Path(data_dir)
    if data_path.exists() and any(data_path.iterdir()):
        raise ValidatorError("data directory is nonempty without a valid checkpoint")
    should_sync = args.sync or args.yes
    if not args.yes and not should_sync:
        should_sync = ask_bool("Download checkpoint now", False)
    if not should_sync:
        data_path.mkdir(parents=True, exist_ok=True, mode=0o700)
        emit(event="checkpoint", status="missing", path=data_dir)
        return
    sync_binary = Path(args.sync_binary).resolve()
    if not sync_binary.is_file():
        raise ValidatorError(f"state sync client is missing: {sync_binary}")
    sources = args.sync_url or state_sync_sources(values["OCTRA_STATE_SYNC_SOURCES"])
    if not args.sync_url and not args.yes:
        entered = ask("State sync URLs, comma separated", ",".join(sources))
        sources = state_sync_sources(entered)
    elif args.sync_url:
        sources = state_sync_sources(",".join(args.sync_url))
    stage = resolve_sync_stage(args.sync_stage, data_path)
    sync_snapshot(
        sync_binary,
        stage,
        data_path,
        values,
        sources,
        args.sync_concurrency,
        args.sync_source_concurrency,
    )

def parser():
    value = argparse.ArgumentParser(prog="config_val.sh")
    value.add_argument("--advertise")
    value.add_argument("--api-port", default="8080")
    value.add_argument("--binary", default=str(DEFAULT_BINARY))
    value.add_argument("--build", action="store_true")
    value.add_argument("--check-runtime", action="store_true")
    value.add_argument("--check-sync", action="store_true")
    value.add_argument("--config", default=str(DEFAULT_CONFIG))
    value.add_argument("--consensus-port", default="19000")
    value.add_argument("--control-binary", default=str(DEFAULT_CONTROL_BINARY))
    value.add_argument("--data-dir", default=str(DEFAULT_DATA))
    value.add_argument("--identity-only", action="store_true")
    value.add_argument("--install", action="store_true")
    value.add_argument("--name")
    value.add_argument("--network")
    value.add_argument("--network-sha")
    value.add_argument("--p2p-port", default="9000")
    value.add_argument("--role", choices=["observer", "validator"])
    value.add_argument("--rebind-runtime", action="store_true")
    value.add_argument("--rpc", default="https://devnet.octrascan.io/rpc")
    value.add_argument("--sync", action="store_true")
    value.add_argument("--sync-binary", default=str(DEFAULT_SYNC_BINARY))
    value.add_argument("--sync-concurrency", type=int, default=4)
    value.add_argument("--sync-source-concurrency", type=int, default=4)
    value.add_argument("--sync-stage")
    value.add_argument("--sync-url", action="append")
    value.add_argument("--worker", default=str(DEFAULT_WORKER))
    value.add_argument("--yes", action="store_true")
    return value

def main():
    args = parser().parse_args()
    modes = [args.check_runtime, args.check_sync, args.rebind_runtime]
    if sum(bool(mode) for mode in modes) > 1:
        raise ValidatorError("runtime and state sync checks are mutually exclusive")
    if args.check_runtime:
        require_runtime_binding(args.config)
        return
    if args.check_sync:
        network = args.network
        if not network and not args.yes:
            network = ask("Network bundle path", str(ROOT / "config/network.env"))
        if not network:
            raise ValidatorError("network bundle path is required")
        expected_hash = resolve_digest(args, network)
        _, _, values = load_network(network, expected_hash)
        sync_binary = Path(args.sync_binary).resolve()
        if not sync_binary.is_file():
            raise ValidatorError(f"state sync client is missing: {sync_binary}")
        sources = args.sync_url or state_sync_sources(
            values["OCTRA_STATE_SYNC_SOURCES"]
        )
        if args.sync_url:
            sources = state_sync_sources(",".join(args.sync_url))
        stage = Path(
            args.sync_stage or ROOT / "runtime_data/state_sync_check"
        ).expanduser().resolve()
        check_sync_sources(sync_binary, stage, values, sources)
        return
    if args.rebind_runtime:
        rebind_runtime(args.config)
        return
    install = args.install
    if not args.yes and not install and missing_runtime():
        install = ask_bool("Install validator runtime packages", True)
    if install:
        install_runtime()
    missing = missing_runtime()
    if missing:
        raise ValidatorError("missing runtime packages: " + ",".join(missing))
    if args.identity_only:
        create_identity(
            str(Path(args.data_dir).expanduser().resolve()),
            args.role or "observer",
        )
        return
    network = args.network
    if not network and not args.yes:
        network = ask("Network bundle path", str(ROOT / "config/network.env"))
    if not network:
        raise ValidatorError("network bundle path is required")
    expected_hash = resolve_digest(args, network)
    bundle, bundle_hash, values = load_network(network, expected_hash)
    binary = Path(args.binary).resolve()
    build = args.build
    if not binary.is_file() and not args.yes and not build:
        build = ask_bool("Build candidate from source", True)
    if build:
        build_candidate()
    if not binary.is_file():
        raise ValidatorError(f"node candidate is missing: {binary}")
    worker = Path(args.worker).resolve()
    if not worker.is_file():
        raise ValidatorError(f"PVAC worker is missing: {worker}")
    sync_binary = Path(args.sync_binary).resolve()
    if not sync_binary.is_file():
        raise ValidatorError(f"state sync client is missing: {sync_binary}")
    binary_hash = sha256_file(binary)
    worker_hash = sha256_file(worker)
    sync_hash = sha256_file(sync_binary)
    control_binary = Path(args.control_binary).resolve()
    if not control_binary.is_file():
        raise ValidatorError(f"validator control binary is missing: {control_binary}")
    control_hash = sha256_file(control_binary)
    role = args.role
    if not role and not args.yes:
        role = ask("Node role", "observer")
    if role not in {"observer", "validator"}:
        raise ValidatorError("node role must be observer or validator")
    p2p_port = valid_port(args.p2p_port)
    api_port = valid_port(args.api_port)
    consensus_port = valid_port(args.consensus_port)
    name = args.name or (ask("Node name", socket.gethostname()) if not args.yes else socket.gethostname())
    if not NAME.fullmatch(name):
        raise ValidatorError("invalid node name")
    data_dir = str(Path(args.data_dir).expanduser().resolve())
    resource_report(data_dir, role)
    maybe_sync(args, data_dir, values)
    wallet = bind_wallet(data_dir)
    if role == "validator":
        require_validator_membership(
            role,
            membership(
                {**values, "OCTRA_API_PORT": str(api_port)},
                wallet,
            ),
        )
    advertise = args.advertise
    if not advertise and not args.yes:
        advertise = ask(
            "Public consensus endpoint",
            f"{socket.getfqdn()}:{consensus_port}",
        )
    advertise = validate_advertise(advertise, consensus_port)
    key_dir = ROOT / ".keys/validator"
    bundle_copy = copy_network(bundle, key_dir)
    local = {
        "NO_COLOR": "1",
        "OCTRA_ADVERTISE_ENDPOINT": advertise,
        "OCTRA_API_PORT": str(api_port),
        "OCTRA_CONSENSUS_MODE": "bft" if role == "validator" else "observer",
        "OCTRA_CONSENSUS_PORT": str(consensus_port),
        "OCTRA_DATA_DIR": data_dir,
        "OCTRA_LOG_COLOR": "never",
        "OCTRA_LOG_LEVEL": "info",
        "OCTRA_BINARY_HASH": binary_hash,
        "OCTRA_OPERATOR_BINARY": str(binary),
        "OCTRA_OPERATOR_CONFIG": str(Path(args.config).resolve()),
        "OCTRA_OPERATOR_CONTROL_BINARY": str(control_binary),
        "OCTRA_OPERATOR_CONTROL_BINARY_HASH": control_hash,
        "OCTRA_OPERATOR_LOG_DIR": str((ROOT / "data/operator_logs").resolve()),
        "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle_copy),
        "OCTRA_OPERATOR_NETWORK_SHA256": bundle_hash,
        "OCTRA_OPERATOR_PM2_NAME": operator_pm2_name(name),
        "OCTRA_OPERATOR_ROLE": role,
        "OCTRA_OPERATOR_RPC_URL": rpc_url(args.rpc),
        "OCTRA_OPERATOR_SYNC_BINARY": str(sync_binary),
        "OCTRA_OPERATOR_SYNC_BINARY_HASH": sync_hash,
        "OCTRA_OPERATOR_SYNC_CONCURRENCY": str(args.sync_concurrency),
        "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": str(args.sync_source_concurrency),
        "OCTRA_OPERATOR_SYNC_STAGE": str(resolve_sync_stage(args.sync_stage, data_dir)),
        "OCTRA_P2P_PORT": str(p2p_port),
        "OCTRA_PVAC_VERIFY_WORKER": str(worker),
        "OCTRA_PVAC_VERIFY_WORKER_HASH": worker_hash,
        "OCTRA_PROOF_POOL_WORKERS": "2",
        "OCTRA_PVAC_VERIFY_WORKERS": "1",
    }
    write_env(args.config, {**values, **local})
    emit(event="configured", role=role, address=wallet["address"])
    emit(event="identity", pubkey=wallet["pub"], endpoint=advertise)
    emit(event="candidate", sha256=binary_hash)
    emit(event="pvac_worker", sha256=worker_hash)
    emit(event="state_sync", sha256=sync_hash)
    emit(event="validator_control", sha256=control_hash)
    emit(event="network", sha256=bundle_hash, chain=values["OCTRA_CHAIN_ID"])
    emit(
        event="next",
        command="./controls/run.sh" if state_ready(data_dir) else "restore_checkpoint",
    )

if __name__ == "__main__":
    try:
        main()
    except (ValidatorError, subprocess.CalledProcessError, OSError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)