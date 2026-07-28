import argparse
import importlib.util
import json
import os
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
from validator_common import read_digest
from validator_common import sha256_file
from validator_common import state_ready
from validator_common import state_sync_sources
from validator_common import validate_checkpoint
from validator_common import validator_entries
from validator_common import write_env

SOURCE = Path(__file__).resolve()
ROOT = SOURCE.parents[2] if SOURCE.parents[1].name == "controls" else SOURCE.parents[3]
ARTIFACT_BINARY = ROOT / "artifacts/octra_node.exe"
ARTIFACT_SYNC_BINARY = ROOT / "artifacts/octra_state_sync_client.exe"
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
DEFAULT_CONFIG = ROOT / ".keys/validator/node.env"
DEFAULT_DATA = ROOT / "data"
IDENTITY_WALLET = ROOT / ".keys/validator/wallet.json"
MIN_VALIDATOR_CPU = 8
MIN_VALIDATOR_RAM = 31 * 1024 ** 3
MIN_VALIDATOR_DISK = 1800 * 1000 ** 3
MIN_VALIDATOR_FREE = 500 * 1000 ** 3
MIN_OBSERVER_DISK = 900 * 1000 ** 3
MIN_OBSERVER_FREE = 200 * 1000 ** 3

def canonical_pm2_name(name):
    return name if name.startswith("octra-") else f"octra-{name}"

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

def run(command, cwd=ROOT):
    emit(event="run", command=command[0])
    subprocess.run(command, cwd=cwd, check=True)

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
    run(elevated(["npm", "install", "-g", "pm2"]), cwd=Path("/"))
    run(["pm2", "install", "pm2-logrotate"])
    run(["pm2", "set", "pm2-logrotate:max_size", "100M"])
    run(["pm2", "set", "pm2-logrotate:retain", "10"])
    run(["pm2", "set", "pm2-logrotate:compress", "true"])
    user = os.environ.get("SUDO_USER") or os.environ.get("USER")
    home = str(Path.home())
    if not user:
        raise ValidatorError("cannot determine PM2 service user")
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
    ]), cwd=Path("/"))

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

def ensure_build_toolchain():
    commands = ["cargo", "g++", "make", "opam", "rustc"]
    missing = [command for command in commands if shutil.which(command) is None]
    if missing:
        raise ValidatorError("missing build tools: " + ",".join(missing))
    rustc = subprocess.run(
        ["rustc", "--version"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    if tool_version(rustc) < (1, 80, 0):
        raise ValidatorError("source build requires rustc 1.80 or newer")

def build_candidate():
    ensure_build_toolchain()
    switch = "octra-validator-4.14.2"
    run(["opam", "init", "--bare", "--disable-sandboxing", "-y"])
    switches = subprocess.run(
        ["opam", "switch", "list", "--short"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout.splitlines()
    if switch not in switches:
        run(["opam", "switch", "create", switch, "4.14.2", "-y"])
    run(["opam", "install", "--switch", switch, ".", "--deps-only", "--with-test", "-y"])
    run(["make", "-C", "mcl", "MCL_FP_BIT=256", "MCL_FR_BIT=256", "lib/libmcl.a"])
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
        "bin/octra_state_sync_client.exe",
        "bin/octra_state_sync_manifest.exe",
    ])

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
    if role == "validator":
        if cpu < MIN_VALIDATOR_CPU:
            raise ValidatorError(f"validator requires at least {MIN_VALIDATOR_CPU} CPU threads")
        if memory < MIN_VALIDATOR_RAM:
            raise ValidatorError("validator requires at least 32 GiB RAM")
        if usage.total < MIN_VALIDATOR_DISK or usage.free < MIN_VALIDATOR_FREE:
            raise ValidatorError("validator requires a 2 TB class disk with 500 GB free")
    elif usage.total < MIN_OBSERVER_DISK or usage.free < MIN_OBSERVER_FREE:
        raise ValidatorError("full-history observer requires a 1 TB class disk with 200 GB free")

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

def resolve_digest(args, network):
    if args.network_sha:
        return args.network_sha
    digest_path = Path(str(network) + ".sha256")
    if digest_path.is_file():
        return read_digest(digest_path)
    if args.yes:
        raise ValidatorError("network bundle digest is required")
    return ask("Expected network bundle SHA-256")

def copy_network(bundle, values, target_dir):
    target_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
    bundle_copy = target_dir / "network.env"
    entitlement_copy = target_dir / "migration_entitlements.json"
    copy_private(bundle, bundle_copy)
    copy_private(values["OCTRA_PVAC_MIGRATION_ENTITLEMENTS"], entitlement_copy)
    values["OCTRA_PVAC_MIGRATION_ENTITLEMENTS"] = str(entitlement_copy)
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

def maybe_sync(args, data_dir, values):
    if state_ready(data_dir):
        validate_checkpoint(data_dir, values, allow_progress=True)
        emit(event="checkpoint", status="ready", path=data_dir)
        return
    data_path = Path(data_dir)
    if data_path.exists() and any(data_path.iterdir()):
        raise ValidatorError("data directory is nonempty without a valid checkpoint")
    should_sync = args.sync
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
    validators = validator_entries(values["OCTRA_VALIDATORS"])
    exporters = exporter_entries(values["OCTRA_STATE_SYNC_EXPORTERS"])
    stage = Path(args.sync_stage).expanduser().resolve()
    command = [
        str(sync_binary),
        "--stage",
        str(stage),
        "--chain-id",
        values["OCTRA_CHAIN_ID"],
        "--config-hash",
        values["OCTRA_CONSENSUS_CONFIG_HASH"],
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
    run(command)
    snapshot = load_verified_snapshot(stage, values)
    install_verified_snapshot(snapshot, data_path)
    if not state_ready(data_path):
        raise ValidatorError("state sync completed without a valid checkpoint")
    validate_checkpoint(data_path, values, allow_progress=True)

def parser():
    value = argparse.ArgumentParser(prog="config_val.sh")
    value.add_argument("--advertise")
    value.add_argument("--api-port", default="8080")
    value.add_argument("--binary", default=str(DEFAULT_BINARY))
    value.add_argument("--build", action="store_true")
    value.add_argument("--config", default=str(DEFAULT_CONFIG))
    value.add_argument("--consensus-port", default="19000")
    value.add_argument("--data-dir", default=str(DEFAULT_DATA))
    value.add_argument("--identity-only", action="store_true")
    value.add_argument("--install", action="store_true")
    value.add_argument("--name")
    value.add_argument("--network")
    value.add_argument("--network-sha")
    value.add_argument("--p2p-port", default="9000")
    value.add_argument("--role", choices=["observer", "validator"])
    value.add_argument("--sync", action="store_true")
    value.add_argument("--sync-binary", default=str(DEFAULT_SYNC_BINARY))
    value.add_argument("--sync-stage", default=str(ROOT / "tmp/state_sync"))
    value.add_argument("--sync-url", action="append")
    value.add_argument("--yes", action="store_true")
    return value

def main():
    args = parser().parse_args()
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
    binary_hash = sha256_file(binary)
    if binary_hash != values["OCTRA_BINARY_HASH"]:
        raise ValidatorError(f"candidate hash mismatch: {binary_hash}")
    role = args.role
    if not role and not args.yes:
        role = ask("Node role", "observer")
    if role not in {"observer", "validator"}:
        raise ValidatorError("node role must be observer or validator")
    name = args.name or (ask("Node name", socket.gethostname()) if not args.yes else socket.gethostname())
    if not NAME.fullmatch(name):
        raise ValidatorError("invalid node name")
    data_dir = str(Path(args.data_dir).expanduser().resolve())
    resource_report(data_dir, role)
    maybe_sync(args, data_dir, values)
    wallet = bind_wallet(data_dir)
    members = dict(validator_entries(values["OCTRA_VALIDATORS"]))
    if role == "validator" and members.get(wallet["address"]) != wallet["pub"]:
        raise ValidatorError("local identity is not in the active validator set")
    p2p_port = valid_port(args.p2p_port)
    api_port = valid_port(args.api_port)
    consensus_port = valid_port(args.consensus_port)
    advertise = args.advertise
    if not advertise and not args.yes:
        advertise = ask(
            "Public consensus endpoint",
            f"{socket.getfqdn()}:{consensus_port}",
        )
    advertise = validate_advertise(advertise, consensus_port)
    key_dir = ROOT / ".keys/validator"
    bundle_copy = copy_network(bundle, values, key_dir)
    local = {
        "NO_COLOR": "1",
        "OCTRA_ADVERTISE_ENDPOINT": advertise,
        "OCTRA_API_PORT": str(api_port),
        "OCTRA_CONSENSUS_MODE": "bft" if role == "validator" else "observer",
        "OCTRA_CONSENSUS_PORT": str(consensus_port),
        "OCTRA_DATA_DIR": data_dir,
        "OCTRA_LOG_COLOR": "never",
        "OCTRA_LOG_LEVEL": "info",
        "OCTRA_OPERATOR_BINARY": str(binary),
        "OCTRA_OPERATOR_CONFIG": str(Path(args.config).resolve()),
        "OCTRA_OPERATOR_LOG_DIR": str((ROOT / "data/operator_logs").resolve()),
        "OCTRA_OPERATOR_NETWORK_BUNDLE": str(bundle_copy),
        "OCTRA_OPERATOR_NETWORK_SHA256": bundle_hash,
        "OCTRA_OPERATOR_PM2_NAME": canonical_pm2_name(name),
        "OCTRA_OPERATOR_ROLE": role,
        "OCTRA_P2P_PORT": str(p2p_port),
        "OCTRA_VERIFY_WORKERS": "1",
    }
    write_env(args.config, {**values, **local})
    emit(event="configured", role=role, address=wallet["address"])
    emit(event="identity", pubkey=wallet["pub"], endpoint=advertise)
    emit(event="candidate", sha256=binary_hash)
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
