# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import base64
import hashlib
import ipaddress
import json
import os
import re
import shlex
import shutil
import stat
from pathlib import Path
from urllib.parse import urlparse

BASE58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
HEX64 = re.compile(r"^[0-9a-f]{64}$")
NAME = re.compile(r"^[a-zA-Z0-9][a-zA-Z0-9._-]{0,63}$")
ENDPOINT = re.compile(r"^[a-zA-Z0-9][a-zA-Z0-9.-]*:[1-9][0-9]{0,4}$")
NETWORK_KEYS = frozenset({
    "OCTRA_BFT_LIVENESS_STALL_SEC",
    "OCTRA_BFT_PROPOSAL_BUILD_GRACE_MS",
    "OCTRA_BFT_PROPOSAL_MAX_BYTES",
    "OCTRA_BFT_PROPOSAL_MAX_OU",
    "OCTRA_BFT_PROPOSAL_MAX_TXS",
    "OCTRA_BFT_PROPOSAL_VERIFY_GRACE_MS",
    "OCTRA_BFT_RELEASE_PROFILE",
    "OCTRA_BFT_PROPOSE_TIMEOUT_MS",
    "OCTRA_BFT_TIMEOUT_BASE_MS",
    "OCTRA_BFT_TIMEOUT_ROUND_MS",
    "OCTRA_BINARY_HASH",
    "OCTRA_BOOTSTRAP_PEERS",
    "OCTRA_BUNDLE_CACHE_CAP",
    "OCTRA_CATCHUP_MAX_LAG",
    "OCTRA_CATCHUP_RANGE_TIMEOUT_SEC",
    "OCTRA_CATCHUP_SOFT_MAX_LAG",
    "OCTRA_CHAIN_ID",
    "OCTRA_CHECKPOINT_EPOCH",
    "OCTRA_CHECKPOINT_STATE_ROOT",
    "OCTRA_CHECKPOINT_TXID_HI",
    "OCTRA_CONSENSUS_CONFIG_HASH",
    "OCTRA_EMISSION_ACTIVATION_EPOCH",
    "OCTRA_EMISSION_PROFILE",
    "OCTRA_EPOCH_DURATION",
    "OCTRA_FHE_MAX_PER_EPOCH",
    "OCTRA_MULTI_EXEC_MAX_CALLS",
    "OCTRA_P2P_REQUIRE_BINARY_HASH",
    "OCTRA_P2P_ROLLBACK_ACTIVATE_EPOCH",
    "OCTRA_P2P_ROLLBACK_BINARY_HASH",
    "OCTRA_P2P_ROLLBACK_CONFIG_HASH",
    "OCTRA_P2P_UPGRADE_ACTIVATE_EPOCH",
    "OCTRA_P2P_UPGRADE_BINARY_HASH",
    "OCTRA_P2P_UPGRADE_CONFIG_HASH",
    "OCTRA_PEERS",
    "OCTRA_PREVERIFY_CACHE_MAX",
    "OCTRA_PREVERIFY_CACHE_TTL",
    "OCTRA_PREVERIFY_PENDING_CEILING",
    "OCTRA_PREVERIFY_PENDING_MAX",
    "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH",
    "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH",
    "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH",
    "OCTRA_PROGRAM_RELEASE_KEYS",
    "OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH",
    "OCTRA_PVAC_MIGRATION_ENTITLEMENTS",
    "OCTRA_PVAC_MIGRATION_ROOT",
    "OCTRA_QUARANTINE_AHEAD_DRIFT_TOLERANCE",
    "OCTRA_QUARANTINE_AHEAD_GRACE_EPOCHS",
    "OCTRA_QUARANTINE_AHEAD_STREAK_THRESHOLD",
    "OCTRA_QUARANTINE_MISMATCH_THRESHOLD",
    "OCTRA_QUARANTINE_POLL_SEC",
    "OCTRA_STATE_ATTEST_MIN_PEERS",
    "OCTRA_STATE_SYNC_EXPORTERS",
    "OCTRA_STATE_SYNC_SOURCES",
    "OCTRA_STEALTH_MAX_DEFER",
    "OCTRA_STEALTH_MAX_PER_EPOCH",
    "OCTRA_STEALTH_MIN_OU",
    "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH",
    "OCTRA_VALIDATORS",
    "OCTRA_VALIDATOR_READY_MAX_LAG_EPOCHS",
    "OCTRA_VALIDATOR_READY_MIN_SHADOW_EPOCHS",
    "OCTRA_VALIDATOR_READY_STRICT",
})
FORBIDDEN_KEYS = frozenset({
    "OCTRA_ALLOW_UNSAFE_QUORUM",
    "OCTRA_BFT_ADMIT_OPS",
    "OCTRA_BFT_CRYPTO_PROFILE",
    "OCTRA_CHAOS_FAIL_AT",
    "OCTRA_CHAOS_KILL_AT",
    "OCTRA_CHAOS_PAUSE_AT",
    "OCTRA_CHAOS_PAUSE_MS",
    "OCTRA_CONSENSUS_ALLOW_DYNAMIC_PEERS",
    "OCTRA_EMISSION_GUARD",
    "OCTRA_EMISSION_PROFILE",
    "OCTRA_SKIP_RECONCILE",
    "OCTRA_SKIP_RECOVERY",
    "OCTRA_STEALTH_INLINE_VERIFY",
    "OCTRA_VALIDATORS_ACTIVATE_EPOCH",
    "OCTRA_VALIDATORS_NEXT",
    "OCTRA_VALIDATORS_NEXT_EPOCH",
})
REQUIRED_KEYS = frozenset({
    "OCTRA_BFT_RELEASE_PROFILE",
    "OCTRA_BINARY_HASH",
    "OCTRA_BOOTSTRAP_PEERS",
    "OCTRA_CHAIN_ID",
    "OCTRA_CHECKPOINT_EPOCH",
    "OCTRA_CHECKPOINT_STATE_ROOT",
    "OCTRA_CHECKPOINT_TXID_HI",
    "OCTRA_CONSENSUS_CONFIG_HASH",
    "OCTRA_EMISSION_ACTIVATION_EPOCH",
    "OCTRA_EPOCH_DURATION",
    "OCTRA_FHE_MAX_PER_EPOCH",
    "OCTRA_P2P_REQUIRE_BINARY_HASH",
    "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH",
    "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH",
    "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH",
    "OCTRA_PROGRAM_RELEASE_KEYS",
    "OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH",
    "OCTRA_PVAC_MIGRATION_ENTITLEMENTS",
    "OCTRA_PVAC_MIGRATION_ROOT",
    "OCTRA_STEALTH_MAX_PER_EPOCH",
    "OCTRA_STATE_SYNC_EXPORTERS",
    "OCTRA_STATE_SYNC_SOURCES",
    "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH",
    "OCTRA_VALIDATORS",
})
ENTITLEMENT_KEYS = frozenset({
    "schema",
    "chain_id",
    "snapshot_epoch",
    "state_root",
    "activation_epoch",
    "root",
    "entries",
})
ENTITLEMENT_SCHEMA = "octra_pvac_migration_entitlements_v2"
ENTITLEMENT_MAX_BYTES = 64 * 1024 * 1024

class ValidatorError(RuntimeError):
    pass

def sha256_file(path):
    digest = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()

def parse_env(path, allowed=None):
    values = {}
    for number, raw in enumerate(Path(path).read_text(encoding="utf-8").splitlines(), 1):
        line = raw.strip()
        if not line:
            continue
        try:
            parts = shlex.split(line, posix=True)
        except ValueError as error:
            raise ValidatorError(f"invalid env syntax at line {number}: {error}") from error
        if len(parts) != 1 or "=" not in parts[0]:
            raise ValidatorError(f"invalid env entry at line {number}")
        key, value = parts[0].split("=", 1)
        if not re.fullmatch(r"[A-Z][A-Z0-9_]*", key):
            raise ValidatorError(f"invalid env key at line {number}")
        if allowed is not None and key not in allowed:
            raise ValidatorError(f"unsupported env key: {key}")
        if key in values:
            raise ValidatorError(f"duplicate env key: {key}")
        if any(ord(char) < 32 or ord(char) == 127 for char in value):
            raise ValidatorError(f"control byte in env value: {key}")
        if len(value) > 131072:
            raise ValidatorError(f"env value too large: {key}")
        values[key] = value
    return values

def write_env(path, values):
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    temporary = target.with_name(target.name + ".new")
    body = "".join(f"{key}={shlex.quote(str(values[key]))}\n" for key in sorted(values))
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
        handle.write(body)
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, target)
    os.chmod(target, 0o600)

def write_private_json(path, payload):
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    temporary = target.with_name(target.name + ".new")
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, separators=(",", ":"), sort_keys=True)
        handle.write("\n")
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, target)
    os.chmod(target, 0o600)

def base58_encode(payload):
    value = int.from_bytes(payload, "big")
    encoded = ""
    while value:
        value, digit = divmod(value, 58)
        encoded = BASE58[digit] + encoded
    leading = len(payload) - len(payload.lstrip(b"\x00"))
    return "1" * leading + encoded

def address_from_pubkey(public_key):
    body = base58_encode(hashlib.sha256(public_key).digest())
    return "oct" + body.rjust(44, "1")

def canonical_address(value):
    return len(value) == 47 and value.startswith("oct") and all(char in BASE58 for char in value[3:])

def decode_public_key(value):
    try:
        decoded = base64.b64decode(value, validate=True)
    except Exception as error:
        raise ValidatorError("invalid validator public key") from error
    if len(decoded) != 32:
        raise ValidatorError("invalid validator public key length")
    return decoded

def signer_entries(raw):
    entries = []
    seen_addresses = set()
    seen_keys = set()
    for item in raw.split(","):
        parts = item.strip().split(":", 1)
        if len(parts) != 2:
            raise ValidatorError("invalid validator entry")
        address, encoded = parts
        public_key = decode_public_key(encoded)
        if not canonical_address(address):
            raise ValidatorError(f"noncanonical validator address: {address}")
        if address_from_pubkey(public_key) != address:
            raise ValidatorError(f"validator address and public key mismatch: {address}")
        if address in seen_addresses or public_key in seen_keys:
            raise ValidatorError("duplicate validator identity")
        seen_addresses.add(address)
        seen_keys.add(public_key)
        entries.append((address, encoded))
    return entries

def validator_entries(raw):
    entries = signer_entries(raw)
    if len(entries) != 5:
        raise ValidatorError("first devnet BFT set requires exactly five identities")
    return entries

def exporter_entries(raw):
    entries = signer_entries(raw)
    if not entries:
        raise ValidatorError("state sync exporter set is empty")
    return entries

def private_sync_host(host):
    if host == "localhost":
        return True
    try:
        address = ipaddress.ip_address(host)
    except ValueError:
        return False
    networks = [
        ipaddress.ip_network("10.0.0.0/8"),
        ipaddress.ip_network("127.0.0.0/8"),
        ipaddress.ip_network("172.16.0.0/12"),
        ipaddress.ip_network("192.168.0.0/16"),
        ipaddress.ip_network("169.254.0.0/16"),
        ipaddress.ip_network("fc00::/7"),
        ipaddress.ip_network("fe80::/10"),
        ipaddress.ip_network("::1/128"),
    ]
    return any(address in network for network in networks)

def state_sync_sources(raw):
    sources = [item.strip().rstrip("/") for item in raw.split(",") if item.strip()]
    if len(sources) < 2:
        raise ValidatorError("at least two state sync sources are required")
    if len(sources) != len(set(sources)):
        raise ValidatorError("duplicate state sync source")
    for source in sources:
        parsed = urlparse(source)
        if not parsed.hostname or parsed.username or parsed.password or parsed.query or parsed.fragment:
            raise ValidatorError(f"invalid state sync source: {source}")
        if parsed.scheme == "https":
            continue
        if parsed.scheme == "http" and private_sync_host(parsed.hostname):
            continue
        raise ValidatorError(f"state sync source requires HTTPS: {source}")
    return sources

def rpc_url(raw):
    value = raw.strip().rstrip("/")
    parsed = urlparse(value)
    if (
        not parsed.hostname
        or parsed.username
        or parsed.password
        or parsed.query
        or parsed.fragment
        or parsed.path not in {"", "/rpc"}
    ):
        raise ValidatorError(f"invalid RPC URL: {raw}")
    if parsed.scheme == "https":
        return value if parsed.path == "/rpc" else value + "/rpc"
    if parsed.scheme == "http" and private_sync_host(parsed.hostname):
        return value if parsed.path == "/rpc" else value + "/rpc"
    raise ValidatorError(f"RPC URL requires HTTPS: {raw}")

def endpoint_list(raw):
    endpoints = [item.strip() for item in raw.split(",") if item.strip()]
    if not endpoints:
        raise ValidatorError("peer endpoint list is empty")
    for endpoint in endpoints:
        if not ENDPOINT.fullmatch(endpoint):
            raise ValidatorError(f"invalid peer endpoint: {endpoint}")
        port = int(endpoint.rsplit(":", 1)[1])
        if port > 65535:
            raise ValidatorError(f"invalid peer port: {endpoint}")
    return endpoints

def validate_program_keys(raw):
    entries = [item.strip() for item in raw.split(",") if item.strip()]
    if not entries:
        raise ValidatorError("Program release key list is empty")
    seen = set()
    for entry in entries:
        parts = entry.split("=", 1)
        if len(parts) != 2 or not parts[0] or parts[0] in seen:
            raise ValidatorError("invalid Program release key entry")
        seen.add(parts[0])
        decode_public_key(parts[1])

def nonnegative_int(values, key):
    try:
        value = int(values[key])
    except ValueError as error:
        raise ValidatorError(f"invalid integer: {key}") from error
    if value < 0:
        raise ValidatorError(f"negative integer: {key}")
    return value

def validate_entitlement(path, values):
    artifact = Path(path)
    try:
        if artifact.stat().st_size > ENTITLEMENT_MAX_BYTES:
            raise ValidatorError("migration entitlement artifact exceeds size cap")
        payload = json.loads(artifact.read_text(encoding="utf-8"))
    except ValidatorError:
        raise
    except Exception as error:
        raise ValidatorError("invalid migration entitlement artifact") from error
    if not isinstance(payload, dict) or set(payload) != ENTITLEMENT_KEYS:
        raise ValidatorError("invalid migration entitlement fields")
    expected = {
        "schema": ENTITLEMENT_SCHEMA,
        "chain_id": values["OCTRA_CHAIN_ID"],
        "snapshot_epoch": int(values["OCTRA_CHECKPOINT_EPOCH"]),
        "state_root": values["OCTRA_CHECKPOINT_STATE_ROOT"],
        "activation_epoch": int(values["OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH"]),
        "root": values["OCTRA_PVAC_MIGRATION_ROOT"],
    }
    for key, value in expected.items():
        if payload.get(key) != value:
            raise ValidatorError(f"migration entitlement mismatch: {key}")
    if not isinstance(payload.get("entries"), list):
        raise ValidatorError("migration entitlement entries must be a list")

def validate_network(values, bundle_dir):
    unknown = set(values) - NETWORK_KEYS
    if unknown:
        raise ValidatorError("unsupported network keys: " + ",".join(sorted(unknown)))
    forbidden = set(values) & FORBIDDEN_KEYS
    if forbidden:
        raise ValidatorError("forbidden network keys: " + ",".join(sorted(forbidden)))
    missing = REQUIRED_KEYS - set(values)
    if missing:
        raise ValidatorError("missing network keys: " + ",".join(sorted(missing)))
    if values["OCTRA_BFT_RELEASE_PROFILE"] != "devnet_full_v1":
        raise ValidatorError("release profile must be devnet_full_v1")
    if not values["OCTRA_CHAIN_ID"].startswith("octra-devnet-"):
        raise ValidatorError("chain id must identify devnet")
    if values["OCTRA_P2P_REQUIRE_BINARY_HASH"] != "1":
        raise ValidatorError("binary hash admission must be enabled")
    if values["OCTRA_FHE_MAX_PER_EPOCH"] != "1":
        raise ValidatorError("first quorum run requires one FHE operation per epoch")
    if values["OCTRA_STEALTH_MAX_PER_EPOCH"] != "1":
        raise ValidatorError("first quorum run requires one stealth operation per epoch")
    if values["OCTRA_EPOCH_DURATION"] != "10":
        raise ValidatorError("first quorum run requires a ten second minimum cadence")
    activation = nonnegative_int(values, "OCTRA_EMISSION_ACTIVATION_EPOCH")
    checkpoint = nonnegative_int(values, "OCTRA_CHECKPOINT_EPOCH")
    nonnegative_int(values, "OCTRA_CHECKPOINT_TXID_HI")
    receipt = nonnegative_int(values, "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH")
    private_result = nonnegative_int(
        values, "OCTRA_PRIVATE_RESULT_ACTIVATION_EPOCH"
    )
    protocol_activation = nonnegative_int(
        values, "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH"
    )
    migration = nonnegative_int(values, "OCTRA_PVAC_MIGRATION_ACTIVATION_EPOCH")
    validator_activation = nonnegative_int(
        values, "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH"
    )
    if activation != receipt or activation != migration:
        raise ValidatorError("activation epochs must match")
    if activation != checkpoint + 1:
        raise ValidatorError("activation epoch must equal checkpoint epoch plus one")
    if private_result <= checkpoint:
        raise ValidatorError("private result activation must follow checkpoint")
    if protocol_activation <= checkpoint:
        raise ValidatorError("proposal protocol activation must follow checkpoint")
    if validator_activation <= checkpoint:
        raise ValidatorError("validator admission activation must follow checkpoint")
    if validator_activation < activation:
        raise ValidatorError("validator admission cannot precede emission")
    if not HEX64.fullmatch(values["OCTRA_BINARY_HASH"]):
        raise ValidatorError("invalid binary hash")
    if not HEX64.fullmatch(values["OCTRA_CONSENSUS_CONFIG_HASH"]):
        raise ValidatorError("invalid consensus config hash")
    if not HEX64.fullmatch(values["OCTRA_CHECKPOINT_STATE_ROOT"]):
        raise ValidatorError("invalid checkpoint state root")
    if not HEX64.fullmatch(values["OCTRA_PVAC_MIGRATION_ROOT"]):
        raise ValidatorError("invalid migration root")
    validator_entries(values["OCTRA_VALIDATORS"])
    exporter_entries(values["OCTRA_STATE_SYNC_EXPORTERS"])
    state_sync_sources(values["OCTRA_STATE_SYNC_SOURCES"])
    validate_program_keys(values["OCTRA_PROGRAM_RELEASE_KEYS"])
    bootstraps = endpoint_list(values["OCTRA_BOOTSTRAP_PEERS"])
    if len(bootstraps) < 2:
        raise ValidatorError("at least two bootstrap endpoints are required")
    peers = values.get("OCTRA_PEERS", values["OCTRA_BOOTSTRAP_PEERS"])
    endpoint_list(peers)
    values["OCTRA_PEERS"] = peers
    entitlement = Path(values["OCTRA_PVAC_MIGRATION_ENTITLEMENTS"])
    if not entitlement.is_absolute():
        entitlement = Path(bundle_dir) / entitlement
    entitlement = entitlement.resolve()
    if not entitlement.is_file():
        raise ValidatorError(f"migration entitlement file is missing: {entitlement}")
    validate_entitlement(entitlement, values)
    values["OCTRA_PVAC_MIGRATION_ENTITLEMENTS"] = str(entitlement)
    return values

def load_network(path, expected_hash):
    bundle = Path(path).resolve()
    if not bundle.is_file():
        raise ValidatorError(f"network bundle is missing: {bundle}")
    actual_hash = sha256_file(bundle)
    if not HEX64.fullmatch(expected_hash) or actual_hash != expected_hash:
        raise ValidatorError(f"network bundle hash mismatch: {actual_hash}")
    values = parse_env(bundle)
    validate_network(values, bundle.parent)
    return bundle, actual_hash, values

def private_mode(path):
    mode = stat.S_IMODE(Path(path).stat().st_mode)
    if mode & 0o077:
        raise ValidatorError(f"private file permissions are too broad: {path}")

def load_wallet(path):
    wallet_path = Path(path)
    try:
        payload = json.loads(wallet_path.read_text(encoding="utf-8"))
        private_key = base64.b64decode(payload["priv"], validate=True)
        public_key = base64.b64decode(payload["pub"], validate=True)
        address = payload["address"]
    except Exception as error:
        raise ValidatorError("invalid wallet file") from error
    if len(private_key) != 32 or len(public_key) != 32:
        raise ValidatorError("invalid wallet key length")
    try:
        from nacl.signing import SigningKey
        expected_public = bytes(SigningKey(private_key).verify_key)
    except Exception as error:
        raise ValidatorError("wallet key verification failed") from error
    if public_key != expected_public or address_from_pubkey(public_key) != address:
        raise ValidatorError("wallet identity mismatch")
    if not canonical_address(address):
        raise ValidatorError("wallet address is noncanonical")
    private_mode(wallet_path)
    return payload

def ensure_wallet(path):
    wallet_path = Path(path)
    wallet_path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    if wallet_path.exists():
        return load_wallet(wallet_path)
    try:
        from nacl.signing import SigningKey
    except Exception as error:
        raise ValidatorError("python3-nacl is required") from error
    signing_key = SigningKey.generate()
    private_key = bytes(signing_key)
    public_key = bytes(signing_key.verify_key)
    payload = {
        "priv": base64.b64encode(private_key).decode("ascii"),
        "pub": base64.b64encode(public_key).decode("ascii"),
        "address": address_from_pubkey(public_key),
    }
    temporary = wallet_path.with_name(wallet_path.name + ".new")
    descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, separators=(",", ":"))
        handle.write("\n")
        handle.flush()
        os.fsync(handle.fileno())
    os.replace(temporary, wallet_path)
    os.chmod(wallet_path, 0o600)
    return load_wallet(wallet_path)

def copy_private(source, target):
    source_path = Path(source)
    target_path = Path(target)
    target_path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
    temporary = target_path.with_name(target_path.name + ".new")
    with source_path.open("rb") as reader:
        descriptor = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
        with os.fdopen(descriptor, "wb") as writer:
            shutil.copyfileobj(reader, writer)
            writer.flush()
            os.fsync(writer.fileno())
    os.replace(temporary, target_path)
    os.chmod(target_path, 0o600)

def read_digest(path):
    try:
        value = Path(path).read_text(encoding="utf-8").split()[0]
    except Exception as error:
        raise ValidatorError(f"cannot read digest file: {path}") from error
    if not HEX64.fullmatch(value):
        raise ValidatorError(f"invalid digest file: {path}")
    return value

def state_ready(data_dir):
    root = Path(data_dir)
    required = [root / "HEAD.json", root / "irmin_store", root / "chaindata"]
    return all(path.exists() for path in required)

def load_head(data_dir):
    path = Path(data_dir) / "HEAD.json"
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
        epoch = int(value["epoch_id"])
        state_root = value["state_root"]
        txid_hi = int(value["txid_hi"])
    except Exception as error:
        raise ValidatorError("invalid checkpoint HEAD.json") from error
    if epoch < 0 or txid_hi < 0 or not HEX64.fullmatch(state_root):
        raise ValidatorError("invalid checkpoint fields")
    return {"epoch": epoch, "state_root": state_root, "txid_hi": txid_hi}

def validate_checkpoint(data_dir, values, allow_progress=False):
    head = load_head(data_dir)
    expected = {
        "epoch": int(values["OCTRA_CHECKPOINT_EPOCH"]),
        "state_root": values["OCTRA_CHECKPOINT_STATE_ROOT"],
        "txid_hi": int(values["OCTRA_CHECKPOINT_TXID_HI"]),
    }
    if head["epoch"] < expected["epoch"] or head["txid_hi"] < expected["txid_hi"]:
        raise ValidatorError("checkpoint does not match the network bundle")
    if head["epoch"] == expected["epoch"] and head != expected:
        raise ValidatorError("checkpoint does not match the network bundle")
    if head["epoch"] > expected["epoch"] and not allow_progress:
        raise ValidatorError("checkpoint is ahead of the network bundle")
    return head