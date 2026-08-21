# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import base64
import binascii
import datetime
import json
import os
import re
import stat
import subprocess
import urllib.error
import urllib.request
from pathlib import Path

from nacl.exceptions import BadSignatureError
from nacl.signing import VerifyKey

from validator_common import ValidatorError

URL = "https://releases.octra.network/v1/devnet/latest.json"
KEY_ID = "devnet-release-f912b4891be62acc"
CHAIN = "octra-devnet-9871-cluster"
KEY = base64.b64decode("3csWqO5Eoat2ZtlmcclQ1LuCCafYPKv2uo9CwZv0Mzs=")
STAGE_SERIAL = 0
FIELDS = (
    "schema",
    "chain_id",
    "key_id",
    "sequence",
    "action",
    "notice_code",
    "public_commit",
    "source_commit",
    "network_sha256",
    "runtime_profile_hash",
    "consensus_profile",
    "consensus_rules_id",
    "issued_at",
    "expires_at",
)
ACTIONS = frozenset({"current", "recommended", "required", "hold"})
NOTICES = frozenset({
    "consensus_recovery",
    "routine_update",
    "release_hold",
    "release_current",
})
HEX40 = re.compile(r"^[0-9a-f]{40}$")
HEX64 = re.compile(r"^[0-9a-f]{64}$")

class OriginError(ValidatorError):
    pass

class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, request, file_pointer, code, message, headers, url):
        return None

def release_time(raw):
    if not isinstance(raw, str):
        raise ValidatorError("release marker time is invalid")
    try:
        value = datetime.datetime.fromisoformat(raw.replace("Z", "+00:00"))
    except ValueError as error:
        raise ValidatorError("release marker time is invalid") from error
    if value.tzinfo is None:
        raise ValidatorError("release marker time is invalid")
    return value.astimezone(datetime.timezone.utc)

def payload(value):
    return json.dumps(
        [value[field] for field in FIELDS],
        ensure_ascii=False,
        separators=(",", ":"),
    ).encode("utf-8")

def validate(value, fresh=True, now=None):
    if not isinstance(value, dict) or set(value) != {*FIELDS, "signature"}:
        raise ValidatorError("release marker fields are invalid")
    if value["schema"] != "octra-devnet-release-v2":
        raise ValidatorError("release marker schema is invalid")
    if value["key_id"] != KEY_ID:
        raise ValidatorError("release marker key id is invalid")
    if value["chain_id"] != CHAIN:
        raise ValidatorError("release marker chain is invalid")
    if value["action"] not in ACTIONS:
        raise ValidatorError("release marker action is invalid")
    if value["notice_code"] not in NOTICES:
        raise ValidatorError("release marker notice code is invalid")
    sequence = value["sequence"]
    if not isinstance(sequence, int) or isinstance(sequence, bool) or sequence < 1:
        raise ValidatorError("release marker sequence is invalid")
    for field in ("public_commit", "source_commit"):
        if not isinstance(value[field], str) or not HEX40.fullmatch(value[field]):
            raise ValidatorError(f"release marker {field} is invalid")
    if value["public_commit"] == "0" * 40:
        raise ValidatorError("release marker public commit is not published")
    for field in ("network_sha256", "runtime_profile_hash"):
        if not isinstance(value[field], str) or not HEX64.fullmatch(value[field]):
            raise ValidatorError(f"release marker {field} is invalid")
    profile = value["consensus_profile"]
    if not isinstance(profile, int) or isinstance(profile, bool) or profile < 1:
        raise ValidatorError("release marker consensus profile is invalid")
    rules = value["consensus_rules_id"]
    if not isinstance(rules, str) or not re.fullmatch(r"[a-z0-9_]{1,64}", rules):
        raise ValidatorError("release marker consensus rules id is invalid")
    signature = value["signature"]
    if not isinstance(signature, str) or not re.fullmatch(r"[A-Za-z0-9+/]{86}==", signature):
        raise ValidatorError("release marker signature encoding is invalid")
    issued = release_time(value["issued_at"])
    expires = release_time(value["expires_at"])
    clock = now or datetime.datetime.now(datetime.timezone.utc)
    if issued > clock + datetime.timedelta(minutes=5):
        raise ValidatorError("release marker was issued in the future")
    if fresh and expires <= clock:
        raise ValidatorError("release marker is expired")
    if issued >= expires or expires - issued > datetime.timedelta(hours=72):
        raise ValidatorError("release marker time range is invalid")

def verify(value):
    try:
        signature = base64.b64decode(value["signature"], validate=True)
    except (ValueError, binascii.Error) as error:
        raise ValidatorError("release marker signature is invalid") from error
    if len(signature) != 64:
        raise ValidatorError("release marker signature is invalid")
    try:
        VerifyKey(KEY).verify(payload(value), signature)
    except (BadSignatureError, ValueError) as error:
        raise ValidatorError("release marker signature verification failed") from error

def decode(raw, fresh=True, now=None):
    if len(raw) > 16_384:
        raise ValidatorError("release marker exceeds size limit")
    try:
        value = json.loads(raw.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        raise ValidatorError("release marker is not valid JSON") from error
    validate(value, fresh=fresh, now=now)
    verify(value)
    return value

def cache_path(root):
    result = subprocess.run(
        ["git", "rev-parse", "--git-path", "octra-release-v2.json"],
        cwd=root,
        check=True,
        capture_output=True,
        text=True,
    )
    path = Path(result.stdout.strip())
    return path if path.is_absolute() else (root / path).resolve()

def read(path, fresh, now=None):
    try:
        descriptor = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    except FileNotFoundError:
        return None
    except OSError as error:
        raise ValidatorError("cached release marker is unreadable") from error
    try:
        with os.fdopen(descriptor, "rb") as handle:
            info = os.fstat(handle.fileno())
            if not stat.S_ISREG(info.st_mode) or info.st_size > 16_384:
                raise ValidatorError("cached release marker is invalid")
            raw = handle.read(16_385)
        if len(raw) > 16_384:
            raise ValidatorError("cached release marker is invalid")
        return decode(raw, fresh=fresh, now=now)
    except OSError as error:
        raise ValidatorError("cached release marker is unreadable") from error

def sync_dir(path):
    descriptor = os.open(path, os.O_RDONLY | os.O_DIRECTORY)
    try:
        os.fsync(descriptor)
    finally:
        os.close(descriptor)

def open_stage(path):
    global STAGE_SERIAL
    for _ in range(1024):
        serial = STAGE_SERIAL
        STAGE_SERIAL += 1
        stage = path.with_name(f".{path.name}.{os.getpid()}.{serial}.staged")
        try:
            descriptor = os.open(
                stage,
                os.O_WRONLY | os.O_CREAT | os.O_EXCL,
                0o600,
            )
            return descriptor, stage
        except FileExistsError:
            pass
    raise ValidatorError("release marker staged record limit exceeded")

def write(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    encoded = (json.dumps(value, ensure_ascii=False, indent=2) + "\n").encode("utf-8")
    descriptor, stage = open_stage(path)
    try:
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(encoded)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(stage, path)
        sync_dir(path.parent)
    finally:
        try:
            os.unlink(stage)
        except FileNotFoundError:
            pass

def fetch(url=URL):
    request = urllib.request.Request(
        url,
        headers={"Accept": "application/json", "User-Agent": "octra-upgrade/1"},
    )
    opener = urllib.request.build_opener(NoRedirect)
    try:
        with opener.open(request, timeout=15.0) as response:
            if response.geturl() != url:
                raise ValidatorError("release marker redirect is refused")
            raw = response.read(16_385)
    except (OSError, urllib.error.URLError, urllib.error.HTTPError) as error:
        raise OriginError("release marker origin is unavailable") from error
    return decode(raw)

def trusted(root):
    path = cache_path(root)
    prior = read(path, fresh=False)
    try:
        value = fetch()
        source = "origin"
    except OriginError:
        cached = read(path, fresh=True)
        if cached is None:
            raise
        value = cached
        source = "cache"
    if prior is not None:
        if value["sequence"] < prior["sequence"]:
            raise ValidatorError("release marker sequence would roll back")
        if value["sequence"] == prior["sequence"] and value != prior:
            raise ValidatorError("release marker sequence was reused")
    if prior != value:
        write(path, value)
    return value, source