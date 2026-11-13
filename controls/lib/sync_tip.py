# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import os
import socket
import stat
import time
from http.client import HTTPException
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.request import Request, HTTPRedirectHandler, build_opener
from validator_common import ValidatorError, state_sync_sources

HEAD_LIMIT = 65_536
CERT_LIMIT = 33_554_432
HEADERS = {"Accept": "application/json", "User-Agent": "octra-upgrade/1"}

class NoRedirect(HTTPRedirectHandler):
    def http_error_302(self, request, response, code, message, headers):
        raise HTTPError(request.full_url, code, message, headers, response)

    http_error_301 = http_error_303 = http_error_307 = http_error_308 = http_error_302

http = build_opener(NoRedirect())

def epoch_value(value):
    if type(value) is not int and not (isinstance(value, str) and value.isascii() and value.isdecimal()):
        raise ValidatorError("state sync epoch is invalid")
    epoch = int(value)
    if not 0 <= epoch < 2 ** 63:
        raise ValidatorError("state sync epoch is invalid")
    return epoch

def decode_target(payload, chain):
    fields = {"chainId", "status", "localEpoch", "headRoot", "setStamp", "targetEpoch"}
    if not isinstance(payload, dict) or set(payload) != fields:
        return None
    if not isinstance(chain, str) or not chain or payload["chainId"] != chain or payload["status"] != "agreed":
        return None
    if any(not isinstance(payload[name], str) or not 0 < len(payload[name]) <= 19
            for name in ("localEpoch", "targetEpoch")):
        return None
    try:
        local = epoch_value(payload["localEpoch"])
        target = epoch_value(payload["targetEpoch"])
    except ValidatorError:
        return None
    root, stamp = payload["headRoot"], payload["setStamp"]
    if (str(local) != payload["localEpoch"] or str(target) != payload["targetEpoch"]
            or target < local or not isinstance(root, str) or len(root) not in (64, 128)
            or any(value not in "0123456789abcdef" for value in root)
            or not isinstance(stamp, str) or not 0 < len(stamp) <= 1024
            or any(not 33 <= ord(value) <= 126 for value in stamp)):
        return None
    return {"head": target, "local": local, "root": root, "stamp": stamp}

def control_path(data_dir):
    data = Path(data_dir).expanduser().resolve(strict = True)
    folder = data / "control"
    path = folder / "sync.sock"
    entries = [item.lstat() for item in (data, folder, path)]
    parent, directory, endpoint = entries
    if (not stat.S_ISDIR(parent.st_mode) or parent.st_mode & 0o022
            or not stat.S_ISDIR(directory.st_mode) or directory.st_mode & 0o077
            or not stat.S_ISSOCK(endpoint.st_mode) or endpoint.st_mode & 0o077
            or any(item.st_uid != parent.st_uid for item in entries)):
        raise ValueError("sync control ownership differs")
    return path, (endpoint.st_dev, endpoint.st_ino)

def control_target(data_dir):
    path, identity = control_path(data_dir)
    deadline = time.monotonic() + 3.0
    request = {"jsonrpc": "2.0", "id": 1, "method": "octra_syncTarget", "params": []}
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
        client.settimeout(3.0)
        address = min(os.fspath(path), os.path.relpath(path), key = lambda value: len(os.fsencode(value)))
        client.connect(address)
        if control_path(data_dir) != (path, identity):
            raise ValueError("sync control socket changed")
        client.sendall(json.dumps(request).encode("ascii") + b"\n")
        raw = bytearray()
        while b"\n" not in raw:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError("sync control deadline expired")
            client.settimeout(remaining)
            chunk = client.recv(4097 - len(raw))
            if not chunk or len(raw) + len(chunk) > 4096:
                raise ValueError("sync control response exceeds frame")
            raw.extend(chunk)
        if raw.count(b"\n") != 1 or not raw.endswith(b"\n"):
            raise ValueError("sync control response framing differs")
    if control_path(data_dir) != (path, identity):
        raise ValueError("sync control socket changed")
    reply = json.loads(raw.decode("utf-8"))
    if (not isinstance(reply, dict) or set(reply) != {"jsonrpc", "id", "result"}
            or reply["jsonrpc"] != "2.0" or type(reply["id"]) is not int or reply["id"] != 1):
        raise ValueError("sync control response differs")
    return reply["result"]

def native_tip(values, *, query=None):
    data = values.get("OCTRA_DATA_DIR")
    if not values.get("OCTRA_CHAIN_ID") or not isinstance(data, str) or not data:
        return None
    try:
        payload = control_target(data)
    except (OSError, ValueError, TypeError, RecursionError):
        return None
    return decode_target(payload, values["OCTRA_CHAIN_ID"])

def cert_epoch(source):
    request = Request(source + "/state-sync/manifest", headers = HEADERS)
    with http.open(request, timeout = 15.0) as response:
        raw = response.read(CERT_LIMIT + 1)
    if len(raw) > CERT_LIMIT:
        raise ValidatorError("state sync certificate exceeds byte limit")
    try:
        payload = json.loads(raw.decode("utf-8"))
        epoch = epoch_value(payload["checkpoint"]["epoch"])
        files = payload["manifest"]["files"]
        roots = [item for item in files
            if isinstance(item, dict) and item.get("path") == "ready_roots"]
        root_size = epoch_value(roots[0]["size"]) if len(roots) == 1 else 0
    except (KeyError, TypeError, ValueError, UnicodeDecodeError, RecursionError) as error:
        raise ValidatorError("state sync certificate is invalid") from error
    if payload.get("version") != "octra-state-sync" or not isinstance(files, list) or epoch < 0:
        raise ValidatorError("state sync certificate fields differ")
    if len(roots) != 1 or root_size <= 0:
        raise ValidatorError("state sync root window is unavailable")
    return epoch

def sync_epoch(source, manifest = True):
    request = Request(source + "/state-sync/head", headers = HEADERS)
    with http.open(request, timeout = 15.0) as response:
        raw = response.read(HEAD_LIMIT + 1)
    if len(raw) > HEAD_LIMIT:
        raise ValidatorError("state sync manifest exceeds byte limit")
    try:
        payload = json.loads(raw.decode("utf-8"))
        epoch = epoch_value(payload["head_epoch"])
        current = epoch_value(payload["current_epoch"])
        snapshot = payload.get("snapshot_epoch")
        snapshot = None if snapshot is None else epoch_value(snapshot)
    except (KeyError, TypeError, ValueError, UnicodeDecodeError, RecursionError) as error:
        raise ValidatorError("state sync head is invalid") from error
    if (payload.get("version") != "octra-state-sync"
            or payload.get("mode") != "live_head" or epoch < 0 or current != epoch + 1
            or (snapshot is not None and snapshot > epoch)):
        raise ValidatorError("state sync head fields differ")
    if not manifest:
        return {"head": epoch, "snapshot": None, "latest": None,
            "status": "head_only", "advertised_status": "unknown", "source": source, "reason": None}
    try:
        certificate = cert_epoch(source)
        if certificate > epoch or (snapshot is not None and snapshot != certificate):
            raise ValidatorError("state sync certificate epoch differs")
    except (OSError, HTTPException, ValidatorError, ValueError) as error:
        if isinstance(error, HTTPError):
            error.close()
        certificate, reason = None, source_reason(error)
    else:
        reason = None
    advertised = payload.get("snapshot_status")
    return {"head": epoch, "snapshot": certificate, "latest": snapshot,
        "status": "ready" if certificate is not None else "unavailable",
        "advertised_status": advertised if advertised in ("ready", "old_epoch", "missing") else "unknown",
        "source": source, "reason": reason}

def source_reason(error):
    if isinstance(error, HTTPError):
        return f"http_{error.code}"
    if isinstance(error, URLError):
        error = error.reason
    if isinstance(error, TimeoutError):
        return "timeout"
    if isinstance(error, HTTPException):
        return "http_response"
    if isinstance(error, ValidatorError):
        return str(error)
    if isinstance(error, OSError):
        return "transport"
    if isinstance(error, ValueError):
        return "url_invalid"
    return "transport"

def head_routes(values, sources):
    heads = state_sync_sources(values["OCTRA_JOIN_RPC"]) if values.get("OCTRA_JOIN_RPC") else []
    return [head for head in heads if head not in sources]

def sync_head(values):
    tips = []
    sources = state_sync_sources(values["OCTRA_STATE_SYNC_SOURCES"]) if values.get("OCTRA_STATE_SYNC_SOURCES") else []
    heads = state_sync_sources(values["OCTRA_JOIN_RPC"]) if values.get("OCTRA_JOIN_RPC") else []
    native = native_tip(values)
    for source in dict.fromkeys(sources + heads):
        try:
            tips.append(sync_epoch(source, manifest = source in sources))
        except (OSError, HTTPException, ValidatorError, ValueError) as error:
            if isinstance(error, HTTPError):
                error.close()
            tips.append({"head": None, "snapshot": None, "latest": None,
                "status": "unavailable", "advertised_status": "unknown",
                "source": source, "reason": source_reason(error)})
    if not tips and native is None:
        return None
    snapshots = [value["snapshot"] for value in tips if value["snapshot"] is not None]
    latest = max((value for value in tips if value["source"] in sources),
        key = lambda value: value["latest"] if value["latest"] is not None else -1,
        default = {"latest": None, "status": "unavailable", "advertised_status": "unknown",
            "reason": "snapshot sources unavailable"})
    return {"head": native["head"] if native is not None else None,
        "snapshot": max(snapshots, default = None), "latest": latest["latest"],
        "status": "ready" if snapshots else latest["status"],
        "advertised_status": latest["advertised_status"],
        "reason": None if snapshots else latest["reason"], "sources": tips,
        "native": native, "head_status": "ready" if native is not None else "native_unavailable"}

def sync_fields(tip, floor):
    tip = tip or {}
    head = tip.get("head")
    available = tip.get("snapshot")
    advertised = tip.get("latest")
    return {"head": head if head is not None else "unknown", "required": floor,
        "available": available if available is not None else "unknown",
        "advertised": advertised if advertised is not None else "unknown",
        "lag": head - advertised if head is not None and advertised is not None else "unknown",
        "snapshot_status": "below_minimum" if available is not None and available < floor
            else tip.get("status", "ready" if available is not None else "unavailable"),
        "advertised_status": tip.get("advertised_status", "unknown"),
        "manifest_error": json.dumps(tip.get("reason"), ensure_ascii = True),
        "source_errors": json.dumps({row["source"]: row["reason"]
            for row in tip.get("sources", []) if row.get("reason") is not None},
            ensure_ascii = True, sort_keys = True),
        "head_status": "ready" if head is not None else tip.get("head_status", "native_unavailable")}

def sync_issue(tip, floor):
    if tip is not None and tip["snapshot"] is not None and tip["snapshot"] >= floor:
        return None
    return "wait for signed snapshot: " + " ".join(
        f"{key} = {value}" for key, value in sync_fields(tip, floor).items())