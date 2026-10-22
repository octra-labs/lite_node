# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
from http.client import HTTPException
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
    if not values.get("OCTRA_STATE_SYNC_SOURCES"):
        return None
    sources = state_sync_sources(values["OCTRA_STATE_SYNC_SOURCES"])
    heads = state_sync_sources(values["OCTRA_JOIN_RPC"]) if values.get("OCTRA_JOIN_RPC") else []
    if not head_routes(values, sources):
        return {"head": None, "snapshot": None, "latest": None,
            "status": "unavailable", "advertised_status": "unknown", "sources": [],
            "reason": "configure OCTRA_JOIN_RPC with a route outside OCTRA_STATE_SYNC_SOURCES",
            "head_status": "independent_missing"}
    for source in dict.fromkeys(sources + heads):
        try:
            tips.append(sync_epoch(source, manifest = source in sources))
        except (OSError, HTTPException, ValidatorError, ValueError) as error:
            if isinstance(error, HTTPError):
                error.close()
            tips.append({"head": None, "snapshot": None, "latest": None,
                "status": "unavailable", "advertised_status": "unknown",
                "source": source, "reason": source_reason(error)})
    if not tips:
        return None
    snapshots = [value["snapshot"] for value in tips if value["snapshot"] is not None]
    independent = any(value["head"] is not None and value["source"] in heads
        and value["source"] not in sources for value in tips)
    latest = max((value for value in tips if value["source"] in sources),
        key = lambda value: value["latest"] if value["latest"] is not None else -1,
        default = {"latest": None, "status": "unavailable", "advertised_status": "unknown",
            "reason": "snapshot sources unavailable"})
    head = max((value["head"] for value in tips if value["head"] is not None), default = None)
    return {"head": head if independent else None,
        "snapshot": max(snapshots, default = None), "latest": latest["latest"],
        "status": "ready" if snapshots else latest["status"],
        "advertised_status": latest["advertised_status"],
        "reason": None if snapshots else latest["reason"], "sources": tips}

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
        "head_status": "ready" if head is not None else tip.get("head_status", "independent_unavailable")}

def sync_issue(tip, floor):
    if (tip is not None and tip["head"] is not None
            and tip["snapshot"] is not None and tip["snapshot"] >= floor):
        return None
    prefix = "state sync configuration error: " if tip and tip.get("head_status") == "independent_missing" \
        else "wait for signed snapshot: "
    return prefix + " ".join(
        f"{key} = {value}" for key, value in sync_fields(tip, floor).items())