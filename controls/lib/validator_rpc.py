# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import http.client
import time
import urllib.error
import urllib.request

from validator_common import ValidatorError

USER_AGENT = "octra-validator-controls/1"

def call(url, method, params, timeout = 10, *, missing = False):
    body = json.dumps({
        "jsonrpc": "2.0",
        "id": int(time.time() * 1000) % 1000000,
        "method": method,
        "params": params,
    }).encode("utf-8")
    request = urllib.request.Request(
        url,
        data=body,
        headers={
            "Accept": "application/json",
            "Content-Type": "application/json",
            "User-Agent": USER_AGENT,
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            payload = json.loads(response.read())
    except (OSError, ValueError, urllib.error.URLError, http.client.HTTPException) as error:
        raise ValidatorError(f"RPC unavailable: {url}") from error
    if not isinstance(payload, dict):
        raise ValidatorError("invalid RPC response")
    if payload.get("error") is not None:
        error = payload["error"]
        if missing and isinstance(error, dict) and type(error.get("code")) is int and error["code"] == 112:
            return None
        raise ValidatorError("RPC rejected request: " + json.dumps(
            payload["error"],
            separators=(",", ":"),
            sort_keys=True,
        ))
    if "result" not in payload:
        raise ValidatorError("RPC response has no result")
    if missing and payload["result"] is None:
        raise ValidatorError("invalid transaction RPC response")
    return payload["result"]

def transaction(url, tx_hash):
    value = call(url, "octra_transaction", [tx_hash], missing = True)
    if value is None:
        return None
    statuses = ("confirmed", "rejected", "dropped", "pending", "staging")
    if not isinstance(value, dict) or value.get("tx_hash") != tx_hash or value.get("status") not in statuses:
        raise ValidatorError("invalid transaction RPC response")
    return value

def wait_transaction(url, tx_hash, timeout, poll):
    deadline = time.monotonic() + timeout
    read_error = None
    while time.monotonic() < deadline:
        try:
            value = transaction(url, tx_hash)
            read_error = None
        except ValidatorError as error:
            read_error = error
            time.sleep(poll)
            continue
        status = value.get("status") if isinstance(value, dict) else None
        if status == "confirmed":
            return value
        if status in {"rejected", "dropped"}:
            detail = json.dumps(value, separators=(",", ":"), sort_keys=True)
            raise ValidatorError(f"transaction {status}: {detail}")
        time.sleep(poll)
    detail = f"; last read error: {read_error}" if read_error is not None else ""
    raise ValidatorError(f"transaction confirmation timed out: {tx_hash}{detail}")