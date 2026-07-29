# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import time
import urllib.error
import urllib.request

from validator_common import ValidatorError

USER_AGENT = "octra-validator-controls/1"

def call(url, method, params, timeout=10):
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
    except (OSError, ValueError, urllib.error.URLError) as error:
        raise ValidatorError(f"RPC unavailable: {url}") from error
    if not isinstance(payload, dict):
        raise ValidatorError("invalid RPC response")
    if payload.get("error") is not None:
        raise ValidatorError("RPC rejected request: " + json.dumps(
            payload["error"],
            separators=(",", ":"),
            sort_keys=True,
        ))
    if "result" not in payload:
        raise ValidatorError("RPC response has no result")
    return payload["result"]

def transaction(url, tx_hash):
    try:
        return call(url, "octra_transaction", [tx_hash])
    except ValidatorError:
        return None

def wait_transaction(url, tx_hash, timeout, poll):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        value = transaction(url, tx_hash)
        status = value.get("status") if isinstance(value, dict) else None
        if status == "confirmed":
            return value
        if status in {"rejected", "dropped"}:
            detail = json.dumps(value, separators=(",", ":"), sort_keys=True)
            raise ValidatorError(f"transaction {status}: {detail}")
        time.sleep(poll)
    raise ValidatorError(f"transaction confirmation timed out: {tx_hash}")