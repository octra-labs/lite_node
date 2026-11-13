# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import base64
import hashlib
import json
import math
import re

from nacl.signing import VerifyKey

from validator_common import ValidatorError
from validator_exit import BOND_ESCROW, valid_hash

FEE = 1000
FIELDS = ("from", "to_", "amount", "nonce", "ou", "timestamp", "op_type")

def amount_raw(text):
    if not isinstance(text, str) or len(text) > 40 or not re.fullmatch(
        r"[0-9]+(?:\.[0-9]{1,6})?", text
    ):
        raise ValidatorError("amount must be OCT with at most six decimal places")
    whole, _, fraction = text.partition(".")
    value = int(whole) * 1000000 + int(fraction.ljust(6, "0"))
    if value < 1000000:
        raise ValidatorError("bond increase must be at least 1 OCT")
    return value

def wire(fields, stamp):
    encoded = json.dumps(fields, separators = (",", ":"), ensure_ascii = True)
    return re.sub(r'"timestamp":[0-9.eE+\-]+', '"timestamp":' + stamp,
        encoded, count = 1).encode()

def payment(record, wallet, chain, amount):
    tx = record.get("tx")
    stamp = record.get("stamp")
    if not isinstance(tx, dict) or not isinstance(stamp, str) or not re.fullmatch(
        r"[0-9]+(?:\.[0-9]+)?(?:[eE][+\-]?[0-9]+)?", stamp
    ):
        raise ValidatorError("saved bond payment is incomplete")
    expected = {"from": wallet["address"], "to_": BOND_ESCROW,
        "public_key": wallet["pub"], "op_type": "validator_bond",
        "amount": str(amount), "ou": str(FEE)}
    if any(tx.get(key) != value for key, value in expected.items()) or (
        tx.get("encrypted_data") is not None
        or set(tx) - set(FIELDS) - {"public_key", "signature", "message", "encrypted_data"}
        or type(tx.get("nonce")) is not int or tx["nonce"] < 1
        or type(tx.get("timestamp")) not in (int, float)
        or not math.isfinite(tx["timestamp"]) or tx["timestamp"] <= 0
        or float(stamp) != tx["timestamp"] or not valid_hash(record.get("tx_hash"))
    ):
        raise ValidatorError("saved bond payment differs")
    try:
        public = base64.b64decode(wallet["pub"], validate = True)
        key = VerifyKey(public)
        signed = {name: tx[name] for name in FIELDS}
        signed["message"] = tx["message"]
        key.verify(wire(signed, stamp), base64.b64decode(tx["signature"], validate = True))
        proof = json.loads(tx["message"])
        if set(proof) != {"consensus_pubkey", "proof"} or proof["consensus_pubkey"] != wallet["pub"]:
            raise ValueError("bond key differs")
        parts = [b"octra:validator_bond:v1", chain.encode(), wallet["address"].encode(),
            str(amount).encode(), str(tx["nonce"]).encode(), public]
        message = b"".join(str(len(part)).encode() + b":" + part for part in parts)
        key.verify(message, base64.b64decode(proof["proof"], validate = True))
    except Exception as error:
        raise ValidatorError("bond payment signature or network proof is invalid") from error
    fields = {name: tx[name] for name in FIELDS[:-1]}
    fields.update({"signature": tx["signature"], "op_type": tx["op_type"],
        "public_key": tx["public_key"], "message": tx["message"], "encrypted_data": None})
    if hashlib.sha256(wire(fields, stamp)).hexdigest() != record["tx_hash"]:
        raise ValidatorError("bond transaction hash differs")
    return tx

def receipt(value, record, head):
    tx = record["tx"]
    fields = {"tx_hash": record["tx_hash"], "from": tx["from"], "to": tx["to_"],
        "amount_raw": tx["amount"], "nonce": tx["nonce"], "op_type": tx["op_type"]}
    if not isinstance(value, dict) or any(
        type(value.get(key)) is not type(expected) or value[key] != expected
        for key, expected in fields.items()
    ):
        raise ValidatorError("bond transaction receipt differs")
    status = value.get("status")
    if status == "confirmed" and (
        type(value.get("epoch")) is not int or not record["head"] <= value["epoch"] <= head
    ):
        raise ValidatorError("bond confirmation epoch differs")
    if status not in {"confirmed", "pending", "staging", "rejected", "dropped"}:
        raise ValidatorError("bond transaction status is invalid")
    return status