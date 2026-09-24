# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import fcntl
import hashlib
import json
import math
import os
import re
import stat
import subprocess
from contextlib import contextmanager
from pathlib import Path

from validator_common import ValidatorError, address_from_pubkey, copy_private, sha256_file, write_private_json
from validator_rpc import call, transaction

ATTEMPT_LIMIT = 32
BOND_ESCROW = address_from_pubkey(hashlib.sha256(b"octra:validator_bond:escrow").digest())
PAYMENT_FIELDS = frozenset({
    "from", "to_", "public_key", "amount", "ou", "op_type",
    "nonce", "message", "encrypted_data",
})

def valid_hash(value):
    return isinstance(value, str) and len(value) == 64 and all(
        char in "0123456789abcdef" for char in value
    )

def directory(values, *, create = True):
    path = Path(values["OCTRA_DATA_DIR"]) / "validator-control"
    if create:
        path.mkdir(mode = 0o700, exist_ok = True)
    try:
        info = path.lstat()
    except FileNotFoundError:
        if create:
            raise
        return path
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValidatorError("validator control directory is not private")
    if create:
        sync_directory(path.parent)
    return path

def sync_directory(path):
    fd = os.open(path, os.O_RDONLY)
    try:
        os.fsync(fd)
    finally:
        os.close(fd)

@contextmanager
def command_lock(values):
    data = Path(values["OCTRA_DATA_DIR"]).expanduser().resolve()
    path = data.with_name(data.name + ".enrollment.lock")
    fd = os.open(path, os.O_RDWR | os.O_CREAT | os.O_NOFOLLOW, 0o600)
    try:
        info = os.fstat(fd)
        if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
            raise ValidatorError("validator command lock is not private")
        try:
            fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            raise ValidatorError("another enrollment command is running") from error
        yield
    finally:
        os.close(fd)

def restore_control(source, target):
    prior = source / "validator-control"
    if not os.path.lexists(prior):
        return
    info = prior.lstat()
    if not stat.S_ISDIR(info.st_mode) or info.st_uid != os.getuid() or info.st_mode & 0o077:
        raise ValidatorError("preserved validator control directory is not private")
    destination = directory({"OCTRA_DATA_DIR": str(target)})
    for path in prior.iterdir():
        if path.name in {"lock", "exit.next"} or path.name.endswith(".json.new"):
            continue
        value = read_outbox(path)
        if value is None:
            raise ValidatorError("preserved validator control file disappeared")
        output = destination / path.name
        if os.path.lexists(output):
            raise ValidatorError("snapshot contains validator control state")
        copy_private(path, output)
    sync_directory(destination)

def control_view(value, values, wallet, bonded_epoch):
    if not isinstance(value, dict) or (
        value.get("chain_id") != values["OCTRA_CHAIN_ID"]
        or value.get("address") != wallet["address"]
        or value.get("consensus_pubkey") != wallet["pub"]
        or str(value.get("bonded_epoch")) != str(bonded_epoch)
    ):
        raise ValidatorError("validator exit intent enrollment differs")
    control = value.get("local_control")
    if not isinstance(control, dict) or control.get("exit_intent") is not True:
        raise ValidatorError("local node lacks exit coordination; update the node first")
    if control.get("error") is not None:
        raise ValidatorError("validator exit control: " + str(control["error"]))
    requested = control.get("exit_requested")
    intent_id = control.get("intent_id")
    if not isinstance(requested, bool) or (
        (requested and not valid_hash(intent_id)) or (not requested and intent_id is not None)
    ):
        raise ValidatorError("invalid validator exit control response")
    return control

def intent(values, wallet, wallet_path, bonded_epoch, url, action):
    value = call(url, "octra_validatorEnrollment", [])
    control_view(value, values, wallet, bonded_epoch)
    if action == "cancel" and value.get("exit_epoch") is not None:
        raise ValidatorError("confirmed exit cannot resume duty")
    binary = Path(values["OCTRA_OPERATOR_CONTROL_BINARY"])
    if sha256_file(binary) != values["OCTRA_OPERATOR_CONTROL_BINARY_HASH"]:
        raise ValidatorError("validator control binary hash mismatch")
    result = subprocess.run(
        [str(binary), "--wallet", str(wallet_path), "--exit-intent", action,
         "--chain-id", values["OCTRA_CHAIN_ID"], "--bonded-epoch", str(bonded_epoch)],
        check=False, capture_output=True, text=True, timeout=10,
    )
    if result.returncode != 0:
        raise ValidatorError("validator exit intent refused: " + result.stderr.strip()[:1024])
    try:
        payload = json.loads(result.stdout)
    except ValueError as error:
        raise ValidatorError("invalid validator exit intent response") from error
    if not isinstance(payload, dict) or payload.get("status") != action:
        raise ValidatorError("validator exit intent action differs")
    intent_id = payload.get("intent_id")
    if (action == "request" and not valid_hash(intent_id)) or (
        action == "cancel" and intent_id is not None
    ):
        raise ValidatorError("validator exit intent id is invalid")
    view = control_view(
        call(url, "octra_validatorEnrollment", []), values, wallet, bonded_epoch,
    )
    if view["intent_id"] != intent_id or view["exit_requested"] != (action == "request"):
        raise ValidatorError("local node has not acknowledged exit intent; nothing submitted")
    return intent_id

def require_prepare(binary):
    result = subprocess.run([str(binary), "--capabilities"], check=False,
                            capture_output=True, text=True, timeout=10)
    try:
        value = json.loads(result.stdout)
    except ValueError as error:
        raise ValidatorError("control binary lacks safe preparation; update first") from error
    if result.returncode != 0 or not isinstance(value, dict) or (
        value.get("prepare_transaction") is not True
    ):
        raise ValidatorError("control binary lacks safe preparation; update first")

def outbox_path(values, operation, epoch):
    if not isinstance(operation, str) or operation not in {
        "validator_bond", "validator_exit", "validator_withdraw"
    } or (
        type(epoch) is not int or epoch < 0
    ):
        raise ValidatorError("invalid validator outbox operation")
    return directory(values) / f"{operation}-{epoch}.json"

def read_outbox(path):
    try:
        fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW | os.O_NONBLOCK)
    except FileNotFoundError:
        return None
    with os.fdopen(fd, "r", encoding="utf-8") as stream:
        info = os.fstat(stream.fileno())
        if not stat.S_ISREG(info.st_mode) or info.st_uid != os.getuid() or (
            info.st_mode & 0o077 or info.st_size > 65536
        ):
            raise ValidatorError("invalid validator exit outbox file")
        try:
            value = json.load(stream)
        except ValueError as error:
            raise ValidatorError("invalid validator exit outbox JSON") from error
    if not isinstance(value, dict):
        raise ValidatorError("invalid validator exit outbox object")
    return value

def check_outbox(value, values, wallet, operation, epoch):
    field = "head_epoch" if operation == "validator_bond" else "bonded_epoch"
    if value.get("chain_id") != values["OCTRA_CHAIN_ID"] or (
        type(value.get(field)) is not int or value[field] != epoch
    ):
        raise ValidatorError("validator exit outbox identity differs")
    tx = value.get("tx")
    if not valid_hash(value.get("tx_hash")) or not isinstance(tx, dict):
        raise ValidatorError("validator exit outbox transaction missing")
    amount = tx.get("amount")
    bond = operation == "validator_bond"
    amount_ok = (
        isinstance(amount, str) and amount.isascii() and amount.isdecimal()
        and not amount.startswith("0")
    ) if bond else amount == "0"
    target = BOND_ESCROW if bond else wallet["address"]
    if tx.get("from") != wallet["address"] or tx.get("to_") != target or (
        tx.get("public_key") != wallet["pub"] or tx.get("op_type") != operation
        or not amount_ok
        or (bond and value.get("bonded_epoch") is not None)
        or not isinstance(tx.get("nonce"), int) or isinstance(tx["nonce"], bool)
        or tx["nonce"] < 1
    ):
        raise ValidatorError("validator exit outbox transaction differs")
    fee = tx.get("ou")
    stamp = tx.get("timestamp")
    signature = tx.get("signature")
    time_ok = (type(stamp) is int and stamp >= 0) or (
        type(stamp) is float and math.isfinite(stamp) and stamp >= 0
    )
    if not isinstance(signature, str) or not signature or not time_ok or (
        not isinstance(fee, str) or not fee.isascii() or not fee.isdecimal()
        or (len(fee) > 1 and fee.startswith("0"))
    ):
        raise ValidatorError("incomplete signed transaction; restore control records")
    return tx

def renew_outbox(path, saved, values, wallet, operation, bonded_epoch, prepare):
    attempts = saved_attempts(path, values, wallet, operation, bonded_epoch)
    if len(attempts) >= ATTEMPT_LIMIT:
        raise ValidatorError("saved transaction attempt limit reached; renewal not signed")
    prepared = prepare(saved["tx"])
    if not isinstance(prepared, dict):
        raise ValidatorError("invalid renewed validator transaction")
    field = "head_epoch" if operation == "validator_bond" else "bonded_epoch"
    value = {**prepared, "chain_id": values["OCTRA_CHAIN_ID"], field: bonded_epoch}
    tx = check_outbox(value, values, wallet, operation, bonded_epoch)
    prior = saved["tx"]
    if any(tx.get(name) != prior.get(name) for name in PAYMENT_FIELDS):
        raise ValidatorError("renewal must keep the same nonce, fee and operation")
    if value["tx_hash"] == saved["tx_hash"]:
        if value != saved:
            raise ValidatorError("renewed transaction reuses hash with different bytes")
        return saved
    archive = path.with_name(path.stem + "-" + saved["tx_hash"] + ".json")
    if not os.path.lexists(archive):
        copy_private(path, archive)
    sync_directory(path.parent)
    write_private_json(path, value)
    return value

def remember_identity(target, value):
    prior = read_outbox(target)
    if prior is not None and any(
        prior.get(key) != value[key] for key in ("chain_id", "address", "pubkey")
    ):
        raise ValidatorError("saved validator attempt identity differs")
    if prior != value:
        write_private_json(target, value)
    sync_directory(target.parent)

def begin_bond(values, wallet, epoch = None):
    latest = read_pointer(values, wallet)
    if latest is not None and latest.get("operation") == "validator_bond" and (
        latest.get("head_epoch") == epoch and latest.get("tx_hash") is not None
    ):
        path = outbox_path(values, "validator_bond", epoch)
        attempts = saved_attempts(path, values, wallet, "validator_bond", epoch)
        if not any(item["tx_hash"] == latest["tx_hash"] for item in attempts):
            raise ValidatorError("saved bond attempt differs from outbox")
        return
    remember_identity(directory(values) / "last.json", {
        "chain_id": values["OCTRA_CHAIN_ID"], "address": wallet["address"],
        "pubkey": wallet["pub"], "operation": "validator_bond",
        "bonded_epoch": None, "tx_hash": None,
        **({"head_epoch": epoch} if epoch is not None else {}),
    })

def attempt_pointer(saved, wallet):
    return {
        "chain_id": saved["chain_id"], "address": wallet["address"],
        "pubkey": wallet["pub"], "operation": saved["tx"]["op_type"],
        "bonded_epoch": saved.get("bonded_epoch"), "tx_hash": saved["tx_hash"],
        **({"head_epoch": saved["head_epoch"]}
           if saved["tx"]["op_type"] == "validator_bond" else {}),
    }

def remember_attempt(path, saved, wallet):
    remember_identity(path.parent / "last.json", attempt_pointer(saved, wallet))

def read_pointer(values, wallet):
    root = directory(values, create = False)
    latest = read_outbox(root / "last.json")
    if latest is None:
        if any(root.glob("validator_*.json")):
            raise ValidatorError("saved validator pointer missing; run enroll.sh repair-pointer")
        return None
    if any(latest.get(key) != value for key, value in (
        ("chain_id", values["OCTRA_CHAIN_ID"]),
        ("address", wallet["address"]), ("pubkey", wallet["pub"]),
    )):
        raise ValidatorError("saved validator attempt identity differs")
    return latest

def pointer_status(values, wallet):
    path = Path(values["OCTRA_DATA_DIR"]) / "validator-control" / "last.json"
    try:
        value = read_pointer(values, wallet)
        if value is None:
            return {"status": "empty", "path": str(path)}
        operation = value.get("operation")
        epoch = value.get("head_epoch" if operation == "validator_bond" else "bonded_epoch")
        if not isinstance(operation, str) or operation not in {
            "validator_bond", "validator_exit", "validator_withdraw"
        } or (
            type(epoch) is not int or epoch < 0
        ):
            raise ValidatorError("saved validator pointer operation or epoch is invalid")
        record = path.parent / f"{operation}-{epoch}.json"
        unsigned = operation == "validator_bond" and value.get("tx_hash") is None
        if unsigned and not os.path.lexists(record):
            require_new_outbox(record, operation, epoch)
            status = "unsigned"
        else:
            attempts = saved_attempts(record, values, wallet, operation, epoch)
            if not unsigned and not any(item["tx_hash"] == value.get("tx_hash") for item in attempts):
                raise ValidatorError("saved validator pointer differs from outbox")
            status = "prepared" if unsigned else "signed"
        return {"status": status, "path": str(path), "operation": operation,
                "tx": value.get("tx_hash"), "action": "keep_latest_private_backup"}
    except (ValidatorError, OSError) as error:
        return {"status": "invalid" if os.path.lexists(path) else "missing",
                "path": str(path), "reason": str(error), "action": "restore_control_records"}

def repair_pointer(values, wallet, source):
    root = directory(values)
    value = read_outbox(Path(source))
    if value is None or any(value.get(key) != expected for key, expected in (
        ("chain_id", values["OCTRA_CHAIN_ID"]),
        ("address", wallet["address"]), ("pubkey", wallet["pub"]),
    )):
        raise ValidatorError("backup validator pointer missing or identity differs")
    operation = value.get("operation")
    field = "head_epoch" if operation == "validator_bond" else "bonded_epoch"
    epoch = value.get(field)
    selected_path = outbox_path(values, operation, epoch)
    tx_hash = value.get("tx_hash")
    unsigned = operation == "validator_bond" and tx_hash is None
    if not unsigned and not valid_hash(tx_hash):
        raise ValidatorError("backup validator transaction hash is invalid")
    groups = set()
    for path in root.glob("validator_*.json"):
        match = re.fullmatch(
            r"(validator_(?:bond|exit|withdraw))-(0|[1-9][0-9]*)(?:-[0-9a-f]{64})?\.json",
            path.name,
        )
        if match is None:
            raise ValidatorError("invalid validator record name; restore control records")
        groups.add((match[1], int(match[2])))
    by_nonce = {}
    selected = None
    for op, scope in sorted(groups):
        current = outbox_path(values, op, scope)
        saved = read_outbox(current)
        if saved is None:
            raise ValidatorError("saved transaction missing; restore control records before repairing pointer")
        attempts = saved_attempts(current, values, wallet, op, scope)
        nonce = saved["tx"]["nonce"]
        if nonce in by_nonce:
            raise ValidatorError("ambiguous validator nonce; restore control records before repairing pointer")
        by_nonce[nonce] = saved
        if current == selected_path:
            selected = next((attempt for attempt in attempts if attempt["tx_hash"] == tx_hash), None)
        if unsigned and (scope > epoch or current == selected_path):
            raise ValidatorError("backup intent predates saved validator records")
    if unsigned:
        expected = {
            "chain_id": values["OCTRA_CHAIN_ID"], "address": wallet["address"],
            "pubkey": wallet["pub"], "operation": operation,
            "bonded_epoch": None, "head_epoch": epoch, "tx_hash": None,
        }
    else:
        if selected is None or selected["tx"]["nonce"] != max(by_nonce, default = 0):
            raise ValidatorError("backup pointer does not identify the latest saved transaction")
        expected = attempt_pointer(selected, wallet)
    if value != expected:
        raise ValidatorError("backup validator pointer differs from saved transaction")
    target = root / "last.json"
    prior = read_outbox(target)
    if prior is not None and prior != value:
        raise ValidatorError("validator pointer already exists and differs; nothing changed")
    remember_identity(target, value)
    return tx_hash

def saved_attempts(path, values, wallet, operation, epoch):
    saved = read_outbox(path)
    if saved is None:
        raise ValidatorError("saved validator outbox missing")
    tx = check_outbox(saved, values, wallet, operation, epoch)
    attempts = {saved["tx_hash"]: saved}
    count = 1
    for archive in path.parent.glob(path.stem + "-*.json"):
        count += 1
        if count > ATTEMPT_LIMIT:
            raise ValidatorError("saved validator attempt limit exceeded; inspect before retrying")
        value = read_outbox(archive)
        if value is None or archive.name != path.stem + "-" + str(value.get("tx_hash")) + ".json":
            raise ValidatorError("saved validator archive hash differs")
        prior = check_outbox(value, values, wallet, operation, epoch)
        if any(prior.get(name) != tx.get(name) for name in PAYMENT_FIELDS):
            raise ValidatorError("saved validator archive payment differs")
        tx_hash = value["tx_hash"]
        if tx_hash in attempts and attempts[tx_hash] != value:
            raise ValidatorError("saved transaction hash has different bytes")
        attempts[tx_hash] = value
    return [attempts[tx_hash] for tx_hash in sorted(attempts)]

def confirmed_attempt(saved, url, epoch, head_epoch):
    receipt = transaction(url, saved["tx_hash"])
    if not isinstance(receipt, dict) or receipt.get("status") != "confirmed":
        return None
    tx = saved["tx"]
    fields = {
        "tx_hash": saved["tx_hash"], "from": tx["from"], "to": tx["to_"],
        "op_type": tx["op_type"], "nonce": tx["nonce"], "amount_raw": tx["amount"],
    }
    if any(receipt.get(key) != value or type(receipt.get(key)) is not type(value)
           for key, value in fields.items()):
        raise ValidatorError("saved transaction confirmation unavailable or differs")
    included = receipt.get("epoch")
    if type(included) is not int or not epoch <= included <= head_epoch:
        raise ValidatorError("saved transaction confirmation epoch differs")
    return saved, included

def bond_attempt(values, wallet, url, head):
    latest = read_pointer(values, wallet)
    if latest is None:
        return None
    if latest.get("operation") != "validator_bond":
        return None
    epoch = latest.get("head_epoch")
    if type(epoch) is not int or not 0 <= epoch <= head:
        raise ValidatorError("saved bond intent has no valid head; inspect before retrying")
    path = outbox_path(values, "validator_bond", epoch)
    saved = read_outbox(path)
    if saved is None:
        if latest.get("tx_hash") is not None:
            raise ValidatorError("saved bond transaction missing; inspect before retrying")
        return epoch, None, None
    attempts = saved_attempts(path, values, wallet, "validator_bond", epoch)
    if latest.get("tx_hash") is not None and not any(
        value["tx_hash"] == latest["tx_hash"] for value in attempts
    ):
        raise ValidatorError("saved bond attempt differs from outbox")
    confirmed = [value for attempt in attempts
                 if (value := confirmed_attempt(attempt, url, epoch + 1, head)) is not None]
    if len(confirmed) > 1:
        raise ValidatorError("multiple bond confirmations; inspect before retrying")
    return epoch, saved, confirmed[0] if confirmed else None

def completed_withdraw(values, wallet, url, head_epoch):
    latest = read_pointer(values, wallet)
    if latest is None:
        return None
    operation = latest.get("operation")
    epoch = latest.get("bonded_epoch")
    if operation == "validator_bond":
        return None
    path = outbox_path(values, operation, epoch)
    if operation != "validator_withdraw":
        return None
    attempts = saved_attempts(path, values, wallet, operation, epoch)
    if not any(saved["tx_hash"] == latest.get("tx_hash") for saved in attempts):
        raise ValidatorError("saved withdrawal attempt differs from outbox")
    confirmed = [value for saved in attempts
                 if (value := confirmed_attempt(saved, url, epoch, head_epoch)) is not None]
    if not confirmed:
        raise ValidatorError("saved withdrawal confirmation unavailable or differs")
    if len(confirmed) != 1:
        raise ValidatorError("multiple withdrawal confirmations; inspect before retrying")
    saved, _ = confirmed[0]
    tx = saved["tx"]
    balance = call(url, "octra_balance", [wallet["address"]])
    nonce = balance.get("nonce") if isinstance(balance, dict) else None
    if isinstance(nonce, str) and nonce.isascii() and nonce.isdecimal():
        nonce = int(nonce)
    if type(nonce) is not int or nonce < tx["nonce"]:
        raise ValidatorError("saved withdrawal nonce consumption unproved")
    if nonce != tx["nonce"]:
        raise ValidatorError("account nonce advanced after withdrawal; latest enrollment unproved")
    return saved["tx_hash"], epoch

def can_rebond(values, wallet, url, head):
    latest = read_pointer(values, wallet)
    if latest is None or latest.get("operation") != "validator_withdraw":
        return False
    epoch = latest.get("bonded_epoch")
    path = outbox_path(values, "validator_withdraw", epoch)
    if head <= epoch:
        raise ValidatorError("new bond requires a later committed absent state")
    attempts = saved_attempts(path, values, wallet, "validator_withdraw", epoch)
    saved = next((value for value in attempts if value["tx_hash"] == latest.get("tx_hash")), None)
    if saved is None:
        raise ValidatorError("saved withdrawal attempt differs from outbox")
    balance = call(url, "octra_balance", [wallet["address"]])
    nonce = balance.get("nonce") if isinstance(balance, dict) else None
    if isinstance(nonce, str) and nonce.isascii() and nonce.isdecimal():
        nonce = int(nonce)
    if type(nonce) is not int or nonce < saved["tx"]["nonce"]:
        raise ValidatorError("saved withdrawal nonce consumption unproved; new bond refused")
    return True

def require_new_outbox(path, operation, epoch):
    latest = read_outbox(path.parent / "last.json")
    field = "head_epoch" if operation == "validator_bond" else "bonded_epoch"
    referenced = latest is not None and (
        latest.get("operation") == operation and latest.get(field) == epoch
        and latest.get("tx_hash") is not None
    )
    if referenced or any(path.parent.glob(path.stem + "-*.json")):
        raise ValidatorError("saved transaction missing; restore control records before retrying")

def submit(values, wallet, operation, bonded_epoch, url, prepare, *, renew = False, amount = 0):
    label = "bond" if operation == "validator_bond" else "exit"
    path = outbox_path(values, operation, bonded_epoch)
    saved = read_outbox(path)
    if saved is None:
        require_new_outbox(path, operation, bonded_epoch)
        prepared = prepare()
        if not isinstance(prepared, dict):
            raise ValidatorError("invalid prepared validator transaction")
        field = "head_epoch" if operation == "validator_bond" else "bonded_epoch"
        saved = {**prepared, "chain_id": values["OCTRA_CHAIN_ID"], field: bonded_epoch}
        tx = check_outbox(saved, values, wallet, operation, bonded_epoch)
        if tx["amount"] != str(amount):
            raise ValidatorError("prepared transaction amount differs")
        write_private_json(path, saved)
        renew = False
    tx = check_outbox(saved, values, wallet, operation, bonded_epoch)
    if tx["amount"] != str(amount):
        raise ValidatorError("saved transaction amount differs; nothing signed")
    sync_directory(path.parent)
    remember_attempt(path, saved, wallet)
    tx_hash = saved["tx_hash"]
    observed = [
        (attempt["tx_hash"], transaction(url, attempt["tx_hash"]))
        for attempt in saved_attempts(path, values, wallet, operation, bonded_epoch)
    ]
    statuses = {key: value.get("status") if isinstance(value, dict) else None
                for key, value in observed}
    for state in ("confirmed", "pending", "staging"):
        for key, status in statuses.items():
            if status == state:
                return key
    if statuses[tx_hash] == "rejected" and not renew:
        raise ValidatorError(
            f"saved {label} transaction rejected; inspect transaction {tx_hash}; "
            "use --renew for the same payment after correcting the refusal"
        )
    balance = call(url, "octra_balance", [wallet["address"]])
    nonce = balance.get("nonce") if isinstance(balance, dict) else None
    if isinstance(nonce, str) and nonce.isascii() and nonce.isdecimal():
        nonce = int(nonce)
    if not isinstance(nonce, int) or isinstance(nonce, bool) or nonce < 0:
        raise ValidatorError(f"invalid account nonce before {label} submission")
    if nonce + 1 != tx["nonce"]:
        raise ValidatorError(f"saved {label} nonce differs from account; inspect before retrying")
    if renew:
        saved = renew_outbox(path, saved, values, wallet, operation, bonded_epoch, prepare)
        tx = saved["tx"]
        tx_hash = saved["tx_hash"]
        sync_directory(path.parent)
        remember_attempt(path, saved, wallet)
    try:
        result = call(values["OCTRA_OPERATOR_RPC_URL"], "octra_submit", [tx], timeout=30)
    except ValidatorError as error:
        raise ValidatorError(
            f"{label} submission unresolved; signed transaction retained; retry same command; tx = {tx_hash}"
        ) from error
    if not isinstance(result, dict) or result.get("status") not in {"accepted", "pending"} or (
        result.get("tx_hash") != tx_hash
    ):
        raise ValidatorError(f"{label} submission not acknowledged; signed transaction retained")
    return tx_hash

def allow_resume(values, wallet, bonded_epoch, url):
    path = outbox_path(values, "validator_exit", bonded_epoch)
    saved = read_outbox(path)
    if saved is None:
        require_new_outbox(path, "validator_exit", bonded_epoch)
        return
    attempts = saved_attempts(path, values, wallet, "validator_exit", bonded_epoch)
    for attempt in attempts:
        observed = transaction(url, attempt["tx_hash"])
        if not isinstance(observed, dict) or observed.get("status") != "rejected":
            raise ValidatorError("exit submission exists or is unresolved; duty remains paused")
    balance = call(url, "octra_balance", [wallet["address"]])
    nonce = balance.get("nonce") if isinstance(balance, dict) else None
    if isinstance(nonce, str) and nonce.isascii() and nonce.isdecimal():
        nonce = int(nonce)
    if type(nonce) is not int or nonce < 0:
        raise ValidatorError("exit nonce consumption unproved; duty remains paused")
    if nonce < saved["tx"]["nonce"]:
        raise ValidatorError("exit nonce has not been consumed; duty remains paused")