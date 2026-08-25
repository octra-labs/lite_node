# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import json
import subprocess
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

from validator_common import ValidatorError
from validator_common import load_wallet
from validator_common import parse_env
from validator_common import private_mode
from validator_common import rpc_url
from validator_common import sha256_file
from validator_common import write_env
from validator_common import write_private_json
from validator_rpc import call
from validator_rpc import transaction
from validator_rpc import wait_transaction

SOURCE = Path(__file__).resolve()
ROOT = SOURCE.parents[2] if SOURCE.parents[1].name == "controls" else SOURCE.parents[3]
DEFAULT_CONFIG = ROOT / ".keys/validator/node.env"
STATE_VERSION = "octra-validator-enrollment"
MIN_BOND = 1000000

class EnrollmentState(Enum):
    ABSENT = "absent"
    BONDED = "bonded"
    READY = "ready"
    EXITING = "exiting"

class JoinStep(Enum):
    ACTIVATE = "activate"
    SUBMIT_BOND = "submit_bond"
    SUBMIT_READY = "submit_ready"
    WAIT_SELECTION = "wait_selection"
    REFUSE = "refuse"

@dataclass(frozen=True)
class Enrollment:
    state: EnrollmentState
    head_epoch: int
    bond: int | None
    bonded_epoch: int | None
    ready_epoch: int | None
    exit_epoch: int | None

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def local_rpc(values):
    return rpc_url(f"http://127.0.0.1:{values['OCTRA_API_PORT']}/rpc")

def state_path(config):
    return Path(config).resolve().parent / "enrollment.json"

def load_state(config):
    path = state_path(config)
    if not path.is_file():
        return {"version": STATE_VERSION, "transactions": {}}
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except Exception as error:
        raise ValidatorError("invalid validator enrollment state") from error
    if value.get("version") != STATE_VERSION:
        raise ValidatorError("validator enrollment state version mismatch")
    if not isinstance(value.get("transactions"), dict):
        raise ValidatorError("invalid validator enrollment transactions")
    return value

def record_transaction(config, operation, tx_hash):
    state = load_state(config)
    state["transactions"][operation] = {
        "tx_hash": tx_hash,
        "submitted_at": int(time.time()),
    }
    write_private_json(state_path(config), state)

def node_status(url):
    value = call(url, "node_status", [])
    if not isinstance(value, dict):
        raise ValidatorError("invalid local node status")
    head_epoch = value.get("head_epoch")
    state_root = value.get("state_root")
    if not isinstance(head_epoch, int) or not isinstance(state_root, str):
        raise ValidatorError("local node has no committed head")
    return head_epoch, state_root

def require_admission_active(values):
    try:
        activation_epoch = int(values["OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH"])
    except (KeyError, TypeError, ValueError) as error:
        raise ValidatorError("invalid validator admission activation epoch") from error
    if activation_epoch < 0:
        raise ValidatorError("validator admission is inactive")
    head_epoch, _ = node_status(local_rpc(values))
    if head_epoch < activation_epoch:
        raise ValidatorError(
            f"validator admission activates at epoch {activation_epoch}, current head is {head_epoch}"
        )

def next_nonce(values, wallet):
    value = call(
        local_rpc(values),
        "octra_account",
        [wallet["address"], 1],
    )
    if not isinstance(value, dict):
        raise ValidatorError("invalid local account response")
    try:
        nonce = int(value["nonce"])
    except (KeyError, TypeError, ValueError) as error:
        raise ValidatorError("local account has no valid nonce") from error
    if nonce < 0:
        raise ValidatorError("local account nonce is negative")
    return nonce + 1

def exact_member(entries, wallet):
    if not isinstance(entries, list):
        raise ValidatorError("invalid validator set")
    matches = [
        entry
        for entry in entries
        if isinstance(entry, dict) and entry.get("address") == wallet["address"]
    ]
    if len(matches) > 1:
        raise ValidatorError("duplicate local address in validator set")
    if not matches:
        return False
    if matches[0].get("pubkey") != wallet["pub"]:
        raise ValidatorError("validator set public key mismatch")
    return True

def membership(values, wallet):
    proof = call(local_rpc(values), "octra_validatorSetProof", [])
    if not isinstance(proof, dict):
        raise ValidatorError("invalid validator set proof")
    if proof.get("chain_id") != values["OCTRA_CHAIN_ID"]:
        raise ValidatorError("validator set proof chain mismatch")
    active = exact_member(proof.get("validators"), wallet)
    scheduled_value = proof.get("scheduled")
    if scheduled_value is None:
        scheduled = False
        activate_epoch = None
        next_set_epoch = None
    elif isinstance(scheduled_value, dict):
        scheduled = exact_member(scheduled_value.get("validators"), wallet)
        activate_raw = scheduled_value.get("activate_epoch")
        try:
            next_set_epoch = int(activate_raw)
        except (TypeError, ValueError) as error:
            raise ValidatorError("invalid scheduled activation epoch") from error
        activate_epoch = next_set_epoch if scheduled else None
    else:
        raise ValidatorError("invalid scheduled validator set")
    return {
        "active": active,
        "scheduled": scheduled,
        "activate_epoch": activate_epoch,
        "next_set_epoch": next_set_epoch,
        "validator_set_hash": proof.get("validator_set_hash", "unknown"),
    }

def optional_nonnegative_integer(value, name):
    if value is None:
        return None
    if isinstance(value, bool) or not (
        isinstance(value, int)
        or (
            isinstance(value, str)
            and value.isascii()
            and value.isdecimal()
        )
    ):
        raise ValidatorError(f"invalid committed validator {name}")
    number = int(value)
    if number < 0:
        raise ValidatorError(f"negative committed validator {name}")
    return number

def optional_epoch(value, name):
    return optional_nonnegative_integer(value, name)

def committed_enrollment(values, wallet):
    value = call(local_rpc(values), "octra_validatorEnrollment", [])
    if not isinstance(value, dict):
        raise ValidatorError("invalid committed validator enrollment")
    if value.get("address") != wallet["address"]:
        raise ValidatorError("committed validator enrollment address differs")
    if value.get("consensus_pubkey") != wallet["pub"]:
        raise ValidatorError("committed validator enrollment public key differs")
    if value.get("identity") != "self_reported":
        raise ValidatorError("committed validator enrollment identity is invalid")
    try:
        state = EnrollmentState(value.get("state"))
    except (TypeError, ValueError) as error:
        raise ValidatorError("invalid committed validator enrollment state") from error
    head_epoch = optional_epoch(value.get("head_epoch"), "head epoch")
    bond = optional_nonnegative_integer(value.get("bond"), "bond")
    bonded_epoch = optional_epoch(value.get("bonded_epoch"), "bonded epoch")
    ready_epoch = optional_epoch(value.get("ready_epoch"), "ready epoch")
    exit_epoch = optional_epoch(value.get("exit_epoch"), "exit epoch")
    if head_epoch is None:
        raise ValidatorError("committed validator head epoch is missing")
    local_head, _ = node_status(local_rpc(values))
    if head_epoch != local_head:
        raise ValidatorError("committed validator enrollment is stale")
    if state is EnrollmentState.ABSENT:
        if any(item is not None for item in (bond, bonded_epoch, ready_epoch, exit_epoch)):
            raise ValidatorError("absent validator enrollment carries state")
    elif bond is None or bond <= 0 or bonded_epoch is None:
        raise ValidatorError("bonded validator enrollment is incomplete")
    elif state is EnrollmentState.BONDED and (
        ready_epoch is not None or exit_epoch is not None
    ):
        raise ValidatorError("bonded validator enrollment has invalid epochs")
    elif state is EnrollmentState.READY and (
        ready_epoch is None or exit_epoch is not None
    ):
        raise ValidatorError("ready validator enrollment has invalid epochs")
    elif state is EnrollmentState.EXITING and exit_epoch is None:
        raise ValidatorError("exiting validator enrollment has no exit epoch")
    return Enrollment(
        state=state,
        head_epoch=head_epoch,
        bond=bond,
        bonded_epoch=bonded_epoch,
        ready_epoch=ready_epoch,
        exit_epoch=exit_epoch,
    )

def join_step(member, enrollment):
    if member["active"] or member["scheduled"]:
        return JoinStep.ACTIVATE
    if enrollment.state is EnrollmentState.EXITING:
        return JoinStep.REFUSE
    if enrollment.state is EnrollmentState.ABSENT:
        return JoinStep.SUBMIT_BOND
    if enrollment.state is EnrollmentState.BONDED:
        return JoinStep.SUBMIT_READY
    return JoinStep.WAIT_SELECTION

def control_result(
    values,
    wallet,
    wallet_path,
    operation,
    amount=0,
    message=None,
):
    binary = Path(values["OCTRA_OPERATOR_CONTROL_BINARY"])
    if sha256_file(binary) != values["OCTRA_OPERATOR_CONTROL_BINARY_HASH"]:
        raise ValidatorError("validator control binary hash mismatch")
    command = [
        str(binary),
        "--wallet",
        str(wallet_path),
        "--rpc",
        values["OCTRA_OPERATOR_RPC_URL"],
        "--chain-id",
        values["OCTRA_CHAIN_ID"],
        "--op",
        operation,
        "--amount",
        str(amount),
        "--nonce",
        str(next_nonce(values, wallet)),
    ]
    if message is not None:
        command.extend([
            "--message",
            json.dumps(message, separators=(",", ":"), sort_keys=True),
        ])
    result = subprocess.run(
        command,
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    try:
        payload = json.loads(result.stdout.strip())
        tx_hash = payload["tx_hash"]
    except Exception as error:
        raise ValidatorError("invalid validator control response") from error
    if payload.get("status") not in {"accepted", "pending"}:
        raise ValidatorError("validator control transaction was not accepted")
    return tx_hash

def wait_confirmed(values, tx_hash, args):
    if args.no_wait:
        return
    confirmed = wait_transaction(
        local_rpc(values),
        tx_hash,
        args.wait_seconds,
        args.poll_seconds,
    )
    emit(
        event="transaction",
        status=confirmed["status"],
        tx=tx_hash,
        epoch=confirmed["epoch"],
    )

def resume_join_transaction(config, values, operation, args, missing_ok=False):
    record = load_state(config)["transactions"].get(operation)
    if not isinstance(record, dict):
        return False
    tx_hash = record.get("tx_hash")
    if not isinstance(tx_hash, str) or len(tx_hash) != 64:
        raise ValidatorError("invalid saved enrollment transaction")
    value = transaction(local_rpc(values), tx_hash)
    if not isinstance(value, dict):
        if missing_ok:
            return False
        raise ValidatorError(f"saved {operation} transaction is unavailable: {tx_hash}")
    status = value.get("status")
    if status == "confirmed":
        emit(
            event=operation,
            status="resumed",
            tx=tx_hash,
            epoch=value.get("epoch", "unknown"),
        )
        return True
    if status in {"rejected", "dropped"}:
        detail = json.dumps(value, separators=(",", ":"), sort_keys=True)
        raise ValidatorError(f"saved {operation} transaction {status}: {detail}")
    emit(event=operation, status="waiting", tx=tx_hash)
    wait_confirmed(values, tx_hash, args)
    return True

def submit_bond(config, values, wallet, wallet_path, amount, args):
    if amount < MIN_BOND:
        raise ValidatorError(f"validator bond must be at least {MIN_BOND}")
    enrollment = committed_enrollment(values, wallet)
    if enrollment.state is not EnrollmentState.ABSENT:
        raise ValidatorError("validator bond already exists in committed state")
    tx_hash = control_result(
        values,
        wallet,
        wallet_path,
        "validator_bond",
        amount=amount,
    )
    record_transaction(config, "bond", tx_hash)
    emit(event="bond", status="submitted", tx=tx_hash, amount=amount)
    wait_confirmed(values, tx_hash, args)
    return tx_hash

def submit_ready(config, values, wallet, wallet_path, args):
    enrollment = committed_enrollment(values, wallet)
    if enrollment.state is EnrollmentState.ABSENT:
        raise ValidatorError("validator bond is absent from committed state")
    if enrollment.state is EnrollmentState.EXITING:
        raise ValidatorError("exiting validator cannot become ready")
    if enrollment.state is EnrollmentState.READY:
        emit(
            event="ready",
            status="committed",
            epoch=enrollment.ready_epoch,
        )
        return None
    head_epoch, state_root = node_status(local_rpc(values))
    message = {
        "consensus_pubkey": wallet["pub"],
        "head_epoch": str(head_epoch),
        "state_root": state_root,
    }
    tx_hash = control_result(
        values,
        wallet,
        wallet_path,
        "validator_ready",
        message=message,
    )
    record_transaction(config, "ready", tx_hash)
    emit(event="ready", status="submitted", tx=tx_hash, head_epoch=head_epoch)
    wait_confirmed(values, tx_hash, args)
    return tx_hash

def submit_self(config, values, wallet, wallet_path, operation, args):
    tx_hash = control_result(values, wallet, wallet_path, operation)
    record_transaction(config, operation, tx_hash)
    emit(event=operation, status="submitted", tx=tx_hash)
    wait_confirmed(values, tx_hash, args)
    return tx_hash

def wait_scheduled(values, wallet, args):
    deadline = time.monotonic() + args.wait_seconds
    while time.monotonic() < deadline:
        state = membership(values, wallet)
        if state["active"] or state["scheduled"]:
            return state
        time.sleep(args.poll_seconds)
    return None

def wait_active_commit(values, wallet, state, args):
    if state["scheduled"] or not state["active"]:
        return state
    initial_head, _ = node_status(local_rpc(values))
    deadline = time.monotonic() + args.wait_seconds
    while time.monotonic() < deadline:
        current = membership(values, wallet)
        head, _ = node_status(local_rpc(values))
        if current["active"] and head > initial_head:
            return current
        time.sleep(args.poll_seconds)
    raise ValidatorError("validator activation commit timed out")

def set_validator_mode(config, values, state, restart):
    if not state["active"] and not state["scheduled"]:
        raise ValidatorError("validator is not active or scheduled")
    next_values = dict(values)
    next_values["OCTRA_CONSENSUS_MODE"] = "bft"
    next_values["OCTRA_OPERATOR_ROLE"] = "validator"
    write_env(config, next_values)
    emit(
        event="validator_mode",
        status="armed",
        active=str(state["active"]).lower(),
        scheduled=str(state["scheduled"]).lower(),
        activate_epoch=state["activate_epoch"],
    )
    if restart:
        subprocess.run(
            ["sh", str(ROOT / "controls/run.sh")],
            cwd=ROOT,
            check=True,
        )

def transaction_status(url, tx_hash):
    value = transaction(url, tx_hash)
    return value.get("status", "missing") if isinstance(value, dict) else "missing"

def show_status(config, values, wallet):
    state = membership(values, wallet)
    enrollment = committed_enrollment(values, wallet)
    head_epoch, state_root = node_status(local_rpc(values))
    emit(event="identity", address=wallet["address"], role=values["OCTRA_OPERATOR_ROLE"])
    emit(
        event="chain",
        head_epoch=head_epoch,
        state_root=state_root,
        validator_set=state["validator_set_hash"],
    )
    emit(
        event="membership",
        active=str(state["active"]).lower(),
        scheduled=str(state["scheduled"]).lower(),
        activate_epoch=state["activate_epoch"],
        next_set_epoch=state["next_set_epoch"],
    )
    emit(
        event="committed_enrollment",
        state=enrollment.state.value,
        head_epoch=enrollment.head_epoch,
        bond=enrollment.bond,
        bonded_epoch=enrollment.bonded_epoch,
        ready_epoch=enrollment.ready_epoch,
        exit_epoch=enrollment.exit_epoch,
    )
    saved = load_state(config)
    for operation, record in sorted(saved["transactions"].items()):
        tx_hash = record.get("tx_hash", "")
        emit(
            event="enrollment_tx",
            operation=operation,
            status=transaction_status(local_rpc(values), tx_hash),
            tx=tx_hash,
        )

def parser():
    value = argparse.ArgumentParser(prog="enroll.sh")
    value.add_argument(
        "command",
        choices=[
            "activate",
            "bond",
            "exit",
            "join",
            "ready",
            "status",
            "withdraw",
        ],
    )
    value.add_argument("--amount", type=int, default=MIN_BOND)
    value.add_argument("--config", default=str(DEFAULT_CONFIG))
    value.add_argument("--no-restart", action="store_true")
    value.add_argument("--no-wait", action="store_true")
    value.add_argument("--poll-seconds", type=float, default=2.0)
    value.add_argument("--rpc")
    value.add_argument("--wait-seconds", type=float, default=3600.0)
    return value

def main():
    args = parser().parse_args()
    if args.poll_seconds <= 0 or args.wait_seconds <= 0:
        raise ValidatorError("wait and poll intervals must be positive")
    if args.command == "join" and args.no_wait:
        raise ValidatorError("join requires transaction confirmation")
    private_mode(args.config)
    values = parse_env(args.config)
    if args.rpc:
        values["OCTRA_OPERATOR_RPC_URL"] = rpc_url(args.rpc)
    wallet_path = Path(values["OCTRA_DATA_DIR"]) / "wallet.json"
    wallet = load_wallet(wallet_path)
    if args.command == "status":
        show_status(args.config, values, wallet)
        return
    require_admission_active(values)
    if args.command == "bond":
        submit_bond(
            args.config,
            values,
            wallet,
            wallet_path,
            args.amount,
            args,
        )
    elif args.command == "ready":
        submit_ready(args.config, values, wallet, wallet_path, args)
    elif args.command == "activate":
        set_validator_mode(
            args.config,
            values,
            membership(values, wallet),
            not args.no_restart,
        )
    elif args.command == "exit":
        submit_self(
            args.config,
            values,
            wallet,
            wallet_path,
            "validator_exit",
            args,
        )
    elif args.command == "withdraw":
        submit_self(
            args.config,
            values,
            wallet,
            wallet_path,
            "validator_withdraw",
            args,
        )
    else:
        state = membership(values, wallet)
        enrollment = committed_enrollment(values, wallet)
        step = join_step(state, enrollment)
        if step is JoinStep.REFUSE:
            raise ValidatorError("validator exit is already committed")
        if step is JoinStep.SUBMIT_BOND:
            if not resume_join_transaction(args.config, values, "bond", args):
                submit_bond(
                    args.config,
                    values,
                    wallet,
                    wallet_path,
                    args.amount,
                    args,
                )
            enrollment = committed_enrollment(values, wallet)
            step = join_step(state, enrollment)
            if step is JoinStep.SUBMIT_BOND:
                raise ValidatorError("confirmed validator bond is absent from committed state")
        if step is JoinStep.SUBMIT_READY:
            if not resume_join_transaction(
                args.config,
                values,
                "ready",
                args,
                missing_ok=True,
            ):
                submit_ready(args.config, values, wallet, wallet_path, args)
            step = JoinStep.WAIT_SELECTION
        if step is JoinStep.WAIT_SELECTION:
            state = wait_scheduled(values, wallet, args)
            if state is None:
                emit(event="selection", status="pending")
                return
        state = wait_active_commit(values, wallet, state, args)
        set_validator_mode(
            args.config,
            values,
            state,
            not args.no_restart,
        )

if __name__ == "__main__":
    try:
        main()
    except (
        KeyError,
        OSError,
        subprocess.CalledProcessError,
        ValidatorError,
        ValueError,
    ) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)