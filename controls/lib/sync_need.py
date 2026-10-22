# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

from dataclasses import dataclass
import re
from typing import Optional

from validator_common import ValidatorError

SCHEMA = "octra_sync_need_v2"
CAUSES = frozenset({"root", "journal", "range", "conflict"})
FIELDS = frozenset({"schema", "chain_id", "cause", "epoch", "head", "target"})
MAX_BYTES = 4096
HISTORY_LIMIT = 4096

def history_limit(values):
    text = values.get("OCTRA_CATCHUP_MAX_LAG")
    if not isinstance(text, str) or re.fullmatch(r"[+-]?[0-9]{1,19}", text) is None:
        return HISTORY_LIMIT
    return min(HISTORY_LIMIT, max(1, int(text)))

def snapshot_lag(values):
    return min(3000, history_limit(values) - 1)

@dataclass(frozen=True)
class Need:
    chain: str
    cause: str
    epoch: int
    head: int
    target: Optional[int]

def make(chain, cause, epoch, head, target):
    if not isinstance(chain, str) or not chain:
        raise ValidatorError("recovery marker binding differs")
    if type(cause) is not str or cause not in CAUSES:
        raise ValidatorError("recovery marker cause is invalid")
    if type(epoch) is not int or type(head) is not int:
        raise ValidatorError("recovery marker height is invalid")
    if head < 0 or epoch != head + 1:
        raise ValidatorError("recovery marker boundary is invalid")
    if cause == "range" or (cause == "root" and target is not None):
        if type(target) is not int or target <= head:
            raise ValidatorError("recovery marker target is invalid")
    elif target is not None:
        raise ValidatorError("recovery marker target is invalid")
    return Need(chain, cause, epoch, head, target)

def decode(value, chain):
    if not isinstance(value, dict) or set(value) != FIELDS:
        raise ValidatorError("recovery marker fields are invalid")
    if value.get("schema") not in (SCHEMA, "octra_sync_need_v1") or value.get("chain_id") != chain:
        raise ValidatorError("recovery marker binding differs")
    cause = value["cause"]
    if value["schema"] == "octra_sync_need_v1" and cause == "journal":
        cause = "conflict"
    return make(
        chain,
        cause,
        value["epoch"],
        value["head"],
        value["target"],
    )

def bind(value, chain):
    if not isinstance(value, Need):
        raise ValidatorError("recovery plan type is invalid")
    if value.chain != chain:
        raise ValidatorError("recovery plan binding differs")
    return value

def choose(marked, planned, chain):
    marked = bind(marked, chain) if marked is not None else None
    planned = bind(planned, chain) if planned is not None else None
    return marked if marked is not None else planned

def carry_guard(marked, planned, chain, *, saved = None):
    needs = [bind(item, chain) for item in (marked, planned, saved) if item is not None]
    guards = [item for item in needs if item.cause in ("root", "range")]
    if not guards:
        return None
    head = max(item.head for item in guards)
    targets = [item.target for item in guards if item.target is not None]
    target = max([head + 1] + targets) if targets else None
    return make(chain, "range" if targets else "root", head + 1, head, target)