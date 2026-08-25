# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

from dataclasses import dataclass
from typing import Optional

from validator_common import ValidatorError

SCHEMA = "octra_sync_need_v1"
CAUSES = frozenset({"root", "journal", "range"})
FIELDS = frozenset({"schema", "chain_id", "cause", "epoch", "head", "target"})
MAX_BYTES = 4096

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
    if cause == "range":
        if type(target) is not int or target <= head:
            raise ValidatorError("recovery marker target is invalid")
    elif target is not None:
        raise ValidatorError("recovery marker target is invalid")
    return Need(chain, cause, epoch, head, target)

def decode(value, chain):
    if not isinstance(value, dict) or set(value) != FIELDS:
        raise ValidatorError("recovery marker fields are invalid")
    if value.get("schema") != SCHEMA or value.get("chain_id") != chain:
        raise ValidatorError("recovery marker binding differs")
    return make(
        chain,
        value["cause"],
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