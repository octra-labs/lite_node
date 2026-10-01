# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import stat
from pathlib import Path

def position(value, key):
    number = value.get(key)
    if type(number) is not int or number < 0:
        raise ValueError(f"invalid {key}")
    return number

def validate(name, value):
    if not isinstance(value, dict):
        raise ValueError("record is not an object")
    epoch = position(value, "epoch_id")
    if name.endswith(".pending"):
        round_id = position(value, "round")
        expected = f"{epoch:010d}_{round_id:04d}.pending"
        strings = ("proposal_id", "proposed_state_root", "txid_hi", "validator_addr")
        timestamp = "ts"
        integer_strings = ("txid_hi",)
        for key in ("proposal_b64", "vote_b64"):
            field = value.get(key)
            if field is not None and not isinstance(field, str):
                raise ValueError(f"invalid {key}")
        for key in ("tx_hashes", "txs_json", "receipts_json"):
            field = value.get(key)
            if field is not None and (not isinstance(field, list)
                or any(not isinstance(item, str) for item in field)):
                raise ValueError(f"invalid {key}")
    else:
        expected = f"{epoch:010d}.wal"
        strings = ("pre_state_root", "post_state_root", "parent_commit", "start_txid", "finalized_by")
        timestamp = "finalized_at"
        integer_strings = ("start_txid",)
        for key in ("tx_count", "irmin_last_epoch_before"):
            if type(value.get(key)) is not int:
                raise ValueError(f"invalid {key}")
    if name != expected:
        raise ValueError("record position differs from filename")
    for key in strings:
        if not isinstance(value.get(key), str):
            raise ValueError(f"invalid {key}")
    for key in integer_strings:
        number = int(value[key])
        if not -(2**63) <= number < 2**63:
            raise ValueError(f"invalid {key}")
    if type(value.get(timestamp)) not in (int, float):
        raise ValueError(f"invalid {timestamp}")

def inspect(data_dir):
    wal = Path(data_dir) / "wal"
    try:
        meta = wal.lstat()
    except FileNotFoundError:
        return []
    except OSError as error:
        return [("wal_directory_unreadable", wal, str(error))]
    if not stat.S_ISDIR(meta.st_mode):
        return [("wal_directory_unreadable", wal, "not a directory")]
    try:
        paths = sorted(path for path in wal.iterdir() if path.suffix in (".wal", ".pending"))
    except OSError as error:
        return [("wal_directory_unreadable", wal, str(error))]
    faults = []
    for path in paths:
        kind = "pending" if path.suffix == ".pending" else "wal"
        try:
            meta = path.lstat()
            if not stat.S_ISREG(meta.st_mode) or meta.st_size <= 0:
                raise ValueError("record is not a nonempty regular file")
            if kind == "pending" and meta.st_size > 128 * 1024 * 1024:
                raise ValueError("pending record exceeds size limit")
            validate(path.name, json.loads(path.read_text(encoding = "utf-8")))
        except (OSError, UnicodeError, ValueError) as error:
            faults.append((kind + "_store_unreadable", path, str(error)))
    return faults