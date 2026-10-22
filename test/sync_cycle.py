# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import hashlib
import json
import os
import signal
import sys
import time
from contextlib import ExitStack
from pathlib import Path
from unittest import mock

sys.path.insert(0, sys.argv[1])
import validator_recover as recovery
from sync_need import make
from validator_common import copy_private, ensure_wallet

directory = Path(sys.argv[2])
cause = sys.argv[3]
planned = sys.argv[4] == "plan"
carried = sys.argv[4] == "carry"
conflict = sys.argv[4] == "conflict"
floor = 3000 if carried else 2000
assert not planned or cause == "range"
data = directory / "data"
data.mkdir()
(data / "irmin_store").mkdir()
(data / "chaindata").mkdir()
wallet = directory / "identity.json"
identity = ensure_wallet(wallet)
copy_private(wallet, data / "wallet.json")
binary = directory / "sync"
binary.write_bytes(b"local checker")
old = {"epoch": 100}
(data / "HEAD.json").write_text(json.dumps(old), encoding = "ascii")
values = {
    "OCTRA_CHAIN_ID": "octra-test", "OCTRA_DATA_DIR": str(data),
    "OCTRA_OPERATOR_NETWORK_BUNDLE": str(directory / "network"),
    "OCTRA_OPERATOR_NETWORK_SHA256": "0" * 64,
    "OCTRA_OPERATOR_SYNC_BINARY": str(binary),
    "OCTRA_OPERATOR_SYNC_BINARY_HASH": hashlib.sha256(binary.read_bytes()).hexdigest(),
    "OCTRA_OPERATOR_SYNC_STAGE": str(directory / "stage"),
    "OCTRA_STATE_SYNC_SOURCES": "https://snapshot.invalid",
    "OCTRA_CHECKPOINT_EPOCH": "100",
    "OCTRA_OPERATOR_SYNC_CONCURRENCY": "1",
    "OCTRA_OPERATOR_SYNC_SOURCE_CONCURRENCY": "1",
}
need = make("octra-test", cause, 101, 100, 6000 if carried else 5000 if cause == "range" else None)
if not planned:
    recovery.write_private_json(data / "recovery/sync_need.json", {
        "schema": "octra_sync_need_v2", "chain_id": need.chain, "cause": need.cause,
        "epoch": need.epoch, "head": need.head, "target": need.target,
    })
if conflict:
    recovery.write_private_json(data / "recovery/sync_conflict.json", {
        "schema": "octra_sync_need_v2", "chain_id": need.chain, "cause": "conflict",
        "epoch": need.epoch, "head": need.head, "target": None,
    })
selected = make(need.chain, "conflict", need.epoch, need.head, None) if conflict else need

def snapshot(epoch):
    data.mkdir()
    (data / "irmin_store").mkdir()
    (data / "chaindata").mkdir()
    (data / "HEAD.json").write_text(json.dumps({"epoch": epoch}), encoding = "ascii")

def install(*args, **kwargs):
    assert kwargs["min_epoch"] == floor
    root = directory / "stage"
    root.mkdir(exist_ok = True)
    saved = root / str(len(list(root.iterdir())))
    source = saved / "data"
    source.mkdir(parents = True)
    (source / "irmin_store").mkdir()
    (source / "chaindata").mkdir()
    (source / "HEAD.json").write_text(json.dumps({"epoch": floor}), encoding = "ascii")
    for name in ["snapshot_verified.json", "certificate.json"]:
        (saved / name).write_text("{}", encoding = "ascii")
    recovery.validator_config.install_verified_snapshot(saved, data,
        guard = kwargs["guard"], prior = kwargs["prior"], identity = kwargs["identity"])

with ExitStack() as scope:
    changes = {
        "state_ready": lambda path: (path / "HEAD.json").is_file(),
        "validate_checkpoint": lambda path, *args, **kwargs: json.loads((path / "HEAD.json").read_text()),
        "read_plan": lambda *args: need if planned else None,
        "load_network": lambda *args: (None, None, {}),
        "validate_network_binding": lambda *args: None,
        "require_idle": lambda *args: None,
        "sync_snapshot": install,
        "sync_head": lambda *args: {"head": 5000, "snapshot": floor},
    }
    for name, effect in changes.items():
        scope.enter_context(mock.patch.object(recovery, name, side_effect = effect))
    scope.enter_context(mock.patch.object(recovery.validator_config, "operator_wallet",
        return_value = (wallet, identity)))
    emit = scope.enter_context(mock.patch.object(recovery, "emit"))
    before = sorted(path.name for path in directory.iterdir())
    inode = data.stat().st_ino
    for tip in [None, {"head": 10_000, "snapshot": None, "latest": 100, "status": "unavailable",
            "advertised_status": "old_epoch", "reason": "http_503"}]:
        with mock.patch.object(recovery, "sync_head", return_value = tip), \
                mock.patch.object(recovery, "move_state") as move, \
                mock.patch.object(recovery, "sync_snapshot") as sync:
            recovery.recover_state(directory / "node.env", values, False, None, None)
            status = emit.call_args.kwargs
            assert status["status"] == "held" and status["cause"] == selected.cause and status["epoch"] == 101
            assert status["action"].startswith("wait for signed snapshot:")
            if tip is not None:
                assert "required = 7000 available = unknown advertised = 100 lag = 9900" in status["action"]
                assert 'snapshot_status = unavailable advertised_status = old_epoch manifest_error = "http_503"' in status["action"]
            for minimum in [None, 7000]:
                try:
                    recovery.recover_state(directory / "node.env", values, True, None, minimum)
                    raise AssertionError("unavailable snapshot changed state")
                except recovery.ValidatorError as error:
                    assert "wait for signed snapshot:" in str(error)
            move.assert_not_called()
            sync.assert_not_called()
        assert data.stat().st_ino == inode
        assert json.loads((data / "HEAD.json").read_text()) == old
        assert sorted(path.name for path in directory.iterdir()) == before
    recovery.recover_state(directory / "node.env", values, False, None, None)
    assert emit.call_args.kwargs["status"] == "held"
    assert "--replace-state" in emit.call_args.kwargs["action"]
    if cause == "root":
        action = emit.call_args.kwargs["action"]
        with mock.patch.object(recovery, "sync_head", return_value = None):
            try:
                recovery.recover_state(directory / "node.env", values, True, None, None)
                raise AssertionError("missing network head accepted")
            except recovery.ValidatorError as error:
                assert "head = unknown" in str(error)
        def old_snapshot(*args, **kwargs):
            snapshot(floor - 1)
        with mock.patch.object(recovery, "sync_snapshot", side_effect = old_snapshot):
            try:
                recovery.recover_state(directory / "node.env", values, True, None, None)
                raise AssertionError("old snapshot accepted")
            except recovery.ValidatorError as error:
                assert "below recovery boundary" in str(error)
        assert json.loads((data / "HEAD.json").read_text()) == old
        assert recovery.read_need(data, need.chain) == selected
        assert action == f"recover.sh --replace-state --min-epoch {floor}"
    if conflict:
        with mock.patch.object(recovery.validator_config, "write_private_json",
                side_effect = OSError("disk write failed")):
            try:
                recovery.recover_state(directory / "node.env", values, True, None, None)
                raise AssertionError("marker write failure accepted")
            except OSError:
                pass
        assert json.loads((data / "HEAD.json").read_text()) == old
        assert recovery.read_need(data, need.chain) == selected
        assert recovery.read_marker(data / "recovery/sync_need.json", need.chain) == need
    if conflict:
        child = os.fork()
        if child == 0:
            replace = os.replace
            def interrupted(source, target):
                replace(source, target)
                if Path(target) == data:
                    os.kill(os.getpid(), signal.SIGKILL)
            with mock.patch.object(recovery.validator_config.os, "replace", side_effect = interrupted):
                recovery.recover_state(directory / "node.env", values, True, None, None)
            os._exit(1)
        stop = time.monotonic() + 10
        while True:
            result, status = os.waitpid(child, os.WNOHANG)
            if result == child:
                assert os.WIFSIGNALED(status) and os.WTERMSIG(status) == signal.SIGKILL
                break
            if time.monotonic() >= stop:
                os.kill(child, signal.SIGKILL)
                os.waitpid(child, 0)
                raise AssertionError("recovery child did not exit")
            time.sleep(0.01)
        assert (data / "wallet.json").read_bytes() == wallet.read_bytes()
        with mock.patch.object(recovery.validator_config, "IDENTITY_WALLET", wallet), \
                mock.patch.object(recovery.validator_config, "ensure_wallet", return_value = identity), \
                mock.patch.object(recovery.validator_config, "load_wallet", return_value = identity):
            assert recovery.validator_config.bind_wallet(data) == identity
    else:
        recovery.recover_state(directory / "node.env", values, True, None, None)
    stored = make(need.chain, "range", need.epoch, need.head, need.target) if carried else need
    assert recovery.read_need(data, need.chain) == stored
    assert not (data / "recovery/sync_conflict.json").exists()
    assert (directory / "data.prior-100/HEAD.json").is_file()
    assert (data / "wallet.json").is_file()
    recovery.recover_state(directory / "node.env", values, False, None, None)
    assert emit.call_args.kwargs["status"] == "verify"
    assert emit.call_args.kwargs["action"] == "start_observer"