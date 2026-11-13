# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import io
import json
import stat
import unittest
from http.client import HTTPResponse, BadStatusLine
from types import SimpleNamespace
from unittest import mock
from urllib.error import HTTPError
import sync_tip
import upgrade
from sync_need import Need
from validator_common import ValidatorError

class SyncTipTest(unittest.TestCase):
    def response(self, value):
        reply = mock.MagicMock()
        reply.__enter__.return_value.read.return_value = json.dumps(value).encode("ascii")
        return reply

    def head(self, epoch, snapshot):
        return self.response({"version": "octra-state-sync", "mode": "live_head",
            "head_epoch": epoch, "current_epoch": epoch + 1, "snapshot_epoch": snapshot,
            "snapshot_status": "old_epoch" if epoch - snapshot > 3000 else "ready"})

    def certificate(self, epoch):
        return self.response({"version": "octra-state-sync", "checkpoint": {"epoch": epoch},
            "manifest": {"files": [{"path": "ready_roots", "size": 1}]}})

    def unavailable_head(self, sources, replies):
        with mock.patch.object(sync_tip.http, "open", side_effect = replies + [OSError("head unavailable")]):
            return sync_tip.sync_head({"OCTRA_STATE_SYNC_SOURCES": sources,
                "OCTRA_JOIN_RPC": "https://head.example"})

    def test_http_head_only(self):
        for heads in ["", "https://a.example", "https://a.example,https://b.example"]:
            with self.subTest(heads = heads), mock.patch.object(sync_tip, "sync_epoch",
                    return_value = {"head": 9999999, "snapshot": 100, "latest": 100,
                        "status": "ready", "advertised_status": "ready", "source": "https://a.example",
                        "reason": None}):
                tip = sync_tip.sync_head({"OCTRA_STATE_SYNC_SOURCES": "https://a.example,https://b.example",
                    "OCTRA_JOIN_RPC": heads})
                issue = sync_tip.sync_issue(tip, 100)
                self.assertIsNone(tip["head"])
                self.assertIsNone(issue)
                self.assertEqual(sync_tip.sync_fields(tip, 100)["head_status"], "native_unavailable")

    def test_native_target(self):
        values = {"OCTRA_DATA_DIR": "/node", "OCTRA_CHAIN_ID": "test"}
        payload = {"chainId": "test", "status": "agreed", "localEpoch": "40",
            "headRoot": "a" * 64, "setStamp": "source:verified", "targetEpoch": "42"}
        self.assertEqual(sync_tip.decode_target(payload, "test"),
            {"head": 42, "local": 40, "root": "a" * 64, "stamp": "source:verified"})
        with mock.patch.object(sync_tip, "control_target", return_value = payload) as rpc:
            self.assertEqual(sync_tip.native_tip(values),
                {"head": 42, "local": 40, "root": "a" * 64, "stamp": "source:verified"})
            rpc.assert_called_once_with("/node")
        for change in [{"chainId": "other"}, {"status": "changed"}, {"status": "no_agreement"},
                {"localEpoch": True}, {"targetEpoch": "39"}, {"targetEpoch": 2 ** 63},
                {"headRoot": "g" * 64}, {"setStamp": ""}, {"setStamp": "bad\nstamp"},
                {"targetEpoch": "042"}, {"targetEpoch": "9" * 100}, {"extra": True}]:
            with self.subTest(change = change), mock.patch.object(sync_tip, "control_target",
                    return_value = payload | change):
                self.assertIsNone(sync_tip.native_tip(values))

    def test_native_data(self):
        for data in [None, True, 0, ""]:
            with self.subTest(data = data), mock.patch.object(sync_tip, "control_target") as request:
                self.assertIsNone(sync_tip.native_tip({"OCTRA_CHAIN_ID": "test", "OCTRA_DATA_DIR": data}))
                request.assert_not_called()

    def control_stats(self):
        return [SimpleNamespace(st_mode = mode, st_uid = 1000, st_dev = 1, st_ino = index)
            for index, mode in enumerate([stat.S_IFDIR | 0o700, stat.S_IFDIR | 0o700,
                stat.S_IFSOCK | 0o600])]

    def test_control_socket(self):
        payload = {"chainId": "test", "status": "agreed", "localEpoch": "40",
            "headRoot": "a" * 64, "setStamp": "source:verified", "targetEpoch": "42"}
        wire = json.dumps({"jsonrpc": "2.0", "id": 1, "result": payload}).encode() + b"\n"
        http = mock.Mock(side_effect = AssertionError("HTTP target must not be queried"))
        with mock.patch.object(sync_tip.Path, "resolve", return_value = sync_tip.Path("/node")), \
                mock.patch.object(sync_tip.Path, "lstat", side_effect = self.control_stats() * 3), \
                mock.patch.object(sync_tip.socket, "socket") as connect:
            client = connect.return_value.__enter__.return_value
            client.recv.return_value = wire
            target = sync_tip.native_tip({"OCTRA_DATA_DIR": "/node", "OCTRA_CHAIN_ID": "test",
                "OCTRA_API_PORT": "8080@evil"}, query = http)
            self.assertEqual(target["head"], 42)
            connect.assert_called_once_with(sync_tip.socket.AF_UNIX, sync_tip.socket.SOCK_STREAM)
            self.assertEqual(sync_tip.Path(client.connect.call_args.args[0]).absolute(),
                sync_tip.Path("/node/control/sync.sock"))
            request = json.loads(client.sendall.call_args.args[0])
            self.assertEqual(request, {"jsonrpc": "2.0", "id": 1,
                "method": "octra_syncTarget", "params": []})
            http.assert_not_called()

    def test_control_ownership(self):
        for index, change in [(0, {"st_mode": stat.S_IFDIR | 0o777}),
                (1, {"st_mode": stat.S_IFDIR | 0o755}),
                (1, {"st_mode": stat.S_IFLNK | 0o700}),
                (2, {"st_mode": stat.S_IFREG | 0o600}),
                (2, {"st_mode": stat.S_IFSOCK | 0o666}), (2, {"st_uid": 999})]:
            entries = self.control_stats()
            entries[index] = SimpleNamespace(**(vars(entries[index]) | change))
            with self.subTest(index = index, change = change), \
                    mock.patch.object(sync_tip.Path, "resolve", return_value = sync_tip.Path("/node")), \
                    mock.patch.object(sync_tip.Path, "lstat", side_effect = entries), \
                    mock.patch.object(sync_tip.socket, "socket") as connect:
                self.assertIsNone(sync_tip.native_tip({"OCTRA_DATA_DIR": "/node", "OCTRA_CHAIN_ID": "test"}))
                connect.assert_not_called()

    def test_control_wire(self):
        for wire in [b"", b"x" * 4097, b"{}\n{}\n", b"HTTP/1.1 200 OK\n",
                b'{"jsonrpc":"2.0","id":true,"result":{}}\n',
                b'{"jsonrpc":"2.0","id":2,"result":{}}\n',
                b'{"jsonrpc":"2.0","id":1,"error":{}}\n']:
            with self.subTest(wire = wire[:80]), \
                    mock.patch.object(sync_tip.Path, "resolve", return_value = sync_tip.Path("/node")), \
                    mock.patch.object(sync_tip.Path, "lstat", side_effect = self.control_stats() * 3), \
                    mock.patch.object(sync_tip.socket, "socket") as connect:
                client = connect.return_value.__enter__.return_value
                client.recv.return_value = wire
                self.assertIsNone(sync_tip.native_tip({"OCTRA_DATA_DIR": "/node", "OCTRA_CHAIN_ID": "test"}))

    def test_control_replaced(self):
        entries = self.control_stats() + self.control_stats()
        entries[-1] = SimpleNamespace(**(vars(entries[-1]) | {"st_ino": 99}))
        with mock.patch.object(sync_tip.Path, "resolve", return_value = sync_tip.Path("/node")), \
                mock.patch.object(sync_tip.Path, "lstat", side_effect = entries), \
                mock.patch.object(sync_tip.socket, "socket") as connect:
            self.assertIsNone(sync_tip.native_tip({"OCTRA_DATA_DIR": "/node", "OCTRA_CHAIN_ID": "test"}))
            connect.return_value.__enter__.return_value.sendall.assert_not_called()

    def test_control_deadline(self):
        with mock.patch.object(sync_tip.Path, "resolve", return_value = sync_tip.Path("/node")), \
                mock.patch.object(sync_tip.Path, "lstat", side_effect = self.control_stats() * 2), \
                mock.patch.object(sync_tip.socket, "socket") as connect, \
                mock.patch.object(sync_tip.time, "monotonic", side_effect = [0.0, 4.0]):
            self.assertIsNone(sync_tip.native_tip({"OCTRA_DATA_DIR": "/node", "OCTRA_CHAIN_ID": "test"}))
            connect.return_value.__enter__.return_value.recv.assert_not_called()

    def test_control_mount(self):
        with mock.patch.object(sync_tip.Path, "resolve", return_value = sync_tip.Path("/node")) as resolve, \
                mock.patch.object(sync_tip.Path, "lstat", side_effect = self.control_stats()):
            path, identity = sync_tip.control_path("/mount/alias")
            self.assertEqual(path, sync_tip.Path("/node/control/sync.sock"))
            self.assertEqual(identity, (1, 2))
            resolve.assert_called_once_with(strict = True)

    def test_native_over_http(self):
        native = {"head": 42, "local": 40, "root": "a" * 64, "stamp": "source:verified"}
        row = {"head": 1000000, "snapshot": 41, "latest": 41,
            "status": "ready", "advertised_status": "ready", "source": "https://a.example", "reason": None}
        with mock.patch.object(sync_tip, "native_tip", return_value = native), \
                mock.patch.object(sync_tip, "sync_epoch", return_value = row):
            tip = sync_tip.sync_head({"OCTRA_STATE_SYNC_SOURCES": "https://a.example"})
        self.assertEqual(tip["head"], 42)
        self.assertEqual(tip["sources"][0]["head"], 1000000)
        state = {"process": "online", "rpc": "ready", "head_epoch": 40, "peer_epoch": 9999999}
        self.assertEqual(upgrade.sync_target(state, tip), 42)
        self.assertIsNone(upgrade.sync_target(state, {"head": 9999999}))

    def test_native_without_http(self):
        native = {"head": 42, "local": 40, "root": "a" * 64, "stamp": "source:verified"}
        for sources in ({}, {"OCTRA_STATE_SYNC_SOURCES": "https://a.example"}):
            with self.subTest(sources = sources), \
                    mock.patch.object(sync_tip, "native_tip", return_value = native), \
                    mock.patch.object(sync_tip, "sync_epoch", side_effect = OSError("offline")):
                tip = sync_tip.sync_head(sources)
            self.assertEqual(tip["head"], 42)
            self.assertEqual(tip["native"], native)
            self.assertIsNone(tip["snapshot"])
            self.assertEqual(tip["status"], "unavailable")
            self.assertIsNotNone(sync_tip.sync_issue(tip, 40))

    def test_old_manifest(self):
        replies = [self.head(1_716_000, 1_703_160),
            HTTPError("https://a.example/state-sync/manifest", 503, "old_epoch", {}, None),
            self.head(1_716_000, 1_703_160)]
        with mock.patch.object(sync_tip.http, "open", side_effect = replies), \
                mock.patch.object(sync_tip, "native_tip", return_value = {"head": 1_716_000}):
            tip = sync_tip.sync_head({"OCTRA_STATE_SYNC_SOURCES": "https://a.example",
                "OCTRA_JOIN_RPC": "https://b.example"})
        row = {"head": 1_716_000, "snapshot": None, "latest": 1_703_160,
            "status": "unavailable", "advertised_status": "old_epoch",
            "source": "https://a.example", "reason": "http_503"}
        self.assertEqual(tip["sources"][0], row)
        self.assertEqual(tip["head"], 1_716_000)
        self.assertIsNone(tip["snapshot"])
        self.assertEqual(sync_tip.sync_issue(tip, 1_713_000),
            "wait for signed snapshot: head = 1716000 required = 1713000 "
            'available = unknown advertised = 1703160 lag = 12840 snapshot_status = unavailable '
            'advertised_status = old_epoch manifest_error = "http_503" '
            'source_errors = {"https://a.example": "http_503"} head_status = ready')
        need = Need("test", "range", 1_706_812, 1_706_811, 1_716_000)
        with mock.patch.object(upgrade, "sync_head", return_value = tip), \
                mock.patch.object(upgrade, "emit") as emit:
            with self.assertRaises(ValidatorError) as refused:
                upgrade.sync_wait({}, need, 0, 0)
        self.assertEqual(str(refused.exception), sync_tip.sync_issue(tip, 1_713_000))
        self.assertEqual(emit.call_args.kwargs,
            {"event": "sync_snapshot", "status": "wait", **sync_tip.sync_fields(tip, 1_713_000)})

    def test_sources(self):
        source = {"OCTRA_STATE_SYNC_SOURCES": "https://a.example,https://b.example",
            "OCTRA_JOIN_RPC": "https://head.example"}
        for first, second, expected in [(5000, None, 5000), (4000, 5000, 5000), (None, None, None)]:
            replies = []
            for epoch in (first, second):
                replies += [OSError("unavailable")] if epoch is None else [self.head(epoch, epoch), self.certificate(epoch)]
            with self.subTest(first = first, second = second):
                tip = self.unavailable_head(source["OCTRA_STATE_SYNC_SOURCES"], replies)
                self.assertIsNone(tip["head"])
                self.assertEqual(max((row["head"] for row in tip["sources"]
                    if row["head"] is not None), default = None), expected)
        with mock.patch.object(sync_tip, "native_tip", return_value = {"head": 6000}), \
                mock.patch.object(sync_tip.http, "open", side_effect = [
            self.head(6000, 1000), OSError("unavailable"), self.head(5000, 4000), self.certificate(4000),
            self.head(6000, 6000)]):
            tip = sync_tip.sync_head(source)
        self.assertEqual(tip["head"], 6000)
        self.assertEqual(tip["sources"][0]["head"], 6000)
        self.assertEqual(tip["snapshot"], 4000)
        self.assertIsNone(sync_tip.sync_issue(tip, 1904))
        self.assertIsNone(sync_tip.sync_issue(tip, 4000))
        self.assertEqual(sync_tip.sync_fields(tip, 4001)["snapshot_status"], "below_minimum")
        refused = sync_tip.sync_issue(tip, 4001)
        self.assertIsNotNone(refused)
        self.assertIn("snapshot_status = below_minimum", refused)

    def test_bad_numbers(self):
        for value in [float("inf"), float("nan"), True, -1, 2 ** 63, 1.5, "1e400"]:
            for field in ["head", "certificate"]:
                replies = [self.response({"version": "octra-state-sync", "mode": "live_head",
                    "head_epoch": value, "current_epoch": 5001, "snapshot_epoch": 0})] \
                    if field == "head" else [self.head(5000, 4000), self.certificate(value)]
                replies += [self.head(6000, 6000), self.certificate(6000)]
                with self.subTest(value = value, field = field):
                    tip = self.unavailable_head("https://a.example,https://b.example", replies)
                self.assertIsNone(tip["head"])
                self.assertEqual(tip["sources"][1]["head"], 6000)
                self.assertEqual(tip["snapshot"], 6000)

    def test_broken_http(self):
        wire = b"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n8\r\n{}"
        for manifest in [False, True]:
            socket = mock.Mock()
            socket.makefile.return_value = io.BytesIO(wire)
            reply = HTTPResponse(socket)
            reply.begin()
            replies = [self.head(5000, 4000), reply] if manifest else [reply]
            replies += [self.head(6000, 6000), self.certificate(6000)]
            with self.subTest(manifest = manifest):
                tip = self.unavailable_head("https://a.example,https://b.example", replies)
            self.assertEqual(tip["snapshot"], 6000)
        tip = self.unavailable_head("https://a.example", [BadStatusLine("invalid")])
        self.assertIsNone(tip["head"])
        self.assertEqual(tip["reason"], "http_response")

    def test_missing_tip(self):
        self.assertEqual(sync_tip.sync_issue(None, 5000),
            "wait for signed snapshot: head = unknown required = 5000 available = unknown "
            "advertised = unknown lag = unknown snapshot_status = unavailable "
            "advertised_status = unknown manifest_error = null "
            "source_errors = {} head_status = native_unavailable")

    def test_deep_json(self):
        loads = json.loads
        def read(value):
            if value == "[":
                raise RecursionError("depth")
            return loads(value)
        for path in ["head", "manifest"]:
            for healthy in [False, True]:
                bad = mock.MagicMock()
                bad.__enter__.return_value.read.return_value = b"["
                replies = [bad] if path == "head" else [self.head(5000, 4000), bad]
                if healthy:
                    replies += [self.head(6000, 6000), self.certificate(6000)]
                sources = "https://a.example" + (",https://b.example" if healthy else "")
                with self.subTest(path = path, healthy = healthy), \
                        mock.patch.object(sync_tip.json, "loads", side_effect = read):
                    tip = self.unavailable_head(sources, replies)
                    if healthy:
                        self.assertEqual((tip["head"], tip["snapshot"]), (None, 6000))
                        self.assertEqual(tip["sources"][1]["head"], 6000)
                    elif path == "head":
                        self.assertIsNone(tip["head"])
                        self.assertEqual(tip["reason"], "state sync head is invalid")
                    else:
                        row = {"head": 5000, "snapshot": None, "latest": 4000,
                            "status": "unavailable", "advertised_status": "ready", "source": "https://a.example",
                            "reason": "state sync certificate is invalid"}
                        self.assertEqual(tip["sources"][0], row)
                        self.assertIsNone(tip["head"])
                        self.assertIsNone(tip["snapshot"])

    def test_manifest_errors(self):
        for response, reason in [
                (HTTPError("https://a.example", 429, "busy", {}, None), "http_429"),
                (TimeoutError(), "timeout"),
                (BadStatusLine("bad"), "http_response"),
                (self.certificate(float("inf")), "state sync epoch is invalid"),
                (self.certificate(3999), "state sync certificate epoch differs"),
                (self.response({"version": "octra-state-sync", "checkpoint": {"epoch": 4000},
                    "manifest": {"files": []}}), "state sync root window is unavailable")]:
            with self.subTest(reason = reason):
                tip = self.unavailable_head("https://a.example", [self.head(5000, 4000), response])
            row = {"head": 5000, "snapshot": None, "latest": 4000,
                "status": "unavailable", "advertised_status": "ready", "source": "https://a.example", "reason": reason}
            self.assertEqual(tip["sources"][0], row)
            self.assertIsNone(tip["head"])
            self.assertIsNone(tip["snapshot"])
            self.assertEqual(tip["reason"], reason)

    def test_source_reasons(self):
        tip = self.unavailable_head("https://a.example,https://b.example", [
            self.head(6000, 1000), TimeoutError(), self.head(5000, 4000), self.certificate(4000)])
        self.assertEqual([(row["source"], row["reason"]) for row in tip["sources"][:2]],
            [("https://a.example", "timeout"), ("https://b.example", None)])
        self.assertEqual(sync_tip.sync_fields(tip, 4500)["snapshot_status"], "below_minimum")
        self.assertIn('source_errors = {"https://a.example": "timeout", "https://head.example": "transport"}',
            sync_tip.sync_issue(tip, 4500))

    def test_tls_reason(self):
        from ssl import SSLCertVerificationError
        from urllib.error import URLError
        for error in [SSLCertVerificationError(1, "certificate verify failed"),
                URLError(SSLCertVerificationError(1, "certificate verify failed"))]:
            tip = self.unavailable_head("https://a.example", [self.head(5000, 4000), error])
            self.assertEqual(tip["reason"], "transport")
            self.assertIsNone(tip["snapshot"])
        self.assertEqual(sync_tip.source_reason(ValueError("invalid URL")), "url_invalid")

    def test_head_error_row(self):
        from ssl import SSLCertVerificationError
        from urllib.error import URLError
        for healthy in (False, True):
            error = URLError(SSLCertVerificationError(1, "certificate verify failed"))
            replies = [error]
            if healthy:
                replies += [self.head(6000, 5000), self.certificate(5000)]
            sources = "https://a.example" + (",https://b.example" if healthy else "")
            tip = self.unavailable_head(sources, replies)
            self.assertIsNotNone(tip)
            self.assertIsNone(tip["head"])
            if healthy:
                self.assertEqual(tip["sources"][1]["head"], 6000)
            self.assertEqual(tip["sources"][0]["reason"], "transport")
            self.assertIsNone(tip["sources"][0]["head"])
            if not healthy:
                self.assertIn('manifest_error = "transport"', sync_tip.sync_issue(tip, 100))

if __name__ == "__main__":
    unittest.main()