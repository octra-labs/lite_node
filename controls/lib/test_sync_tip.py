# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import io
import json
import unittest
from http.client import HTTPResponse, BadStatusLine
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

    def test_missing_route(self):
        for heads in ["", "https://a.example", "https://a.example,https://b.example"]:
            with self.subTest(heads = heads), mock.patch.object(sync_tip.http, "open") as read:
                tip = sync_tip.sync_head({"OCTRA_STATE_SYNC_SOURCES": "https://a.example,https://b.example",
                    "OCTRA_JOIN_RPC": heads})
                issue = sync_tip.sync_issue(tip, 100)
                self.assertTrue(issue.startswith("state sync configuration error:"))
                self.assertIn("configure OCTRA_JOIN_RPC", issue)
                self.assertEqual(sync_tip.sync_fields(tip, 100)["head_status"], "independent_missing")
                read.assert_not_called()

    def test_old_manifest(self):
        replies = [self.head(1_716_000, 1_703_160),
            HTTPError("https://a.example/state-sync/manifest", 503, "old_epoch", {}, None),
            self.head(1_716_000, 1_703_160)]
        with mock.patch.object(sync_tip.http, "open", side_effect = replies):
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
        with mock.patch.object(sync_tip.http, "open", side_effect = [
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
            "source_errors = {} head_status = independent_unavailable")

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