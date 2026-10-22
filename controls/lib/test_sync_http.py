# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from unittest import mock
import sync_tip
import test_sync_tip

class SourceReply(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        pass

    def do_GET(self):
        self.server.paths.append(self.path)
        if self.path == self.server.bad_path:
            self.send_response(self.server.code)
            self.send_header("Location", self.server.location)
            self.send_header("Content-Length", "0")
            self.end_headers()
            return
        if self.path.endswith("/head"):
            payload = {"version": "octra-state-sync", "mode": "live_head",
                "head_epoch": 5000, "current_epoch": 5001,
                "snapshot_epoch": 4000, "snapshot_status": "ready"}
        else:
            payload = {"version": "octra-state-sync", "checkpoint": {"epoch": 4000},
                "manifest": {"files": [{"path": "ready_roots", "size": 1}]}}
        raw = json.dumps(payload).encode("ascii")
        self.send_response(200)
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)

class SyncHttpTest(unittest.TestCase):
    def test_redirects(self):
        with ThreadingHTTPServer(("127.0.0.1", 0), SourceReply) as server:
            thread = threading.Thread(target = server.serve_forever, kwargs = {"poll_interval": 0.01})
            thread.start()
            try:
                base = f"http://127.0.0.1:{server.server_port}"
                values = {"OCTRA_STATE_SYNC_SOURCES": base + "/a," + base + "/b",
                    "OCTRA_JOIN_RPC": base + "/c"}
                for path in ["head", "manifest"]:
                    for code in [301, 302, 303, 307, 308]:
                        for location in ["https://[/", "https://a..b/", "https://" + "a" * 64 + "/",
                                base + "/visited", "/visited"]:
                            with self.subTest(path = path, code = code, location = location):
                                server.paths = []
                                server.bad_path = "/a/state-sync/" + path
                                server.code, server.location = code, location
                                tip = sync_tip.sync_head(values)
                                self.assertEqual((tip["head"], tip["snapshot"]), (5000, 4000))
                                self.assertEqual(server.paths, (["/a/state-sync/head"] if path == "head"
                                    else ["/a/state-sync/head", "/a/state-sync/manifest"])
                                    + ["/b/state-sync/head", "/b/state-sync/manifest", "/c/state-sync/head"])
                                if path == "manifest":
                                    self.assertEqual(tip["sources"][0]["reason"], f"http_{code}")
            finally:
                server.shutdown()
                thread.join(120)
                self.assertFalse(thread.is_alive())

    def test_url_errors(self):
        replies = test_sync_tip.SyncTipTest()
        for path in ["head", "manifest"]:
            for healthy in [False, True]:
                data = [ValueError("url")] if path == "head" else [
                    replies.head(5000, 4000), ValueError("url")]
                if healthy:
                    data += [replies.head(6000, 6000), replies.certificate(6000)]
                sources = "https://a.example" + (",https://b.example" if healthy else "")
                with self.subTest(path = path, healthy = healthy), \
                        mock.patch.object(sync_tip.http, "open", side_effect = data + [OSError("head unavailable")]):
                    tip = sync_tip.sync_head({"OCTRA_STATE_SYNC_SOURCES": sources,
                        "OCTRA_JOIN_RPC": "https://head.example"})
                    if healthy:
                        self.assertEqual((tip["head"], tip["snapshot"]), (None, 6000))
                        self.assertEqual(tip["sources"][1]["head"], 6000)
                    elif path == "head":
                        self.assertIsNone(tip["head"])
                        self.assertEqual(tip["reason"], "url_invalid")
                    else:
                        self.assertIsNone(tip["head"])
                        self.assertEqual(tip["sources"][0]["head"], 5000)
                        self.assertIsNone(tip["snapshot"])
                        self.assertEqual(tip["reason"], "url_invalid")

if __name__ == "__main__":
    unittest.main()