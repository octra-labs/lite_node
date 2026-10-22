# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import shutil
import threading
import unittest
import uuid
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from unittest import mock
import sync_tip
import validator_config as config
from validator_common import ValidatorError

class StartReply(BaseHTTPRequestHandler):
    def log_message(self, format, *args):
        pass

    def do_GET(self):
        self.server.paths.append(self.path)
        code, payload = self.server.replies.get(self.path, (404, {}))
        raw = json.dumps(payload).encode("ascii")
        self.send_response(code)
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)

class SyncStartTest(unittest.TestCase):
    def setUp(self):
        self.root = config.ROOT / "runtime_data/sync_start" / uuid.uuid4().hex
        self.root.mkdir(parents = True)
        self.data = self.root / "data"
        self.data.mkdir()
        self.inode = self.data.stat().st_ino
        binary = self.root / "client"
        binary.write_bytes(b"not executed")
        self.args = config.parser().parse_args(["--yes", "--sync-binary", str(binary),
            "--sync-stage", str(self.root / "stage")])
        self.server = ThreadingHTTPServer(("127.0.0.1", 0), StartReply)
        self.server.replies, self.server.paths = {}, []
        self.thread = threading.Thread(target = self.server.serve_forever, kwargs = {"poll_interval": 0.01})
        self.thread.start()
        self.base = f"http://127.0.0.1:{self.server.server_port}"
        self.values = {"OCTRA_STATE_SYNC_SOURCES": self.base + "/primary",
            "OCTRA_JOIN_RPC": self.base + "/primary," + self.base + "/secondary",
            "OCTRA_CHECKPOINT_EPOCH": "1278039"}
        self.source("primary", 1_706_811, 1_706_760)
        self.source("secondary", 1_723_441, None)

    def tearDown(self):
        self.server.shutdown()
        self.thread.join(2)
        self.server.server_close()
        shutil.rmtree(self.root)

    def source(self, name, head, snapshot):
        prefix = f"/{name}/state-sync"
        self.server.replies[prefix + "/head"] = (200, {
            "version": "octra-state-sync", "mode": "live_head", "head_epoch": head,
            "current_epoch": head + 1, "snapshot_epoch": snapshot,
            "snapshot_status": "ready" if snapshot is not None else "missing"})
        self.server.replies[prefix + "/manifest"] = (503, {}) if snapshot is None else (200, {
            "version": "octra-state-sync", "checkpoint": {"epoch": snapshot},
            "manifest": {"files": [{"path": "ready_roots", "size": 1}]}})

    def unchanged(self):
        self.assertEqual(self.data.stat().st_ino, self.inode)
        self.assertEqual(list(self.data.iterdir()), [])
        self.assertFalse((self.root / "stage").exists())

    def test_join_head(self):
        tip = sync_tip.sync_head(self.values)
        self.assertEqual(tip["head"], 1_723_441)
        self.assertEqual(tip["snapshot"], 1_706_760)
        self.assertEqual(self.server.paths, ["/primary/state-sync/head",
            "/primary/state-sync/manifest", "/secondary/state-sync/head"])

    def test_old_start(self):
        with mock.patch.object(config, "sync_snapshot") as download:
            with self.assertRaisesRegex(ValidatorError, "required = 1720441"):
                config.maybe_sync(self.args, self.data, self.values)
            download.assert_not_called()
        self.unchanged()

    def test_join_not_snapshot(self):
        self.source("secondary", 1_723_441, 1_723_400)
        with mock.patch.object(config, "sync_snapshot") as download:
            with self.assertRaisesRegex(ValidatorError, "available = 1706760"):
                config.maybe_sync(self.args, self.data, self.values)
            download.assert_not_called()
        self.assertNotIn("/secondary/state-sync/manifest", self.server.paths)
        self.unchanged()

    def test_fresh_start(self):
        for epoch in [1_719_345, 1_720_440, 1_720_441, 1_723_441]:
            self.source("primary", 1_723_441, epoch)
            with self.subTest(epoch = epoch), mock.patch.object(config, "sync_snapshot") as download:
                if epoch < 1_720_441:
                    with self.assertRaises(ValidatorError):
                        config.maybe_sync(self.args, self.data, self.values)
                    download.assert_not_called()
                else:
                    config.maybe_sync(self.args, self.data, self.values)
                    self.assertEqual(download.call_args.kwargs.get("min_epoch"), 1_720_441)
                    self.assertEqual(download.call_args.args[4], [self.base + "/primary"])
            self.unchanged()

    def test_selected_source(self):
        self.source("chosen", 1_706_811, 1_706_760)
        self.args.sync_url = [self.base + "/chosen"]
        with mock.patch.object(config, "sync_snapshot") as download:
            with self.assertRaisesRegex(ValidatorError, "required = 1720441"):
                config.maybe_sync(self.args, self.data, self.values)
            download.assert_not_called()
        self.assertIn("/chosen/state-sync/manifest", self.server.paths)
        self.assertNotIn("/primary/state-sync/manifest", self.server.paths)
        self.unchanged()

    def test_no_head(self):
        self.server.replies.clear()
        with mock.patch.object(config, "sync_snapshot") as download:
            with self.assertRaisesRegex(ValidatorError, "head = unknown"):
                config.maybe_sync(self.args, self.data, self.values)
            download.assert_not_called()
        self.unchanged()

    def test_independent_head_failure(self):
        for head in [1_706_811, 1_723_441]:
            self.source("primary", head, head - 1)
            for route in [self.base + "/primary", self.base + "/secondary", ""]:
                self.server.replies["/secondary/state-sync/head"] = (503, {})
                values = self.values | {"OCTRA_JOIN_RPC": route}
                status = "independent_unavailable" if route.endswith("/secondary") else "independent_missing"
                with self.subTest(head = head, route = route), \
                        mock.patch.object(config, "sync_snapshot") as download:
                    with self.assertRaisesRegex(ValidatorError, "head_status = " + status):
                        config.maybe_sync(self.args, self.data, values)
                    download.assert_not_called()
                self.unchanged()

    def test_primary_unavailable(self):
        self.server.replies["/primary/state-sync/head"] = (503, {})
        tip = sync_tip.sync_head(self.values)
        self.assertEqual(tip["head"], 1_723_441)
        self.assertIsNone(tip["snapshot"])
        with mock.patch.object(config, "sync_snapshot") as download:
            with self.assertRaisesRegex(ValidatorError, "required = 1720441"):
                config.maybe_sync(self.args, self.data, self.values)
            download.assert_not_called()
        self.unchanged()

    def test_window_and_checkpoint(self):
        for checkpoint, limit, floor in [(1_723_400, "100", 1_723_400),
                (1_278_039, "100", 1_723_342), (1_278_039, "8192", 1_720_441),
                (1_278_039, "1", 1_723_441)]:
            values = self.values | {"OCTRA_CHECKPOINT_EPOCH": str(checkpoint), "OCTRA_CATCHUP_MAX_LAG": limit}
            for snapshot in [floor - 1, floor]:
                self.source("primary", 1_723_441, snapshot)
                with self.subTest(checkpoint = checkpoint, limit = limit, snapshot = snapshot), \
                        mock.patch.object(config, "sync_snapshot") as download:
                    if snapshot < floor:
                        with self.assertRaises(ValidatorError):
                            config.maybe_sync(self.args, self.data, values)
                        download.assert_not_called()
                    else:
                        config.maybe_sync(self.args, self.data, values)
                        self.assertEqual(download.call_args.kwargs.get("min_epoch"), floor)
                self.unchanged()

if __name__ == "__main__":
    unittest.main()