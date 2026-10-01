# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import socket
import unittest
from contextlib import ExitStack
from pathlib import Path
from unittest.mock import MagicMock, patch

import test_wal_start as gate

class IpcTest(unittest.TestCase):
    def run_case(self, phase):
        events = []
        channel = MagicMock()
        channel.__enter__.return_value = channel
        channel.__exit__.side_effect = lambda *_args: events.append('close')

        def bind(_path):
            events.append('bind')
            if phase == 'bind':
                raise PermissionError('socket bind denied')

        def invoke(*_args, **_kwargs):
            events.append('pm2')
            raise RuntimeError('PM2 invoked')

        channel.bind.side_effect = bind
        with ExitStack() as scope:
            for method in ['mkdir', 'write_text', 'chmod']:
                scope.enter_context(patch.object(Path, method))
            unlink = scope.enter_context(patch.object(Path, 'unlink',
                side_effect=lambda: events.append('unlink')))
            create = scope.enter_context(patch.object(socket, 'socket', return_value=channel))
            if phase == 'create':
                create.side_effect = PermissionError('socket creation denied')
            run = scope.enter_context(patch.object(gate.subprocess, 'run', side_effect=invoke))
            expected = RuntimeError if phase == 'allow' else PermissionError
            with self.assertRaises(expected):
                gate.pm2_cases(Path(__file__).resolve().parents[1], Path('runtime_data/ipc'),
                    Path('node'), Path('pm2'), {})
            create.assert_called_once_with(socket.AF_UNIX, socket.SOCK_STREAM)
            if phase == 'allow':
                self.assertEqual(events[:4], ['bind', 'unlink', 'close', 'pm2'])
                unlink.assert_called_once()
            else:
                run.assert_not_called()
                unlink.assert_not_called()
            if phase != 'create':
                channel.__exit__.assert_called_once()

    def test_create_denied(self):
        self.run_case('create')

    def test_bind_denied(self):
        self.run_case('bind')

    def test_probe_first(self):
        self.run_case('allow')

    def test_socket_path_limit(self):
        output = Path('x' * (104 - len('/pm2/interactor.sock')))
        self.assertLess(len(str(output / 'pm2/pub.sock')), 104)
        with patch.object(Path, 'mkdir') as mkdir, \
            patch.object(socket, 'socket', side_effect = RuntimeError('probe reached')) as create:
            with self.assertRaisesRegex(ValueError, 'socket path exceeds'):
                gate.pm2_cases(Path('root'), output, Path('node'), Path('pm2'), {})
            mkdir.assert_not_called()
            create.assert_not_called()

if __name__ == '__main__':
    unittest.main()