# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import fcntl
import hashlib
import json
import os
from pathlib import Path
import shlex
import socket
from string import Template
import subprocess
import sys
import time
from unittest import mock

def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def files(data):
    return {str(path.relative_to(data)): (
        ('link', os.readlink(path)) if path.is_symlink() else
        ('dir', path.stat().st_mode) if path.is_dir() else
        ('file', path.stat().st_mode, digest(path)))
        for path in data.rglob('*')}

def record_cases(data, name):
    head = {'schema_version': 3, 'generation': 1, 'epoch_id': 1, 'state_root': 'root',
        'ledger_state_root': 'ledger', 'irmin_commit': 'commit', 'txid_hi': '9',
        'txlog_seg': 0, 'txlog_off': 42, 'epochlog_off': 63, 'commit_id': 'first',
        'ts': 0.0, 'quorum_cert_hash': None, 'epoch_index_hash': 'index', 'epoch_index_root': 'epochs'}
    row = {'type': 'PREPARE', 'commit_id': 'first', 'prev_generation': 0,
        'epoch_id': 1, 'planned_txid_hi': '9', 'planned_state_root': 'root', 'ts': 0.0}
    group, case = name.split('_', 1)
    path = data / ('HEAD.json' if group == 'head' else 'commit_journal.log')
    valid = json.dumps(head) if group == 'head' else json.dumps(row) + '\n'
    if case == 'link':
        target = data / 'retained'
        target.write_text(valid)
        path.symlink_to(target)
    elif case == 'directory':
        path.mkdir(mode=0o700)
    else:
        content = {
            'valid': valid,
            'empty': '',
            'json': '{',
            'field': json.dumps({**head, 'epoch_id': -1}),
            'suffix': valid + '{',
            'unknown': json.dumps({**row, 'type': 'UNKNOWN'}) + '\n',
            'blank': '\n',
        }[case]
        path.write_text(content)
    return path

def boot_cases(root, output, binary, env):
    valid = {'epoch_id': 41, 'pre_state_root': 'before', 'post_state_root': 'after',
        'parent_commit': 'parent', 'start_txid': '1', 'tx_count': 0,
        'finalized_by': 'tester', 'finalized_at': 1, 'irmin_last_epoch_before': 40}
    names = ['bad_json', 'empty', 'wrong_epoch', 'pending', 'dir_link',
        'file_link', 'dir_file', 'entry_dir', 'valid', 'absent',
        'head_json', 'head_empty', 'head_field', 'head_link', 'head_directory', 'head_valid',
        'journal_json', 'journal_suffix', 'journal_unknown', 'journal_blank',
        'journal_link', 'journal_directory', 'journal_valid', 'journal_empty']
    rows = []
    for name in names:
        data = output / '.keys' / name
        data.mkdir(parents=True, mode=0o700)
        wallet = data / 'wallet.json'
        wallet.write_text('{}')
        wallet.chmod(0o600)
        wal = data / 'wal'
        record = None
        if name.startswith(('head_', 'journal_')):
            record = record_cases(data, name)
        elif name == 'dir_link':
            wal.symlink_to(data / 'missing')
        elif name == 'dir_file':
            wal.write_text('not a directory')
        elif name != 'absent':
            wal.mkdir(mode=0o700)
            path = wal / ('0000000041_0000.pending' if name == 'pending' else '0000000041.wal')
            if name == 'file_link':
                path.symlink_to(wallet)
            elif name == 'entry_dir':
                path.mkdir(mode=0o700)
            else:
                content = json.dumps({**valid, 'epoch_id': 42}) if name == 'wrong_epoch' else json.dumps(valid)
                if name in ['bad_json', 'pending']:
                    content = '{'
                elif name == 'empty':
                    content = ''
                path.write_text(content)
        before = files(data)
        run = subprocess.run([str(binary), '--observer'], cwd=root,
            env={**env, 'OCTRA_DATA_DIR': str(data), 'OCTRA_STORE_WAIT_SECONDS': '0'}, capture_output=True, text=True, timeout=30)
        text = run.stdout + run.stderr
        (output / (name + '.log')).write_text(text)
        bad = name not in ['valid', 'absent', 'head_valid', 'journal_valid', 'journal_empty']
        expected = 78 if bad else 1
        reason = 'event = wal_start status = refused' if bad else 'wallet_load = failed'
        after = files(data)
        unchanged = before == after
        ready = 'event = pvac_worker status = ready' in text
        path_found = not bad or record is None or str(record) in text
        offset_found = name != 'journal_suffix' or f'offset = {record.stat().st_size - 1} ' in text
        rows.append({'name': name, 'exit_code': run.returncode,
            'expected_exit': expected, 'reason_found': reason in text,
            'files_preserved': unchanged, 'worker_ready': ready, 'path_found': path_found,
            'offset_found': offset_found,
            'changed_paths': sorted(path for path in before.keys() | after.keys()
                if before.get(path) != after.get(path)),
            'passed': run.returncode == expected and reason in text and ready and path_found and offset_found
                and (not bad or unchanged)})
    return rows

def store_owner_case(root, output, binary, env):
    folder = root / 'controls/lib'
    sys.path.insert(0, str(folder))
    from validator_common import ensure_wallet
    data = output / '.keys' / 'held_store'
    data.mkdir(parents=True, mode=0o700)
    ensure_wallet(data / 'wallet.json')
    chain = data / 'chaindata'
    chain.mkdir(mode=0o700)
    descriptor = os.open(chain, os.O_RDONLY | os.O_CLOEXEC)
    before = files(data)
    try:
        fcntl.flock(descriptor, fcntl.LOCK_EX | fcntl.LOCK_NB)
        run = subprocess.run([str(binary), '--observer'], cwd=root,
            env={**env, 'OCTRA_DATA_DIR': str(data), 'OCTRA_STORE_WAIT_SECONDS': '0'},
            capture_output=True, text=True, timeout=30)
    finally:
        os.close(descriptor)
    text = run.stdout + run.stderr
    (output / 'held_store.log').write_text(text)
    unchanged = before == files(data)
    reason = 'store ownership is held by another process' in text
    return {'name': 'held_store', 'exit_code': run.returncode,
        'files_preserved': unchanged, 'reason_found': reason,
        'passed': run.returncode == 78 and unchanged and reason}

def systemd_cases(root, output):
    folder = root / 'controls/lib'
    sys.path.insert(0, str(folder))
    import upgrade
    from validator_common import ValidatorError
    policies = [
        ({}, False),
        ({'Restart': 'unknown'}, False),
        ({'Restart': 'always'}, False),
        ({'Restart': 'on-failure'}, False),
        ({'Restart': 'on-success', 'SuccessExitStatus': '78'}, False),
        ({'Restart': 'no', 'RestartForceExitStatus': '78'}, False),
        ({'Restart': 'always', 'RestartPreventExitStatus': '78', 'RestartForceExitStatus': '78'}, False),
        ({'Restart': 'on-failure', 'RestartPreventExitStatus': '78'}, True),
        ({'Restart': 'always', 'RestartPreventExitStatus': 'CONFIG'}, True),
        ({'Restart': 'always', 'RestartPreventExitStatus': 'EX_CONFIG'}, True),
        ({'Restart': 'no'}, True),
        ({'Restart': 'on-success'}, True),
        ({'Restart': 'on-abnormal'}, True),
        ({'Restart': 'on-abort'}, True),
        ({'Restart': 'on-watchdog'}, True),
        ({'Restart': 'on-failure', 'SuccessExitStatus': '78'}, True),
    ]
    rows = []
    for policy, expected in policies:
        with mock.patch.object(upgrade, 'data_pids', return_value=[]), mock.patch.object(
            upgrade, 'inspect_pending', return_value=[]), mock.patch.object(
            upgrade.shutil, 'disk_usage', return_value=mock.Mock(free=8 * 1024**3)), mock.patch.object(
            upgrade, 'call') as call, mock.patch.object(upgrade, 'emit'):
            reason = ''
            try:
                upgrade.preflight(root, {'kind': 'systemd', 'pid': 0, 'restart': policy},
                    {'OCTRA_DATA_DIR': str(output / 'unused')}, False)
                accepted = True
            except ValidatorError as error:
                reason = str(error)
                accepted = False
            rows.append({'policy': policy, 'accepted': accepted, 'expected': expected,
                'reason': reason, 'passed': accepted == expected and not call.called
                    and (accepted or 'storage restart policy' in reason)})
    return rows

def pm2_cases(root, output, binary, pm2, env):
    home = output / 'pm2'
    if len(os.fsencode(home / 'interactor.sock')) >= 104:
        raise ValueError('PM2 socket path exceeds platform limit; use a shorter output path')
    home.mkdir(mode=0o700)
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as probe:
        probe.bind(str(home / 'pub.sock'))
        (home / 'pub.sock').unlink()
    source = root / 'controls/run.sh'
    script = source.read_text()
    assert script.count('pm2 start ') == 1
    command = 'pm2 start ' + script.split('pm2 start ', 1)[1].split('\n\npm2 save', 1)[0]
    tokens = shlex.split(command.replace('\\\n', ''))
    assert tokens[:2] == ['pm2', 'start']
    assert all(token not in [';', '&&', '|', '>'] for token in tokens)
    selected = {**env, 'PM2_HOME': str(home), 'PM2_DISABLE_VERSION_CHECK': '1', 'PM2_SILENT': 'true'}

    def invoke(args, config=selected):
        run = subprocess.run([str(pm2), *args], cwd=root, env=config,
            capture_output=True, text=True, timeout=30)
        if run.returncode:
            raise RuntimeError(run.stderr + run.stdout)
        return run.stdout

    def start(name, executable, data):
        values = {'OCTRA_OPERATOR_BINARY': str(executable), 'OCTRA_OPERATOR_PM2_NAME': name,
            'ROOT': str(root), 'OCTRA_OPERATOR_LOG_DIR': str(output)}
        args = [Template(token).substitute(values) for token in tokens[1:]]
        invoke(args, {**selected, 'OCTRA_DATA_DIR': str(data)})
        return args

    def state(name):
        entries = json.loads(invoke(['jlist', '--silent']))
        chosen = [entry for entry in entries if entry.get('name') == name]
        assert len(chosen) == 1
        meta = chosen[0]['pm2_env']
        return {key: meta.get(key) for key in ['status', 'restart_time', 'exit_code', 'stop_exit_codes']}

    rows = []
    try:
        for name in ['wal', 'head', 'journal']:
            data = output / '.keys' / ('pm2_' + name)
            data.mkdir(parents=True, mode=0o700)
            (data / 'wallet.json').write_text('{}')
            (data / 'wallet.json').chmod(0o600)
            if name == 'wal':
                (data / 'wal').mkdir(mode=0o700)
                (data / 'wal/0000000041.wal').write_text('{')
            else:
                record_cases(data, name + ('_field' if name == 'head' else '_suffix'))
            before = files(data)
            label = name + '-refusal'
            args = start(label, binary, data)
            time.sleep(1)
            first = state(label)
            time.sleep(2)
            last = state(label)
            rows.append({'name': label, 'command': args, 'first': first, 'last': last,
                'files_preserved': before == files(data),
                'passed': first['status'] == last['status'] == 'stopped'
                    and first['exit_code'] == last['exit_code'] == 78
                    and first['restart_time'] == last['restart_time'] == 0
                    and before == files(data)})
            invoke(['delete', label])
        retry = output / 'exit_retry'
        subprocess.run(['cc', str(root / 'test/exit_retry.c'), '-o', str(retry)], check=True)
        args = start('retry-75', retry, data)
        time.sleep(1)
        last = state('retry-75')
        rows.append({'name': 'retry-75', 'command': args, 'last': last,
            'passed': isinstance(last['restart_time'], int) and last['restart_time'] > 0
                and last['exit_code'] == 75})
        return rows
    finally:
        invoke(['kill'])
        (output / 'pm2_cases.json').write_text(json.dumps(rows, indent=2) + '\n')

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--root', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--binary', type=Path, required=True)
    parser.add_argument('--worker', type=Path, required=True)
    parser.add_argument('--pm2', type=Path, required=True)
    args = parser.parse_args()
    root, output, binary, worker, pm2 = [value.absolute() for value in
        [args.root, args.output, args.binary, args.worker, args.pm2]]
    for path in [binary, worker, pm2]:
        if not path.is_file() or not os.access(path, os.X_OK):
            raise ValueError('required executable missing: ' + str(path))
    output.mkdir(mode=0o700)
    env = {key: value for key, value in os.environ.items() if not key.startswith(('OCTRA_', 'PM2_'))}
    env.update({'OCTRA_CHAIN_ID': 'octra-devnet-9871-cluster', 'OCTRA_CONSENSUS_MODE': 'observer',
        'OCTRA_PVAC_VERIFY_WORKER': str(worker), 'OCTRA_PVAC_VERIFY_WORKER_HASH': digest(worker),
        'OCTRA_CONSENSUS_PORT': '0'})
    rows = {'binary_sha256': digest(binary), 'worker_sha256': digest(worker)}
    rows['boot'] = boot_cases(root, output, binary, env)
    rows['boot'].append(store_owner_case(root, output, binary, env))
    rows['systemd'] = systemd_cases(root, output)
    rows['pm2'] = pm2_cases(root, output, binary, pm2, env)
    (output / 'checks.json').write_text(json.dumps(rows, indent=2) + '\n')
    failures = [(group, number) for group in ['boot', 'systemd', 'pm2']
        for number, row in enumerate(rows[group]) if not row['passed']]
    for group, number in failures:
        print(f'event = test group = {group} case = {number} status = failed')
    print(f'event = test name = wal_start failures = {len(failures)}')
    return 1 if failures else 0

if __name__ == '__main__':
    raise SystemExit(main())