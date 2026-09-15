# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import json
import os
import socket
import subprocess
from pathlib import Path

import grpc
from octra.node.v1 import node_pb2 as pb
from octra.node.v1 import node_pb2_grpc as api

def expect_error(call, code):
    try:
        call()
    except grpc.RpcError as error:
        assert error.code() == code, (error.code(), code, error.details())
        return dict(error.trailing_metadata())
    else:
        raise AssertionError('call succeeded instead of returning an error')

def check(channel):
    stub = api.NodeStub(channel)
    status = json.loads(stub.Status(pb.Empty(), timeout=2).json)
    assert status == {'method': 'node_status', 'params': []}
    address = 'oct3SSKjCGK8pVxPHH1Y6LZEVqm94rZn3StXHt31AD1UUVN'
    account = json.loads(stub.Account(pb.AddressRequest(address=address), timeout=2).json)
    assert account['params'] == [address]
    txhash = '1' * 64
    transaction = json.loads(stub.Transaction(pb.HashRequest(hash=txhash), timeout=2).json)
    assert transaction['params'] == [txhash]
    epoch = json.loads(stub.Epoch(pb.EpochRequest(epoch=9), timeout=2).json)
    assert epoch['params'] == [9]
    first = stub.Epochs(pb.EpochPageRequest(start=0, limit=3), timeout=2)
    assert first.anchor.chain_id == 'octra-test' and first.anchor.epoch == 9
    assert first.stop == pb.EPOCH_PAGE_MORE and first.next_epoch == 3
    assert [row.epoch for row in first.epochs] == [0, 1, 2]
    for row in first.epochs:
        assert row.start_txid == 9007199254740993 + row.epoch
        assert row.finalized_at == row.epoch + 0.25
        assert row.state_root == f'{row.epoch + 1:064x}'
    rows, result = list(first.epochs), first
    for _ in range(10):
        if result.stop != pb.EPOCH_PAGE_MORE:
            break
        result = stub.Epochs(pb.EpochPageRequest(start=result.next_epoch, limit=3, anchor=first.anchor, previous_root=rows[-1].state_root), timeout=2)
        rows.extend(result.epochs)
    assert [row.epoch for row in rows] == list(range(10))
    assert result.stop == pb.EPOCH_PAGE_COMPLETE and result.next_epoch == 10
    default = stub.Epochs(pb.EpochPageRequest(), timeout=2)
    assert len(default.epochs) == 10 and default.stop == pb.EPOCH_PAGE_COMPLETE
    for request in [pb.EpochPageRequest(limit=0), pb.EpochPageRequest(limit=65), pb.EpochPageRequest(start=1 << 31)]:
        expect_error(lambda: stub.Epochs(request, timeout=2), grpc.StatusCode.INVALID_ARGUMENT)
    expect_error(lambda: stub.Status(pb.Empty(), timeout=0.005), grpc.StatusCode.DEADLINE_EXCEEDED)
    assert json.loads(stub.Status(pb.Empty(), timeout=2).json)['method'] == 'node_status'
    unknown = channel.unary_unary('/octra.node.v1.Node/Missing')
    expect_error(lambda: unknown(b'', timeout=2), grpc.StatusCode.UNIMPLEMENTED)
    tx = {'from': address, 'to_': address, 'amount': '1', 'nonce': 7, 'ou': '1000',
          'timestamp': 1.0, 'signature': 'sig', 'op_type': 'standard'}
    def send(value):
        return stub.Submit(pb.SubmitRequest(transaction_json=json.dumps(value).encode()), timeout=2)
    sent = json.loads(send(tx).json)
    assert sent['tx_hash'] == 'a' * 64 and sent['nonce'] == 7
    refused = expect_error(lambda: send({**tx, 'nonce': 8}), grpc.StatusCode.FAILED_PRECONDITION)
    assert refused['octra-rpc-code'] == '102'
    assert json.loads(refused['octra-rpc-error-bin'])['code'] == 102
    expect_error(lambda: send({}), grpc.StatusCode.INVALID_ARGUMENT)
    expect_error(lambda: send([tx]), grpc.StatusCode.INVALID_ARGUMENT)
    large = {**tx, 'op_type': 'key_switch', 'encrypted_data': 'a' * 4_142_248}
    assert json.loads(send(large).json)['tx_hash'] == sent['tx_hash']
    return {'status': 'pass', 'grpcio': grpc.__version__, 'methods': 6, 'page_rows': len(rows), 'integer_exact': True, 'errors': 8, 'channel_reused': True, 'submit_validation': 'test_backend', 'large_bytes': 4_142_248}

def main():
    root = Path.cwd()
    output_dir = root / 'runtime_data/release'
    output_dir.mkdir(parents=True, exist_ok=True)
    with socket.socket() as reserved:
        reserved.bind(('127.0.0.1', 0))
        port = reserved.getsockname()[1]
    env = {**os.environ, 'DYLD_LIBRARY_PATH': str(root / 'lib')}
    with (output_dir / 'grpc-client-server.log').open('w') as output:
        process = subprocess.Popen([str(root / '_build/default/test/test_epoch_page.exe'), '--listen', str(port)], env=env, stdout=output, stderr=subprocess.STDOUT)
        try:
            with grpc.insecure_channel(f'127.0.0.1:{port}', options=[('grpc.enable_retries', 0)]) as channel:
                grpc.channel_ready_future(channel).result(timeout=5)
                print(json.dumps(check(channel), indent=2))
        finally:
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait(timeout=5)

if __name__ == '__main__':
    main()