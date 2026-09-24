# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import os
import subprocess
import sys
from pathlib import Path

def check(binary, chain, mode, flags):
    root = Path.cwd() / "runtime_data" / "rule_start"
    assert not root.exists()
    env = {key: value for key, value in os.environ.items()
           if not key.startswith("OCTRA_")}
    env.update({
        "OCTRA_DATA_DIR": str(root / "data"),
        "OCTRA_PVAC_VERIFY_WORKER": str(root / "worker"),
        "OCTRA_CONSENSUS_PORT": "0",
    })
    if chain is not None:
        env["OCTRA_CHAIN_ID"] = chain
    if mode is not None:
        env["OCTRA_CONSENSUS_MODE"] = mode
    result = subprocess.run(
        [binary, *flags], env = env, capture_output = True,
        text = True, timeout = 15,
    )
    output = result.stdout + result.stderr
    denied = "event = rule_plan status = rejected reason = unsupported_chain"
    assert result.returncode == 1, output
    assert not root.exists(), "startup wrote before validation"
    if chain == "octra-devnet-9871-cluster":
        assert denied not in output, output
        assert "event = pvac_worker status = rejected" in output, output
    else:
        assert denied in output, (chain, mode, flags, output)
        assert "event = pvac_worker" not in output, output

def main():
    binary = str(Path(sys.argv[1]).resolve())
    chains = [
        None, "", "octra-mainnet", "octra-test", "octra-test-4node",
        "octra-devnet", "OCTRA-DEVNET-9871-CLUSTER",
        "octra-devnet-9871-cluster ", "octra-devnet-9871-cluster\n",
        "octra-devnet-9871-cluster",
    ]
    roles = [
        (None, []), ("bft", []), ("observer", []),
        ("legacy", []), ("follower", []), (None, ["--observer"]),
        (None, ["--state-sync-publisher"]), (None, ["--init"]),
    ]
    for chain in chains:
        for mode, flags in roles:
            check(binary, chain, mode, flags)
    print(f"status = pass gate = rule_start cases = {len(chains) * len(roles)}")

if __name__ == "__main__":
    main()