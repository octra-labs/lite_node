# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import argparse
import sys
from pathlib import Path

from validator_common import ValidatorError
from validator_common import load_network
from validator_common import sha256_file

def emit(**fields):
    print(" ".join(f"{key} = {value}" for key, value in fields.items()))

def validate_bundle(network, binary=None):
    bundle = Path(network).resolve()
    digest = sha256_file(bundle)
    _, _, values = load_network(bundle, digest)
    if binary is not None:
        candidate = Path(binary).resolve()
        if not candidate.is_file():
            raise ValidatorError(f"node candidate is missing: {candidate}")
        actual = sha256_file(candidate)
        if actual != values["OCTRA_BINARY_HASH"]:
            raise ValidatorError(f"candidate hash mismatch: {actual}")
    return values, digest

def main():
    parser = argparse.ArgumentParser(prog="validator_bundle.py")
    parser.add_argument("--binary")
    parser.add_argument("--network", required=True)
    args = parser.parse_args()
    values, digest = validate_bundle(args.network, args.binary)
    emit(
        status="ready",
        chain=values["OCTRA_CHAIN_ID"],
        network_sha256=digest,
        validator_activation=values["OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH"],
    )

if __name__ == "__main__":
    try:
        main()
    except (KeyError, OSError, ValidatorError, ValueError) as error:
        emit(status="refused", reason=str(error))
        sys.exit(1)