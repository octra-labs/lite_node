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

def validate_bundle(network):
    bundle = Path(network).resolve()
    digest = sha256_file(bundle)
    _, _, values = load_network(bundle, digest)
    return values, digest

def main():
    parser = argparse.ArgumentParser(prog="validator_bundle.py")
    parser.add_argument("--network", required=True)
    args = parser.parse_args()
    values, digest = validate_bundle(args.network)
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