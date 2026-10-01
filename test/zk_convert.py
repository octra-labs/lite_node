# SPDX-License-Identifier: BSD-3-Clause
# Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

import sys
import json
import argparse
from pathlib import Path

FE_SIZE = 32
G1_SIZE = 64
G2_SIZE = 128
MAGIC_VK = b"OG16V1"
MAGIC_PROOF = b"OG16P1"

BN254_P = 0x30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd47
BN254_R = 0x30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000001

def fe_to_be32(value_str, modulus, label):
    n = int(value_str)
    if n < 0 or n >= modulus:
        raise ValueError(f"{label}: {n} out of range [0, {modulus})")
    return n.to_bytes(FE_SIZE, "big")

def fp_to_be32(value_str, label):
    return fe_to_be32(value_str, BN254_P, label)

def fr_to_be32(value_str, label):
    return fe_to_be32(value_str, BN254_R, label)

def encode_g1(point, label):
    if not isinstance(point, list) or len(point) < 2:
        raise ValueError(f"{label}: expected [x, y] or [x, y, z]")
    if len(point) >= 3 and str(point[2]) != "1":
        raise ValueError(f"{label}: z must be 1 (snarkjs uses Jacobian flag), got {point[2]}")
    if str(point[0]) == "0" and str(point[1]) == "0":
        raise ValueError(f"{label}: point at infinity not allowed")
    return fp_to_be32(point[0], f"{label}.x") + fp_to_be32(point[1], f"{label}.y")

def encode_fp2(coords, label):
    if not isinstance(coords, list) or len(coords) != 2:
        raise ValueError(f"{label}: expected [c0, c1]")
    return fp_to_be32(coords[0], f"{label}.c0") + fp_to_be32(coords[1], f"{label}.c1")

def encode_g2(point, label):
    if not isinstance(point, list) or len(point) < 2:
        raise ValueError(f"{label}: expected G2 point with x and y")
    if len(point) >= 3:
        z = point[2]
        if not isinstance(z, list) or len(z) != 2 or str(z[0]) != "1" or str(z[1]) != "0":
            raise ValueError(f"{label}: z must be [1, 0] (Jacobian flag), got {z}")
    if str(point[0][0]) == "0" and str(point[0][1]) == "0" and \
       str(point[1][0]) == "0" and str(point[1][1]) == "0":
        raise ValueError(f"{label}: point at infinity not allowed")
    return encode_fp2(point[0], f"{label}.x") + encode_fp2(point[1], f"{label}.y")

def encode_vk(vk_json):
    expected_curve = "bn128"
    if vk_json.get("curve") and vk_json["curve"] != expected_curve:
        raise ValueError(f"unsupported curve: {vk_json['curve']} (need {expected_curve})")
    proto = vk_json.get("protocol")
    if proto and proto != "groth16":
        raise ValueError(f"unsupported protocol: {proto} (need groth16)")
    n_public = int(vk_json["nPublic"])
    if n_public < 0 or n_public > 1024:
        raise ValueError(f"nPublic = {n_public} minimum = 0 maximum = 1024")
    ic = vk_json["IC"]
    if len(ic) != n_public + 1:
        raise ValueError(f"IC length {len(ic)} != nPublic+1 = {n_public + 1}")
    out = bytearray()
    out += MAGIC_VK
    out += n_public.to_bytes(4, "big")
    out += encode_g1(vk_json["vk_alpha_1"], "vk_alpha_1")
    out += encode_g2(vk_json["vk_beta_2"], "vk_beta_2")
    out += encode_g2(vk_json["vk_gamma_2"], "vk_gamma_2")
    out += encode_g2(vk_json["vk_delta_2"], "vk_delta_2")
    for i, ic_point in enumerate(ic):
        out += encode_g1(ic_point, f"IC[{i}]")
    return bytes(out)

def encode_proof(proof_json):
    proto = proof_json.get("protocol")
    if proto and proto != "groth16":
        raise ValueError(f"unsupported protocol: {proto} (need groth16)")
    out = bytearray()
    out += MAGIC_PROOF
    out += encode_g1(proof_json["pi_a"], "pi_a")
    out += encode_g2(proof_json["pi_b"], "pi_b")
    out += encode_g1(proof_json["pi_c"], "pi_c")
    return bytes(out)

def encode_inputs(public_json):
    if not isinstance(public_json, list):
        raise ValueError("public.json must be a JSON array of decimal strings")
    out = bytearray()
    for i, v in enumerate(public_json):
        out += fr_to_be32(v, f"input[{i}]")
    return bytes(out)

def cmd_vk(args):
    data = json.loads(Path(args.input).read_text())
    out = encode_vk(data)
    Path(args.output).write_bytes(out)
    print(f"status = pass kind = vk bytes = {len(out)} inputs = {data['nPublic']} output = {args.output}")

def cmd_proof(args):
    data = json.loads(Path(args.input).read_text())
    out = encode_proof(data)
    Path(args.output).write_bytes(out)
    print(f"status = pass kind = proof bytes = {len(out)} output = {args.output}")

def cmd_inputs(args):
    data = json.loads(Path(args.input).read_text())
    out = encode_inputs(data)
    Path(args.output).write_bytes(out)
    print(f"status = pass kind = inputs bytes = {len(out)} count = {len(data)} output = {args.output}")

def cmd_all(args):
    src = Path(args.input_dir)
    dst = Path(args.output_dir)
    dst.mkdir(parents=True, exist_ok=True)
    vk_data = json.loads((src / "verification_key.json").read_text())
    proof_data = json.loads((src / "proof.json").read_text())
    public_data = json.loads((src / "public.json").read_text())
    n_public = int(vk_data["nPublic"])
    if not isinstance(public_data, list):
        raise ValueError("public.json must be a JSON array")
    if len(public_data) != n_public:
        raise ValueError(
            f"public input count mismatch: expected = {n_public} actual = {len(public_data)}"
        )
    vk_bin = encode_vk(vk_data)
    proof_bin = encode_proof(proof_data)
    inputs_bin = encode_inputs(public_data)
    (dst / "vk.bin").write_bytes(vk_bin)
    (dst / "proof.bin").write_bytes(proof_bin)
    (dst / "inputs.bin").write_bytes(inputs_bin)
    print(f"status = pass kind = all vk_bytes = {len(vk_bin)} proof_bytes = {len(proof_bin)} input_bytes = {len(inputs_bin)} inputs = {n_public}")

def main():
    parser = argparse.ArgumentParser(
        description="Convert snarkjs Groth16 BN254 JSON to OG16V1/OG16P1 binary format"
    )
    sub = parser.add_subparsers(dest="cmd", required=True)

    p_vk = sub.add_parser("vk", help="convert verification_key.json -> vk.bin")
    p_vk.add_argument("input")
    p_vk.add_argument("output")
    p_vk.set_defaults(func=cmd_vk)

    p_proof = sub.add_parser("proof", help="convert proof.json -> proof.bin")
    p_proof.add_argument("input")
    p_proof.add_argument("output")
    p_proof.set_defaults(func=cmd_proof)

    p_inputs = sub.add_parser("inputs", help="convert public.json -> inputs.bin")
    p_inputs.add_argument("input")
    p_inputs.add_argument("output")
    p_inputs.set_defaults(func=cmd_inputs)

    p_all = sub.add_parser("all", help="convert all 3 from a directory")
    p_all.add_argument("input_dir")
    p_all.add_argument("output_dir")
    p_all.set_defaults(func=cmd_all)

    args = parser.parse_args()
    try:
        args.func(args)
    except (KeyError, ValueError, json.JSONDecodeError) as e:
        print(f"status = fail reason = {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()