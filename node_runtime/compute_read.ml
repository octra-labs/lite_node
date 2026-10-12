(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  chain_id : string;
  node_id : string;
  caller : string;
  caller_public_key : string;
  job_id : string;
  epoch_id : int64;
  expires_epoch : int64;
  caller_signature : string;
}

let max_epochs = 16L

let shape read =
  read.chain_id <> "" && String.length read.chain_id <= 128
  && Octra_core.Crypto.Address.is_valid_address read.node_id
  && Octra_core.Crypto.Address.is_valid_address read.caller
  && String.length read.caller_public_key = 32
  && String.length read.job_id = 32
  && String.length read.caller_signature = 64
  && read.epoch_id >= 0L && read.expires_epoch >= read.epoch_id
  && Int64.sub read.expires_epoch read.epoch_id <= max_epochs

let sign_bytes read =
  if not (shape read) then invalid_arg "compute read shape";
  Octra_net.Hash_domain.hash_encoded "octra:compute_read" (fun buffer ->
    Octra_net.Oce1.put_string buffer read.chain_id;
    Octra_net.Oce1.put_string buffer read.node_id;
    Octra_net.Oce1.put_string buffer read.caller;
    Octra_net.Oce1.put_hash32 buffer read.caller_public_key;
    Octra_net.Oce1.put_hash32 buffer read.job_id;
    Octra_net.Oce1.put_u64 buffer read.epoch_id;
    Octra_net.Oce1.put_u64 buffer read.expires_epoch)

let verify ~chain_id ~node_id ~epoch read =
  shape read
  && read.chain_id = chain_id && read.node_id = node_id
  && read.epoch_id <= epoch && epoch <= read.expires_epoch
  && Octra_core.Crypto.Address.verify_address_pubkey read.caller
       (Base64.encode_exn read.caller_public_key)
  && Octra_ed25519.safe read.caller_public_key
  && Octra_ed25519.verify ~pub:read.caller_public_key
       ~msg:(sign_bytes read) read.caller_signature