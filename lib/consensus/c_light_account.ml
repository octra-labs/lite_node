(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  chain_id : string;
  address : string;
  head_epoch : int64;
  state_root : string;
  balance : string;
  nonce : int;
  public_key : string option;
  encrypted_balance : string option;
  decrypt_allowance : string;
  account_hash : string;
}

let hash_ok s =
  String.length s = 32

let digits s =
  String.length s > 0 && String.for_all (function '0' .. '9' -> true | _ -> false) s

let opt_text_ok = function
  | None -> true
  | Some s -> String.length s <= 100_000_000

let u32_int_ok value =
  value >= 0 && value <= 0xffff_ffff

let sane t =
  t.chain_id <> ""
  && t.address <> ""
  && Int64.compare t.head_epoch 0L >= 0
  && hash_ok t.state_root
  && digits t.balance
  && u32_int_ok t.nonce
  && opt_text_ok t.public_key
  && opt_text_ok t.encrypted_balance
  && digits t.decrypt_allowance
  && hash_ok t.account_hash

let hash_fields ~chain_id ~address ~head_epoch ~state_root ~balance ~nonce ~public_key ~encrypted_balance ~decrypt_allowance =
  Octra_net.Hash_domain.hash_encoded "octra:light_account:v1" (fun buf ->
    Octra_net.Oce1.put_string buf chain_id;
    Octra_net.Oce1.put_string buf address;
    Octra_net.Oce1.put_u64 buf head_epoch;
    Octra_net.Oce1.put_hash32 buf state_root;
    Octra_net.Oce1.put_string buf balance;
    Octra_net.Oce1.put_u32_int buf nonce;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_string buf public_key;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_string buf encrypted_balance;
    Octra_net.Oce1.put_string buf decrypt_allowance)

let make ~chain_id ~address ~head_epoch ~state_root ~balance ~nonce ~public_key ~encrypted_balance ~decrypt_allowance =
  let account_hash =
    hash_fields ~chain_id ~address ~head_epoch ~state_root ~balance ~nonce ~public_key ~encrypted_balance ~decrypt_allowance in
  {
    chain_id;
    address;
    head_epoch;
    state_root;
    balance;
    nonce;
    public_key;
    encrypted_balance;
    decrypt_allowance;
    account_hash;
  }

let verify t =
  sane t
  && t.account_hash =
     hash_fields
       ~chain_id:t.chain_id
       ~address:t.address
       ~head_epoch:t.head_epoch
       ~state_root:t.state_root
       ~balance:t.balance
       ~nonce:t.nonce
       ~public_key:t.public_key
       ~encrypted_balance:t.encrypted_balance
       ~decrypt_allowance:t.decrypt_allowance