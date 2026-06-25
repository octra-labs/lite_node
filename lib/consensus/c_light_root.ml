(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type t = {
  chain_id : string;
  validator_addr : string;
  head_epoch : int64;
  state_root : string;
  ledger_state_root : string option;
  epoch_index_root : string option;
  txid_hi : int64;
  config_hash : string;
  signature : string;
}

let hash_ok s =
  String.length s = 32

let opt_hash_ok = function
  | None -> true
  | Some s -> hash_ok s

let text_root_ok s =
  String.length s > 0 && String.length s <= 256

let opt_text_root_ok = function
  | None -> true
  | Some s -> text_root_ok s

let sane t =
  t.chain_id <> ""
  && t.validator_addr <> ""
  && Int64.compare t.head_epoch 0L >= 0
  && Int64.compare t.txid_hi (-1L) >= 0
  && hash_ok t.state_root
  && hash_ok t.config_hash
  && opt_text_root_ok t.ledger_state_root
  && opt_hash_ok t.epoch_index_root

let sign_bytes t =
  Octra_net.Hash_domain.hash_encoded "octra:light_root:v1" (fun buf ->
    Octra_net.Oce1.put_string buf t.chain_id;
    Octra_net.Oce1.put_string buf t.validator_addr;
    Octra_net.Oce1.put_u64 buf t.head_epoch;
    Octra_net.Oce1.put_hash32 buf t.state_root;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_string buf t.ledger_state_root;
    Octra_net.Oce1.put_option Octra_net.Oce1.put_hash32 buf t.epoch_index_root;
    Octra_net.Oce1.put_u64 buf t.txid_hi;
    Octra_net.Oce1.put_hash32 buf t.config_hash)

let sign ~sign_fn t =
  if not (sane t) then Error "invalid_root_attestation"
  else Ok { t with signature = sign_fn (sign_bytes t) }

let verify ~validator_set ~chain_id ~config_hash t =
  sane t
  && t.chain_id = chain_id
  && t.config_hash = config_hash
  && match C_types.pubkey_of_addr validator_set t.validator_addr with
     | None -> false
     | Some pubkey_raw ->
       C_hash.verify_ed25519
         ~pubkey_raw
         ~msg:(sign_bytes t)
         ~signature:t.signature