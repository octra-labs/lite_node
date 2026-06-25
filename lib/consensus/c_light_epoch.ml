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
  epoch_id : int64;
  prev_state_root : string;
  state_root : string;
  tx_list_hash : string;
  tx_hashes : string list;
  start_txid : int64;
  tx_count : int;
  proposer : string;
  commit_round : int;
}

type tx_inclusion = {
  epoch : t;
  tx_hash : string;
  index : int;
}

let is_hex_char = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let hex64 s =
  String.length s = 64 && String.for_all is_hex_char s

let raw_to_hex s =
  String.concat "" (List.init (String.length s) (fun i ->
    Printf.sprintf "%02x" (Char.code s.[i])))

let tx_list_hash_hex tx_hashes =
  tx_hashes
  |> C_engine.tx_list_hash_for_header
  |> raw_to_hex

let sane t =
  t.chain_id <> ""
  && Int64.compare t.epoch_id 0L >= 0
  && Int64.compare t.start_txid 0L >= 0
  && t.tx_count >= 0
  && List.length t.tx_hashes = t.tx_count
  && hex64 t.prev_state_root
  && hex64 t.state_root
  && hex64 t.tx_list_hash
  && List.for_all hex64 t.tx_hashes
  && t.commit_round >= 0

let verify t =
  sane t && String.lowercase_ascii t.tx_list_hash = tx_list_hash_hex t.tx_hashes

let verify_chain_step ~prev t =
  verify prev
  && verify t
  && Int64.succ prev.epoch_id = t.epoch_id
  && String.lowercase_ascii t.prev_state_root = String.lowercase_ascii prev.state_root

let verify_tx_inclusion proof =
  verify proof.epoch
  && proof.index >= 0
  && proof.index < proof.epoch.tx_count
  && List.nth proof.epoch.tx_hashes proof.index = String.lowercase_ascii proof.tx_hash