(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Protocol = Octra_consensus.Resource_compute_protocol

type terms = private {
  chain_id : string;
  payer : string;
  public_key : string;
  target : string;
  call_hash : string;
  nonce : int64;
  amount : int64;
  expires_epoch : int64;
}

type t

val terms : t -> terms

val prepare : call:Protocol.call -> target:string -> nonce:int64 -> (terms, string) result
val prepare_header : header:Protocol.header -> target:string -> nonce:int64 -> (terms, string) result
val prepare_until : expires_epoch:int64 -> call:Protocol.call -> target:string -> nonce:int64 -> (terms, string) result
val prepare_header_until : expires_epoch:int64 -> header:Protocol.header -> target:string -> nonce:int64 -> (terms, string) result
val sign_bytes : terms -> string
val authorize : terms -> signature:string -> (t, string) result
val encode : t -> string
val decode : string -> (t, string) result
val verify :
  chain_id:string -> epoch:int64 -> nonce:int64 -> target:string ->
  call:Protocol.call -> t -> (int64, string) result
val verify_header :
  ?extended:bool -> chain_id:string -> epoch:int64 -> nonce:int64 -> target:string ->
  header:Protocol.header -> t -> (int64, string) result

type deposit = private {
  request_id : string;
  permission : t;
  opened_epoch : int64;
  returned_epoch : int64 option;
  paid : (int64 * string) option;
}

val escrow : string
val reserve :
  ?extended:bool -> chain_id:string -> epoch:int64 -> nonce:int64 -> header:Protocol.header ->
  t -> (deposit, string) result
val return_deposit : chain_id:string -> epoch:int64 -> deposit -> (deposit, string) result
val encode_request : header:Protocol.header -> t -> string
val decode_request : string -> (Protocol.header * t, string) result
val encode_deposit : deposit -> string
val deposit_path : string -> string list
val verify_deposit :
  chain_id:string -> epoch:int64 -> header:Protocol.header ->
  string -> (deposit, string) result
val read :
  ?snapshot:Store_irmin.read_snapshot -> Store_irmin.t -> id:string ->
  (deposit option, string) result Lwt.t

type funded = private {
  header : Protocol.header;
  deposit : deposit;
  epoch : int64;
  state_root : string;
}

val observe :
  ?index_root:string -> chain_id:string -> epoch:int64 -> state_root:string ->
  header:Protocol.header -> Store_irmin.t -> (funded option, string) result Lwt.t
val hold :
  ledger:Ledger.t -> chain_id:string -> epoch:int64 -> fee:Z.t -> call:Protocol.call ->
  t -> (string * Z.t, string) result Lwt.t
val hold_header :
  ledger:Ledger.t -> chain_id:string -> epoch:int64 -> fee:Z.t -> header:Protocol.header ->
  t -> (string * Z.t, string) result Lwt.t
val refund :
  ledger:Ledger.t -> chain_id:string -> epoch:int64 -> id:string ->
  (Z.t, string) result Lwt.t

val settle :
  ledger:Ledger.t -> chain_id:string -> epoch:int64 -> Compute_bill.t ->
  (Z.t * Z.t, string) result Lwt.t

val payment :
  chain_id:string -> epoch:int64 -> Compute_bill.t -> deposit ->
  (deposit * (string * Z.t) list, string) result