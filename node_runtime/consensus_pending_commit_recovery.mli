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


type decision =
  | Confirmed_elsewhere of {
      agreed : int;
      quorum : int;
    }
  | All_peers_missing of {
      responses : int;
    }
  | Inconclusive of {
      agreed : int;
      peers : int;
      responses : int;
      quorum : int;
    }

val raw32_of_hex : string -> string option

val decide :
  validator_count:int ->
  peer_quorum:int ->
  promised_root_raw:string option ->
  Octra_consensus.C_driver.epoch_root_response_record list ->
  decision

type deps = {
  read_pending_commits : unit -> Octra_core.Wal.pending_commit list;
  head_epoch : unit -> int;
  query_epoch_root :
    epoch_id:int64 ->
    Octra_consensus.C_driver.epoch_root_response_record list Lwt.t;
  run_catchup_to_target : target_epoch:int64 -> reason:string -> unit Lwt.t;
  delete_pending_commit : epoch_id:int -> round:int -> unit;
}

val run_once :
  deps ->
  validator_count:int ->
  peer_quorum:int ->
  unit Lwt.t