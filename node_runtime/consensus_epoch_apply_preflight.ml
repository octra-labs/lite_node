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


module Env = Consensus_epoch_apply_env
module Guard = Consensus_epoch_apply_guard

type effects = {
  begin_store_batch : unit -> unit Lwt.t;
  begin_chaindata_batch : unit -> unit;
  ledger_hash : unit -> string Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  expected_prev_root : int -> string option;
  fatal : string -> unit;
  exit : unit -> unit;
}

type request = {
  epoch_id : int;
}

type result = {
  pre_state : Env.pre_state;
}

let run effects request =
  let open Lwt.Syntax in
  let* () = effects.begin_store_batch () in
  effects.begin_chaindata_batch ();
  let* pre_state =
    Env.read_pre_state
      {
        ledger_hash = effects.ledger_hash;
        cached_head = effects.cached_head;
      }
  in
  ignore
    (Guard.run_prev_root_check
       {
         fatal = effects.fatal;
         exit = effects.exit;
       }
       {
         epoch_id = request.epoch_id;
         local_pre_root = pre_state.consensus_root;
         expected_prev = effects.expected_prev_root request.epoch_id;
       });
  Lwt.return { pre_state }