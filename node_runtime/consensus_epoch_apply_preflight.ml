(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Env = Consensus_epoch_apply_env
module Guard = Consensus_epoch_apply_guard
module Head = Octra_core.Head_manifest

type effects = {
  begin_store_batch : unit -> unit Lwt.t;
  begin_chaindata_batch : unit -> unit;
  ledger_hash : unit -> string Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  expected_prev_root : int -> string option;
  fatal : string -> unit;
  require_sync : Sync_need.t -> unit;
  exit : unit -> unit;
}

type request = {
  epoch_id : int;
}

type result = {
  pre_state : Env.pre_state;
}

type fault = {
  head_epoch : int;
  head_root : string;
  live_root : string;
}

let check_head ~epoch ~root = function
  | Some head when head.Head.epoch_id = epoch - 1 ->
    let head_root = Head.ledger_state_root head in
    if String.equal head_root root then Ok ()
    else
      Error {
        head_epoch = head.epoch_id;
        head_root;
        live_root = root;
      }
  | _ -> Ok ()

let short value =
  String.sub value 0 (min 16 (String.length value))

let stop effects epoch fault =
  effects.fatal
    (Printf.sprintf
       "event = epoch_preflight action = refuse reason = head_root_mismatch epoch = %d head_epoch = %d head_root = %s live_root = %s"
       epoch
       fault.head_epoch
       (short fault.head_root)
       (short fault.live_root));
  effects.require_sync (Sync_need.root ~epoch ~head:fault.head_epoch);
  effects.exit ();
  Lwt.fail_with "epoch preflight head root mismatch"

let run effects request =
  let open Lwt.Syntax in
  let* ledger_root = effects.ledger_hash () in
  let head = effects.cached_head () in
  let consensus_root =
    match head with
    | Some head -> head.Head.state_root
    | None -> ledger_root
  in
  let pre_state = Env.{ ledger_root; consensus_root } in
  match check_head ~epoch:request.epoch_id ~root:ledger_root head with
  | Error fault -> stop effects request.epoch_id fault
  | Ok () ->
    let prev =
      Guard.run_prev_root_check
        {
          fatal = effects.fatal;
          require_sync = effects.require_sync;
          exit = effects.exit;
        }
        {
          epoch_id = request.epoch_id;
          local_pre_root = pre_state.consensus_root;
          expected_prev = effects.expected_prev_root request.epoch_id;
        }
    in
    match prev with
    | Guard.Prev_root_exited -> Lwt.fail_with "epoch preflight root mismatch"
    | Guard.Prev_root_checked ->
      let* () = effects.begin_store_batch () in
      effects.begin_chaindata_batch ();
      Lwt.return { pre_state }