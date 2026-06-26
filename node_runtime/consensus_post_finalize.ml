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


module Log = Octra_log

type deps = {
  deactivate_gap : unit -> unit;
  set_consensus_finalized : bool -> unit;
  current_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  remove_pending_finalized : epoch:int -> unit;
  fatal_exit : unit -> unit;
}

let zero_root = String.make 32 '\x00'

let short_hex8 s =
  String.concat ""
    (List.init (min 8 (String.length s)) (fun i ->
      Printf.sprintf "%02x" (Char.code s.[i])))

let rec wait_local_apply deps ~target_epoch ~tries =
  if deps.current_epoch () > target_epoch then Lwt.return_unit
  else if tries <= 0 then begin
    Log.fatal "consensus"
      "local apply did not complete finalized_epoch = %d current_epoch = %d action = exit"
      target_epoch (deps.current_epoch ());
    Log.fatal "consensus"
      "bft preview race guard action = exit reason = unapplied_state";
    deps.fatal_exit ();
    Lwt.return_unit
  end else
    let open Lwt.Syntax in
    let* () = deps.sleep 0.05 in
    wait_local_apply deps ~target_epoch ~tries:(tries - 1)

let rec wait_commit deps ~pre_root ~tries =
  if tries <= 0 then Lwt.return_unit
  else
    let open Lwt.Syntax in
    let* () = deps.sleep 0.05 in
    let* cur_root = deps.read_commit_root () in
    if cur_root <> pre_root then Lwt.return_unit
    else wait_commit deps ~pre_root ~tries:(tries - 1)

let verify_root ~epoch_id ~proposed_root ~actual_root =
  if proposed_root <> zero_root && actual_root <> proposed_root then
    Log.trace "consensus"
      "post-finalize root diff proposed = %s actual = %s epoch = %Ld"
      (short_hex8 proposed_root)
      (short_hex8 actual_root)
      epoch_id
  else if proposed_root <> zero_root then
    Log.info "consensus"
      "post-finalize root verified epoch = %Ld"
      epoch_id

let run deps ~epoch_id ~proposed_root =
  let open Lwt.Syntax in
  deps.deactivate_gap ();
  let pre_root = deps.read_pre_finalize_root () in
  deps.set_consensus_finalized true;
  let target_epoch = Int64.to_int epoch_id in
  let* () = wait_local_apply deps ~target_epoch ~tries:200 in
  let* () = wait_commit deps ~pre_root ~tries:60 in
  let* actual_root = deps.read_local_root_raw () in
  verify_root ~epoch_id ~proposed_root ~actual_root;
  deps.remove_pending_finalized ~epoch:target_epoch;
  Lwt.return_unit