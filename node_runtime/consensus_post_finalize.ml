(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Log = Octra_log

type deps = {
  deactivate_gap : unit -> unit;
  set_consensus_finalized : bool -> unit;
  committed_head_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  commit_finality_journal : unit -> unit;
  remove_pending_finalized : epoch:int -> unit;
  set_state_attested : head:int -> root:string -> unit;
  apply_timeout_seconds : float;
  fatal_exit : unit -> unit;
}

let zero_root = String.make 32 '\x00'

let short_hex8 s =
  String.concat ""
    (List.init (min 8 (String.length s)) (fun i ->
      Printf.sprintf "%02x" (Char.code s.[i])))

let rec wait_local_apply deps ~target_epoch ~remaining =
  if deps.committed_head_epoch () >= target_epoch then Lwt.return_unit
  else if remaining <= 0. then begin
    Log.warn "consensus"
      "event = finalized_apply_pending finalized_epoch = %d committed_head = %d action = wait"
      target_epoch (deps.committed_head_epoch ());
    wait_local_apply
      deps
      ~target_epoch
      ~remaining:deps.apply_timeout_seconds
  end else
    let open Lwt.Syntax in
    let delay = min 0.05 remaining in
    let* () = deps.sleep delay in
    wait_local_apply deps ~target_epoch ~remaining:(remaining -. delay)

let rec wait_commit deps ~pre_root ~tries =
  if tries <= 0 then Lwt.return_unit
  else
    let open Lwt.Syntax in
    let* () = deps.sleep 0.05 in
    let* cur_root = deps.read_commit_root () in
    if cur_root <> pre_root then Lwt.return_unit
    else wait_commit deps ~pre_root ~tries:(tries - 1)

let verify_root ~epoch_id ~proposed_root ~actual_root =
  if proposed_root = zero_root then true
  else if actual_root = proposed_root then begin
    Log.info "consensus"
      "event = post_finalize_root_verified epoch = %Ld"
      epoch_id;
    true
  end else begin
    Log.fatal "consensus"
      "event = post_finalize_root_mismatch proposed = %s actual = %s epoch = %Ld action = exit"
      (short_hex8 proposed_root)
      (short_hex8 actual_root)
      epoch_id;
    false
  end

let run deps ~epoch_id ~proposed_root =
  let open Lwt.Syntax in
  deps.deactivate_gap ();
  let pre_root = deps.read_pre_finalize_root () in
  deps.set_consensus_finalized true;
  let target_epoch = Int64.to_int epoch_id in
  let* () =
    wait_local_apply
      deps
      ~target_epoch
      ~remaining:deps.apply_timeout_seconds
  in
  let* () = wait_commit deps ~pre_root ~tries:60 in
  let* actual_root = deps.read_local_root_raw () in
  if verify_root ~epoch_id ~proposed_root ~actual_root then begin
    deps.commit_finality_journal ();
    deps.remove_pending_finalized ~epoch:target_epoch;
    if proposed_root <> zero_root then
      deps.set_state_attested ~head:target_epoch ~root:actual_root;
    Lwt.return_unit
  end else begin
    deps.fatal_exit ();
    Lwt.fail_with "post-finalize root mismatch"
  end