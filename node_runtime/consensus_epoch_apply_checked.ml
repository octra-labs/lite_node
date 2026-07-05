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


module Admission = Consensus_epoch_apply_admission
module Catchup = Consensus_catchup_shell

type deps = {
  head : unit -> int;
  set_current_epoch : int -> unit;
  catchup_active : unit -> bool;
  queue_gap : active:bool -> target_epoch:int64 -> reason:string -> Catchup.queue_event;
  clear_state_attested : unit -> unit;
  log_already : current_epoch:int -> head:int -> unit;
  log_defer : Admission.catchup -> Catchup.queue_event -> unit;
  apply : unit -> unit Lwt.t;
}

let run deps ~consensus_mode ~current_epoch =
  match Admission.plan ~consensus_mode ~head:(deps.head ()) ~current_epoch with
  | Admission.Already_applied applied ->
    deps.log_already ~current_epoch ~head:applied.head;
    deps.set_current_epoch applied.next_epoch;
    Lwt.return_unit
  | Admission.Need_catchup catchup ->
    let event =
      deps.queue_gap
        ~active:(deps.catchup_active ())
        ~target_epoch:catchup.target_epoch
        ~reason:catchup.queue_reason
    in
    deps.clear_state_attested ();
    deps.log_defer catchup event;
    Lwt.return_unit
  | Admission.Apply_now ->
    deps.apply ()

let run_node ~consensus_mode ~current_epoch ~head ~catchup_queue
    ~catchup_in_progress ~clear_state_attested ~apply =
  run
    {
      head;
      set_current_epoch = (fun epoch -> current_epoch := epoch);
      catchup_active = (fun () -> !catchup_in_progress);
      queue_gap = (fun ~active ~target_epoch ~reason ->
        Catchup.queue_gap_event catchup_queue ~active ~target_epoch ~reason);
      clear_state_attested;
      log_already = (fun ~current_epoch ~head ->
        Log.warn "finality"
          "skip already applied finalized height = %d head = %d"
          current_epoch
          head);
      log_defer = (fun catchup event ->
        Log.warn "finality"
          "defer apply: %s queued = %s active = %b"
          catchup.Admission.reason
          event.Catchup.queued_label
          event.queued_active);
      apply;
    }
    ~consensus_mode
    ~current_epoch:!current_epoch