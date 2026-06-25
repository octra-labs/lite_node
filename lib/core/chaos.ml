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


let pause_ms_default = 2000

let read_pause_ms () =
  match Sys.getenv_opt "OCTRA_CHAOS_PAUSE_MS" with
  | Some s -> (try int_of_string s with _ -> pause_ms_default)
  | None -> pause_ms_default

let pause_at_phase phase =
  match Sys.getenv_opt "OCTRA_CHAOS_PAUSE_AT" with
  | Some target when target = phase ->
    let pause_ms = read_pause_ms () in
    Octra_log.stdout "[CHAOS] paused at phase=%s for %dms\n%!" phase pause_ms;
    Unix.sleepf (float_of_int pause_ms /. 1000.0);
    Octra_log.stdout "[CHAOS] resumed at phase=%s\n%!" phase
  | _ -> ()

let kill_at_phase phase =
  match Sys.getenv_opt "OCTRA_CHAOS_KILL_AT" with
  | Some target when target = phase ->
    Octra_log.stdout "[CHAOS] kill at phase=%s — exit(137)\n%!" phase;
    exit 137
  | _ -> ()

exception Chaos_injected_failure of string

let fail_at_phase phase =
  match Sys.getenv_opt "OCTRA_CHAOS_FAIL_AT" with
  | Some target when target = phase ->
    Octra_log.stdout "[CHAOS] raising exception at phase=%s\n%!" phase;
    raise (Chaos_injected_failure phase)
  | _ -> ()

let inject phase =
  pause_at_phase phase;
  kill_at_phase phase;
  fail_at_phase phase