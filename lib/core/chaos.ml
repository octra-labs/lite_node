(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let pause_ms_default = 2000

let read_pause_ms () =
  match Sys.getenv_opt "OCTRA_CHAOS_PAUSE_MS" with
  | Some s -> (try int_of_string s with _ -> pause_ms_default)
  | None -> pause_ms_default

let pause_at_phase phase =
  match Sys.getenv_opt "OCTRA_CHAOS_PAUSE_AT" with
  | Some target when target = phase ->
    let pause_ms = read_pause_ms () in
    Octra_log.warn "chaos" "event = pause phase = %s duration_ms = %d"
      phase pause_ms;
    Unix.sleepf (float_of_int pause_ms /. 1000.0);
    Octra_log.warn "chaos" "event = resume phase = %s" phase
  | _ -> ()

let kill_at_phase phase =
  match Sys.getenv_opt "OCTRA_CHAOS_KILL_AT" with
  | Some target when target = phase ->
    Octra_log.fatal "chaos" "event = kill phase = %s exit_code = 137" phase;
    exit 137
  | _ -> ()

exception Chaos_injected_failure of string

let fail_at_phase phase =
  match Sys.getenv_opt "OCTRA_CHAOS_FAIL_AT" with
  | Some target when target = phase ->
    Octra_log.error "chaos" "event = inject_failure phase = %s" phase;
    raise (Chaos_injected_failure phase)
  | _ -> ()

let inject phase =
  pause_at_phase phase;
  kill_at_phase phase;
  fail_at_phase phase