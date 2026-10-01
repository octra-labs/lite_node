(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let excluded cfg ~start ~source ~active state =
  List.iter (fun address ->
    match Set_fold.exclusion cfg ~start ~source ~address state with
    | None -> ()
    | Some Set_fold.Member_missing ->
      Octra_log.warn "validator"
        "event = set_fold_refused source_epoch = %Ld address = %s reason = member_missing"
        source address
    | Some Set_fold.Pulse_missing ->
      Octra_log.warn "validator"
        "event = set_fold_refused source_epoch = %Ld address = %s reason = pulse_missing"
        source address
    | Some (Set_fold.Pulse_future last) ->
      Octra_log.warn "validator"
        "event = set_fold_refused source_epoch = %Ld address = %s reason = pulse_future pulse = %Ld"
        source address last
    | Some (Set_fold.Pulse_old last) ->
      Octra_log.warn "validator"
        "event = set_fold_refused source_epoch = %Ld address = %s reason = pulse_old pulse = %Ld maximum_gap = %Ld"
        source address last cfg.Set_fold.pulse_gap
    | Some (Set_fold.Pulse_short { first; last }) ->
      Octra_log.warn "validator"
        "event = set_fold_refused source_epoch = %Ld address = %s reason = pulse_short first = %Ld last = %Ld required_span = %Ld"
        source address first last cfg.Set_fold.rejoin_span
    | Some (Set_fold.Marks_short { low; high; signed; required }) ->
      Octra_log.warn "validator"
        "event = set_fold_refused source_epoch = %Ld address = %s reason = marks_short first = %Ld last = %Ld signed = %d required = %d"
        source address low high signed required) active