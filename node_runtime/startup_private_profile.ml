(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let sample values =
  let rec take n = function
    | _ when n = 0 -> []
    | [] -> []
    | value :: rest -> value :: take (n - 1) rest
  in
  take 3 values |> String.concat ","

let run ~enabled ~store ~exit_fatal =
  if enabled then begin
    let report =
      Lwt_main.run (Octra_core.Store_irmin.inspect_pvac_blobs store)
    in
    match report.missing, report.corrupt with
    | [], [] ->
      Log.info
        "init"
        "event = private_profile_ready bound_keys = %d"
        report.bound
    | missing, corrupt ->
      Log.fatal
        "init"
        "event = private_profile_refused missing = %d corrupt = %d sample = %s"
        (List.length missing)
        (List.length corrupt)
        (sample (missing @ corrupt));
      exit_fatal ()
  end