(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Tx_staging

let sweep ~now ~confirmed_nonce ~save ~notify () =
  let expired = S.expire_old ~now ~confirmed_nonce () in
  if expired <> [] then begin
    save expired;
    S.cleanup_dropped ();
    Preverify_cache.retain (fun hash -> Option.is_some (S.find_by_hash hash));
    notify ()
  end;
  List.length expired

let run ?(sleep = Lwt_unix.sleep) ?(now = Unix.gettimeofday)
    ~confirmed_nonce ~save ~notify () =
  let open Lwt.Syntax in
  let rec loop () =
    let* () = sleep 30. in
    let* () = Lwt.catch (fun () ->
      ignore (sweep ~now:(now ()) ~confirmed_nonce ~save ~notify ());
      Lwt.return_unit) (function
        | Lwt.Canceled -> Lwt.fail Lwt.Canceled
        | error ->
          Octra_log.error "staging" "event = expiry_failed reason = %s" (Printexc.to_string error);
          Lwt.return_unit) in
    loop () in
  loop ()