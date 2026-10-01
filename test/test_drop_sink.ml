(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Drop_sink
module D = Octra_core.Drop_record

let need value reason = if not value then failwith reason

let rec nested depth finish =
  if depth = 0 then finish () else
    let ready, wake = Lwt.wait () in
    let result = Lwt.bind ready (fun () -> nested (depth - 1) finish) in
    Lwt.wakeup wake ();
    result

let exit_process code =
  let pid = Unix.create_process Sys.executable_name
    [|Sys.executable_name; "--exit-child"; string_of_int code|]
    Unix.stdin Unix.stdout Unix.stderr in
  let clock = Mtime_clock.counter () in
  let rec wait () = match Unix.waitpid [Unix.WNOHANG] pid with
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
    | 0, _ ->
      if Mtime.Span.to_float_ns (Mtime_clock.count clock) > 3e9 then begin
        Unix.kill pid Sys.sigkill;
        ignore (Unix.waitpid [] pid);
        failwith "ordinary exit waited for local drop writer"
      end;
      Unix.sleepf 0.01;
      wait ()
    | _, Unix.WEXITED actual when actual = code -> ()
    | _ -> failwith "ordinary exit code differs" in
  wait ()

let row n = D.{
  hash = Printf.sprintf "%064x" n; from_addr = "from"; to_addr = "to";
  nonce = n; ou = Z.one; op_type = Octra_core.Transaction.Standard;
  reason = "expired"; detail = "test"; dropped_at = float_of_int n;
}

let run () =
  let open Lwt.Syntax in
  let active = ref 0 and peak = ref 0 and saved = ref [] in
  let hold, release = Lwt.wait () in
  let sink = S.create ~now:(fun () -> 1.) ~write:(fun rows ->
    incr active;
    peak := max !peak !active;
    let* () = Lwt.protected hold in
    saved := !saved @ rows;
    decr active;
    Lwt.return_ok ()) in
  need (S.submit sink [row 1] = Ok ()) "initial queue failed";
  let* () = Lwt_unix.sleep 0.03 in
  need (!active = 1) "writer did not start";
  need (S.submit sink [row 2; row 2; row 3] = Ok ()) "queue blocked during write";
  let stop = S.shutdown sink in
  need (Lwt.is_sleeping stop) "shutdown passed active write";
  need (Result.is_error (S.submit sink [row 4])) "closing accepted input";
  Lwt.cancel stop;
  Lwt.wakeup_later release ();
  let* () = S.shutdown sink in
  need (!peak = 1) "overlapping writers";
  need (List.map (fun r -> r.D.nonce) !saved = [1; 2; 3]) "lost or duplicate rows";
  let writes = ref 0 in
  let broken = S.create ~now:(fun () -> 1.) ~write:(fun _ ->
    incr writes; Lwt.return_error "disk failure") in
  ignore (S.submit broken [row 1]);
  let* () = S.shutdown broken in
  need (!writes = 1) "write failure was retried";
  need (Result.is_error (S.submit broken [row 2])) "failed writer accepted input";
  let saved = ref [] and now = ref 0. in
  let sink = S.create ~now:(fun () -> !now) ~write:(fun rows ->
    saved := rows @ !saved; Lwt.return_ok ()) in
  need (S.submit sink [row 1] = Ok ()) "queue setup";
  need (Result.is_error (S.submit sink (List.init 4097 (fun i -> row (i + 2)))))
    "oversized queue accepted";
  now := 31.;
  let* () = S.shutdown sink in
  need (!saved = []) "expired rows were saved";
  let closed = ref false in
  let hold, release = Lwt.wait () in
  let sink = S.create ~now:(fun () -> 1.)
    ~write:(fun _ -> Lwt.map (fun () -> Ok ()) (Lwt.protected hold)) in
  need (S.submit sink [row 1] = Ok ()) "shutdown queue setup";
  let* () = S.finish ~close:(fun () -> closed := true) sink in
  need (not !closed) "shutdown closed active database";
  need (Result.is_error (S.submit sink [row 2])) "shutdown accepted new writes";
  Lwt.wakeup_later release ();
  let* () = S.finish ~close:(fun () -> closed := true) sink in
  need !closed "drained database was not closed";
  Lwt.return_unit

let () =
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "--exit-child" then begin
    let sink = S.create ~now:(fun () -> 1.) ~write:(fun _ -> Lwt.return_ok ()) in
    ignore (Sys.opaque_identity sink);
    let code = int_of_string Sys.argv.(2) in
    Lwt_main.run (nested 3 (fun () ->
      if code = 137 then begin
        Unix.putenv "OCTRA_CHAOS_KILL_AT" "drop_exit";
        Octra_core.Chaos.kill_at_phase "drop_exit"
      end;
      exit code))
  end else begin
    List.iter exit_process [0; 75; 137];
    Lwt_main.run (run ());
    print_endline "event = drop_sink status = passed"
  end