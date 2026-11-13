(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module F = Proof_frame
module P = Pvac_verify_protocol

type phase = Idle of int | Running of int * bool | Closed
type message = Begin of int | Read of int | End of int | Cancel of int | Stop
type effect = Fork | Kill of bool | Replied | Quit

let delta state message = match state, message with
  | Idle last, Begin id when id > last -> Running (id, false), [Fork]
  | Running (id, false), Read found when id = found -> Running (id, true), []
  | Running (id, _), End found when id = found -> Idle id, [Replied]
  | Running (id, loaded), Cancel found when id = found ->
    (if loaded then Idle id else Closed), [Kill loaded]
  | Running _, Stop -> Closed, [Kill false; Quit]
  | Closed, _ -> Closed, []
  | Idle _, Stop -> Closed, [Quit]
  | Running _, _ -> Closed, [Kill false; Quit]
  | Idle _, _ -> Closed, [Quit]

let answer id result sums rejected =
  if rejected > 0 && not (Proof_ready.check ()) then failwith "proof result image";
  F.Answer (id, sums, rejected, P.response_bytes result)

let child ~parent ~input ~output ~id ~length =
  try
    F.prefer_oom ();
    Pvac_ffi.isolate_worker ();
    if Unix.getppid () <> parent then Unix._exit 2;
    F.close Unix.stdin;
    F.close Unix.stdout;
    let raw = F.read input length in
    F.close input;
    F.send output (F.Loaded id);
    let request = match P.request_of_string raw with
      | Ok request -> request
      | Error _ -> Unix._exit 2 in
    let result = Pvac_verify_direct.response ~keyed:false request in
    F.send output (answer id result (Proof_ready.sums ()) (Proof_ready.rejected ()));
    F.close output;
    Unix._exit 0
  with _ -> Unix._exit 2

let run_child input phase id length memory =
  let read, write = Unix.pipe ~cloexec:true () in
  let parent = Unix.getpid () in
  let pid = try Unix.fork () with error -> F.close read; F.close write; raise error in
  if pid = 0 then begin
    F.close read;
    child ~parent ~input ~output:write ~id ~length
  end;
  F.close write;
  Unix.set_nonblock read;
  let owned = ref true in
  let kill () = if !owned then begin F.signal pid; ignore (F.reap pid); owned := false end in
  Fun.protect ~finally:(fun () -> kill (); F.close read) (fun () ->
    F.send Unix.stdout (F.Started (id, pid));
    let commands = Bytes.create 32 in
    let started = F.now () in
    let rec loop phase parser used reply status ended =
      let status = match status with
        | Some _ -> status
        | None ->
          let found, result = Unix.waitpid [Unix.WNOHANG] pid in
          if found = 0 then None else begin owned := false; Some result end in
      if status <> None && (reply <> None || ended) then begin
        let next, actions = delta phase (End id) in
        if actions <> [Replied] then failwith "proof completion state";
        (match status, reply with
        | Some (Unix.WEXITED 0), Some (sums, rejected, raw) -> F.send Unix.stdout (F.Answer (id, sums, rejected, raw))
        | _ -> F.send Unix.stdout (F.Fault id));
        next
      end else if F.now () -. started >= 30.
          || (status = None && match F.total_rss parent pid with
             | Some total -> total > memory | None -> true) then begin
        kill ();
        F.send Unix.stdout (F.Fault id);
        Closed
      end else begin
        let readable, _, _ = Unix.select (if ended then [Unix.stdin] else [Unix.stdin; read]) [] [] 0.05 in
        if List.mem Unix.stdin readable then begin
          let count = Unix.read Unix.stdin commands used (32 - used) in
          if count = 0 then begin kill (); Closed end
          else if used + count < 32 then loop phase parser (used + count) reply status ended
          else match F.command (Bytes.unsafe_to_string commands) with
            | F.Abort found ->
              let next, effects = delta phase (Cancel found) in
              kill ();
              (match effects with
              | [Kill reuse] -> F.send Unix.stdout (F.Cancelled (id, reuse))
              | _ -> F.send Unix.stdout (F.Fault id));
              next
            | F.Stop -> kill (); Closed
            | F.Check _ -> kill (); Closed
        end else if List.mem read readable then
          try
            let parser, frame = F.receive read parser in
            match frame with
            | Some (F.Loaded found) when found = id ->
              let next, effects = delta phase (Read id) in
              if effects <> [] || next = Closed then failwith "proof read state";
              loop next parser used reply status ended
            | Some (F.Answer (found, sums, rejected, raw)) when found = id && reply = None ->
              loop phase parser used (Some (sums, rejected, raw)) status ended
            | None -> loop phase parser used reply status ended
            | _ -> failwith "proof child reply"
          with End_of_file ->
            loop phase parser used reply status true
        else loop phase parser used reply status ended
      end in
    loop phase (F.reader ()) 0 None None false)

let serve () =
  Pvac_ffi.isolate_worker ();
  ignore (Unix.setsid ());
  F.shed ();
  let input = Unix.dup ~cloexec:true Unix.stderr in
  let error = Unix.openfile "/dev/null" [Unix.O_WRONLY; Unix.O_CLOEXEC] 0 in
  Unix.dup2 error Unix.stderr;
  F.close error;
  Proof_ready.prepare ();
  F.send Unix.stdout F.Ready;
  let rec loop phase = match phase with
    | Closed -> ()
    | Running _ -> failwith "proof child ownership"
    | Idle _ ->
      match F.command (F.read Unix.stdin 32) with
      | F.Stop -> ()
      | F.Abort _ -> failwith "proof idle cancel"
      | F.Check (id, length, memory) ->
        let phase, effects = delta phase (Begin id) in
        if effects <> [Fork] then failwith "proof request order";
        loop (run_child input phase id length memory) in
  Fun.protect ~finally:(fun () -> F.close input)
    (fun () -> try loop (Idle 0) with End_of_file -> ())