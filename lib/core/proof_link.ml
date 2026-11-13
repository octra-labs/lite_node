(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module F = Proof_frame
module P = Pvac_verify_protocol

type control = Continue | Cancel
type t = {pid : int; input : Unix.file_descr; output : Unix.file_descr;
  data : Unix.file_descr; mutable live : bool}
type result = Checked of P.response * int * int | Retry | Cancelled | Timed_out
type readiness = Waiting of F.reader | Prepared | Unprepared

let reserve_mb = 512

let retire link =
  if link.live then begin
    link.live <- false;
    (try F.write link.input (F.command_bytes F.Stop) with _ -> ());
    F.close link.input;
    F.close link.data;
    let deadline = F.now () +. 2. in
    let rec wait () =
      try match Unix.waitpid [Unix.WNOHANG] link.pid with
        | 0, _ when F.now () < deadline -> Thread.delay 0.01; wait ()
        | 0, _ ->
          F.signal (-link.pid);
          F.signal link.pid;
          ignore (F.reap link.pid)
        | _ -> ()
      with Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
    Fun.protect ~finally:(fun () -> F.close link.output) wait
  end

let spawn program =
  let descriptors = ref [] in
  let pipe () =
    let read, write = Unix.pipe ~cloexec:true () in
    descriptors := read :: write :: !descriptors;
    read, write in
  try
    let input, write = pipe () in
    let read, output = pipe () in
    let data, feed = pipe () in
    let pid = Unix.create_process_env program [|program; "--proof-session"|]
      (Unix.environment ()) input output data in
    List.iter F.close [input; output; data];
    descriptors := [write; read; feed];
    let link = {pid; input = write; output = read; data = feed; live = true} in
    (try List.iter Unix.set_nonblock [write; read; feed] with error -> retire link; raise error);
    link
  with error -> List.iter (fun fd -> try F.close fd with _ -> ()) !descriptors; raise error

let ready_step ~stop ~deadline link parser =
  try
    if Atomic.get stop || F.now () >= deadline || F.rss link.pid > reserve_mb then Unprepared
    else
      let readable, _, _ = Unix.select [link.output] [] [] 0.05 in
      if readable = [] then Waiting parser else
      let parser, frame = F.receive link.output parser in
      match frame with Some F.Ready -> Prepared | Some _ -> Unprepared | None -> Waiting parser
  with _ -> Unprepared

let ready ~stop link =
  let deadline = F.now () +. 120. in
  let rec loop parser = match ready_step ~stop ~deadline link parser with
    | Waiting parser -> loop parser
    | Prepared -> true
    | Unprepared -> false in
  loop (F.reader ())

let run ~stop ~control ~deadline ~memory ~id link raw =
  let expected = P.hash_bytes raw in
  let rec loop parser sent started abort =
    let cancelled = Atomic.get stop || Atomic.get control = Cancel in
    let expired = F.now () >= deadline in
    let abort = match abort with
      | Some _ -> abort
      | None when cancelled || expired ->
        F.write link.input (F.command_bytes (F.Abort id));
        Some (F.now (), if cancelled then Cancelled else Timed_out)
      | None -> None in
    match abort with
    | Some (time, result) when F.now () -. time >= 2. -> retire link; result
    | _ ->
      let writes = if abort = None && sent < String.length raw then [link.data] else [] in
      let readable, writable, _ = Unix.select [link.output] writes [] 0.05 in
      let sent = if writable <> [] then
        sent + F.write_part link.data raw sent (min 65536 (String.length raw - sent))
        else sent in
      if readable = [] then loop parser sent started abort else
      let parser, frame = F.receive link.output parser in
      match frame with
      | None -> loop parser sent started abort
      | Some (F.Started (found, _)) when found = id && not started ->
        loop parser sent true abort
      | Some (F.Answer (found, sums, rejected, bytes)) when found = id && started && sent = String.length raw ->
        (match abort with
        | Some (_, result) -> retire link; result
        | None -> match P.response_of_string bytes with
          | Ok response when response.request_hash = expected
              && (not response.accepted || rejected = 0) -> Checked (response, sums, rejected)
          | _ -> retire link; Retry)
      | Some (F.Cancelled (found, reuse)) when found = id ->
        if not reuse then retire link;
        (match abort with Some (_, result) -> result | None -> retire link; Retry)
      | _ -> retire link; Retry in
  try
    F.write link.input (F.command_bytes (F.Check (id, String.length raw, memory)));
    loop (F.reader ()) 0 false None
  with _ ->
    retire link;
    if Atomic.get stop || Atomic.get control = Cancel then Cancelled
    else if F.now () >= deadline then Timed_out else Retry