(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module String_map = Map.Make (String)
module Int64_set = Set.Make (Int64)

type admission =
  | Accepted
  | Busy
  | Stopped

type action =
  | Pulse
  | Appeal of Octra_core.Set_fold.proof

type sample = {
  epoch : int64;
  active : bool;
  bonded : bool;
}

type stats = {
  queued : int;
  appeals : int;
  generation : int64;
  sent : int64;
}

type deps = {
  sample : unit -> sample;
  peers : unit -> int;
  send : action -> (unit, string) result Lwt.t;
  warn : string -> unit;
}

type appeal = {
  proof : Octra_core.Set_fold.proof;
  ready : int64;
  expires : int64;
}

type state = {
  appeals : appeal String_map.t;
  last_pulse : int64 option;
  sent : int64;
}

type notice = {
  epoch : int64;
  proof : Octra_core.Set_fold.proof option;
}

type message =
  | Notice of notice
  | Read_stats
  | Stop

type reply =
  | Stats of stats
  | Stopped_reply

type command = {
  generation : int64;
  correlation : int64;
  accepted_at : float;
  message : message;
  response : (reply Lwt.t * reply Lwt.u) option;
}

type t = {
  deps : deps;
  mutable state : state;
  stream : command Queue.t;
  control : command Queue.t;
  ready : unit Lwt_condition.t;
  mutable open_ : bool;
  mutable generation : int64;
  mutable next_correlation : int64;
  mutable pending : Int64_set.t;
}

let stream_capacity = 16
let control_capacity = 2
let appeal_capacity = 32
let lifetime = 120.0

let empty = {
  appeals = String_map.empty;
  last_pulse = None;
  sent = 0L;
}

let proof_key (proof : Octra_core.Set_fold.proof) =
  let vote = proof.vote in
  let raw =
    Octra_consensus.C_hash.vote_sign_bytes vote
    ^ vote.Octra_consensus.C_types.signature
  in
  Digestif.SHA256.digest_string raw |> Digestif.SHA256.to_hex

let prune epoch appeals =
  String_map.filter
    (fun _ appeal -> Int64.compare epoch appeal.expires <= 0)
    appeals

let trim appeals =
  if String_map.cardinal appeals <= appeal_capacity then appeals
  else
    let ordered =
      String_map.bindings appeals
      |> List.sort (fun (left_key, (left : appeal))
                        (right_key, (right : appeal)) ->
        let by_expiry = Int64.compare left.expires right.expires in
        if by_expiry <> 0 then by_expiry
        else String.compare left_key right_key)
    in
    match List.rev ordered with
    | [] -> appeals
    | (key, _) :: _ -> String_map.remove key appeals

let add_proof epoch proof appeals =
  let vote = proof.Octra_core.Set_fold.vote in
  let ready = Int64.add vote.Octra_consensus.C_types.epoch_id 2L in
  let expires =
    Int64.add
      vote.epoch_id
      Octra_core.Set_fold.standard.challenge
  in
  if Int64.compare epoch expires > 0 then appeals
  else
    String_map.add (proof_key proof) { proof; ready; expires } appeals
    |> trim

let ingest notice state =
  let appeals = prune notice.epoch state.appeals in
  let appeals =
    match notice.proof with
    | None -> appeals
    | Some proof -> add_proof notice.epoch proof appeals
  in
  { state with appeals }

let ready_appeal epoch appeals =
  String_map.bindings appeals
  |> List.find_opt (fun (_, (appeal : appeal)) ->
    Int64.compare epoch appeal.ready >= 0
    && Int64.compare epoch appeal.expires <= 0)

let pulse_due epoch = function
  | None -> true
  | Some prior ->
    Int64.compare
      (Int64.sub epoch prior)
      Octra_core.Set_fold.standard.pulse_gap
    >= 0

let decide (sample : sample) state =
  if not sample.bonded then None
  else
    match ready_appeal sample.epoch state.appeals with
    | Some (key, appeal) -> Some (key, Appeal appeal.proof)
    | None when not sample.active && pulse_due sample.epoch state.last_pulse ->
      Some ("", Pulse)
    | None -> None

let settle epoch key action state =
  let state = { state with sent = Int64.succ state.sent } in
  match action with
  | Pulse -> { state with last_pulse = Some epoch }
  | Appeal _ -> { state with appeals = String_map.remove key state.appeals }

let actor_stats t = {
  queued = Queue.length t.stream;
  appeals = String_map.cardinal t.state.appeals;
  generation = t.generation;
  sent = t.state.sent;
}

let resolve command reply =
  match command.response with
  | None -> ()
  | Some (promise, resolver) ->
    if Lwt.is_sleeping promise then Lwt.wakeup_later resolver reply

let rec drain queue =
  match Queue.take_opt queue with
  | None -> ()
  | Some command ->
    resolve command Stopped_reply;
    drain queue

let stop t =
  t.open_ <- false;
  t.generation <- Int64.succ t.generation;
  t.state <- empty;
  t.pending <- Int64_set.empty;
  drain t.control;
  drain t.stream;
  Lwt_condition.broadcast t.ready ()

let take t =
  match Queue.take_opt t.control with
  | Some _ as command -> command
  | None -> Queue.take_opt t.stream

let expired command =
  match command.message with
  | Notice _ -> Unix.gettimeofday () -. command.accepted_at > lifetime
  | Read_stats
  | Stop -> false

let handle_notice t notice =
  let open Lwt.Syntax in
  t.state <- ingest notice t.state;
  let sample = t.deps.sample () in
  match decide sample t.state with
  | None -> Lwt.return_unit
  | Some _ when t.deps.peers () <= 0 ->
    t.deps.warn "validator set fold transport has no peers";
    Lwt.return_unit
  | Some (key, action) ->
    let* result = t.deps.send action in
    begin
      match result with
      | Ok () ->
        t.state <- settle sample.epoch key action t.state;
        Lwt.return_unit
      | Error error ->
        t.deps.warn error;
        Lwt.return_unit
    end

let rec loop t =
  match take t with
  | None when not t.open_ -> Lwt.return_unit
  | None ->
    let open Lwt.Syntax in
    let* () = Lwt_condition.wait t.ready in
    loop t
  | Some command when
      command.generation <> t.generation
      || not (Int64_set.mem command.correlation t.pending) ->
    resolve command Stopped_reply;
    loop t
  | Some command ->
    t.pending <- Int64_set.remove command.correlation t.pending;
    if expired command then loop t
    else
    begin
      match command.message with
      | Stop ->
        stop t;
        resolve command Stopped_reply;
        Lwt.return_unit
      | Read_stats ->
        resolve command (Stats (actor_stats t));
        loop t
      | Notice notice ->
        let open Lwt.Syntax in
        let* () =
          Lwt.catch
            (fun () -> handle_notice t notice)
            (fun exn ->
              t.deps.warn (Printexc.to_string exn);
              Lwt.return_unit)
        in
        loop t
    end

let make_command t ?response message =
  let correlation = Int64.succ t.next_correlation in
  t.next_correlation <- correlation;
  t.pending <- Int64_set.add correlation t.pending;
  {
    generation = t.generation;
    correlation;
    accepted_at = Unix.gettimeofday ();
    message;
    response;
  }

let notify t ~epoch event =
  if not t.open_ then Stopped
  else if Queue.length t.stream >= stream_capacity then Busy
  else
    let proof =
      Option.map
        (fun (vote, commit) -> Octra_core.Set_fold.{ vote; commit })
        event
    in
    Queue.push (make_command t (Notice { epoch; proof })) t.stream;
    Lwt_condition.signal t.ready ();
    Accepted

let wake t ~head =
  notify t ~epoch:(Int64.succ (Int64.of_int head)) None

let create deps =
  let t = {
    deps;
    state = empty;
    stream = Queue.create ();
    control = Queue.create ();
    ready = Lwt_condition.create ();
    open_ = true;
    generation = 0L;
    next_correlation = 0L;
    pending = Int64_set.empty;
  } in
  Lwt.async (fun () -> loop t);
  t

let control t message =
  if not t.open_ || Queue.length t.control >= control_capacity then None
  else
    let response = Lwt.wait () in
    Queue.push (make_command t ~response message) t.control;
    Lwt_condition.signal t.ready ();
    Some (fst response)

let stats t =
  match control t Read_stats with
  | None -> Lwt.return (actor_stats t)
  | Some response ->
    let open Lwt.Syntax in
    let* response = response in
    begin
      match response with
      | Stats stats -> Lwt.return stats
      | Stopped_reply -> Lwt.return (actor_stats t)
    end

let shutdown t =
  match control t Stop with
  | None -> Lwt.return_unit
  | Some response ->
    let open Lwt.Syntax in
    let* _ = response in
    Lwt.return_unit