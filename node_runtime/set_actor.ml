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
  bonded : (bool, string) result;
}

type point = { epoch : int64; head : int64 option; finalized : bool }

type refusal = Moved | Uncommitted | Finalized

type fault = Control | Receipt | Transport | Send | Internal

type stage = Available | Attempt | Marked | Expired | Unread | Capacity | Overload | Closed

type observation = {
  stage : stage;
  epoch : int64;
  proof : Octra_core.Set_fold.proof;
}

let stage_name = function
  | Available -> "available"
  | Attempt -> "attempt"
  | Marked -> "marked"
  | Expired -> "expired_unobserved"
  | Unread -> "receipt_unavailable"
  | Capacity -> "capacity"
  | Overload -> "overload"
  | Closed -> "closed"

let event = function
  | Control -> "set_actor_control_failed"
  | Receipt -> "set_actor_read_failed"
  | Transport -> "set_actor_transport_failed"
  | Send -> "set_actor_send_failed"
  | Internal -> "set_actor_internal_failed"

type stats = {
  queued : int;
  appeals : int;
  generation : int64;
  sent : int64;
}

type deps = {
  sample : unit -> sample;
  read : epoch:int64 -> (Octra_core.Set_fold.receipt, string) result Lwt.t;
  peers : unit -> int;
  send : epoch:int64 -> action -> (unit, fault * string) result Lwt.t;
  warn : fault -> string -> unit;
}

type appeal = {
  proof : Octra_core.Set_fold.proof;
  ready : int64;
  expires : int64;
}

type state = {
  appeals : appeal String_map.t;
  last_send : int64 option;
  sent : int64;
  sample_error : string option;
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
  observe : observation -> unit;
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

let pulse_step =
  let half = Int64.div Octra_core.Set_fold.standard.pulse_gap 2L in
  if Int64.compare half 1L < 0 then 1L else half

let plan ~epoch (point : point) =
  if not (Int64.equal epoch point.epoch) then Error Moved
  else if epoch <= 0L then Error Uncommitted
  else match point.head with
    | Some head when head = Int64.pred epoch ->
      if point.finalized then Error Finalized else Ok head
    | _ -> Error Uncommitted

let reason = function
  | Moved -> "validator duty epoch changed"
  | Uncommitted -> "validator duty head is not committed"
  | Finalized -> "validator duty epoch is already finalized"

let empty = {
  appeals = String_map.empty;
  last_send = None;
  sent = 0L;
  sample_error = None;
}

let proof_key (proof : Octra_core.Set_fold.proof) =
  let vote = proof.vote in
  let raw =
    Octra_consensus.C_hash.vote_sign_bytes vote
    ^ vote.Octra_consensus.C_types.signature
  in
  Digestif.SHA256.digest_string raw |> Digestif.SHA256.to_hex

let observe t stage epoch proof =
  try t.observe { stage; epoch; proof } with exn ->
    try t.deps.warn Internal (Printexc.to_string exn) with _ -> ()

let removed epoch receipt before after =
  String_map.bindings before
  |> List.filter_map (fun (key, (appeal : appeal)) ->
    if String_map.mem key after then None
    else
      let stage =
        match receipt with
        | Some (_, marked) when List.mem appeal.proof.vote.epoch_id marked -> Marked
        | Some (read_epoch, _) when epoch > appeal.expires && read_epoch > appeal.expires -> Expired
        | _ when epoch > appeal.expires -> Unread
        | Some _ | None -> Capacity
      in
      Some { stage; epoch; proof = appeal.proof })

let record_changes t before epoch receipt next =
  let events = removed epoch receipt before next.appeals in
  t.state <- next;
  List.iter (fun entry -> observe t entry.stage entry.epoch entry.proof) events

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
    match ordered with
    | [] -> appeals
    | (key, _) :: _ -> String_map.remove key appeals

let appeal proof =
  let vote = proof.Octra_core.Set_fold.vote in
  let ready = Int64.add vote.Octra_consensus.C_types.epoch_id 2L in
  let expires =
    Int64.add
      vote.epoch_id
      Octra_core.Set_fold.standard.challenge
  in
  { proof; ready; expires }

let add_proof epoch proof appeals =
  let entry = appeal proof in
  if epoch > entry.expires then appeals
  else String_map.add (proof_key proof) entry appeals |> trim

let ingest epoch notice state =
  let appeals = prune epoch state.appeals in
  let appeals =
    match notice.proof with
    | None -> appeals
    | Some proof -> add_proof epoch proof appeals
  in
  { state with appeals }

let ready_appeal epoch appeals =
  String_map.bindings appeals
  |> List.filter (fun (_, (appeal : appeal)) ->
    Int64.compare epoch appeal.ready >= 0
    && Int64.compare epoch appeal.expires <= 0)
  |> List.sort (fun (left_key, left) (right_key, right) ->
    let order = Int64.compare left.expires right.expires in
    if order = 0 then String.compare left_key right_key else order)
  |> function [] -> None | first :: _ -> Some first

let pulse_due epoch = function
  | None -> true
  | Some prior ->
    Int64.compare
      (Int64.sub epoch prior)
      pulse_step
    >= 0

let acknowledge epoch (receipt : Octra_core.Set_fold.receipt) state =
  let appeals =
    prune epoch state.appeals
    |> String_map.filter (fun _ (appeal : appeal) ->
      not (List.mem appeal.proof.vote.epoch_id receipt.marked))
  in
  { state with appeals }

let decide (sample : sample) (receipt : Octra_core.Set_fold.receipt) state =
  if sample.bonded <> Ok true || state.last_send = Some sample.epoch then None
  else if not sample.active then
    if pulse_due sample.epoch receipt.pulse then Some Pulse else None
  else
    match ready_appeal sample.epoch state.appeals with
    | Some (_, appeal) -> Some (Appeal appeal.proof)
    | None -> None

let settle epoch state =
  { state with sent = Int64.succ state.sent; last_send = Some epoch }

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

let permit t (sample : sample) =
  let error = match sample.bonded with Error reason -> Some reason | Ok _ -> None in
  let changed = error <> t.state.sample_error in
  t.state <- { t.state with sample_error = error };
  if changed then Option.iter (t.deps.warn Control) error;
  sample.bonded = Ok true

let handle_notice t notice =
  let open Lwt.Syntax in
  let sample = t.deps.sample () in
  let before = match notice.proof with
    | None -> t.state.appeals
    | Some proof -> String_map.add (proof_key proof) (appeal proof) t.state.appeals in
  t.state <- ingest sample.epoch notice t.state;
  let allowed = permit t sample in
  if not allowed && String_map.is_empty before then Lwt.return_unit
  else
    let* receipt = Lwt.catch
      (fun () -> t.deps.read ~epoch:sample.epoch)
      (fun exn -> Lwt.return_error (Printexc.to_string exn)) in
    let current = t.deps.sample () in
    let allowed = permit t current in
    let settle_read marked next =
      record_changes t before current.epoch marked next in
    match receipt with
    | Error error ->
      settle_read None { t.state with appeals = prune current.epoch t.state.appeals };
      t.deps.warn Receipt error;
      Lwt.return_unit
    | Ok receipt ->
      settle_read (Some (sample.epoch, receipt.marked))
        (acknowledge current.epoch receipt t.state);
      if current.epoch <> sample.epoch || not allowed then Lwt.return_unit else
      match decide current receipt t.state with
      | None -> Lwt.return_unit
      | Some _ when t.deps.peers () <= 0 ->
        t.deps.warn Transport "validator set fold transport has no peers";
        Lwt.return_unit
      | Some action ->
        begin match action with
        | Pulse -> ()
        | Appeal proof -> observe t Attempt current.epoch proof
        end;
        let* result = t.deps.send ~epoch:current.epoch action in
        begin
          match result with
          | Ok () ->
            t.state <- settle current.epoch t.state;
            Lwt.return_unit
          | Error (fault, error) ->
            t.deps.warn fault error;
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
              t.deps.warn Internal (Printexc.to_string exn);
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
  let proof =
    Option.map
      (fun (vote, commit) -> Octra_core.Set_fold.{ vote; commit })
      event
  in
  Option.iter (observe t Available epoch) proof;
  let queued =
    Option.is_none proof
    && Queue.fold (fun found command ->
      found || match command.message with
      | Notice { proof = None; _ } -> not (expired command)
      | _ -> false) false t.stream
  in
  if not t.open_ then begin
    Option.iter (observe t Closed epoch) proof;
    Stopped
  end else if queued then
    Accepted
  else if Queue.length t.stream >= stream_capacity then begin
    Option.iter (observe t Overload epoch) proof;
    Busy
  end else begin
    Queue.push (make_command t (Notice { epoch; proof })) t.stream;
    Lwt_condition.signal t.ready ();
    Accepted
  end

let wake t ~head =
  notify t ~epoch:(Int64.succ (Int64.of_int head)) None

let create ?(observe = fun _ -> ()) deps =
  let t = {
    deps;
    observe;
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