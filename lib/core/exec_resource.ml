(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type resource = Memory | Stack | Host | Time
type allowance = Operation of float | Completion of float

exception Unavailable of resource
exception Exhausted of string * resource
exception Window_expired of string

type slice = { limit : float option; clock : unit -> float; running : unit -> bool; mutable used : float }

let selection = Lwt.new_key ()
let active_slice = Lwt.new_key ()
let work_record = Lwt.new_key ()

type timer = {clock : unit -> float; started : float;
  mutable idle : float; mutable waiting : (int * float) option}
let timers = Lwt.new_key ()

let elapsed timer =
  let now = timer.clock () in
  let waiting = Option.fold ~none:0. ~some:(fun (_, started) -> max 0. (now -. started))
    timer.waiting in
  max 0. (now -. timer.started -. timer.idle -. waiting)

let queued action =
  let parents = Option.value (Lwt.get timers) ~default:[] in
  List.iter (fun timer -> timer.waiting <- Some (match timer.waiting with
    | None -> 1, timer.clock ()
    | Some (count, started) -> count + 1, started)) parents;
  let resumed = ref false in
  let resume () = if not !resumed then begin
    resumed := true;
    List.iter (fun timer -> match timer.waiting with
      | Some (1, started) ->
        timer.idle <- timer.idle +. max 0. (timer.clock () -. started);
        timer.waiting <- None
      | Some (count, started) -> timer.waiting <- Some (count - 1, started)
      | None -> ()) parents
  end in
  Lwt.finalize (fun () -> action resume) (fun () -> resume (); Lwt.return_unit)

type progress = Open | Partial | Complete
type preview = {accept : unit -> bool; replay : string -> bool; allow : string -> bool;
  defer : string -> resource -> unit; omit : (string -> unit) option; mutable progress : progress}
let preview_state = Lwt.new_key ()

let preview ?(replay = fun _ -> false) ?(allow = fun _ -> true) ?omit ~accept ~defer action =
  let state = {accept; replay; allow; defer; omit; progress = Open} in
  Lwt.with_value preview_state (Some state) (fun () ->
    Lwt.map (fun result -> result, state.progress <> Open) (action ()))

let next ?hash () = match Lwt.get preview_state with
  | None -> true
  | Some {progress = Complete; _} -> false
  | Some state ->
    let allowed = Option.fold ~none:false ~some:state.replay hash
      || (Option.fold ~none:true ~some:state.allow hash && state.accept ()) in
    if not allowed then state.progress <- Partial;
    allowed

let defer hash resource = match Lwt.get preview_state with
  | None | Some {progress = Complete; _} -> false
  | Some state -> state.defer hash resource; state.progress <- Partial; true

let omit hash = match Lwt.get preview_state with
  | Some {progress = Complete; _} -> false
  | Some ({omit = Some omit; _} as state) ->
    omit hash;
    state.progress <- if state.accept () then Partial else Complete;
    true
  | _ -> false

let seconds () = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1_000_000_000.

let scaling = Lwt.new_key ()

let select ?limit ?scale ?record ?(clock = seconds) ~running action =
  Option.iter (fun limit ->
    if not (Float.is_finite limit) || limit <= 0. then invalid_arg "selection duration invalid") limit;
  let scale = match scale with Some _ -> scale | None -> Lwt.get scaling in
  let record = match record with Some _ -> record | None -> Lwt.get work_record in
  Lwt.with_value work_record record (fun () ->
  Lwt.with_value scaling scale (fun () ->
    Lwt.with_value selection (Some (limit, clock, running)) action))

let check slice =
  if not (slice.running ()) || Option.fold ~none:false
      ~some:(fun limit -> slice.used >= limit) slice.limit then raise (Unavailable Time)

let work_in slice action =
  match slice with
  | None -> action ()
  | Some slice ->
    check slice;
    let started = Option.map (fun _ -> slice.clock ()) slice.limit in
    let used = slice.used in
    let charge () =
      Option.iter (fun started ->
        slice.used <- max slice.used (used +. max 0. (slice.clock () -. started))) started;
      check slice in
    match action () with
    | value -> charge (); value
    | exception ((Out_of_memory | Stack_overflow | Unavailable _ | Window_expired _) as error) -> raise error
    | exception error -> charge (); raise error

let work action = work_in (Lwt.get active_slice) action

let interrupt ?(charge = true) action =
  let slice = Lwt.get active_slice in
  let selected = Lwt.get selection in
  let started = if not charge then None else Option.map (fun (slice : slice) ->
    {clock = slice.clock; started = slice.clock (); idle = 0.; waiting = None}, slice.used) slice in
  let check_now () =
    Option.iter (fun (_, _, running) ->
      if not (running ()) then raise (Unavailable Time)) selected;
    Option.iter check slice;
    match slice, started with
    | Some slice, Some (timer, used) ->
      if Option.fold ~none:false ~some:(fun limit ->
        used +. elapsed timer >= limit) slice.limit
      then raise (Unavailable Time)
    | _ -> () in
  let rec watch () =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.01 in
    check_now ();
    watch () in
  check_now ();
  let parents = Option.value (Lwt.get timers) ~default:[] in
  let parents = match started with Some (timer, _) -> timer :: parents | None -> parents in
  let pending = Lwt.with_value timers (Some parents) action in
  let observed, notify = Lwt.wait () in
  Lwt.on_any pending
    (fun value -> if Lwt.is_sleeping observed then Lwt.wakeup_later notify value)
    (fun error -> if Lwt.is_sleeping observed then Lwt.wakeup_later_exn notify error);
  let finished = Lwt.catch (fun () -> Lwt.map (fun _ -> ()) pending)
    (fun _ -> Lwt.return_unit) in
  let monitor = watch () in
  Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* value = Lwt.choose [observed; monitor] in
    (match slice, started with
    | Some slice, Some (timer, used) ->
      slice.used <- max slice.used (used +. elapsed timer)
    | _ -> ());
    check_now ();
    Lwt.return value) (fun () ->
      Lwt.cancel pending;
      Lwt.cancel monitor;
      Lwt.no_cancel finished)

let guard action input =
  try action input with
  | Out_of_memory -> raise (Unavailable Memory)
  | Stack_overflow -> raise (Unavailable Stack)

let protect action input = work (fun () -> guard action input)

let bind work next = Lwt.bind work (protect next)

let catch action handler =
  Lwt.catch (fun () -> protect action ()) (protect handler)

let detach ?(launch = Lwt_preemptive.detach) action input =
  let slice = Lwt.get active_slice in
  let clock = Option.bind slice (fun slice -> Option.map (fun _ -> slice.clock) slice.limit) in
  let measure input =
    let started = Option.map (fun clock -> clock ()) clock in
    let result = try Ok (guard action input) with error -> Error error in
    let elapsed = match clock, started with
      | Some clock, Some started -> max 0. (clock () -. started)
      | _ -> 0. in
    result, elapsed in
  let receive (result, elapsed) =
    match result with
    | Error ((Unavailable _ | Exhausted _ | Window_expired _) as error) -> Lwt.fail error
    | _ ->
      begin match slice with
      | None -> ()
      | Some slice ->
        slice.used <- slice.used +. elapsed;
        work_in (Some slice) (fun () -> ())
      end;
      match result with Ok value -> Lwt.return value | Error error -> Lwt.fail error in
  try
    work_in slice (fun () -> ());
    Lwt.bind (launch measure input) receive
  with
  | (Unavailable _ | Exhausted _ | Window_expired _) as error -> Lwt.fail error
  | Out_of_memory -> Lwt.fail (Unavailable Memory)
  | Stack_overflow -> Lwt.fail (Unavailable Stack)
  | _ -> Lwt.fail (Unavailable Host)

let joined ?launch action input =
  let work = detach ?launch action input in
  Lwt.finalize
    (fun () -> Lwt.protected work)
    (fun () -> Lwt.catch
      (fun () -> Lwt.map (fun _ -> ()) (Lwt.protected work))
      (fun _ -> Lwt.return_unit))

let run ~hash action =
  let selected = Option.map (fun (limit, clock, running) ->
    let allowance = Option.map (fun limit -> match Lwt.get scaling with
      | None -> Operation limit | Some scale -> scale hash limit) limit in
    let limit = Option.map (function Operation seconds | Completion seconds ->
      if not (Float.is_finite seconds) || seconds <= 0. then invalid_arg "execution duration invalid";
      seconds) allowance in
    {limit; clock; running; used = 0.}, allowance)
    (Lwt.get selection) in
  let slice = Option.map fst selected in
  let completion = match selected with
    | Some (slice, Some (Completion _)) -> slice.running
    | _ -> (fun () -> false) in
  let measured = match Lwt.get work_record, selected with
    | Some record, Some (slice, _) ->
      Some (record, {clock = slice.clock; started = slice.clock (); idle = 0.; waiting = None})
    | _ -> None in
  let parents = Option.value (Lwt.get timers) ~default:[] in
  let parents = match measured with Some (_, timer) -> timer :: parents | None -> parents in
  Lwt.with_value timers (Some parents) (fun () ->
  Lwt.finalize (fun () ->
  Lwt.with_value active_slice slice (fun () ->
  Lwt.catch (fun () -> protect action ()) (function
    | Unavailable Time when completion () -> Lwt.fail (Window_expired hash)
    | Unavailable resource -> Lwt.fail (Exhausted (hash, resource))
    | Out_of_memory -> Lwt.fail (Exhausted (hash, Memory))
    | Stack_overflow -> Lwt.fail (Exhausted (hash, Stack))
    | error -> Lwt.fail error))) (fun () ->
      Option.iter (fun (record, timer) -> record hash (elapsed timer)) measured;
      Lwt.return_unit))

let name = function Memory -> "memory" | Stack -> "stack" | Host -> "host" | Time -> "time"

let await ?(steps = 10_000_000) work =
  let rec pump n = match Lwt.state work with
    | Lwt.Return value -> value
    | Lwt.Fail error -> raise error
    | Lwt.Sleep ->
      if n > steps then raise (Unavailable Host);
      Lwt_engine.iter false;
      pump (n + 1) in
  pump 0