(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ids = Map.Make (Int)

type token = { id : int; generation : int }
type job = { token : token; started : float; deadline : float; background : bool; stopping : bool }
type phase = Open | Closing | Closed
type 'a state = {
  phase : phase;
  generation : int;
  issued : int;
  active : job option;
  cached : (float * 'a) option;
  sample : (float * 'a) option;
  attempted : bool;
  next_at : float;
  readers : (int * float) list;
}
type 'a message =
  | Ask of int * float * bool
  | Refresh of float
  | Invalidate
  | Cancel of int
  | Tick of float
  | Completed of token * float * bool * 'a option
  | Stop
type 'a effect = Reply of int * 'a option | Run of token | Interrupt of token

let empty = {phase = Open; generation = 0; issued = 0; active = None;
  cached = None; sample = None; attempted = false; next_at = neg_infinity; readers = []}

let replies readers value = List.map (fun (id, _) -> Reply (id, value)) readers

let interrupt state = match state.active with
  | Some job when not job.stopping ->
    {state with active = Some {job with stopping = true}}, [Interrupt job.token]
  | None | Some _ -> state, []

let delta state = function
  | Invalidate -> {state with cached = None; sample = None; attempted = true}, []
  | Refresh now ->
    if state.phase <> Open || state.active <> None || now < state.next_at
        || state.issued = max_int then state, []
    else
      let token = {id = state.issued + 1; generation = state.generation} in
      {state with issued = token.id; next_at = now +. 3.;
        active = Some {token; started = now; deadline = now +. 10.;
          background = true; stopping = false}}, [Run token]
  | Ask (id, now, fresh) ->
    let state = {state with issued = max id state.issued} in
    if state.phase <> Open || List.length state.readers >= 16 then state, [Reply (id, None)]
    else begin match state.cached with
    | Some (until, value) when fresh && now < until -> state, [Reply (id, Some value)]
    | None | Some _ ->
      let state = {state with cached = None} in
      let readers = (id, now +. 2.5) :: state.readers in
      match state.active with
      | Some job when job.stopping -> state, [Reply (id, None)]
      | Some _ -> {state with readers}, []
      | None when now < state.next_at -> state, [Reply (id, None)]
      | None ->
        let token = {id; generation = state.generation} in
        {state with readers; next_at = now +. 3.;
          active = Some {token; started = now; deadline = now +. 2.5;
            background = false; stopping = false}}, [Run token]
    end
  | Cancel id ->
    let state = {state with readers = List.filter (fun (found, _) -> found <> id) state.readers} in
    if state.readers <> [] then state, [] else
    (match state.active with
      | Some {background = true; _} -> state, []
      | None | Some _ -> interrupt state)
  | Tick now ->
    let expired, readers = List.partition (fun (_, until) -> now >= until) state.readers in
    let state = {state with readers} in
    let state, effects = match state.active with
      | Some job when now >= job.deadline -> interrupt state
      | None | Some _ -> state, [] in
    state, replies expired None @ effects
  | Completed (token, now, fresh, value) ->
    begin match state.active with
    | Some job when job.token = token ->
      let value = if state.phase <> Open || job.stopping || now > job.deadline then None else value in
      let cached = match value with Some value when fresh -> Some (now +. 3., value) | _ -> None in
      let sample = match value with
        | Some value when fresh -> Some (job.started, value)
        | None | Some _ -> None in
      let expired, readers = List.partition (fun (_, until) -> now >= until) state.readers in
      {state with active = None; readers = []; cached; sample; attempted = true;
        phase = if state.phase = Closing then Closed else state.phase},
      replies expired None @ replies readers value
    | None | Some _ -> state, []
    end
  | Stop ->
    let state, effects = interrupt state in
    {state with phase = (if state.active = None then Closed else Closing);
      generation = state.generation + 1; readers = []; cached = None; sample = None},
      replies state.readers None @ effects

type 'a t = {
  clock : unit -> float;
  run : background:bool -> deadline:float -> 'a Lwt.t;
  valid : 'a -> bool;
  retry : 'a -> bool;
  mutable state : 'a state;
  mutable replies : 'a option Lwt.u Ids.t;
  mutable work : (token * 'a Lwt.t) option;
  mutable refresh : unit Lwt.t option;
  changed : unit Lwt_condition.t;
}

let monotonic () = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1_000_000_000.
let valid t value = try t.valid value with _ -> false

let rec dispatch t message =
  let state, effects = delta t.state message in
  t.state <- state;
  List.iter (execute t) effects;
  Lwt_condition.broadcast t.changed ()

and execute t = function
  | Reply (id, value) ->
    begin match Ids.find_opt id t.replies with
    | None -> ()
    | Some wake ->
      t.replies <- Ids.remove id t.replies;
      Lwt.wakeup_later wake value
    end
  | Interrupt token ->
    Option.iter (fun (found, work) -> if found = token then Lwt.cancel work) t.work
  | Run token ->
    let background, deadline = match t.state.active with
      | Some job when job.token = token -> job.background, job.deadline
      | None | Some _ -> false, t.clock () in
    let work = Lwt.catch (fun () -> t.run ~background ~deadline) Lwt.fail in
    t.work <- Some (token, work);
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      let+ () = Lwt_unix.sleep (max 0. (deadline -. t.clock ())) in
      dispatch t (Tick (t.clock ())));
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* value = Lwt.catch
        (fun () -> let+ value = Lwt.protected work in Some value)
        (fun _ -> Lwt.return_none) in
      let fresh = Option.fold ~none:false ~some:(valid t) value in
      t.work <- None;
      dispatch t (Completed (token, t.clock (), fresh, value));
      Lwt.return_unit)

let create ?(clock = monotonic) ?(retry = fun _ -> false) ~valid run = {
  clock; run; valid; retry; state = empty; replies = Ids.empty; work = None; refresh = None;
  changed = Lwt_condition.create ();
}

let query t =
  if t.state.issued = max_int then Lwt.return_none else
  let id = t.state.issued + 1 in
  let promise, wake = Lwt.task () in
  t.replies <- Ids.add id wake t.replies;
  Lwt.on_cancel promise (fun () ->
    t.replies <- Ids.remove id t.replies;
    dispatch t (Cancel id));
  let fresh = Option.fold ~none:false ~some:(fun (_, value) -> valid t value) t.state.cached in
  dispatch t (Ask (id, t.clock (), fresh));
  Lwt.async (fun () ->
    let open Lwt.Syntax in
    let* () = Lwt.catch (fun () -> Lwt.pick [
      Lwt.map (fun _ -> ()) (Lwt.protected promise); Lwt_unix.sleep 2.5])
      (fun _ -> Lwt.return_unit) in
    dispatch t (Tick (t.clock ()));
    Lwt.return_unit);
  promise

type 'a snapshot = Starting | Read_failed | Expired of float | Snapshot of float * 'a

let snapshot t =
  if t.state.phase <> Open then Read_failed else
  match t.state.sample with
  | None -> if t.state.attempted then Read_failed else Starting
  | Some (observed, value) ->
    let age = max 0. (t.clock () -. observed) in
    if age > 20. then Expired age else Snapshot (age, value)

let start t =
  if t.refresh = None && t.state.phase = Open then begin
    let open Lwt.Syntax in
    let rec loop () =
      if t.state.phase <> Open then Lwt.return_unit else
      let now = t.clock () in
      let refresh = match t.state.sample with
        | None -> true
        | Some (observed, value) ->
          if not (valid t value) then begin dispatch t Invalidate; true end
          else t.retry value || now -. observed >= 10. in
      if refresh then dispatch t (Refresh now);
      let* () = Lwt_unix.sleep 0.1 in
      loop () in
    let work = let* () = Lwt.pause () in loop () in
    t.refresh <- Some work;
    Lwt.async (fun () -> Lwt.catch (fun () -> work)
      (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error))
  end

let close t =
  dispatch t Stop;
  Option.iter Lwt.cancel t.refresh;
  t.refresh <- None;
  let rec finished () =
    if t.state.phase = Closed then Lwt.return_true else
    Lwt.bind (Lwt_condition.wait t.changed) finished in
  Lwt.pick [finished (); Lwt.map (fun () -> false) (Lwt_unix.sleep 0.5)]

let read_lock = Lwt_mutex.create ()

let child parent run =
  let module F = Octra_core.Proof_frame in
  try
    Irmin_pack_unix.Async.Unix.mask_for_exit ();
    let output = Octra_core.Store_irmin.pack_child_fd Unix.stdout parent 3 in
    Logs.set_reporter_mutex ~lock:(fun () -> ()) ~unlock:(fun () -> ());
    Logs.set_reporter Logs.nop_reporter;
    Logs.set_level None;
    let raw = run () in
    if String.length raw > 8 * 1024 * 1024 then Unix._exit 2;
    F.write output (F.pack [String.length raw] ^ raw);
    Unix._exit 0
  with _ -> Unix._exit 2

let read_child ~mode args =
  let module F = Octra_core.Proof_frame in
  let module Owner = Irmin_pack_unix.Async.Unix in
  if Lwt_mutex.is_locked read_lock then Lwt.fail_with "sync read is already active" else
  Lwt_mutex.with_lock read_lock (fun () -> Owner.with_spawn (fun () ->
    let open Lwt.Syntax in
    let input, output = Unix.pipe ~cloexec:true () in
    let reader = try Lwt_unix.of_unix_file_descr ~blocking:false input
      with error -> F.close input; F.close output; raise error in
    let program = if Sys.file_exists "/proc/self/exe" then "/proc/self/exe"
      else Sys.executable_name in
    let argv = Array.of_list (program :: mode :: string_of_int (Unix.getpid ()) :: args) in
    let pid = try Unix.create_process_env program argv (Unix.environment ())
      Unix.stdin output Unix.stderr
      with error -> F.close input; F.close output; raise error in
    F.close output;
    let status = ref None in
    let poll () = match !status with
      | Some _ -> ()
      | None ->
        (match Unix.waitpid [Unix.WNOHANG] pid with
         | 0, _ -> ()
         | _, value -> status := Some value
         | exception Unix.Unix_error (Unix.EINTR, _, _) -> ()
         | exception Unix.Unix_error (Unix.ECHILD, _, _) -> status := Some (Unix.WEXITED 255)) in
    let rec wait () = match !status with
      | Some value -> Lwt.return value
      | None ->
        poll ();
        let* () = Lwt_unix.sleep 0.01 in wait () in
    let read size =
      let bytes = Bytes.create size in
      let rec more offset =
        if offset = size then Lwt.return (Bytes.unsafe_to_string bytes) else
        let* count = Lwt_unix.read reader bytes offset (size - offset) in
        if count = 0 then Lwt.fail End_of_file else more (offset + count) in
      more 0 in
    Lwt.finalize (fun () ->
      let* header = read 8 in
      let size = F.number header 0 in
      if size > 8 * 1024 * 1024 then Lwt.fail_with "sync read frame exceeds limit" else
      let* raw = read size in
      let* status = wait () in
      if status = Unix.WEXITED 0 then Lwt.return raw else Lwt.fail_with "sync read child failed")
      (fun () -> Lwt.no_cancel (Lwt.finalize (fun () ->
        poll ();
        if !status = None then F.signal pid;
        let+ _ = wait () in ()) (fun () -> Lwt_unix.close reader)))))