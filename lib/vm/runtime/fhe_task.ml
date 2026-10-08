(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type proof = Zero | Range | Claim of string
type key = Octra_core.Fhe_image.key
type cipher = Octra_core.Fhe_image.cipher

type request =
  | Load_key of string
  | Read_key of string
  | Write_key of key
  | Write_secret of string
  | Read_cipher of bool * bool * string
  | Write_cipher of cipher
  | Add of key * cipher * cipher
  | Sub of key * cipher * cipher
  | Mul of bool * bool * key * cipher * cipher * string
  | Scale of bool * key * cipher * int64
  | Divide of key * cipher * int64
  | Add_int of bool * key * cipher * int64
  | Sub_int of bool * key * cipher * int64
  | Commit of key * cipher
  | Pack of proof * key * cipher * string
  | Verify of bool * Octra_core.Pvac_verify_protocol.request

type value =
  | Key of key
  | Cipher of cipher
  | Text of string
  | Check of Octra_core.Pvac_verify_protocol.request
  | Verified of bool
type error = Invalid | Resource of Octra_core.Exec_resource.resource

module Worker = Octra_core.Pvac_verify_worker
module Calc = Octra_core.Fhe_calc

module Native = Ephemeron.K1.Make (struct
  type t = string
  let equal left right = left == right
  let hash bytes =
    let size = String.length bytes in
    if size < 8 then Hashtbl.hash bytes
    else Hashtbl.hash (size, String.get_int64_be bytes 0, String.get_int64_be bytes (size - 8))
end)

type local = {
  keys : Pvac_ffi.pubkey Native.t;
  ciphers : Pvac_ffi.cipher Native.t;
}

let local () = {keys = Native.create 4; ciphers = Native.create 8}

type action = Local of value | Native of Calc.request
  | Proof of Octra_core.Pvac_verify_protocol.request

let cached_key = function
  | Add (key, _, _) | Sub (key, _, _) | Mul (_, _, key, _, _, _)
  | Scale (_, key, _, _) | Divide (key, _, _)
  | Add_int (_, key, _, _) | Sub_int (_, key, _, _) | Commit (key, _) ->
    if Option.fold ~none:false ~some:(fun size -> size <= 67_108_864) key.size then
      Some key.data else None
  | _ -> None

let prepare = function
  | Verify (math, request) ->
    Proof (if math then Octra_core.Pvac_verify_protocol.Math request else request)
  | Load_key raw -> Native (Calc.Read_key (true, raw))
  | Read_key raw -> Native (Calc.Read_key (false, raw))
  | Read_cipher (strict, cap, raw) -> Native (Calc.Read_cipher (strict, cap, raw))
  | Write_key key -> Local (Text (Base64.encode_exn key.data))
  | Write_secret bytes -> Local (Text (Base64.encode_exn bytes))
  | Write_cipher cipher -> Local (Text (Base64.encode_exn cipher.data))
  | Add (key, left, right) -> Native (Calc.Add (key.data, left.data, right.data))
  | Sub (key, left, right) -> Native (Calc.Sub (key.data, left.data, right.data))
  | Mul (math, work, key, left, right, seed) ->
    let draws = if work then Fhe_view_policy.sample_factor else 0 in
    Native (Calc.Mul (math, draws, key.data, left.data, right.data, seed))
  | Scale (math, key, cipher, amount) -> Native (Calc.Scale (math, key.data, cipher.data, amount))
  | Divide (key, cipher, amount) -> Native (Calc.Divide (key.data, cipher.data, amount))
  | Add_int (math, key, cipher, amount) -> Native (Calc.Add_int (math, key.data, cipher.data, amount))
  | Sub_int (math, key, cipher, amount) -> Native (Calc.Sub_int (math, key.data, cipher.data, amount))
  | Commit (key, cipher) -> Native (Calc.Commit (key.data, cipher.data))
  | Pack (kind, key, cipher, proof) ->
    let pubkey = key.data in
    let cipher = Octra_core.Crypto.FheBalance.prefix ^ Base64.encode_exn cipher.data in
    let request = match kind with
      | Zero -> Octra_core.Pvac_verify_protocol.Zero {pubkey; cipher; proof}
      | Range -> Octra_core.Pvac_verify_protocol.Range {pubkey; cipher; proof; strict = true}
      | Claim commitment ->
        Octra_core.Pvac_verify_protocol.Claim {pubkey; cipher; proof; commitment; strict = true} in
    Local (Check request)

let accept request = function
  | Error Calc.Invalid -> Error Invalid
  | Error Calc.Memory -> Error (Resource Memory)
  | Error Calc.Stack -> Error (Resource Stack)
  | Ok value ->
    match request, value with
    | (Load_key _ | Read_key _), Calc.Key key -> Ok (Key key)
    | (Read_cipher _ | Add _ | Sub _ | Mul _ | Scale _ | Divide _ | Add_int _ | Sub_int _),
        Calc.Cipher cipher -> Ok (Cipher cipher)
    | Commit _, Calc.Digest raw -> Ok (Text (Base64.encode_exn raw))
    | _ -> Error (Resource Host)

let direct state request =
  let save table capacity bytes value =
    if not (Native.mem table bytes) && Native.length table >= capacity then
      Native.clear table;
    Native.replace table bytes value in
  let read table capacity decode bytes =
    match Native.find_opt table bytes with
    | Some value -> value
    | None ->
      let value = decode (Bytes.of_string bytes) in
      save table capacity bytes value;
      value in
  let of_key value =
    let image = Octra_core.Fhe_image.of_key value in
    save state.keys 4 image.data value;
    image in
  let of_cipher value =
    let image = Octra_core.Fhe_image.of_cipher value in
    save state.ciphers 32 image.data value;
    image in
  try
    match prepare request with
    | Local value -> Ok value
    | Native wire ->
      Calc.eval ~key:(read state.keys 4 Pvac_ffi.deserialize_pubkey)
        ~cipher:(read state.ciphers 32 (Pvac_ffi.deserialize_cipher ~strict:false ~cap:false))
        ~of_key ~of_cipher wire |> accept request
    | Proof _ -> Error (Resource Host)
  with
  | Out_of_memory -> Error (Resource Memory)
  | Stack_overflow -> Error (Resource Stack)
  | Invalid_argument _ | Failure _ -> Error Invalid

let key_effort = function
  | Add (key, _, _) | Sub (key, _, _) | Mul (_, _, key, _, _, _)
  | Scale (_, key, _, _) | Divide (key, _, _)
  | Add_int (_, key, _, _) | Sub_int (_, key, _, _) | Commit (key, _) ->
    Option.bind key.size (fun size ->
      let volume = Z.add (Z.of_int size) (Z.of_int (String.length key.data)) in
      if size < 0 then None else Fhe_memory.key_effort ~active:true volume)
  | _ -> Some 0

let isolated ?session ~control ~deadline request =
  try
    if Atomic.get control = Worker.Cancel || Mtime_clock.elapsed_ns () >= deadline then
      raise (Octra_core.Exec_resource.Unavailable Host);
    match prepare request with
    | Local value -> Ok value
    | Proof wire ->
      begin match Worker.worker_path () with
      | None -> Error (Resource Host)
      | Some worker ->
        let deadline = Int64.to_float deadline /. 1e9 in
        begin match Worker.run_process ~control ~deadline worker wire with
        | Worker.Completed response -> Ok (Verified response.accepted)
        | Worker.Memory_exceeded -> Error (Resource Memory)
        | Worker.Timed_out | Worker.Busy | Worker.Unavailable _ | Worker.Failed _ ->
          Error (Resource Host)
        end
      end
    | Native wire ->
      match Worker.worker_path () with
      | None -> Error (Resource Host)
      | Some worker ->
        let id = ref "" in
        let encode request =
          let raw = Calc.request_bytes request in
          id := Calc.hash raw;
          raw in
        let deadline = Int64.to_float deadline /. 1e9 in
        let reply = match session with
          | None -> Worker.exchange ~control ~deadline ~arguments:["--fhe"]
              ~limit:Calc.max_bytes worker (encode wire)
          | Some saved ->
            let process = match !saved with
              | Some process when process.Worker.program = worker -> process
              | _ -> raise (Octra_core.Exec_resource.Unavailable Host) in
            let wire = match process.Worker.key with
              | None -> wire
              | Some (key, reference) -> Calc.map_key (fun bytes ->
                  if bytes = key then reference else bytes) wire in
            let raw = encode wire in
            begin match Worker.session_exchange ~control ~deadline process raw with
            | Ok raw -> Ok (raw, "", Unix.WEXITED 0)
            | Error error ->
              saved := None;
              Worker.close_session process;
              Error error
            end in
        match reply with
        | Error Worker.Memory_exceeded -> Error (Resource Memory)
        | Error _ -> Error (Resource Host)
        | Ok (raw, _, Unix.WEXITED 0) ->
          let response = Calc.response_of_bytes !id raw in
          Option.iter (fun saved ->
            saved := Option.map (fun process ->
              let key = match response, request with
                | Ok (Ok _), Read_cipher _ -> process.Worker.key
                | Ok (Ok _), _ -> Option.map (fun bytes ->
                    match process.Worker.key with
                    | Some (key, reference) when key = bytes -> key, reference
                    | _ -> bytes, Calc.key_ref bytes) (cached_key request)
                | _ -> None in
              {process with Worker.key}) !saved) session;
          begin match response with
          | Ok value -> accept request value
          | Error _ -> Error (Resource Host)
          end
        | Ok _ -> Error (Resource Host)
  with
  | Out_of_memory -> Error (Resource Memory)
  | Stack_overflow -> Error (Resource Stack)
  | _ -> Error (Resource Host)

let eval request =
  let now = Mtime_clock.elapsed_ns () in
  let span = Int64.of_float (Worker.timeout_seconds () *. 1e9) in
  let deadline = Int64.add now span in
  if deadline <= now then Error (Resource Host)
  else isolated ~control:(Atomic.make Worker.Continue) ~deadline request

type job = {
  ticket : Proof_wait.ticket;
  urgent : bool;
  request : request;
  reply : (value, error) result Lwt.t;
  wake : (value, error) result Lwt.u;
  control : Worker.control Atomic.t;
  deadline : int64;
  result : (value, error) result Atomic.t;
  finished : bool Atomic.t;
  process : Worker.session option Atomic.t;
  mutable launched : bool;
  mutable settled : bool;
}

type actor = {
  owner : int;
  clock : unit -> int64;
  reap : Worker.session -> unit Lwt.t;
  mutable closing : unit Lwt.t list;
  mutable sequence : int64;
  mutable state : Fhe_queue.state;
  mutable jobs : job list;
  mutable process : Worker.session option;
  mutable view : Worker.session option;
  mutable idle : unit Lwt.t option;
  mutable restart : bool;
  mutable failures : int;
}

let create ?(clock = Mtime_clock.elapsed_ns) ?(reap = Worker.retire_session) () = {
  owner = Thread.id (Thread.self ());
  clock;
  reap;
  closing = [];
  sequence = 0L;
  state = Fhe_queue.empty;
  jobs = [];
  process = None;
  view = None;
  idle = None;
  restart = false;
  failures = 0;
}
let shared = create ()

let track actor closed =
  actor.closing <- closed :: actor.closing;
  Lwt.on_success closed (fun () ->
    actor.closing <- List.filter (fun entry -> entry != closed) actor.closing)

let retire actor process =
  track actor (Lwt.catch (fun () -> actor.reap process) Lwt.fail)

let find actor ticket = List.find_opt (fun job -> job.ticket = ticket) actor.jobs

let deliver job value =
  if Lwt.is_sleeping job.reply then Lwt.wakeup_later job.wake value

let retire_closed actor =
  begin match actor.state.active with
  | Some entry when Option.fold ~none:true
      ~some:(fun job -> job.settled) (find actor entry.ticket) ->
    actor.state <- fst (Fhe_queue.delta actor.state (Int64.min_int, Fhe_queue.Complete entry.ticket))
  | _ -> ()
  end;
  actor.jobs <- List.filter (fun job -> not job.settled) actor.jobs

let resource = function
  | Out_of_memory | Octra_core.Exec_resource.Unavailable Memory -> Octra_core.Exec_resource.Memory
  | Stack_overflow | Octra_core.Exec_resource.Unavailable Stack -> Stack
  | _ -> Host

let fail actor error =
  actor.restart <- actor.failures = 0;
  actor.failures <- actor.failures + 1;
  Option.iter Lwt.cancel actor.idle;
  actor.idle <- None;
  Option.iter (retire actor) actor.process;
  actor.process <- None;
  Option.iter (retire actor) actor.view;
  actor.view <- None;
  actor.state <- fst (Fhe_queue.delta actor.state (Int64.min_int, Fhe_queue.Stop));
  List.iter (fun job ->
    Atomic.set job.control Worker.Cancel;
    if not job.launched then begin
      Atomic.set job.finished true;
      job.settled <- true
    end;
    try deliver job (Error (Resource (resource error)))
    with _ -> Lwt.cancel job.reply) actor.jobs;
  retire_closed actor

let guard actor action =
  if Thread.id (Thread.self ()) <> actor.owner then
    raise (Octra_core.Exec_resource.Unavailable Host);
  try action () with error -> fail actor error

let rec effects actor actions =
  List.iter (function
    | Fhe_queue.Start ticket ->
      begin match find actor ticket with
      | None -> ignore (send actor Fhe_queue.Stop)
      | Some job ->
        Option.iter Lwt.cancel actor.idle;
        actor.idle <- None;
        let current = if job.urgent then actor.process else actor.view in
        begin match prepare job.request, Worker.worker_path (), current with
        | Native _, Some worker, Some child when child.Worker.program <> worker ->
          if job.urgent then actor.process <- None else actor.view <- None;
          retire actor child
        | _ -> ()
        end;
        if actor.closing <> [] then begin
          Lwt.on_any (Lwt.join actor.closing)
            (fun () -> guard actor (fun () ->
              actor.closing <- List.filter (fun closed ->
                match Lwt.state closed with Lwt.Return () -> false | _ -> true) actor.closing;
              match actor.state.active with
              | Some entry when entry.ticket = ticket && not job.settled ->
                if entry.abandoned || actor.clock () >= entry.deadline then begin
                  job.settled <- true;
                  Atomic.set job.finished true;
                  deliver job (Error (Resource Host));
                  ignore (send actor (Fhe_queue.Complete ticket))
                end else effects actor [Fhe_queue.Start ticket]
              | _ -> ()))
            (fun error -> guard actor (fun () -> fail actor error))
        end else begin
        let process = ref None in
        let complete value =
          let now = actor.clock () in
          let current = actor.state.active in
          let next, actions = Fhe_queue.delta actor.state (now, Fhe_queue.Complete ticket) in
          actor.state <- next;
          let accepted = match current with
            | Some entry -> entry.ticket = ticket && not entry.abandoned
                && now < entry.deadline
            | None -> false in
          if accepted then deliver job value;
          effects actor actions;
          if actor.state.active = None && Option.is_none actor.idle
              && (Option.is_some actor.process || Option.is_some actor.view) then begin
            let timer = Lwt_unix.sleep 10. in
            actor.idle <- Some timer;
            Lwt.on_success timer (fun () ->
              Option.iter (retire actor) actor.process;
              actor.process <- None;
              Option.iter (retire actor) actor.view;
              actor.view <- None;
              actor.idle <- None)
          end
        in
        let request = job.request in
        let notification = Lwt_unix.make_notification ~once:true (fun () ->
          job.settled <- true;
          let process = Atomic.get job.process in
          if actor.state.closed then Option.iter (retire actor) process
          else if job.urgent then actor.process <- process
          else actor.view <- process;
          guard actor (fun () ->
            if actor.state.closed then retire_closed actor
            else complete (Atomic.get job.result))) in
        let execute () =
          begin try
            let result = isolated ~session:process ~control:job.control ~deadline:job.deadline request in
            (match result with
            | Error (Resource _) ->
              Option.iter Worker.close_session !process;
              process := None
            | _ -> ());
            Atomic.set job.result result
          with _ -> () end;
          Atomic.set job.process !process;
          Atomic.set job.finished true;
          Lwt_unix.send_notification notification
        in
        begin
          try
            if job.urgent then begin
              process := actor.process;
              actor.process <- None
            end else begin
              process := actor.view;
              actor.view <- None
            end;
            begin match prepare request with
            | Native _ ->
              let worker = match Worker.worker_path () with
                | Some worker -> worker
                | None -> raise (Octra_core.Exec_resource.Unavailable Host) in
              begin match !process with
              | Some child when child.Worker.program = worker -> ()
              | previous ->
                process := None;
                Option.iter (retire actor) previous;
                if actor.closing <> [] then
                  raise (Octra_core.Exec_resource.Unavailable Host);
                process := Some (Worker.open_session
                  ~on_exit:(fun pid -> track actor (Worker.retire_pid pid)) worker)
              end
            | Local _ | Proof _ -> ()
            end;
            ignore (Thread.create execute ());
            job.launched <- true
          with error ->
            Lwt_unix.stop_notification notification;
            Option.iter (retire actor) !process;
            Atomic.set job.finished true;
            begin match error with
            | Octra_core.Exec_resource.Unavailable _ | Unix.Unix_error _
            | Sys_error _ | Out_of_memory | Stack_overflow ->
              job.settled <- true;
              complete (Error (Resource (resource error)))
            | _ -> fail actor error
            end
        end
        end
      end
    | Fhe_queue.Fail (ticket, _) ->
      Option.iter (fun job ->
        Atomic.set job.control Worker.Cancel;
        deliver job (Error (Resource Host))) (find actor ticket)
    | Fhe_queue.Retire ticket ->
      actor.jobs <- List.filter (fun job -> job.ticket <> ticket) actor.jobs
    | Fhe_queue.Refuse _ -> ()) actions
and transition actor message =
  if Thread.id (Thread.self ()) <> actor.owner then
    raise (Octra_core.Exec_resource.Unavailable Host);
  let now = match message with Fhe_queue.Stop -> Int64.min_int | _ -> actor.clock () in
  let next, actions = Fhe_queue.delta actor.state (now, message) in
  actor.state <- next;
  actions
and send actor message =
  let actions = transition actor message in
  effects actor actions;
  actions

let submit actor urgent ticket deadline request =
  actor.sequence <- Int64.succ actor.sequence;
  let ticket = {ticket with Proof_wait.generation =
    ticket.Proof_wait.generation ^ ":" ^ Int64.to_string actor.sequence} in
  let reply, wake = Lwt.task () in
  let job = {ticket; urgent; request; reply; wake; deadline;
    control = Atomic.make Worker.Continue;
    result = Atomic.make (Error (Resource Host)); finished = Atomic.make false;
    process = Atomic.make None;
    launched = false; settled = false} in
  let jobs = job :: actor.jobs in
  let actions = transition actor (Fhe_queue.Submit {ticket; deadline; abandoned = false; urgent}) in
  let accepted = not (List.exists (function Fhe_queue.Refuse _ -> true | _ -> false) actions) in
  if accepted then actor.jobs <- jobs;
  effects actor actions;
  if not accepted then Lwt.return (Error (Resource Host))
  else begin
    Lwt.on_cancel reply (fun () -> guard actor (fun () -> ignore (send actor (Fhe_queue.Cancel ticket))));
    let seconds = max 0. (Int64.to_float (Int64.sub deadline (actor.clock ())) /. 1e9) in
    let clock = Lwt_unix.sleep seconds in
    Lwt.on_success clock (fun () -> guard actor (fun () -> ignore (send actor Fhe_queue.Tick)));
    Lwt.finalize (fun () -> reply) (fun () -> Lwt.cancel clock; Lwt.return_unit)
  end

let run ?(actor = shared) ?(urgent = true) ~ticket ~deadline request =
  if Thread.id (Thread.self ()) <> actor.owner || actor.sequence = Int64.max_int then
    raise (Octra_core.Exec_resource.Unavailable Host);
  if actor.state.closed then retire_closed actor;
  if actor.restart && actor.state.active = None && actor.jobs = [] then begin
    actor.restart <- false;
    actor.state <- Fhe_queue.empty
  end;
  if actor.state.closed then begin
    retire_closed actor;
    Lwt.return (Error (Resource Host))
  end
  else
    try
      if actor.clock () >= deadline then Lwt.return (Error (Resource Host))
      else match prepare request with
      | Local value -> Lwt.return (Ok value)
      | Native _ | Proof _ -> submit actor urgent ticket deadline request
    with error ->
      fail actor error;
      Lwt.return (Error (Resource (resource error)))

let stop actor =
  guard actor (fun () ->
    actor.restart <- false;
    Option.iter Lwt.cancel actor.idle;
    actor.idle <- None;
    Option.iter (retire actor) actor.process;
    actor.process <- None;
    Option.iter (retire actor) actor.view;
    actor.view <- None;
    ignore (send actor Fhe_queue.Stop))

let stats ?(actor = shared) () =
  if Thread.id (Thread.self ()) <> actor.owner then
    raise (Octra_core.Exec_resource.Unavailable Host);
  if actor.state.closed then retire_closed actor;
  Option.is_some actor.state.active, List.length actor.state.pending