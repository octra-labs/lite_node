(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Native = Circle_wasm_native
module Worker = Pvac_verify_worker

let max_bytes = Sys.max_string_length
let protocol = "circle-session-shared-proof"

let inbox = Compute_memory.create ~limit:Native.max_call_input_bytes
  ~speculative_limit:0 ~slots:65
let inbox_stats () = Compute_memory.stats inbox
let required = Compute_pool.create ~memory:inbox ~capacity:1 ~required_limit:64
  ~speculative_limit:1 ~required_burst:1 ()
let views = Compute_pool.create ~capacity:1 ~required_limit:1
  ~speculative_limit:1 ~required_burst:1 ()
type context = {optional : bool; proof : Compute_pool.reservation option}
let active = Lwt.new_key ()
let frames = 9
let proof_bytes = Worker.P.max_request_bytes
let memory = Compute_memory.create ~limit:(frames * Native.max_call_input_bytes + proof_bytes)
  ~speculative_limit:0 ~slots:(frames + 1)
let memory_stats () = Compute_memory.stats memory

let with_memory ~optional ~bytes action =
  let owner = if optional then Compute_memory.shared else memory in
  match Compute_memory.reserve owner ~speculative:optional bytes with
  | None -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Memory)
  | Some ticket -> Lwt.finalize action (fun () ->
    Compute_memory.release ticket; Lwt.return_unit)

let with_slot ~optional ~bytes action =
  match Lwt.get active with
  | Some context -> with_memory ~optional:context.optional ~bytes action
  | None ->
  let pool = if optional then views else required in
  if optional && (Compute_pool.stats pool).active <> 0 then
    Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
  else
    Exec_resource.queued (fun resume ->
    let job = ref Lwt.return_unit in
    let cancel = ref (fun () -> ()) in
    let rec acquire step delay =
      let open Lwt.Syntax in
      let* result = Compute_pool.run_async ~bytes pool
        ?running_memory:(if optional then None else Some memory)
        (if optional then Compute_pool.Speculative else Required) (fun () ->
          resume ();
          let proof = if optional then None else
            Compute_pool.reserve ~memory ~bytes:proof_bytes Worker.proof_channel in
          if not optional && Option.is_none proof then
            Lwt.fail (Exec_resource.Unavailable Exec_resource.Memory)
          else
          let pending = Lwt.finalize (fun () ->
            Lwt.with_value active (Some {optional; proof}) action)
            (fun () -> Option.iter Compute_pool.close_reservation proof; Lwt.return_unit) in
          cancel := (fun () -> Lwt.cancel pending);
          job := Lwt.catch (fun () -> Lwt.map (fun _ -> ()) pending)
            (fun _ -> Lwt.return_unit);
          pending) in
      match result with
      | Some value -> Lwt.return value
      | None when optional -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
      | None when step = 6 -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Memory)
      | None ->
        let* () = Lwt_unix.sleep delay in
        acquire (step + 1) (delay *. 2.) in
    Lwt.finalize (fun () -> acquire 0 0.05)
      (fun () -> !cancel (); Lwt.no_cancel !job))

external kill : int -> unit = "octra_worker_stop"

let serve () =
  ignore (Unix.setsid ());
  Circle_wasm_hfhe_backend.ensure_registered ();
  let read () =
    let header = really_input_string stdin 8 |> Bytes.of_string in
    let size = Bytes.get_int64_be header 0 in
    if size <= 0L || size > Int64.of_int max_bytes then failwith "circle frame size";
    really_input_string stdin (Int64.to_int size) in
  let verify request =
    Fhe_calc.write_frame ("4" ^ Yojson.Safe.to_string (Worker.P.request_json request));
    match read () with
    | "0" -> Ok ()
    | "1" -> Error (Worker.Proof_rejected "proof rejected")
    | _ -> Error (Worker.Worker_unavailable "proof response differs") in
  Callback.register "octra_circle_wasm_host_hfhe_call_json"
    (Circle_wasm_hfhe_backend.call_json_using ~verify);
  let raw = read () in
  let body = String.sub raw 1 (String.length raw - 1) in
  let result = match raw.[0] with
    | 'P' -> Ok protocol
    | 'C' -> Lwt_main.run (Native.run_call (fun request ->
        Fhe_calc.write_frame ("3" ^ request);
        Lwt.return (read ())) body)
    | 'J' -> Native.run_json_classified body
    | 'V' -> Native.run_json_classified ~optional:true body
    | _ -> failwith "circle frame mode" in
  Fhe_calc.write_frame (match result with
    | Ok output -> "0" ^ output
    | Error (Native.Rejected reason) -> "1" ^ reason
    | Error (Native.Unavailable reason) -> "2" ^ reason)

let execute ?call ?(optional = false) ?(probe = false) body =
  let open Lwt.Syntax in
  let optional = Option.fold ~none:optional ~some:(fun context -> context.optional) (Lwt.get active) in
  if Option.is_none call && String.length body > Native.max_input_bytes then
    Lwt.return (Native.run_json_classified ~optional body)
  else if Option.is_some call && String.length body > Native.max_call_input_bytes then
    Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
  else match Worker.worker_path () with
  | None -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
  | Some program ->
    Exec_resource.interrupt ~charge:false (fun () ->
    with_slot ~optional ~bytes:(String.length body) (fun () ->
      let process = ref None in
      let input = ref None in
      let output = ref None in
      let close raw stream = match !stream with
        | Some stream -> Lwt_unix.close stream
        | None -> Worker.close_noerr raw; Lwt.return_unit in
      let cleanup () =
          match !process with
            | None -> Lwt.return_unit
            | Some session ->
              Lwt.finalize
                (fun () -> kill session.Worker.pid; Lwt.return_unit)
                (fun () -> Lwt.finalize
                  (fun () -> Lwt.finalize
                    (fun () -> close session.Worker.input input)
                    (fun () -> close session.Worker.output output))
                  (fun () -> Worker.retire_pid session.Worker.pid)) in
      Lwt.catch (fun () -> Lwt.finalize (fun () -> Exec_resource.interrupt (fun () ->
        let session = Worker.open_session ~arguments:["--circle-session"]
          ~on_exit:(fun pid -> kill pid; Worker.terminate pid) program in
        process := Some session;
        let write = Lwt_unix.of_unix_file_descr ~blocking:false session.input in
        input := Some write;
        let read = Lwt_unix.of_unix_file_descr ~blocking:false session.output in
        output := Some read;
        let rec transfer stream bytes offset sending =
          if offset = Bytes.length bytes then Lwt.return_unit else begin
            if Option.fold ~none:false ~some:(fun rss -> rss > Worker.max_rss_mb ())
                (Worker.read_rss_mb session.pid) then raise (Exec_resource.Unavailable Exec_resource.Memory);
            let* () = Lwt.pick [(if sending then Lwt_unix.wait_write stream else Lwt_unix.wait_read stream);
              Lwt_unix.sleep 0.1] in
            let count = try
              let fd = Lwt_unix.unix_file_descr stream in
              let size = min 65_536 (Bytes.length bytes - offset) in
              let count = if sending then Unix.single_write fd bytes offset size else Unix.read fd bytes offset size in
              if count = 0 then raise End_of_file;
              count
            with Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> 0 in
            transfer stream bytes (offset + count) sending
          end in
        let rec exchange ?(tag = "") raw =
          let* (kind, body) =
              let header = Bytes.create 8 in
              Bytes.set_int64_be header 0 (Int64.of_int (String.length tag + String.length raw));
              let* () = transfer write header 0 true in
              let* () = transfer write (Bytes.unsafe_of_string tag) 0 true in
              let* () = transfer write (Bytes.unsafe_of_string raw) 0 true in
              let* () = transfer read header 0 false in
              let size = Bytes.get_int64_be header 0 in
              if size <= 0L || size > Int64.of_int max_bytes then
                Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
              else
                let kind = Bytes.create 1 in
                let* () = transfer read kind 0 false in
                let kind = Bytes.get kind 0 and size = Int64.to_int size - 1 in
                if kind = '4' && size > Worker.P.max_request_bytes then
                  Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
                else
                let* body = if kind = '4' then
                  let received = Bytes.create size in
                  let* () = transfer read received 0 false in
                  Lwt.return (Bytes.unsafe_to_string received)
                else
                  let received = Buffer.create (min 65_536 size) in
                  let rec receive remaining =
                    if remaining = 0 then Lwt.return (Buffer.contents received) else
                    let bytes = Bytes.create (min 65_536 remaining) in
                    let* () = transfer read bytes 0 false in
                    Buffer.add_bytes received bytes;
                    receive (remaining - Bytes.length bytes) in
                  receive size in
                Lwt.return (kind, body) in
            begin match kind, call with
            | '0', _ -> Lwt.return_ok body
            | '1', _ -> Lwt.return_error (Native.Rejected body)
            | '2', _ -> Lwt.return_error (Native.Unavailable body)
            | '3', Some call ->
              let* response = call body in
              exchange response
            | '4', _ ->
              begin match Worker.P.request_of_string body with
              | Error _ -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
              | Ok request ->
                let* result = Worker.run_joined
                  ?reservation:(Option.bind (Lwt.get active) (fun context -> context.proof))
                  ~priority:(if optional then Worker.Speculative else Required) request in
                begin match Worker.classified_outcome_result result with
                | Ok () -> exchange "0"
                | Error (Worker.Proof_rejected _) -> exchange "1"
                | Error (Worker.Worker_busy | Worker.Worker_memory_exceeded) ->
                  Lwt.fail (Exec_resource.Unavailable Exec_resource.Memory)
                | Error _ -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
                end
              end
            | _ -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
            end in
        let mode = if probe then "P" else match call with Some _ -> "C" | None -> if optional then "V" else "J" in
        exchange ~tag:mode body)) cleanup) (function
        | End_of_file | Unix.Unix_error _ -> Lwt.fail (Exec_resource.Unavailable Exec_resource.Host)
        | error -> Lwt.fail error)))

let run ?call ?optional body = execute ?call ?optional body

let check () =
  Lwt.catch (fun () -> Lwt_unix.with_timeout 5. (fun () ->
    let open Lwt.Syntax in
    let* result = execute ~probe:true "" in
    match result with
    | Ok response when response = protocol -> Lwt.return_ok ()
    | _ -> Lwt.return_error "circle worker response differs"))
    (fun _ -> Lwt.return_error "circle worker check failed")