(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module R = Octra_core.Exec_resource
module V = Octra_vm.Contract_vm
module S = Octra_core.Tx_staging

let check value reason = if not value then failwith reason
let broken name = Array.exists (String.equal name) Sys.argv

let selection () =
  check (Octra_core.Circle_wasm_worker.frames = V.call_depth_max + 1)
    "required memory depth differs";
  let now = ref 0. in
  let clock () = if not (broken "time") then now := !now +. 0.04; !now in
  let execute () =
    let state = V.create_state ~caller:"" ~origin:"" ~address:""
      ~storage:(Hashtbl.create 1) ~value:Z.zero () in
    V.run_async state (Array.make 20 V.NOP) in
  let refused = Lwt_main.run (Lwt.catch (fun () ->
    R.select ~limit:0.1 ~clock ~running:(fun () -> true) (fun () ->
      R.run ~hash:"slow" (fun () -> Lwt.map (fun _ -> false) (execute ()))))
    (function R.Exhausted ("slow", R.Time) -> Lwt.return_true | error -> Lwt.fail error)) in
  check refused "selection time was not enforced";
  check (Lwt_main.run (R.run ~hash:"foreign" execute)) "foreign execution inherited deadline";
  now := 0.;
  let waited = Lwt_main.run (R.select ~limit:0.1 ~clock:(fun () -> !now) ~running:(fun () -> true) (fun () ->
    R.run ~hash:"proof" (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt.pause () in
      now := 1000.;
      Lwt.return (R.work (fun () -> true))))) in
  check waited "proof wait consumed execution slice"

let expiry () =
  S.clear ();
  let tx = Octra_core.Transaction.{from = "oct" ^ String.make 44 '1';
    to_ = "oct" ^ String.make 44 '2'; amount = Z.of_int 30; ou = Z.of_int 1000;
    nonce = 1; timestamp = 0.; signature = "sig"; public_key = None; message = None;
    encrypted_data = None; op_type = Standard} in
  let lookup _ = Some (Z.of_int 2000, 0) in
  ignore (S.add_smart ~lookup tx |> Result.get_ok);
  let entry = Hashtbl.find S.hash_index (Octra_core.Transaction.hash tx) in
  let records = ref [] and notifications = ref 0 in
  let sweep now =
    if broken "expiry" then 0 else Octra_node_runtime.Staging_expiry.sweep
      ~now ~confirmed_nonce:(fun _ -> Some 0) ~save:(fun rows -> records := !records @ rows)
      ~notify:(fun () -> incr notifications) () in
  check (sweep (entry.added_at +. S.staging_ttl -. 1.) = 0) "entry expired early";
  check (sweep (entry.added_at +. S.staging_ttl +. 1.) = 1) "entry survived TTL";
  check (!notifications = 1 && List.length !records = 1) "expiry was not published";
  check (not (Hashtbl.mem S.virtual_balances tx.from)
    && not (Hashtbl.mem S.virtual_nonces tx.from)) "virtual account remained reserved";
  check (Result.is_ok (S.add_smart ~lookup tx)) "expired nonce or balance retained";
  S.clear ()

let expiry_errors () =
  List.iter (fun failure ->
    S.clear ();
    let tx = Octra_core.Transaction.{from = "oct" ^ String.make 44 '1';
      to_ = "oct" ^ String.make 44 '2'; amount = Z.of_int 30; ou = Z.of_int 1000;
      nonce = 1; timestamp = 0.; signature = "sig"; public_key = None; message = None;
      encrypted_data = None; op_type = Standard} in
    let ticks = ref 0 and saves = ref 0 and notifications = ref 0 in
    let sleep seconds =
      check (seconds = 30.) "expiry retry interval differs";
      incr ticks;
      if !ticks = 3 then Lwt.fail Lwt.Canceled else begin
        check (Result.is_ok (S.add_smart ~lookup:(fun _ -> Some (Z.of_int 2000, 0)) tx))
          "expiry retry retained entry";
        Lwt.pause ()
      end in
    let save rows =
      incr saves;
      check (List.length rows = 1) "expiry retry lost rows";
      if failure = "save" && !saves = 1 then failwith "save refused" in
    let notify () =
      incr notifications;
      if failure = "notify" && !notifications = 1 then failwith "notify refused" in
    let stopped = Lwt_main.run (Lwt.catch (fun () ->
      Lwt.map (fun () -> false) (Octra_node_runtime.Staging_expiry.run ~sleep
        ~now:(fun () -> Unix.gettimeofday () +. S.staging_ttl +. 1.)
        ~confirmed_nonce:(fun _ -> Some 0) ~save ~notify ()))
      (function Lwt.Canceled -> Lwt.return_true | error -> Lwt.fail error)) in
    check (stopped && !ticks = 3 && !saves = 2) "expiry loop stopped after error";
    check (!notifications = if failure = "save" then 1 else 2) "expiry notify retry differs";
    S.clear ()) ["save"; "notify"]

let preparation () =
  let now = ref 0. in
  let action () = now := !now +. 1.; Error "compiler refused" in
  let refused = Lwt_main.run (Lwt.catch (fun () ->
    R.select ~limit:0.1 ~clock:(fun () -> !now) ~running:(fun () -> true) (fun () ->
      R.run ~hash:"prepare" (fun () ->
        let open Lwt.Syntax in
        let* () = Lwt.pause () in
        let* _ = if broken "prepare" then Lwt.return (action ())
          else R.detach ~launch:(fun f x -> Lwt.return (f x)) action () in
        Lwt.return_false)))
    (function R.Exhausted ("prepare", R.Time) -> Lwt.return_true | error -> Lwt.fail error)) in
  check refused "preparation bypassed execution slice";
  let state, _ = S.Preview.step ~limit:1 S.Preview.empty
    (S.Preview.Refuse {head = 4L; epoch = 5L; hash = "refused"}) in
  let state, _ = S.Preview.step ~limit:1 state S.Preview.Sweep in
  let _, result = S.Preview.step ~limit:1 state (S.Preview.Check (Some 4L, "refused")) in
  check (Result.is_error result) "expiry forgot refused hash"

let worker_context () =
  let now = ref 0. in
  let delayed = ref (fun () -> failwith "worker not queued") in
  let launch action input =
    let result, wake = Lwt.wait () in
    delayed := (fun () ->
      try Lwt.wakeup wake (action input)
      with error -> Lwt.wakeup_exn wake error);
    result in
  let action () = now := 1.; true in
  let worker = R.select ~limit:0.1 ~clock:(fun () -> 0.) ~running:(fun () -> true) (fun () ->
    R.run ~hash:"owner" (fun () -> R.detach ~launch (fun () ->
      if broken "context" then R.protect action () else action ()) ())) in
  let other = R.select ~limit:0.1 ~clock:(fun () -> !now) ~running:(fun () -> true) (fun () ->
    R.run ~hash:"other" (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt.pause () in
      !delayed ();
      Lwt.return_true)) in
  let passed = Lwt_main.run (Lwt.catch
    (fun () -> Lwt.map (fun (a, b) -> a && b) (Lwt.both worker other))
    (function R.Exhausted ("owner", R.Time) -> Lwt.return_false
      | error -> Lwt.fail error)) in
  check passed "worker charged another transaction"

let proposal_context () =
  List.iter (fun live ->
    let current = ref true in
    let completed = ref false in
    let work () = R.select ~running:(fun () -> !current) (fun () ->
      R.run ~hash:"proposal" (fun () ->
        let open Lwt.Syntax in
        let* value = R.detach ~launch:(fun action input ->
          let* () = Lwt.pause () in
          current := live;
          Lwt.return (action input)) (fun () -> 7) () in
        completed := true;
        Lwt.return (value = 7))) in
    let accepted = Lwt_main.run (Lwt.catch work
      (function R.Exhausted ("proposal", R.Time) -> Lwt.return_false
        | error -> Lwt.fail error)) in
    check (accepted = live && !completed = live) "closed context accepted worker") [true; false];
  List.iter (fun limit ->
    let refused = try
      ignore (R.select ~limit ~running:(fun () -> true) (fun () -> Lwt.return_unit)); false
      with Invalid_argument _ -> true in
    check refused "invalid selection limit accepted") [0.; -1.; infinity; neg_infinity; nan]

let duration_cause () =
  let observe ~allowance ~seconds ~live ~resource route =
    let now = ref 0. and current = ref true in
    let action () =
      now := seconds;
      current := live;
      Option.iter (fun value -> raise (R.Unavailable value)) resource in
    Lwt_main.run (Lwt.catch (fun () ->
      R.select ~limit:4. ~scale:(fun _ _ -> allowance)
        ~clock:(fun () -> !now) ~running:(fun () -> !current) (fun () ->
          R.run ~hash:"duration" (fun () ->
            let open Lwt.Syntax in
            let* () = Lwt.pause () in
            let* () = match route with
              | "sync" -> Lwt.return (R.work action)
              | "async" -> R.interrupt (fun () -> action (); Lwt.return_unit)
              | "thread" -> R.detach ~launch:(fun f x -> Lwt.return (f x)) action ()
              | _ -> failwith "duration route differs" in
            Lwt.return `Done))) (function
        | R.Exhausted ("duration", resource) -> Lwt.return (`Operation resource)
        | R.Window_expired "duration" -> Lwt.return `Completion
        | Invalid_argument reason when reason = "execution duration invalid" -> Lwt.return `Invalid
        | error -> Lwt.fail error)) in
  List.iter (fun route ->
    List.iter (fun allowance ->
      let call = observe ~allowance ~resource:None route in
      check (call ~seconds:0.5 ~live:true = `Done) "duration rejected completed work";
      check (call ~seconds:1. ~live:true = match allowance with
        | R.Operation _ -> `Operation R.Time | R.Completion _ -> `Completion)
        "duration exhaustion cause differs";
      check (call ~seconds:1. ~live:false = `Operation R.Time)
        "duration classified closed context as completion";
      List.iter (fun resource ->
        check (observe ~allowance ~seconds:0.5 ~live:true ~resource:(Some resource) route
          = `Operation resource) "duration changed resource cause") [R.Memory; R.Stack; R.Host])
      [R.Operation 1.; R.Completion 1.];
    List.iter (fun seconds ->
      List.iter (fun allowance ->
        check (observe ~allowance ~seconds:0. ~live:true ~resource:None route = `Invalid)
          "duration accepted invalid allowance") [R.Operation seconds; R.Completion seconds])
      [0.; -1.; infinity; neg_infinity; nan]) ["sync"; "async"; "thread"];
  let defer _ _ = failwith "window used resource hold" in
  let (), partial = Lwt_main.run (R.preview ~accept:(fun () -> true) ~defer (fun () ->
    check (not (R.omit "missing")) "window omitted without owner";
    Lwt.return_unit)) in
  check (not partial) "missing window owner changed selection";
  List.iter (fun available ->
    let omitted = ref [] in
    let (), partial = Lwt_main.run (R.preview ~accept:(fun () -> available)
      ~replay:(fun _ -> true) ~defer
      ~omit:(fun hash -> omitted := hash :: !omitted) (fun () ->
        check (R.omit "selected") "window owner missing";
        if available then
          check (R.next ~hash:"completed" () && R.next ())
            "partial window discarded remaining work"
        else begin
          check (not (R.next ~hash:"completed" ()) && not (R.next ()))
            "completed window admitted more work";
          check (not (R.omit "selected") && not (R.defer "another" R.Time))
            "completed window repeated effect"
        end;
        Lwt.return_unit)) in
    check (partial && !omitted = ["selected"]) "window omitted wrong selection")
    [false; true]

let () =
  selection ();
  expiry ();
  expiry_errors ();
  preparation ();
  worker_context ();
  proposal_context ();
  duration_cause ();
  Printf.printf "event = vm_governor status = passed\n"