(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module M = Octra_core.Compute_memory
module C = Octra_core.Compute_pool
module P = Octra_core.Pvac_verify_protocol
module W = Octra_core.Pvac_verify_worker
module T = Octra_vm.Fhe_task
module I = Octra_core.Fhe_image
module F = Octra_core.Fhe_calc
open Lwt.Syntax

let checks = ref 0
let need reason value = incr checks; if not value then failwith reason
let quota () = M.create ~limit:100 ~speculative_limit:25 ~slots:16
let take memory speculative bytes = Option.get (M.reserve memory ~speculative bytes)
let usage memory bytes jobs =
  let used = M.stats memory in
  need "memory accounting differs" (used.bytes = bytes && used.jobs = jobs)
let pool memory = C.create ~memory ~capacity:1 ~required_limit:128
  ~speculative_limit:6 ~required_burst:4 ()
let ticket n = Octra_vm.Proof_wait.{request = string_of_int n; generation = "queue"}
let deadline () = Int64.add (Mtime_clock.elapsed_ns ()) 5_000_000_000L
let key = I.{data = "key"; sampling = Pvac_ffi.{rows = 1; columns = 1;
  weight = 1; noise = 1; branches = 1}; words = None; size = None}
let cipher = I.{data = "cipher"; words = None; shape = None}
let wire = P.Range {pubkey = "key"; cipher = "cipher"; proof = "proof"; strict = true}

let model_checks () =
  let memory = quota () in
  let first = take memory true 25 in
  need "speculative reserve lost" (M.reserve memory ~speculative:true 1 = None);
  let second = take memory false 75 in
  need "total limit lost" (M.reserve memory ~speculative:false 1 = None);
  usage memory 100 2;
  M.release first;
  M.release first;
  usage memory 75 1;
  need "speculative bytes retained" ((M.stats memory).speculative = 0);
  let third = take memory false 25 in
  M.release first;
  usage memory 100 2;
  M.release third;
  M.release second;
  usage memory 0 0;
  need "negative bytes accepted" (M.reserve memory ~speculative:false (-1) = None);
  need "size overflow accepted" (M.reserve memory ~speculative:false max_int = None);
  need "size sum wrapped" (M.add max_int 1 = max_int && M.add (-1) 3 = max_int);
  let empty = List.init 16 (fun _ -> take memory false 0) in
  need "zero size slot limit lost" (M.reserve memory ~speculative:false 0 = None);
  List.iter M.release empty;
  usage memory 0 0;
  List.iter (fun (raw, expected) ->
    need "queue configuration differs" (M.limit_of_getenv (fun _ -> raw) = expected * 1_048_576))
    [None, 512; Some "64", 64; Some "4096", 4096; Some "0", 512;
     Some "4097", 512; Some "x", 512; Some "999999999999999999999", 512];
  let random = Random.State.make [|517|] in
  let live = ref [] in
  for _ = 1 to 5000 do
    if Random.State.bool random && !live <> [] then begin
      let held, _, _ = List.hd !live in
      live := List.tl !live;
      M.release held;
      M.release held
    end else begin
      let bytes = Random.State.int random 120 in
      let speculative = Random.State.bool random in
      let total = List.fold_left (fun n (_, b, _) -> n + b) 0 !live in
      let views = List.fold_left (fun n (_, b, s) -> n + if s then b else 0) 0 !live in
      let allowed = total + bytes <= 100 && List.length !live < 16
        && (not speculative || views + bytes <= 25) in
      let held = M.reserve memory ~speculative bytes in
      need "model refusal differs" (Option.is_some held = allowed);
      Option.iter (fun held -> live := (held, bytes, speculative) :: !live) held
    end;
    usage memory (List.fold_left (fun n (_, b, _) -> n + b) 0 !live) (List.length !live)
  done;
  List.iter (fun (held, _, _) -> M.release held) !live;
  usage memory 0 0;
  let failed = Atomic.make false in
  let threads = List.init 4 (fun _ -> Thread.create (fun () ->
    for _ = 1 to 1000 do
      match M.reserve memory ~speculative:false 25 with
      | None -> Thread.yield ()
      | Some held ->
        let used = M.stats memory in
        if used.bytes < 25 || used.bytes > 100 then Atomic.set failed true;
        Thread.yield ();
        M.release held;
        M.release held
    done) ()) in
  List.iter Thread.join threads;
  need "concurrent quota differs" (not (Atomic.get failed));
  usage memory 0 0

let size_checks () =
  let pubkey, cipher, proof, commitment = "key", "cipher", "proof", "commitment" in
  let size = 3 + 6 + 5 + 10 in
  let cases = P.[Ping, 0; Groth16 {key = pubkey; proof; inputs = cipher}, 14;
    Encrypt {pubkey; cipher; proof; commitment; blinding = "blind"; amount = Z.one; strict = true},
      size + 5 + Z.size Z.one * (Sys.word_size / 8);
    Claim {pubkey; cipher; proof; commitment; strict = false}, size;
    Key_switch_claim {pubkey; cipher; proof; commitment; strict = false}, size;
    Stored_key_claim {pubkey; cipher; proof; commitment; strict = true}, size;
    Historical_migration_claim {pubkey; cipher; proof; commitment; strict = true}, size;
    Range {pubkey; cipher; proof; strict = true}, 14;
    Zero {pubkey; cipher; proof}, 14;
    Range_bound {pubkey; cipher; proof; commitment; strict = true}, size;
    Circle_cell {pubkey; cipher; proof; ciphertext_commitment = commitment;
      proof_kind = Circle_range; amount_commitment = commitment; strict = false}, size + 10] in
  List.iter (fun (request, expected) ->
    need "proof input size differs" (P.input_bytes request = expected);
    need "proof wrapper size differs" (P.input_bytes (P.Math (P.Proof_key request)) = expected)) cases;
  let rec wrap n request = if n = 0 then request else wrap (n - 1) (P.Math request) in
  need "nested input count differs" (P.input_bytes (wrap 10000 wire) = 14);
  let cipher = I.{data = cipher; words = None; shape = None} in
  let cases = T.[Load_key "abc", 3; Read_key "abc", 3; Write_secret "secret", 6;
    Read_cipher (true, true, "cipher"), 6; Write_key key, 3; Write_cipher cipher, 6;
    Add (key, cipher, cipher), 15; Sub (key, cipher, cipher), 15;
    Mul (true, true, key, cipher, cipher, "seed"), 19;
    Scale (true, key, cipher, 1L), 9; Divide (key, cipher, 1L), 9;
    Add_int (true, key, cipher, 1L), 9; Sub_int (true, key, cipher, 1L), 9;
    Commit (key, cipher), 9; Pack (Zero, key, cipher, proof), 14;
    Pack (Range, key, cipher, proof), 14; Pack (Claim commitment, key, cipher, proof), size;
    Verify (true, wire), 14] in
  List.iter (fun (request, expected) -> need "FHE input size differs" (T.input_bytes request = expected)) cases

let rec until n condition =
  if condition () then Lwt.return_unit
  else if n = 0 then Lwt.fail_with "completion did not arrive"
  else let* () = Lwt_unix.sleep 0.005 in until (n - 1) condition

let pool_checks () =
  let memory = quota () in
  let left, right = pool memory, pool memory in
  let ready, finish = Lwt.task () in
  let active = C.run_async ~bytes:60 left C.Required (fun () -> ready) in
  let waiting = C.run_async ~bytes:25 left C.Speculative (fun () -> Lwt.return_unit) in
  let other = C.run_async ~bytes:15 right C.Required (fun () -> Lwt.protected ready) in
  usage memory 100 3;
  let* denied = C.run_async ~bytes:1 right C.Required (fun () -> failwith "denied task ran") in
  need "shared pool total lost" (denied = None);
  need "sync quota bypass" (C.run_sync ~bytes:1 right C.Required (fun () -> ()) = None);
  need "try sync quota bypass" (C.try_run_sync ~bytes:1 right C.Required (fun () -> ()) = None);
  Lwt.cancel waiting;
  usage memory 75 2;
  need "busy try sync ran" (C.try_run_sync ~bytes:20 left C.Required (fun () -> ()) = None);
  usage memory 75 2;
  Lwt.cancel active;
  usage memory 75 2;
  Lwt.wakeup finish ();
  let* _ = other in
  usage memory 0 0;
  let* () = Lwt.catch (fun () ->
    let* _ = C.run_async ~bytes:100 left C.Required (fun () -> failwith "test error") in
    Lwt.fail_with "exception absent") (function Failure message when message = "test error" -> Lwt.return_unit | e -> Lwt.fail e) in
  usage memory 0 0;
  (try ignore (C.run_sync ~bytes:100 left C.Required (fun () -> failwith "test error"))
   with Failure message when message = "test error" -> () | e -> raise e);
  usage memory 0 0;
  let started, finish = Atomic.make false, Atomic.make false in
  let threaded = C.run_threaded ~bytes:100 left C.Required (fun () ->
    Atomic.set started true;
    while not (Atomic.get finish) do Thread.delay 0.001 done) () in
  let* () = until 200 (fun () -> Atomic.get started) in
  Lwt.cancel threaded;
  usage memory 100 1;
  Atomic.set finish true;
  let* () = until 200 (fun () -> (M.stats memory).jobs = 0) in
  usage memory 0 0;
  let sync_started, sync_finish = Atomic.make false, Atomic.make false in
  let sync = Thread.create (fun () -> ignore (C.run_sync ~bytes:60 left C.Required (fun () ->
    Atomic.set sync_started true;
    while not (Atomic.get sync_finish) do Thread.delay 0.001 done))) () in
  let* () = until 200 (fun () -> Atomic.get sync_started) in
  let waiting = C.run_async ~bytes:40 left C.Required (fun () -> Lwt.return_unit) in
  usage memory 100 2;
  Atomic.set sync_finish true;
  let* _ = waiting in
  Thread.join sync;
  usage memory 0 0;
  Lwt.return_unit

let reserve_checks () =
  let memory = quota () in
  let pool = C.create ~memory ~reserved_limit:1 ~capacity:1 ~required_limit:2
    ~speculative_limit:2 ~required_burst:1 () in
  need "negative credit accepted" (C.reserve ~memory ~bytes:(-1) pool = None);
  need "oversize credit accepted" (C.reserve ~memory ~bytes:101 pool = None);
  let credit = C.reserve ~memory ~bytes:60 pool |> Option.get in
  need "credit count exceeded" (C.reserve ~memory ~bytes:0 pool = None);
  usage memory 60 1;
  let other = C.create ~memory ~capacity:1 ~required_limit:2
    ~speculative_limit:2 ~required_burst:1 () in
  let no_run () = failwith "invalid credit ran" in
  let* wrong = C.run_async ~reservation:credit ~bytes:1 other C.Required no_run in
  need "foreign credit accepted" (wrong = None);
  let* wrong = C.run_async ~reservation:credit ~bytes:1 pool C.Speculative no_run in
  need "speculative credit accepted" (wrong = None);
  let* wrong = C.run_async ~reservation:credit pool C.Required no_run in
  need "unmeasured credit accepted" (wrong = None);
  let* wrong = C.run_async ~reservation:credit ~bytes:61 pool C.Required no_run in
  need "credit byte cap lost" (wrong = None);
  let ready, release = Lwt.wait () in
  let active = C.run_async ~bytes:20 pool C.Required (fun () -> ready) in
  let queued = C.run_async ~bytes:20 pool C.Required (fun () -> Lwt.return_unit) in
  let full = C.run_async ~bytes:0 pool C.Required no_run in
  need "ordinary queue took credit" (Lwt.state full = Lwt.Return None);
  let waiting = C.run_async ~reservation:credit ~bytes:60 pool C.Required no_run in
  need "reserved queue entry lost" (Lwt.is_sleeping waiting && (C.stats pool).required_waiting = 2);
  usage memory 100 3;
  let duplicate = C.run_async ~reservation:credit ~bytes:0 pool C.Required no_run in
  need "credit borrowed twice" (Lwt.state duplicate = Lwt.Return None);
  Lwt.cancel waiting;
  need "cancelled credit occupied queue" ((C.stats pool).required_waiting = 1);
  usage memory 100 3;
  let waiting = C.run_async ~reservation:credit ~bytes:60 pool C.Required (fun () -> Lwt.return 42) in
  C.close_reservation credit;
  C.close_reservation credit;
  usage memory 100 3;
  need "closing credit reused" (C.reserve ~memory ~bytes:0 pool = None);
  Lwt.wakeup_later release ();
  let* _ = active in
  let* _ = queued in
  let* result = waiting in
  need "reserved work lost" (result = Some 42);
  usage memory 0 0;
  let* duplicate = C.run_async ~reservation:credit ~bytes:0 pool C.Required no_run in
  need "closed credit revived" (duplicate = None);
  let credit = C.reserve ~memory ~bytes:60 pool |> Option.get in
  let* raised = Lwt.catch (fun () ->
    let* _ = C.run_async ~reservation:credit ~bytes:60 pool C.Required
      (fun () -> Lwt.fail_with "reserved error") in Lwt.return_false)
    (function Failure reason when reason = "reserved error" -> Lwt.return_true | error -> Lwt.fail error) in
  need "reserved exception hidden" raised;
  usage memory 60 1;
  let ready, release = Lwt.wait () in
  let active = C.run_async ~reservation:credit ~bytes:60 pool C.Required (fun () -> ready) in
  let duplicate = C.run_async ~reservation:credit ~bytes:0 pool C.Required no_run in
  need "active credit borrowed twice" (Lwt.state duplicate = Lwt.Return None);
  Lwt.cancel active;
  C.close_reservation credit;
  usage memory 60 1;
  need "active credit released" (C.reserve ~memory ~bytes:0 pool = None);
  Lwt.wakeup_later release ();
  let* () = until 200 (fun () -> (M.stats memory).jobs = 0) in
  usage memory 0 0;
  let credit = C.reserve ~memory ~bytes:100 pool |> Option.get in
  let* () = Lwt_list.iter_s (fun (delayed, fails) ->
    let ready, release = Lwt.wait () in
    let finish () = if fails then Lwt.wakeup_exn release (Failure "reserved error")
      else Lwt.wakeup release () in
    if not delayed then finish ();
    let first = C.run_async ~reservation:credit ~bytes:100 pool C.Required (fun () -> ready) in
    let next () = C.run_async ~reservation:credit ~bytes:100 pool C.Required
      (fun () -> Lwt.return 42) in
    let sequence = Lwt.try_bind (fun () -> first)
      (fun result -> need "reserved result differs" (not fails && result = Some ()); next ())
      (function Failure reason when fails && reason = "reserved error" -> next ()
        | error -> Lwt.fail error) in
    if delayed then finish ();
    let* value = sequence in
    need "completion preceded credit return" (value = Some 42);
    usage memory 100 1;
    Lwt.return_unit) [false, false; false, true; true, false; true, true] in
  C.close_reservation credit;
  C.close_reservation credit;
  usage memory 0 0;
  Lwt.return_unit

let move_checks () =
  let queued = quota () and running = quota () in
  let channel = pool queued in
  let no_run () = failwith "unreserved running input executed" in
  let* missing = C.run_async ~running_memory:running channel C.Required no_run in
  need "unmeasured input moved" (missing = None);
  let ready, finish = Lwt.wait () in
  let active = C.run_async ~bytes:60 ~running_memory:running channel C.Required (fun () -> ready) in
  usage queued 0 0;
  usage running 60 1;
  let pending = C.run_async ~bytes:60 ~running_memory:running channel C.Required
    (fun () -> Lwt.return 42) in
  usage queued 60 1;
  usage running 60 1;
  let* refused = C.run_async ~bytes:41 ~running_memory:running channel C.Required no_run in
  need "waiting input was not charged" (refused = None);
  Lwt.cancel pending;
  usage queued 0 0;
  usage running 60 1;
  let pending = C.run_async ~bytes:60 ~running_memory:running channel C.Required
    (fun () -> Lwt.return 42) in
  Lwt.cancel active;
  usage queued 60 1;
  usage running 60 1;
  Lwt.wakeup_later finish ();
  let* result = pending in
  need "moved input lost result" (result = Some 42);
  usage queued 0 0;
  usage running 0 0;
  let held = take running false 80 in
  let* refused = C.run_async ~bytes:60 ~running_memory:running channel C.Required no_run in
  need "running input capacity ignored" (refused = None);
  usage queued 0 0;
  usage running 80 1;
  need "failed move retained slot" ((C.stats channel).active = 0);
  M.release held;
  let* raised = Lwt.catch (fun () ->
    let* _ = C.run_async ~bytes:60 ~running_memory:running channel C.Required
      (fun () -> failwith "move error") in Lwt.return_false)
    (function Failure reason when reason = "move error" -> Lwt.return_true | error -> Lwt.fail error) in
  need "moved input error hidden" raised;
  usage queued 0 0;
  usage running 0 0;
  let* same = C.run_async ~bytes:60 ~running_memory:queued channel C.Required (fun () -> Lwt.return 42) in
  need "same memory move differs" (same = Some 42);
  usage queued 0 0;
  let started, finish = Atomic.make false, Atomic.make false in
  let sync = Thread.create (fun () -> ignore (C.run_sync ~bytes:20 channel C.Required (fun () ->
    Atomic.set started true;
    while not (Atomic.get finish) do Thread.delay 0.001 done))) () in
  let ran = ref false in
  let granted = Fun.protect ~finally:(fun () -> Atomic.set finish true; Thread.join sync) (fun () ->
    let rec await n =
      if Atomic.get started then ()
      else if n = 0 then failwith "sync work did not start"
      else (Thread.delay 0.001; await (n - 1)) in
    await 2000;
    C.run_async ~bytes:60 ~running_memory:running channel C.Required
      (fun () -> ran := true; Lwt.return_unit)) in
  usage queued 60 1;
  usage running 0 0;
  need "grant was not issued" ((C.stats channel).active = 1
    && (C.stats channel).required_waiting = 0 && Lwt.is_sleeping granted);
  Lwt.cancel granted;
  need "granted cancellation retained input" ((M.stats queued).bytes = 0
    && (M.stats queued).jobs = 0 && (C.stats channel).active = 0);
  usage running 0 0;
  let* () = Lwt.pause () in
  need "cancelled grant executed" (not !ran);
  let* next = C.run_async ~bytes:60 ~running_memory:running channel C.Required
    (fun () -> Lwt.return 42) in
  need "cancelled grant lost progress" (next = Some 42);
  usage queued 0 0;
  usage running 0 0;
  let views = M.create ~limit:100 ~speculative_limit:10 ~slots:16 in
  let* refused = C.run_async ~bytes:20 ~running_memory:views channel C.Speculative no_run in
  need "speculative move ignored quota" (refused = None);
  usage queued 0 0;
  usage views 0 0;
  let ready, finish = Lwt.task () in
  let active = C.run_async ~bytes:10 ~running_memory:views channel C.Speculative (fun () -> ready) in
  usage queued 0 0;
  usage views 10 1;
  need "speculative charge lost" ((M.stats queued).speculative = 0
    && (M.stats views).speculative = 10);
  Lwt.wakeup_later finish 42;
  let* value = active in
  need "speculative result differs" (value = Some 42);
  usage views 0 0;
  need "speculative charge retained" ((M.stats views).speculative = 0);
  let ready, finish = Lwt.task () in
  let active = C.run_async ~bytes:60 ~running_memory:running channel C.Required (fun () -> ready) in
  let raised = Lwt.catch (fun () -> let* _ = active in Lwt.return_false)
    (function Failure reason when reason = "move error" -> Lwt.return_true | error -> Lwt.fail error) in
  usage queued 0 0;
  usage running 60 1;
  Lwt.wakeup_later_exn finish (Failure "move error");
  let* raised = raised in
  need "delayed move error hidden" raised;
  usage queued 0 0;
  usage running 0 0;
  let channel = C.create ~memory:queued ~reserved_limit:1 ~capacity:1 ~required_limit:2
    ~speculative_limit:2 ~required_burst:1 () in
  let credit = C.reserve ~memory:queued ~bytes:60 channel |> Option.get in
  let* refused = C.run_async ~reservation:credit ~bytes:60 ~running_memory:running
    channel C.Required no_run in
  need "proof credit moved" (refused = None);
  usage queued 60 1;
  usage running 0 0;
  C.close_reservation credit;
  usage queued 0 0;
  Lwt.return_unit

let worker_checks () =
  let limit = M.limit_of_getenv Sys.getenv_opt in
  let hold = take M.shared false (limit - 13) in
  need "sync worker uncharged" (W.run_sync wire = W.Busy);
  need "try worker uncharged" (W.try_run_sync wire = W.Busy);
  let* result = W.run wire in
  need "async worker uncharged" (result = W.Busy);
  need "busy became invalid" (W.classified_outcome_result result = Error W.Worker_busy);
  need "direct FHE uncharged" (T.direct (T.local ()) (T.Write_secret (String.make 14 'x'))
    = Error (T.Resource Memory));
  need "sync FHE uncharged" (T.eval (T.Write_secret (String.make 14 'x'))
    = Error (T.Resource Memory));
  let* local = T.run ~ticket:(ticket 1) ~deadline:(deadline ()) (T.Write_secret (String.make 14 'x')) in
  need "local preparation uncharged" (local = Error (T.Resource Memory));
  M.release hold;
  let* local = T.run ~ticket:(ticket 2) ~deadline:(deadline ()) (T.Write_secret "abc") in
  need "local recovery differs" (local = Ok (T.Text "YWJj"));
  usage M.shared 0 0;
  let active = W.run wire in
  usage M.shared 14 1;
  Lwt.cancel active;
  usage M.shared 14 1;
  let* () = until 400 (fun () -> (M.stats M.shared).jobs = 0) in
  usage M.shared 0 0;
  Lwt.return_unit

let task_checks () =
  let module Pool = Octra_core.Proof_pool in
  let limit = M.limit_of_getenv Sys.getenv_opt in
  let held = take M.shared false (limit - 16) in
  let ran = Atomic.make false in
  let run () = Atomic.set ran true in
  let* () = Lwt.finalize (fun () ->
    let* denied = Pool.try_run ~bytes:17 run in
    need "proof task bypassed memory" (denied = None && not (Atomic.get ran));
    let* refused = Lwt.catch (fun () ->
      let* () = Pool.run ~bytes:17 run in
      Lwt.return_false) (function
        | Octra_core.Exec_resource.Unavailable Host -> Lwt.return_true
        | _ -> Lwt.return_false) in
    need "proof capacity became verdict" (refused && not (Atomic.get ran));
    usage M.shared (limit - 16) 1;
    Lwt.return_unit)
    (fun () -> M.release held; Lwt.return_unit) in
  let* denied = Pool.try_run ~priority:Pool.Speculative ~bytes:(limit / 4 + 1) run in
  need "proof task bypassed view quota" (denied = None && not (Atomic.get ran));
  let* denied = Pool.try_run ~bytes:(-1) run in
  need "proof task accepted negative size" (denied = None && not (Atomic.get ran));
  usage M.shared 0 0;
  let finish = Atomic.make false in
  let starts = List.init Pool.capacity (fun _ -> Atomic.make false) in
  let tasks = List.map (fun started -> Pool.try_run ~bytes:7 (fun () ->
    Atomic.set started true;
    while not (Atomic.get finish) do Thread.delay 0.001 done)) starts in
  let* () = Lwt.finalize (fun () ->
    let* () = until 400 (fun () -> List.for_all Atomic.get starts) in
    let pending = Pool.try_run ~bytes:11 run in
    usage M.shared (7 * Pool.capacity + 11) (Pool.capacity + 1);
    Lwt.cancel pending;
    usage M.shared (7 * Pool.capacity) Pool.capacity;
    List.iter Lwt.cancel tasks;
    usage M.shared (7 * Pool.capacity) Pool.capacity;
    need "cancelled proof task ran" (not (Atomic.get ran));
    Lwt.return_unit) (fun () ->
      Atomic.set finish true;
      until 400 (fun () -> (M.stats M.shared).jobs = 0)) in
  let* raised = Lwt.catch (fun () ->
    let* _ = Pool.run ~bytes:16 (fun () -> failwith "task error") in
    Lwt.return_false) (function
      | Failure reason when reason = "task error" -> Lwt.return_true
      | error -> Lwt.fail error) in
  need "proof task exception hidden" raised;
  usage M.shared 0 0;
  let* value = Pool.run ~bytes:16 (fun () -> 42) in
  need "proof task recovery differs" (value = 42);
  usage M.shared 0 0;
  Lwt.return_unit

let vm_checks () =
  let module VM = Octra_vm.Contract_vm in
  let module R = Octra_core.Exec_resource in
  let held = take M.shared false (M.limit_of_getenv Sys.getenv_opt - 2) in
  let* () = Lwt_list.iter_s (fun (async, proof_exec) ->
    let ctx = {VM.default_ctx with async_exec = async; math = true;
      proof_exec; fhe_work = proof_exec} in
    let state = VM.create_state ~ctx ~limit:1_000_000 ~caller:"sender"
      ~origin:"sender" ~address:"program" ~value:Z.zero ~storage:(Hashtbl.create 1) () in
    state.regs.(0) <- VM.VPubKey {key with size = Some 3; words = Some 1};
    let code = VM.[|FHE_SER_PK (1, 0); STOP|] in
    let* refused = Lwt.catch (fun () ->
      let* _ = R.run ~hash:"memory" (fun () ->
        if async then VM.run_async state code else Lwt.return (VM.run state code)) in
      Lwt.return_false)
      (function R.Exhausted ("memory", Memory) -> Lwt.return_true | e -> Lwt.fail e) in
    need "VM memory refusal became verdict" refused;
    Lwt.return_unit) [false, Octra_core.Rule_graph.Prior; true, Prior;
      false, Active; true, Active] in
  M.release held;
  usage M.shared 0 0;
  Lwt.return_unit

let actor_checks () =
  let memory = quota () in
  let actor = T.create ~memory () in
  let channel = pool memory in
  let ready, finish = Lwt.task () in
  let held = C.run_async ~bytes:60 channel C.Required (fun () -> ready) in
  let request = T.Commit (key, {cipher with data = String.make 27 'c'}) in
  let active = T.run ~actor ~ticket:(ticket 1) ~deadline:(deadline ()) request in
  let waiting = T.run ~actor ~ticket:(ticket 2) ~deadline:(deadline ()) (T.Read_key (String.make 10 'k')) in
  usage memory 100 3;
  let* denied = T.run ~actor ~ticket:(ticket 3) ~deadline:(deadline ()) (T.Read_key "k") in
  need "actor total limit lost" (denied = Error (T.Resource Memory));
  Lwt.cancel waiting;
  usage memory 90 2;
  Lwt.cancel active;
  usage memory 90 2;
  T.stop actor;
  usage memory 90 2;
  let* () = until 400 (fun () -> (M.stats memory).jobs = 1) in
  usage memory 60 1;
  Lwt.wakeup finish ();
  let* _ = held in
  usage memory 0 0;
  let actor = T.create ~memory () in
  let* denied = T.run ~actor ~urgent:false ~ticket:(ticket 4) ~deadline:(deadline ()) request in
  need "view reserve lost" (denied = Error (T.Resource Memory));
  let* result = T.run ~actor ~ticket:(ticket 5) ~deadline:(deadline ()) request in
  need "actor completion differs" (match result with Ok (T.Text _) -> true | _ -> false);
  usage memory 0 0;
  T.stop actor;
  let memory = M.create ~limit:1000 ~speculative_limit:250 ~slots:20 in
  let actor = T.create ~memory () in
  let jobs = List.init 9 (fun n -> T.run ~actor ~ticket:(ticket n)
    ~deadline:(deadline ()) request) in
  usage memory 270 9;
  let* refused = T.run ~actor ~ticket:(ticket 10) ~deadline:(deadline ()) request in
  need "actor count refusal differs" (refused = Error (T.Resource Host));
  usage memory 270 9;
  T.stop actor;
  usage memory 30 1;
  let* _ = Lwt.all jobs in
  let* () = until 400 (fun () -> (M.stats memory).jobs = 0) in
  usage memory 0 0;
  let actor = T.create ~memory () in
  let near = Int64.add (Mtime_clock.elapsed_ns ()) 20_000_000L in
  let active = T.run ~actor ~ticket:(ticket 20) ~deadline:(deadline ()) request in
  let queued = T.run ~actor ~ticket:(ticket 21) ~deadline:near request in
  let* expired = queued in
  need "queued deadline differs" (expired = Error (T.Resource Host));
  usage memory 30 1;
  let* _ = active in
  usage memory 0 0;
  T.stop actor;
  let actor = T.create ~memory () in
  let* expired = T.run ~actor ~ticket:(ticket 22)
    ~deadline:(Int64.add (Mtime_clock.elapsed_ns ()) 20_000_000L) request in
  need "active deadline differs" (expired = Error (T.Resource Host));
  let* () = until 400 (fun () -> (M.stats memory).jobs = 0) in
  usage memory 0 0;
  T.stop actor;
  let armed = ref false in
  let actor = T.create ~memory ~clock:(fun () ->
    if !armed then failwith "clock" else Mtime_clock.elapsed_ns ()) () in
  let first = T.run ~actor ~ticket:(ticket 30) ~deadline:(deadline ()) request in
  let second = T.run ~actor ~ticket:(ticket 31) ~deadline:(deadline ()) request in
  armed := true;
  let* results = Lwt.all [first; second] in
  need "owner error became verdict" (List.for_all ((=) (Error (T.Resource Host))) results);
  usage memory 0 0;
  T.stop actor;
  Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" "/no/worker";
  let actor = T.create ~memory () in
  let* missing = T.run ~actor ~ticket:(ticket 23) ~deadline:(deadline ()) request in
  need "launch error differs" (missing = Error (T.Resource Host));
  usage memory 0 0;
  T.stop actor;
  Lwt.return_unit

let closing_checks () =
  let memory = quota () in
  let ready, release = Lwt.wait () in
  let reap process = let* () = W.retire_session process in ready in
  let actor = T.create ~memory ~reap () in
  let worker = Sys.executable_name in
  Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker;
  let request = T.Commit (key, cipher) in
  let* result = T.run ~actor ~ticket:(ticket 40) ~deadline:(deadline ()) request in
  need "closing setup differs" (Result.is_ok result);
  Unix.putenv "OCTRA_PVAC_VERIFY_WORKER"
    (Filename.dirname worker ^ "/./" ^ Filename.basename worker);
  let active = T.run ~actor ~ticket:(ticket 41) ~deadline:(deadline ()) request in
  usage memory 9 1;
  Lwt.cancel active;
  usage memory 9 1;
  Lwt.wakeup release ();
  let* () = until 400 (fun () -> (M.stats memory).jobs = 0) in
  usage memory 0 0;
  T.stop actor;
  Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker;
  Lwt.return_unit

let child () =
  let rec loop () =
    let raw = F.read_frame () in
    Unix.sleepf 0.15;
    F.write_frame (F.response_bytes (F.hash raw) (Ok (F.Digest (String.make 32 'd'))));
    loop () in
  try loop () with End_of_file -> ()

let proof_child () =
  let buffer = Buffer.create 1024 in
  (try while true do Buffer.add_char buffer (input_char stdin) done with End_of_file -> ());
  Unix.sleepf 0.15;
  output_string stdout (P.response_bytes {request_hash = P.hash_bytes (Buffer.contents buffer);
    accepted = false; reason = "test response"});
  flush stdout

let () =
  if Sys.getenv_opt "OCTRA_QUEUE_CHILD" = Some "1" then begin
    if Array.length Sys.argv = 2 && Sys.argv.(1) = "--fhe-session" then child ()
    else proof_child ()
  end
  else begin
    model_checks ();
    size_checks ();
    Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" Sys.executable_name;
    Unix.putenv "OCTRA_QUEUE_CHILD" "1";
    Lwt_main.run (let* () = pool_checks () in
      let* () = reserve_checks () in
      let* () = move_checks () in
      let* () = task_checks () in
      let* () = worker_checks () in let* () = vm_checks () in
      let* () = actor_checks () in closing_checks ());
    Printf.printf "event = queue_memory status = passed checks = %d\n%!" !checks
  end