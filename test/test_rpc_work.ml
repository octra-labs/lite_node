(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Work = Octra_core.Rpc_work
module Rpc = Octra_vm.Contract_rpc
module Read = Octra_vm.Source_read

type route = Source | Compile | Verify | View
type outcome = Finished | Refused
type control = {entered : bool Atomic.t; release : bool Atomic.t}

let expect reason condition = if not condition then failwith reason
let control () = {entered = Atomic.make false; release = Atomic.make false}
let stop value = Atomic.set value.release true

let action value () =
  Atomic.set value.entered true;
  while not (Atomic.get value.release) do Thread.delay 0.001 done

let wait test =
  let rec loop () =
    if test () then Lwt.return_unit
    else Lwt.bind (Lwt_unix.sleep 0.001) loop in
  loop ()

let run work = Lwt_main.run (Lwt.pick [work;
  Lwt.bind (Lwt_unix.sleep 120.) (fun () -> Lwt.fail_with "RPC work watchdog expired")])

let input =
  let source = "program Read { public fn run(): int { return 7 } }" in
  let result = Rpc.aml_result ~loops:true ~syntax:Octra_vm.Oct_gen.Source source
    |> Result.get_ok in
  Read.{stored = Some (Base64.encode_exn result.bytecode);
    record = Octra_vm.Program_record.make ~source ~code:result.bytecode result}

let epoch_tx =
  let module P = Octra_vm.Program_package in
  let package = P.compile_at ~loops:true ~compiler:P.Source ~point_ops:true ~main:"main.aml"
    ~sources:[P.{path = "main.aml"; body = "program Read { public fn run(): int { return 7 } }"}]
    |> Result.map_error P.error_message |> Result.get_ok in
  Octra_core.Transaction.{from = "oct" ^ String.make 44 '1'; to_ = "oct" ^ String.make 44 '2';
    amount = Z.zero; nonce = 1; ou = Z.of_int 1_000_000; timestamp = 170.;
    signature = ""; public_key = None; message = None; op_type = ProgramDeploy;
    encrypted_data = Some (Base64.encode_exn package.package)}

let launch ?(seconds = 120.) ?(on_stop = Fun.id) route value =
  let open Lwt.Syntax in
  let result = function
    | Ok _ -> Finished
    | Error error when error.Octra_core.Rpc.code = -32005 -> Refused
    | Error _ -> failwith "RPC returned an unrelated error" in
  match route with
  | Source ->
    let reader = Read.create ~check:(fun _ ->
      let+ () = Octra_core.Exec_resource.detach (action value) () in Read.Checked) () in
    let+ outcome = Read.query reader input in
    begin match outcome with
    | Read.Checked -> Finished
    | Read.Pending -> Refused
    | Read.Different | Read.Failed -> failwith "source check failed"
    end
  | Compile ->
    let+ outcome = Octra_node_runtime.Program_read_rpc.compile_rpc
      (fun () -> action value (); Lwt.return_ok `Null) () in result outcome
  | Verify ->
    let+ outcome = Rpc.run_verify (fun () ->
      let+ () = Octra_core.Exec_resource.detach (action value) () in Ok ()) in result outcome
  | View ->
    let+ outcome = Rpc.run_view ~seconds ~stop:on_stop (fun () ->
      let task = Octra_core.Exec_resource.detach (action value) () in
      Lwt.finalize (fun () -> Lwt.protected task)
        (fun () -> Lwt.map ignore (Lwt.protected task))) in result outcome

let routes = [Source; Compile; Verify]
let name = function Source -> "source" | Compile -> "compile" | Verify -> "verify" | View -> "view"

let refused () =
  List.iter (fun route ->
    let value = control () in
    stop value;
    expect "public work exceeded shared capacity" (run (launch route value) = Refused);
    expect "refused work reached a worker" (not (Atomic.get value.entered))) routes

let drain tasks =
  run (Lwt.join (List.map (fun task -> Lwt.catch
    (fun () -> Lwt.map ignore task) (function Lwt.Canceled -> Lwt.return_unit | e -> Lwt.fail e)) tasks));
  let rec ready () =
    let open Lwt.Syntax in
    let* free = Work.run ~busy:(fun () -> Lwt.return false) (fun () ->
      Work.run ~busy:(fun () -> Lwt.return false) (fun () -> Lwt.return true)) in
    if free then Lwt.return_unit
    else let* () = Lwt_unix.sleep 0.001 in ready () in
  run (ready ());
  run (wait (fun () -> not !(Rpc.view_active)))

let test_pairs () =
  List.iter (fun first -> List.iter (fun second ->
    if first <> second || first = Source then begin
      Printf.printf "event = pair first = %s second = %s\n%!" (name first) (name second);
      let left = control () and right = control () and view = control () in
      let first_task = launch first left in
      let second_task = launch second right in
      let view_task = launch View view in
      let tasks = [first_task; second_task; view_task] in
      Fun.protect ~finally:(fun () -> stop left; stop right; stop view; drain tasks) (fun () ->
        expect "heavy pool blocked view" (Lwt.state view_task <> Lwt.Return Refused);
        run (wait (fun () -> Atomic.get left.entered && Atomic.get right.entered
          && Atomic.get view.entered));
        refused ();
        let second_view = control () in
        stop second_view;
        expect "second view bypassed its own capacity" (run (launch View second_view) = Refused);
        expect "refused view reached a worker" (not (Atomic.get second_view.entered));
        expect "epoch work cannot use a reserved worker"
          (run (Octra_core.Exec_resource.detach (fun () -> true) ()));
        let module G = Octra_core.Rule_graph in
        expect "epoch program preparation was blocked by public work"
          (Result.is_ok (run (Octra_node_runtime.Consensus_epoch_vm_shell.prepare_program_package
            ~records:G.Active ~proof_exec:G.Active ~overlap:false ~program_mode:G.Active
            ~point_ops:true epoch_tx))))
    end) routes) routes

let test_cancel () =
  List.iter (fun route ->
    Printf.printf "event = cancel route = %s\n%!" (name route);
    let left = control () and right = control () in
    let tasks = [launch route left; launch Source right] in
    Fun.protect ~finally:(fun () -> stop left; stop right; drain tasks) (fun () ->
      run (wait (fun () -> Atomic.get left.entered && Atomic.get right.entered));
      Lwt.cancel (List.hd tasks);
      run (Lwt.pause ());
      refused ())) routes

let test_view_lifetime cancel =
  Printf.printf "event = view_lifetime cancel = %b\n%!" cancel;
  let value = control () in
  let stopped = ref false in
  let seconds = if cancel then 120. else 0.01 in
  let view = launch ~seconds ~on_stop:(fun () -> stopped := true) View value in
  Fun.protect ~finally:(fun () -> stop value; drain [view]) (fun () ->
    if cancel then begin
      run (wait (fun () -> Atomic.get value.entered));
      Lwt.cancel view
    end;
    run (wait (fun () -> !stopped));
    let next = control () in
    stop next;
    expect "unfinished view released its own capacity" (run (launch View next) = Refused);
    expect "refused view reached a worker" (not (Atomic.get next.entered));
    expect "unfinished view consumed heavy capacity"
      (run (Work.run ~busy:(fun () -> Lwt.return false) (fun () ->
        Work.run ~busy:(fun () -> Lwt.return false) (fun () -> Lwt.return true)))))

let test_public_view path =
  let module Store = Octra_core.Store_irmin in
  let address = "oct" ^ String.make 44 '1' in
  let bytecode = Rpc.aml_result ~loops:true ~syntax:Octra_vm.Oct_gen.Source
    "program Read { public view fn read(): int { return 7 } }"
    |> Result.get_ok |> fun result -> result.bytecode in
  let store = run (Store.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
    run (let open Lwt.Syntax in
      let* () = Store.begin_epoch_batch store in
      let* () = Store.set_meta store "last_epoch" "0" in
      let* () = Store.deploy_contract store ~address ~owner:address ~ctype:"CUSTOM"
        ~version:"1" ~check:"binary"
        ~code_hash:Digestif.SHA256.(digest_string bytecode |> to_hex)
        ~bytecode_b64:(Base64.encode_exn bytecode) in
      Store.commit_epoch_batch store "read seed");
    let root = run (Store.get_commit_hash store) in
    let profile ~epoch = Ok Rpc.{epoch; point_ops = true; math = false;
      object_cost = false; int_work = Octra_vm.Int_work.Active;
      fhe_work = Octra_core.Rule_graph.Prior; proof_exec = Octra_core.Rule_graph.Prior;
      proof_key = Octra_core.Rule_graph.Prior; wasm_float = Octra_core.Rule_graph.Prior} in
    let left = control () and right = control () in
    let tasks = [launch Source left; launch Source right] in
    Fun.protect ~finally:(fun () -> stop left; stop right; drain tasks) (fun () ->
      run (wait (fun () -> Atomic.get left.entered && Atomic.get right.entered));
      let result = run (Rpc.call_params ~trusted:[] ~profile ~store
        ~ledger:(Octra_core.Ledger.create store) ~get_fhe_pubkey:(fun _ -> None)
        ~storage_json:(fun _ -> `Null)
        (`List [`String address; `String "read"; `List []])) in
      expect "public view was blocked by source checks"
        (result = Ok (`Assoc ["result", `String "7"]));
      expect "public view changed the committed root"
        (run (Store.get_commit_hash store) = root)))

let test_state () =
  let first, effect = Work.step Work.initial Work.Start in
  let id = match effect with Work.Enter id -> id | _ -> failwith "first work refused" in
  let second, _ = Work.step first Work.Start in
  let full, effect = Work.step second Work.Start in
  expect "third work changed state" (full = second && effect = Work.Busy);
  let released, _ = Work.step full (Work.Finish id) in
  let next, _ = Work.step released Work.Start in
  let repeat, _ = Work.step next (Work.Finish id) in
  expect "repeated completion released another task" (repeat = next);
  expect "task identifiers reused" (next.next = 3 && List.length next.active = 2)

let () =
  test_state ();
  test_pairs ();
  test_cancel ();
  test_view_lifetime true;
  test_view_lifetime false;
  Test_workspace.with_dir "rpc_public_view" test_public_view;
  let value = control () in
  stop value;
  expect "completed work retained capacity" (run (launch Compile value) = Finished);
  print_endline "status = pass test = rpc_work"