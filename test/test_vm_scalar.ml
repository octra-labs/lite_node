(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_vm.Contract_vm
module W = Octra_vm.Scalar_work
module R = Octra_core.Rule_graph

let check condition reason = if not condition then failwith reason
let broken name = Array.exists (String.equal name) Sys.argv

let state ?(limit = 1_000_000) ?(kinds = []) active =
  V.create_state ~limit ~storage_kinds:kinds
    ~ctx:{V.default_ctx with scalar_work = active && not (broken "scalar");
      byte_work = Some (Octra_vm.Byte_work.create Octra_vm.Byte_work.consensus_limits)}
    ~caller:"" ~origin:"" ~address:"" ~value:Z.zero ~storage:(Hashtbl.create 4) ()

let conversions () =
  let numbers = [Z.zero; Z.one; Z.minus_one; Z.shift_left Z.one 2048] in
  List.iter (fun number ->
    let numeric = [V.VInt number; V.VU64 number; V.VU128 number; V.VU256 number] in
    let values = numeric @ [V.VBool true; V.VBool false; V.VString "01";
      V.VString ""; V.VString (Z.to_string number); V.VAddr (Z.to_string number);
      V.VBytes (Z.to_string number); V.VBytes32 (Z.to_string number)] in
    List.iter (fun left -> List.iter (fun right ->
      check (V.text_equal left right = String.equal (V.to_string left) (V.to_string right))
        "comparison changed") values) numeric) numbers

let equal_effort () =
  let run active operation left right =
    let current = state ~limit:10_000_000 active in
    current.regs.(1) <- left;
    current.regs.(2) <- right;
    let accepted = V.run current [|operation; V.STOP|] in
    accepted, current in
  List.iter (fun (operation, expected) ->
    let accepted, narrow = run false operation (V.VInt Z.one) (V.VString "1") in
    check (accepted && narrow.regs.(0) = V.VBool expected) "narrow comparison refused";
    List.iter (fun bits ->
      let number = Z.shift_left Z.one bits in
      let text = Z.to_string number in
      List.iter (fun (left, right) ->
        let accepted, old = run false operation left right in
        check (accepted && not old.reverted && old.regs.(0) = V.VBool expected)
          "historical comparison refused";
        check (old.effort_used = narrow.effort_used) "historical comparison price depends on width";
        let accepted, current = run true operation left right in
        if bits < 65_536 then begin
          check (accepted && current.regs.(0) = V.VBool expected) "active comparison refused";
          check (Z.equal (Z.of_int (current.effort_used - old.effort_used))
            (Z.add (W.decimal number) (W.scan (String.length text))))
            "comparison width charge differs"
        end else check (not accepted && current.reverted) "wide comparison accepted")
        [V.VInt number, V.VString text; V.VString text, V.VInt number])
      [60_000; 65_535; 65_536])
    [V.EQ (0, 1, 2), true; V.NEQ (0, 1, 2), false]

let storage () =
  let run ?(limit = 1_000_000) active raw =
    let current = state ~limit ~kinds:["n", V.StorageInt] active in
    Hashtbl.add current.storage "n" raw;
    let result = V.run current [|V.SLOAD (0, "n"); V.STOP|] in
    result, current in
  let prior, old = run false (String.make 70_000 '0') in
  check (prior && not old.reverted) "historical storage refused";
  let next, current = run true (String.make 70_000 '0') in
  check (not next && current.reverted) "large storage parse accepted";
  let next, current = run ~limit:10_000_000 true (String.make 70_000 '0') in
  check (not next && current.reverted) "storage text limit bypassed";
  let _, old = run false "123" and _, current = run true "123" in
  check (old.regs.(0) = current.regs.(0)) "storage value changed";
  check (current.effort_used > old.effort_used) "storage parse not charged"

let widths () =
  let code = [|V.LDI (0, V.VInt (Z.shift_left Z.one 65_536)); V.STOP|] in
  let old = state ~limit:10_000_000 false and current = state ~limit:10_000_000 true in
  check (V.run old code) "historical integer refused";
  check (not (V.run current code) && current.reverted) "integer width accepted"

let allowance () =
  List.iter (fun (declared, prior, expected) ->
    check (W.allowance false declared prior = prior) "old allowance changed";
    check (W.allowance true declared prior = expected) "minimum effort changed")
    [Z.zero, 10_000_000, 1_000_000;
     Z.of_int 1000, 1_000_000, 1_000_000;
     Z.of_int 999_999, 10_000_000, 1_000_000;
     Z.of_int 1_000_000, 10_000_000, 1_000_000;
     Z.of_int 1_000_001, 10_000_000, 1_000_001;
     Z.of_int 10_000_001, 10_000_000, 10_000_000;
     Z.shift_left Z.one 100, 10_000_000, 10_000_000;
     Z.of_int 1000, 200_000, 200_000;
     Z.minus_one, 1_000_000, 0]

let bulk_parse () =
  let current = state true in
  current.regs.(0) <- V.VString (String.make 70_000 '0');
  current.regs.(1) <- V.VInt Z.zero;
  current.regs.(2) <- V.VInt Z.zero;
  let op = V.SLOADN (1, 2, 0) in
  let accepted = if broken "batch" then V.scalar_inputs current op else V.exec_one current op in
  check (not accepted) "bulk count parsed before reservation"

let storage_cache () =
  let current = state ~kinds:["n", V.StorageInt] false in
  let raw = String.make 70_000 '7' in
  Hashtbl.add current.storage "n" raw;
  let first = V.load_storage_value current "n" in
  if broken "storage" then current.storage_decoded <- [];
  let second = V.load_storage_value current "n" in
  check (match first, second with
    | Some (V.VInt left), Some (V.VInt right) -> left == right
    | _ -> false) "large storage value parsed twice";
  Hashtbl.replace current.storage "n" "8";
  check (V.load_storage_value current "n" = Some (V.VInt (Z.of_int 8)))
    "storage cache reused another value"

let yield_clock () =
  let ticks = ref 0L and pauses = ref 0 in
  let clock () =
    if not (broken "yield") then ticks := Int64.add !ticks 1_000_000L;
    !ticks in
  let yield () = incr pauses; Lwt.return_unit in
  let code = Array.make 25 V.NOP in
  let old = state false and current = state false in
  let expected = V.run old code in
  let actual = Lwt_main.run (V.run_async ~clock ~yield current code) in
  check (actual = expected && !pauses > 5) "time quantum did not yield";
  check (current.effort_used = old.effort_used && current.regs = old.regs)
    "yield changed execution"

let activation () =
  let chain_id = "octra-devnet-9871-cluster" in
  if R.scalar_work_activation_for_chain chain_id = None then begin
    List.iter (fun chain_id ->
      check (R.scalar_work_activation_for_chain chain_id = None) "scalar schedule enabled";
      let rules = R.create ~chain_id ~root_at:(fun _ -> failwith "inactive scalar read anchor") in
      List.iter (fun epoch ->
        check (R.scalar_work_at ~chain_id ~epoch = Prior) "inactive scalar mode changed";
        check (R.scalar_work rules ~epoch = Ok Prior) "inactive scalar graph changed")
        [0; 1_759_492; 1_769_491; 1_769_492; 1_769_493; 2_000_000; max_int];
      check (R.root_after_floor ~chain_id ~floor_epoch:max_int ~epoch:1_759_492 = None)
        "inactive scalar anchor retained") [chain_id; "octra-mainnet"; "other"]
  end else begin
    let plan = Option.get (R.scalar_work_activation_for_chain chain_id) in
    check (plan.activation_epoch - plan.anchor_epoch >= 10_000) "activation distance insufficient";
    check (R.scalar_work_at ~chain_id ~epoch:(plan.activation_epoch - 1) = Prior)
      "rule enabled early";
    check (R.scalar_work_at ~chain_id ~epoch:plan.activation_epoch = Active)
      "rule missing at activation";
    check (R.scalar_work_at ~chain_id:"octra-mainnet" ~epoch:max_int = Prior)
      "mainnet rule changed";
    let rules root = R.create ~chain_id ~root_at:(fun _ -> root) in
    check (R.scalar_work (rules (R.Root plan.anchor_state_root)) ~epoch:plan.activation_epoch = Ok Active)
      "matching anchor refused";
    check (Result.is_error (R.scalar_work (rules (R.Root (String.make 64 '0')))
      ~epoch:plan.activation_epoch)) "wrong anchor accepted";
    check (Result.is_error (R.scalar_work (rules R.Missing) ~epoch:plan.activation_epoch))
      "missing anchor accepted"
  end

let yield_trace () =
  let code = Array.of_list (List.concat (List.init 20 (fun index ->
    [V.LDI (0, V.VInt (Z.of_int index)); V.SSTORE ("n", 0);
     V.SLOAD (1, "n"); V.EQ (2, 0, 1); V.EMIT ("value", [0; 2])])) @ [V.STOP]) in
  List.iter (fun active ->
    let direct = state ~kinds:["n", V.StorageInt] active in
    let paused = state ~kinds:["n", V.StorageInt] active in
    let expected = V.run direct code in
    let now = ref 0L in
    let clock () = now := Int64.add !now 3_000_000L; !now in
    let actual = Lwt_main.run (V.run_async ~clock ~yield:Lwt.pause paused code) in
    check (expected = actual && direct.regs = paused.regs
      && direct.effort_used = paused.effort_used && !(direct.logs) = !(paused.logs)
      && Hashtbl.find direct.storage "n" = Hashtbl.find paused.storage "n"
      && direct.reverted = paused.reverted) "yield changed state or receipt") [false; true]

let measured_work () =
  let number = Z.sub (Z.shift_left Z.one 262_144) Z.one in
  let value = V.VInt number in
  let measure work =
    let started = Mtime_clock.elapsed_ns () in
    for _ = 1 to 30 do check (work ()) "comparison result differs" done;
    Int64.to_float (Int64.sub (Mtime_clock.elapsed_ns ()) started) /. 1_000_000. in
  let text_ms = measure (fun () -> String.equal (V.to_string value) (V.to_string value)) in
  let numeric_ms = measure (fun () -> V.text_equal value value) in
  let current = state false in
  current.regs.(0) <- V.VString (String.make 1_048_576 'a');
  let steps = Array.make 80 (V.ISHEX (1, 0)) in
  let active = ref true and ticks = ref 0 and gap = ref 0L in
  let rec tick previous =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.001 in
    let now = Mtime_clock.elapsed_ns () in
    gap := Int64.max !gap (Int64.sub now previous);
    incr ticks;
    if !active then tick now else Lwt.return_unit in
  let ticker = tick (Mtime_clock.elapsed_ns ()) in
  let execute = Lwt.map (fun result -> active := false; result) (V.run_async current steps) in
  let result, () = Lwt_main.run (Lwt.both execute ticker) in
  check (result && !ticks > 1) "execution starved ticker";
  Printf.printf "event = vm_measure text_ms = %.3f numeric_ms = %.3f ticker_gap_ms = %.3f ticks = %d\n"
    text_ms numeric_ms (Int64.to_float !gap /. 1_000_000.) !ticks

let () =
  if Array.exists (String.equal "--allowance") Sys.argv then begin
    allowance ();
    exit 0
  end;
  conversions ();
  equal_effort ();
  storage ();
  widths ();
  allowance ();
  bulk_parse ();
  storage_cache ();
  yield_clock ();
  activation ();
  yield_trace ();
  measured_work ();
  Printf.printf "event = vm_scalar status = passed\n"