(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Commit = Octra_node_runtime.Consensus_epoch_commit
module Ledger = Octra_core.Ledger
module Journal = Octra_core.Commit_journal

exception Commit_stop
exception Cut_error

let save_aux chain epoch =
  List.iter (fun label ->
    Case.SC.save_receipt chain ~tx_hash:(Case.hash label)
      ~contract_addr:"octPROGRAM" ~method_name:"run" ~success:true
      ~effort_used:1 ~events_json:(`List []) ~error:None ~epoch_id:epoch)
    (if epoch = 0 then ['a'] else ['a'; 'b']);
  List.iter (fun label ->
    Case.SC.save_rejected chain ~hash:(Case.hash label)
      ~from_addr:"octFROM" ~to_addr:"octTO" ~amount:"0" ~nonce:0
      ~error_type:"test" ~reason:("epoch-" ^ string_of_int epoch)
      ~epoch_id:epoch ~ts:(float_of_int epoch))
    (if epoch = 0 then ['d'; 'e'] else ['c'; 'd'])

let check_aux dir expected =
  Case.with_stores dir (fun chaindata _ ->
    let receipt hash = Lwt_main.run
      (Octra_vm.Contract_rpc.receipt ~chaindata ~tx_hash:(Case.hash hash)) in
    let epoch = function
      | None -> None
      | Some (_, _, _, _, _, _, epoch, _) -> Some epoch in
    let rejected label = epoch (Case.SC.get_rejected_tx chaindata (Case.hash label)) in
    let future_rows = Case.SC.rejected_by_epoch_rows chaindata 1 ~limit:100 ~offset:0 in
    let current = rejected 'd' in
    let receipt_epoch = match Case.SC.get_contract_receipt chaindata ~tx_hash:(Case.hash 'a') with
      | Some (`Assoc fields) -> List.assoc_opt "epoch" fields
      | _ -> None in
    let checks = [
      "old_receipt", Result.is_ok (receipt 'a');
      "repeated_receipt", receipt_epoch = Some (`Int expected);
      "new_receipt", Result.is_ok (receipt 'b') = (expected = 1);
      "old_rejection", rejected 'e' = Some 0;
      "new_rejection", rejected 'c' = (if expected = 1 then Some 1 else None);
      "repeated_rejection", current = Some expected;
      "epoch_rows", List.length future_rows = (if expected = 1 then 2 else 0);
    ] in
    List.iter (fun (name, ok) ->
      Printf.printf "event = aux_check name = %s head = %d status = %s\n%!"
        name expected (if ok then "pass" else "fail")) checks;
    Case.expect "rollback retained auxiliary writes" (List.for_all snd checks))

let child action =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try action (); Unix._exit 0 with exn ->
      Printf.eprintf "event = inline_stop reason = %s\n%!" (Printexc.to_string exn);
      Unix._exit 2)
  | pid -> Case.wait pid

let commit ?(rollback = true) ?(legacy = false) dir head point mode = child (fun () ->
  Case.with_stores dir (fun chaindata store ->
    let ledger = Ledger.create store in
    let pre_state_root = Option.get (Lwt_main.run (Case.SI.get_head_hash store)) in
    let irmin_parent = Lwt_main.run (Case.SI.get_commit_hash store) in
    let parent_commit = Octra_core.Tree.hash
      (Octra_core.Tree.create ~epoch_id:1 ~parent_commit:"previous") in
    let start_txid = Case.SC.next_txid chaindata in
    Case.SC.begin_batch chaindata;
    save_aux chaindata 1;
    (match Ledger.begin_journal ledger with
    | Ok () -> () | Error reason -> failwith reason);
    Lwt_main.run (Case.SI.begin_epoch_batch store);
    Lwt_main.run (Case.SI.set_meta store "last_epoch" "1");
    Lwt_main.run (Case.SI.set_meta store "current_epoch" "2");
    let tx_hash = Case.hash 'b' in
    Case.SC.save_tx chaindata ~hash:tx_hash ~epoch_id:1 ~from_addr:"octFROM" ~to_addr:"octTO"
      ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let post_state_root = Option.get (Lwt_main.run (Case.SI.get_batch_tree_hash store)) in
    let epoch_index_hash, epoch_index_root = Case.Eic.next_root
      ~prev:(Option.get head.Case.HM.epoch_index_root) ~epoch_id:1
      [Case.Eic.item ~txid:start_txid ~hash:tx_hash] in
    let post_consensus_root = Case.Eic.folded_state_root
      ~ledger_state_root:post_state_root ~epoch_index_root in
    let plan = Octra_core.Epoch_exec.{
      base_reward = Z.zero; fees_burned = Z.zero; fees_rewarded = Z.zero;
      total_reward = Z.zero; proposer_total = Z.zero; each_validator = Z.zero;
      remainder = Z.zero; new_emission_remaining = Z.zero;
      new_total_supply = Z.zero; new_supply_retired = Z.zero;
      supply_tracking_active = false;
    } in
    let deps = Commit.{
      data_dir = dir; store; ledger; chaindata;
      trace = (fun _ -> ()); log = (fun _ -> ());
      fatal = (fun reason -> failwith reason);
      exit = (fun () -> failwith "unexpected delayed exit");
    } in
    let observed = ref false in
    let interrupt event =
      if event = point then begin
        observed := true;
        if mode = "kill" then begin
          Unix.kill (Unix.getpid ()) Sys.sigkill;
          Unix._exit 99
        end else if mode = "error" then raise Cut_error
      end in
    let original = Commit.live_rollback_effects deps ~commit_epoch:1
      ~start_txid ~tx_count:1 in
    let rollback_effects = Commit.{
      original with
      rollback_to_head = (fun head ->
        interrupt "before_cut";
        let result = original.rollback_to_head head in
        interrupt "after_cut";
        result);
      delete_wal = (fun () ->
        interrupt "before_wal";
        original.delete_wal ();
        interrupt "after_wal");
      clear_marker = (fun () ->
        interrupt "before_marker";
        original.clear_marker ();
        interrupt "after_marker");
    } in
    let failure_effects = Commit.live_failure_effects deps
      ~rollback:(fun () -> Commit.run_rollback ~effects:rollback_effects (Commit.Rollback_to_head head)) in
    let original = Commit.live_commit_effects deps in
    let effects = {original with
      Commit.write_wal = (fun entry ->
        Case.expect "live wal lost irmin predecessor" (entry.Case.Wal.irmin_parent = irmin_parent);
        Case.expect "live wal changed epoch tree" (entry.Case.Wal.parent_commit = parent_commit);
        if legacy then begin
          let fields = Case.Wal.to_json entry |> Yojson.Safe.from_string
            |> Yojson.Safe.Util.to_assoc in
          let bytes = `Assoc (List.remove_assoc "irmin_parent" fields)
            |> Yojson.Safe.to_string in
          original.write_wal (Case.Wal.of_json bytes)
        end else original.write_wal entry);
      Commit.chaos = (fun event ->
        if not rollback then interrupt event;
        if rollback && mode <> "success" && event = "after_chaindata_committed" then raise Commit_stop);
      commit_chaindata_batch = (fun anchor ->
        interrupt "before_index";
        original.commit_chaindata_batch anchor;
        interrupt "after_index");
      retire_auxiliary = (fun head ->
        interrupt "before_retire";
        original.retire_auxiliary head;
        interrupt "after_retire");
      delete_wal = (fun epoch ->
        interrupt "before_commit_wal";
        original.delete_wal epoch;
        interrupt "after_commit_wal");
    } in
    let stopped =
      try
        Lwt_main.run (Commit.run_commit ~effects ~failure_effects {
          epoch_id = 1; pre_state_root; post_state_root; post_consensus_root;
          prev_state_root = head.state_root; parent_commit; start_txid; tx_count = 1;
          finalized_by = "octFROM"; finalized_at = 1.;
          proposer = {creator_addr = "octFROM"; commit_round = 0};
          confirmed_fees = Z.zero; plan; reward_recipients = [];
          reward_source = {reward_proposer_addr = "octFROM";
            reward_proposer_public_key = None;
            reward_members = [{reward_address = "octFROM"; reward_public_key = None;
              reward_weight = Z.one}]};
          epoch_receipts_json = []; commit_id = "inline-cut"; prev_generation = 0;
          planned_txid_hi = start_txid; epoch_index_hash; epoch_index_root;
          progress = Commit.commit_progress ();
        });
        false
      with Commit_stop | Cut_error -> true in
    Case.expect "inline commit failure was suppressed" (stopped = (mode <> "success"));
    Case.expect "inline cut injection was missed"
      ((mode <> "error" && mode <> "kill") || !observed)))

let prepare root name =
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o700;
  let head, _ = Case.prepare dir in
  Case.with_stores dir (fun chain _ ->
    Case.SC.begin_batch chain;
    save_aux chain 0;
    Case.SC.commit_batch chain;
    Case.SC.fsync chain);
  dir, head

let run_case root point mode =
  let dir, head = prepare root (point ^ "_" ^ mode) in
  let status = commit dir head point mode in
  Case.expect "inline child status differs"
    (status = if mode = "kill" then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 0);
  let expected = if mode = "success" then 1 else 0 in
  if expected = 0 then
    Case.expect "failed commit changed HEAD" (Case.HM.load_result dir = Case.HM.Present head);
  Case.expect "inline failure recovery refused" (Case.recover dir = Unix.WEXITED 0);
  let after = Option.get (Case.HM.load dir) in
  Case.expect "inline recovery changed epoch" (after.epoch_id = expected);
  Case.expect "inline recovery changed transaction range"
    (after.txid_hi = if expected = 1 then Int64.succ head.txid_hi else head.txid_hi);
  Case.expect "inline recovery retained WAL" (Case.Wal.read_pending dir = []);
  Case.expect "inline recovery cleared boot guard" (Case.Marker.recovery_required dir);
  Case.expect "inline repeat recovery refused" (Case.recover dir = Unix.WEXITED 0);
  Case.expect "inline repeat changed HEAD" (Case.HM.load dir = Some after);
  let journal = Journal.read_all dir in
  Case.expect "inline recovery left an unfinished attempt"
    (List.exists (function
      | Journal.Abort row -> expected = 0 && row.commit_id = "inline-cut"
      | Journal.Commit row -> expected = 1 && row.commit_id = "inline-cut"
      | Journal.Prepare _ -> false) journal);
  check_aux dir expected;
  Printf.printf "event = inline_cut point = %s mode = %s status = pass\n%!" point mode

let run_forward ?(legacy = false) ?(old_head = false) root point mode expected =
  let dir, head = prepare root
    ((if old_head then "old_head_" else if legacy then "legacy_" else "forward_") ^ point ^ "_" ^ mode) in
  let head = if old_head then {head with Case.HM.irmin_commit = None} else head in
  if old_head then Case.HM.atomic_write dir head;
  let status = commit ~rollback:false ~legacy dir head point mode in
  Case.expect "forward child status differs"
    (status = if mode = "kill" then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 0);
  Case.expect "forward recovery refused" (Case.recover dir = Unix.WEXITED 0);
  let after = Option.get (Case.HM.load dir) in
  Case.expect "forward recovery epoch differs" (after.epoch_id = expected);
  Case.expect "forward recovery retained WAL" (Case.Wal.read_pending dir = []);
  Case.with_stores dir (fun chain _ ->
    Case.expect "forward retained auxiliary journal"
      (Case.SC.get_meta chain Octra_core.Aux_index.pending_key = None));
  check_aux dir expected;
  Case.expect "forward repeat refused" (Case.recover dir = Unix.WEXITED 0);
  Case.expect "forward repeat changed HEAD" (Case.HM.load dir = Some after);
  check_aux dir expected;
  Printf.printf "event = aux_forward point = %s mode = %s head = %d status = pass\n%!"
    point mode expected

let completed_wal root =
  let dir, head = prepare root "completed_wal" in
  Case.expect "completed wal cut did not stop"
    (commit ~rollback:false dir head "after_head_write" "kill" = Unix.WSIGNALED Sys.sigkill);
  let entry = match Case.Wal.read_pending dir with
    | [entry] -> entry | _ -> failwith "completed wal missing" in
  List.iter (fun (name, change) ->
    Case.Wal.write dir (change entry);
    let before = Case.evidence dir in
    Case.expect ("completed wal accepted " ^ name) (Case.recover dir = Unix.WEXITED 2);
    Case.expect "completed refusal changed evidence" (Case.evidence dir = before))
    ["tree", (fun row -> {row with Case.Wal.parent_commit = Case.hash '0'});
     "parent", (fun row -> {row with Case.Wal.irmin_parent = Some (Case.hash '0')});
     "pre_root", (fun row -> {row with Case.Wal.pre_state_root = Case.hash '0'});
     "post_root", (fun row -> {row with Case.Wal.post_state_root = Case.hash '0'})];
  Case.Wal.write dir entry;
  Case.expect "valid completed wal refused" (Case.recover dir = Unix.WEXITED 0)

let damage_prior bytes =
  let changes = ref 0 in
  let rec alter = function
    | `List [(`List [`String "receipt"; `String hash] as key); `String prior; next]
      when hash = Case.hash 'a' ->
      let prior = match Base64.decode prior with
        | Ok value -> Yojson.Safe.from_string value
        | Error (`Msg reason) -> failwith reason in
      let fields = match prior with `Assoc fields -> fields | _ -> failwith "prior receipt is not an object" in
      Case.expect "prior effort differs" (List.assoc_opt "effort" fields = Some (`Int 1));
      incr changes;
      let fields = List.map (fun (key, value) -> key, if key = "effort" then `Int 9 else value) fields in
      `List [key; `String (Base64.encode_string (Yojson.Safe.to_string (`Assoc fields))); next]
    | `List values -> `List (List.map alter values)
    | value -> value in
  let result = Yojson.Safe.from_string bytes |> alter |> Yojson.Safe.to_string in
  Case.expect "prior receipt mutation count differs" (!changes = 1);
  result

let run_refusal root name =
  let dir, head = prepare root name in
  let point = if name = "published_value" || name = "published_prior"
    then "after_head_write" else "after_index" in
  Case.expect "refusal cut did not stop"
    (commit ~rollback:false dir head point "kill" = Unix.WSIGNALED Sys.sigkill);
  Case.with_stores dir (fun chain _ ->
    let index = Case.SC.index chain in
    match Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
      if name = "bad_journal" then
        Lmdb.Map.set index.meta ~txn Octra_core.Aux_index.pending_key "{}"
      else if name = "lost_journal" then
        Lmdb.Map.remove index.meta ~txn Octra_core.Aux_index.pending_key
      else if name = "prior_value" || name = "published_prior" then
        let bytes = Lmdb.Map.get index.meta ~txn Octra_core.Aux_index.pending_key in
        Lmdb.Map.set index.meta ~txn Octra_core.Aux_index.pending_key (damage_prior bytes)
      else
        Lmdb.Map.set index.receipts ~txn (Case.hash 'a') {|{"epoch":0}|}) with
    | Some () -> Lmdb.Env.sync index.env
    | None -> failwith "test index write aborted");
  let before = Case.evidence dir in
  Case.expect "auxiliary contradiction accepted" (Case.recover dir = Unix.WEXITED 2);
  Case.expect "auxiliary refusal changed evidence" (Case.evidence dir = before);
  Case.expect "auxiliary refusal lost boot guard" (Case.Marker.recovery_required dir);
  Case.expect "auxiliary repeat accepted" (Case.recover dir = Unix.WEXITED 2);
  Case.expect "auxiliary repeat changed evidence" (Case.evidence dir = before);
  Printf.printf "event = aux_refusal case = %s status = pass\n%!" name

let run_cut_prefix root kind stop after mode =
  let name = Printf.sprintf "%s_%d_%s_%s" kind stop
    (if after then "after" else "before") mode in
  let dir, head = prepare root ("prefix_" ^ name) in
  Case.expect "prefix commit did not stop"
    (commit ~rollback:false dir head "after_index" "kill" = Unix.WSIGNALED Sys.sigkill);
  let status = child (fun () ->
    let count = ref 0 in
    let sync fd =
      incr count;
      if !count <> stop then Unix.fsync fd
      else begin
        if after then Unix.fsync fd;
        if mode = "kill" then begin
          Unix.kill (Unix.getpid ()) Sys.sigkill;
          Unix._exit 99
        end;
        raise Cut_error
      end in
    let stopped = try
      Case.with_stores dir (fun chain store ->
        Case.Marker.require_recovery dir;
        let _, plan, _, _ = Lwt_main.run
          (Case.Recovery.inspect ~data_dir:dir ~chaindata:chain ~store) in
        Case.expect "prefix recovery did not select cut"
          (match plan with Octra_core.Recovery_phase.Cut _ -> true | _ -> false);
        Octra_core.Txlog.truncate_to (Case.SC.txlog chain)
          ~seg_id:(Option.get head.txlog_seg) ~offset:(Option.get head.txlog_off)
          ~sync:(if kind = "tx" then sync else Unix.fsync);
        Case.EL.truncate_to chain.epochlog ~offset:(Option.get head.epochlog_off)
          ~sync:(if kind = "epoch" then sync else Unix.fsync));
      false
    with Cut_error -> true in
    Case.expect "prefix interruption was missed" (stopped && !count = stop)) in
  Case.expect "prefix child status differs"
    (status = if mode = "kill" then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 0);
  Case.expect "prefix cut changed HEAD" (Case.HM.load dir = Some head);
  Case.expect "prefix cut removed WAL" (List.length (Case.Wal.read_pending dir) = 1);
  Case.expect "prefix cut lost recovery guard" (Case.Marker.recovery_required dir);
  for _ = 1 to 2 do
    Case.expect "prefix startup recovery refused" (Case.recover dir = Unix.WEXITED 0);
    Case.expect "prefix startup changed HEAD" (Case.HM.load dir = Some head);
    Case.expect "prefix startup retained WAL" (Case.Wal.read_pending dir = []);
    Case.expect "prefix startup cleared recovery guard" (Case.Marker.recovery_required dir);
    Case.with_stores dir (fun chain _ ->
      Case.expect "prefix transaction end differs"
        (Case.SC.txlog_position chain = (Option.get head.txlog_seg, Option.get head.txlog_off));
      Case.expect "prefix epoch end differs" (Case.SC.epochlog_offset chain = Option.get head.epochlog_off);
      Case.expect "prefix transaction range differs" (Case.SC.next_txid chain = Int64.succ head.txid_hi);
      Case.expect "prefix retained auxiliary journal"
        (Case.SC.get_meta chain Octra_core.Aux_index.pending_key = None));
    check_aux dir 0
  done;
  Printf.printf "event = aux_prefix case = %s status = pass\n%!" name

let run root =
  let failed = ref false in
  completed_wal root;
  List.iter (fun (point, expected) ->
    run_forward ~old_head:true root point "kill" expected)
    ["after_wal", 0; "after_irmin_committed", 1];
  List.iter (fun (point, expected) ->
    run_forward ~legacy:true root point "kill" expected)
    ["after_wal", 0; "after_irmin_committed", 1;
     "before_head_write", 1; "after_head_write", 1];
  let cases = ["control", "success"; "control", "rollback"] @
    List.concat_map (fun point -> List.map (fun mode -> point, mode) ["error"; "kill"])
      ["before_cut"; "after_cut"; "before_wal"; "after_wal"; "before_marker"; "after_marker"] in
  List.iter (fun (point, mode) ->
    try run_case root point mode with exn ->
      failed := true;
      Printf.eprintf "event = inline_cut point = %s mode = %s status = fail reason = %s\n%!"
        point mode (Printexc.to_string exn)) cases;
  List.iter (fun (point, expected) ->
    List.iter (fun mode ->
      try run_forward root point mode expected with exn ->
        failed := true;
        Printf.eprintf "event = aux_forward point = %s mode = %s status = fail reason = %s\n%!"
          point mode (Printexc.to_string exn)) ["error"; "kill"])
    ["after_wal", 0; "before_index", 0; "after_index", 0;
     "after_chaindata_committed", 0; "after_irmin_committed", 1;
     "before_head_write", 1; "after_head_write", 1;
     "before_retire", 1; "after_retire", 1;
     "before_commit_wal", 1; "after_commit_wal", 1];
  List.iter (fun name ->
    try run_refusal root name with exn ->
      failed := true;
      Printf.eprintf "event = aux_refusal case = %s status = fail reason = %s\n%!"
        name (Printexc.to_string exn))
    ["bad_journal"; "lost_journal"; "changed_value"; "published_value";
     "prior_value"; "published_prior"];
  List.iter (fun kind ->
    List.iter (fun stop ->
      List.iter (fun after ->
        List.iter (fun mode ->
          try run_cut_prefix root kind stop after mode with exn ->
            failed := true;
            Printf.eprintf "event = aux_prefix kind = %s sync = %d after = %b mode = %s status = fail reason = %s\n%!"
              kind stop after mode (Printexc.to_string exn)) ["error"; "kill"])
        [false; true]) [1; 2; 3]) ["tx"; "epoch"];
  if !failed then exit 1

let () = Test_workspace.with_dir "aux_commit" run