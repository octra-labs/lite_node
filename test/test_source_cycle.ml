(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Source_case
open Yojson.Safe.Util

let () =
  if Array.length Sys.argv = 4 && Sys.argv.(1) = "inspect" then begin
    let store = run (S.open_store Sys.argv.(2)) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let before = run (S.state_hash store) in
      let tree = run (S.Store.tree store.store) in
      let rows = run (S.Store.Tree.fold ~force:`True ~cache:false ~uniq:`False
        ~contents:(fun path value rows -> Lwt.return ((String.concat "/" path, `String value) :: rows)) tree []) in
      need "inspection changed state" (before = run (S.state_hash store));
      save Sys.argv.(3) (`Assoc ["root", `String before;
        "fields", `Assoc (List.sort (fun (a, _) (b, _) -> String.compare a b) rows)]));
    exit 0
  end

let () =
  need "plan and output required" (Array.length Sys.argv = 3);
  let plan = Yojson.Safe.from_file Sys.argv.(1) and output = Sys.argv.(2) in
  let factory = compile factory_source in
  let key, public, owner = identity_for factory.envelope in
  let pool = member "program" plan |> to_string in
  let actions = member "actions" plan |> to_list in
  let now = member "test_time" plan |> to_float in
  let clock = match member "chain_time" plan with `Null -> now | value -> to_float value in
  let store = run (S.open_store (Filename.concat output "store")) in
  Fun.protect ~finally:(fun () -> C.Tx_staging.clear (); run (S.close store)) (fun () ->
    let ledger = C.Ledger.create store in
    let initial_account = C.Ledger.find_opt ledger owner |> Option.get in
    let first_nonce = initial_account.nonce + 1 in
    let at = epoch + 10 in
    let env = { (environment clock owner) with E.epoch_id = at } in
    run (S.set_meta store "last_epoch" (string_of_int (at - 1)));
    run (S.set_meta store "total_supply" (Z.to_string (C.Ledger.get_total_supply ledger)));
    run (S.set_meta store "emission_remaining" "0");
    Source_proposal.initialize ~store ~ledger ~env;
    let transactions = List.mapi (fun index action ->
      let fee = member "fee" action |> to_int in
      let payment = member "payment" action = `Bool true in
      let target = if payment then let _, _, address = identity 190 in address else pool in
      let tx = transaction ~timestamp:now ~public ~sender:owner ~target ~nonce:(first_nonce + index) ~fee
        ~operation:(if payment then T.Standard else T.ProgramExec)
        ~payload:(member "method" action |> to_string) ~params:(member "params" action) in
      let tx = if payment then {tx with T.amount = Z.one; encrypted_data = None; message = None} else tx in
      sign ~at key tx) actions in
    List.iter (fun tx ->
      need "signed action bundle refused" (Result.is_ok (bundle ledger env [tx]));
      List.iter (fun changed ->
        need "altered signed action accepted by RPC" (Result.is_error (ingress ~at ledger changed));
        need "altered signed action accepted by block" (Result.is_error (bundle ledger env [changed]));
        need "altered signed action retained" (C.Tx_staging.staging_size () = 0))
        [{tx with T.signature = Base64.encode_exn (String.make 64 '\000')};
         {tx with T.nonce = tx.nonce + 1}; {tx with T.ou = Z.succ tx.ou};
         {tx with T.encrypted_data = Some "changed"}]) transactions;
    let rec advance at remaining rows =
      match remaining with
      | [] -> List.rev rows
      | first :: _ ->
        let remaining = List.map (fun tx ->
          if C.Tx_staging.find_by_hash (T.hash tx) <> None then tx
          else sign ~at key {tx with T.timestamp = Float.floor (Unix.gettimeofday ())}) remaining in
        let index = first.T.nonce - first_nonce in
        let action = List.nth actions index in
        let minimum = member "epoch" action |> to_int in
        let env = {env with E.epoch_id = at; epoch_ts = clock +. float_of_int ((at - epoch - 10) * 10)} in
        let eligible tx =
          let item = List.nth actions (tx.T.nonce - first_nonce) in
          (member "epoch" item |> to_int) <= at in
        List.iter (fun tx ->
          let action = List.nth actions (tx.T.nonce - first_nonce) in
          if eligible tx && C.Tx_staging.find_by_hash (T.hash tx) = None then
            match ingress ~at ledger tx with
            | Ok _ -> ()
            | Error error when error.C.Rpc.code = C.Rpc.nonce_too_far.code -> ()
            | Error error -> failwith ((member "name" action |> to_string) ^ ": signed RPC refused: " ^ error.C.Rpc.message)) remaining;
        let ready = C.Tx_staging.ready_epoch_txs ~capacity:C.Tx_staging.max_ou_per_epoch
          ~accept:(fun _ -> true) ~confirmed_nonce:(fun address ->
            Option.map (fun (account : C.Ledger_types.account) -> account.nonce) (C.Ledger.find_opt ledger address)) in
        let allowed = List.filter (fun tx -> eligible tx && C.Tx_staging.find_by_hash (T.hash tx) <> None) remaining in
        need "queued sequence differs" (List.map T.hash ready = List.map T.hash allowed);
        let rec reserve used selected = function
          | [] -> List.rev selected
          | tx :: rest ->
            let next = C.Resource_lanes.add used (C.Resource_lanes.work tx) in
            if C.Resource_lanes.within (C.Resource_lanes.default_budget Program) next then
              reserve next (tx :: selected) rest else List.rev selected in
        let expected = (Consensus_proposal.cap ~limits (reserve C.Resource_lanes.zero [] allowed)).txs in
        need "private action did not become eligible" (expected <> [] || at < minimum);
        let request, execution, seconds = Source_proposal.check_many ~store ~ledger ~env ~expected ready in
        let applied = Source_proposal.apply ~store ~ledger request in
        need "applied block differs from preview" (applied = execution);
        C.Tx_staging.remove_processed (List.map T.hash expected);
        List.iter (fun tx ->
          need "processed transaction remains staged" (C.Tx_staging.find_by_hash (T.hash tx) = None);
          match ingress ~at ledger tx with
          | Error error -> need "replay refused for another reason" (error.C.Rpc.code = C.Rpc.invalid_nonce.code)
          | Ok _ -> failwith "applied transaction replay accepted") expected;
        let picked = List.length expected in
        let remainder = List.filteri (fun index _ -> index >= picked) remaining in
        need "deferred transactions removed" (C.Tx_staging.staging_size () = List.length allowed - picked);
        let names = List.map (fun tx -> member "name" (List.nth actions (tx.T.nonce - first_nonce))) expected in
        let totals = Consensus_proposal.totals expected in
        let reserved = List.fold_left (fun sum tx -> Z.add sum (C.Resource_lanes.work tx).ou) Z.zero expected in
        let row = `Assoc ["epoch", `Int at; "names", `List names; "seconds", `Float seconds;
          "ou", `String (Z.to_string reserved); "bytes", `Int totals.bytes] in
        let rows = row :: rows in
        save (Filename.concat output ("block_" ^ string_of_int at ^ ".json")) row;
        advance (at + 1) remainder rows in
    let rows = advance at transactions [] in
    let ctx = {V.Contract_vm.default_ctx with math = true; point_ops = true; scalar_work = true;
      proof_exec = R.Active; fhe_work = R.Active; proof_key = R.Active;
      byte_work = Some (V.Byte_work.create V.Byte_work.consensus_limits)} in
    List.iter (fun view ->
      let method_name = member "method" view |> to_string in
      let result = V.Contract.execute_view_call ~ctx store pool method_name
        (member "params" view |> to_list) owner in
      need ("final view failed: " ^ method_name ^ ": " ^ Option.value ~default:"none" result.error) result.success;
      let value = match result.return_value with
        | Some (V.Contract_vm.VString text) -> `String text
        | Some (V.Contract_vm.VInt value | V.Contract_vm.VU64 value | V.Contract_vm.VU128 value | V.Contract_vm.VU256 value) ->
          `String (Z.to_string value)
        | Some (V.Contract_vm.VBool value) -> `Bool value
        | _ -> failwith "view result type differs" in
      need ("final view differs: " ^ method_name) (value = member "result" view)) (member "views" plan |> to_list);
    let at = (List.hd (List.rev rows) |> member "epoch" |> to_int) + 1 in
    let env = {env with E.epoch_id = at; epoch_ts = clock +. float_of_int ((at - epoch - 10) * 10)} in
    let state = fields store pool in
    let account = C.Ledger.find_opt ledger owner |> Option.get in
    let _, _, recipient = identity 190 in
    let payments = List.filter (fun action -> member "payment" action = `Bool true) actions |> List.length in
    let received = C.Ledger.find_opt ledger recipient |> Option.fold ~none:Z.zero
      ~some:(fun (entry : C.Ledger_types.account) -> entry.balance) in
    need "load payments differ" (Z.equal received (Z.of_int payments));
    let controls = member "controls" plan |> to_list in
    List.iteri (fun index action ->
      let at = at + index in
      let env = {env with E.epoch_id = at; epoch_ts = env.epoch_ts +. float_of_int (index * 10)} in
      let tx = transaction ~timestamp:(Float.floor (Unix.gettimeofday ())) ~public ~sender:owner ~target:pool ~nonce:(account.nonce + 1)
        ~fee:1_000_000 ~operation:T.ProgramExec ~payload:(member "method" action |> to_string)
        ~params:(member "params" action) |> sign ~at key in
      let reason = match member "name" action |> to_string with
        | "batch_proof" -> "execution reverted [batch proof invalid]"
        | "deposit_replay" | "cancel_replay" -> "execution reverted [deposit already decided]"
        | "spend_replay" | "withdraw_replay" -> "execution reverted [serial spent]"
        | _ -> failwith "unknown refusal" in
      need "signed refusal RPC rejected" (Result.is_ok (ingress ~at ledger tx));
      let request, execution, _ = Source_proposal.check_many ~refused:[tx, reason]
        ~store ~ledger ~env ~expected:[] [tx] in
      let applied = Source_proposal.apply ~store ~ledger request in
      need "refusal block differs from preview" (applied = execution);
      need "refused actions changed pool" (fields store pool = state);
      need "refused action charged account" (C.Ledger.find_opt ledger owner = Some account);
      C.Tx_staging.remove_processed [T.hash tx]) controls;
    save (Filename.concat output "result.json") (`Assoc ["status", `String "passed";
      "blocks", `List rows; "signed_actions", `Int (List.length actions);
      "signature_controls", `Int (List.length transactions * 8);
      "replay_controls", `Int (List.length transactions); "refused_actions", `Int (List.length controls);
      "payments", `Int payments; "received", `String (Z.to_string received);
      "state_root", `String (run (C.Ledger.hash ledger));
      "pool_fields", `Int (List.length (fields store pool));
      "deployment_ready", `Bool false]))