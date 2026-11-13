(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Source_case
open Yojson.Safe.Util

let () =
  need "plan output and mode required" (Array.length Sys.argv = 4);
  let plan = Yojson.Safe.from_file Sys.argv.(1) and output = Sys.argv.(2) in
  let enabled = match Sys.argv.(3) with
    | "active" -> true | "disabled" -> false | _ -> failwith "result mode invalid" in
  let pool = member "program" plan |> to_string in
  let now = member "test_time" plan |> to_float in
  let _, _, proposer = identity_for (compile factory_source).envelope in
  let attack_key, attack_public, attacker = identity 198 in
  let honest_key, honest_public, honest = identity 199 in
  let store = run (S.open_store (Filename.concat output "store")) in
  Fun.protect ~finally:(fun () -> C.Tx_staging.clear (); run (S.close store)) (fun () ->
    let ledger = C.Ledger.create store in
    List.iter (fun (address, public) ->
      need "test writer already exists" (C.Ledger.find_opt ledger address = None);
      C.Ledger.add_account_with_pubkey ledger address initial public |> unwrap)
      [attacker, attack_public; honest, honest_public];
    run (C.Ledger.flush_dirty_lwt ledger);
    let at = epoch + 10 in
    let env = {(environment now proposer) with E.epoch_id = at} in
    need "result schedule differs" (R.program_result_at ~chain_id:chain ~epoch:at =
      if enabled then R.Active else R.Prior);
    run (S.set_meta store "last_epoch" (string_of_int (at - 1)));
    run (S.set_meta store "total_supply" (Z.to_string (C.Ledger.get_total_supply ledger)));
    run (S.set_meta store "emission_remaining" "0");
    Source_proposal.initialize ~store ~ledger ~env;
    let account address = C.Ledger.find_opt ledger address |> Option.get in
    let attack_start = account attacker and honest_start = account honest in
    let tx at ~key ~public ~sender ~nonce slot data =
      transaction ~timestamp:now ~public ~sender ~target:pool ~nonce ~fee:1000
        ~operation:T.ProgramExec ~payload:"put" ~params:(`List [`Int slot; `String data]) |> sign ~at key in
    let block offset ?(paid = []) ?(refused = []) expected =
      let env = {env with E.epoch_id = at + offset; epoch_ts = now +. float_of_int (offset * 10)} in
      let txs = expected @ List.map fst paid @ List.map fst refused in
      List.iter (fun tx -> need "buffer RPC refused" (Result.is_ok (ingress ~at:env.epoch_id ledger tx))) txs;
      let request, execution, seconds = Source_proposal.check_many ~paid ~refused ~progress:false
        ~store ~ledger ~env ~expected txs in
      need "buffer application differs" (Source_proposal.apply ~store ~ledger request = execution);
      C.Tx_staging.remove_processed (List.map T.hash txs);
      save (Filename.concat output ("block_" ^ string_of_int offset ^ ".json"))
        (`Assoc ["seconds", `Float seconds; "fees", `String (Z.to_string execution.artifacts.confirmed_fees);
          "confirmed", `Int (List.length expected); "paid", `Int (List.length paid);
          "refused", `Int (List.length refused)]);
      execution in
    List.iter (fun group -> ignore (block group (List.init 8 (fun index ->
      let slot = group * 8 + index in
      tx (at + group) ~key:attack_key ~public:attack_public ~sender:attacker ~nonce:(slot + 1) slot "x")))) [0; 1];
    need "attack fee differs" ((account attacker).balance = Z.sub attack_start.balance (Z.of_int 16000));
    need "attack nonce differs" ((account attacker).nonce = 16);
    let occupied = fields store pool in
    let failed = tx (at + 2) ~key:honest_key ~public:honest_public ~sender:honest ~nonce:1 0 "y" in
    let reason = "execution reverted [chunk occupied]" in
    let result = if enabled then block 2 ~paid:[failed, reason] []
      else block 2 ~refused:[failed, reason] [] in
    let fee = if enabled then Z.of_int 1000 else Z.zero in
    need "buffer refusal fee differs" ((account honest).balance = Z.sub honest_start.balance fee);
    need "buffer refusal nonce differs" ((account honest).nonce = if enabled then 1 else 0);
    need "buffer refusal fee total differs" (result.artifacts.confirmed_fees = fee);
    need "buffer refusal changed storage" (fields store pool = occupied);
    if enabled then need "paid refusal replay accepted" (Result.is_error (ingress ~at:(at + 3) ledger failed));
    save (Filename.concat output "result.json") (`Assoc ["status", `String "reproduced";
      "shared_slots", `Int 16; "attacker_paid", `String "16000";
      "honest_refusal_paid", `String (Z.to_string fee);
      "honest_nonce", `Int (account honest).nonce; "program_result_active", `Bool enabled;
      "signed_rpc_proposal_verify_apply", `Bool true; "pool_changed_by_refusal", `Bool false;
      "storage_fields", `Int (List.length occupied); "synthetic_schedule", `Bool true;
      "production_fix", `Bool false; "proofs_generated", `Int 0; "deployment_ready", `Bool false]));
  print_endline "event = buffer_result status = reproduced"