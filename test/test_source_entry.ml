(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Source_case
open Yojson.Safe.Util

let policy enabled =
  let mode = if enabled then R.Active else R.Prior in
  need "source schedule differs" (R.source_create_at ~chain_id:chain ~epoch = mode);
  need "source graph differs" (R.source_create (rules ()) ~epoch = Ok mode);
  need "other chain enabled" (R.source_create_at ~chain_id:"other" ~epoch = R.Prior);
  if enabled then begin
    need "source schedule missing" (List.mem epoch (R.profile_epochs ~chain_id:chain));
    need "source before epoch active" (R.source_create_at ~chain_id:chain ~epoch:(epoch - 1) = R.Prior);
    let bad = R.create ~chain_id:chain ~root_at:(fun at -> if at = 1_900_000 then R.Missing else roots at) in
    need "source anchor unchecked" (Result.is_error (Set_rule.policy bad epoch));
    let bad = R.create ~chain_id:chain ~root_at:(fun at -> if at = 1_900_000 then R.Root (String.make 64 'b') else roots at) in
    need "source anchor mismatch accepted" (Result.is_error (Set_rule.policy bad epoch));
    let before = Consensus_profile.hash ~chain_id:chain ~epoch:(epoch - 1) (fun _ -> None) in
    let after = Consensus_profile.hash ~chain_id:chain ~epoch (fun _ -> None) in
    need "source profile unchanged" (before <> after);
    need "source profile bytes differ" (Digestif.SHA256.(of_raw_string after |> to_hex)
      = "a654d412dd2c52c23d7f7a892c75bb5cbe308d5a9f95df58a69652c1d2ed45be");
    need "source profile switch missing" (Consensus_profile.switch_after ~chain_id:chain ~applied_epoch:(epoch - 1))
  end

let signature_cases ledger env tx =
  let foreign, foreign_public, foreign_address = identity 201 in
  C.Tx_staging.clear ();
  need "registered key normalization failed" (Result.is_ok
    (ingress ledger {tx with T.public_key = Some foreign_public}));
  let normalized = C.Tx_staging.ready_epoch_txs ~capacity:C.Tx_staging.max_ou_per_epoch
    ~accept:(fun _ -> true) ~confirmed_nonce:(fun address ->
      Option.map (fun (account : C.Ledger_types.account) -> account.nonce) (C.Ledger.find_opt ledger address)) in
  need "registered key normalization differs" (List.map T.hash normalized = [T.hash tx]);
  C.Tx_staging.clear ();
  let changes = [
    "signature", {tx with T.signature = Base64.encode_exn (String.make 64 '\000')};
    "key", sign foreign {tx with T.signature = ""};
    "sender", {tx with T.from = foreign_address};
    "target", {tx with T.to_ = tx.from};
    "amount", {tx with T.amount = Z.one};
    "time", {tx with T.timestamp = tx.timestamp +. 1.};
    "nonce", {tx with T.nonce = tx.nonce + 1};
    "fee", {tx with T.ou = Z.succ tx.ou};
    "method", {tx with T.encrypted_data = Some "changed"};
    "params", {tx with T.message = Some "[1]"};
  ] in
  List.map (fun (name, changed) ->
    C.Tx_staging.clear ();
    need ("altered RPC accepted " ^ name) (Result.is_error (ingress ledger changed));
    need ("altered RPC retained " ^ name) (C.Tx_staging.staging_size () = 0);
    need ("altered block accepted " ^ name) (Result.is_error (bundle ledger env [changed]));
    name) changes

let flow folder plan engine label =
  let path = Filename.concat folder (engine ^ "_" ^ label) in
  Unix.mkdir path 0o700;
  let factory = compile factory_source in
  let key, public, owner = identity_for factory.envelope in
  let source = plan |> member "source" |> to_string |> fun path -> In_channel.with_open_bin path In_channel.input_all in
  need "pool source differs" (sha source = (plan |> member "source_sha256" |> to_string));
  let pool = compile source in
  let params = plan |> member "params" |> to_list in
  need "pool parameter count" (List.length params = 18);
  let parent = V.Contract.addr_from_code factory.envelope owner 1 in
  let child = V.Contract.addr_from_code pool.envelope parent 0 in
  let timestamp = plan |> member "test_time" |> to_float in
  let env = environment timestamp owner in
  let store = run (S.open_store ~fresh:true (Filename.concat path "store")) in
  Fun.protect ~finally:(fun () -> C.Tx_staging.clear (); run (S.close store)) (fun () ->
    let ledger = C.Ledger.create store in
    C.Ledger.add_account_with_pubkey ledger owner initial public |> unwrap;
    let other_key, other_public, other = identity 200 in
    C.Ledger.add_account_with_pubkey ledger other initial other_public |> unwrap;
    run (C.Ledger.flush_dirty_lwt ledger);
    if engine = "proposal" then begin
      run (S.set_meta store "last_epoch" (string_of_int (epoch - 1)));
      run (S.set_meta store "total_supply" (Z.to_string (C.Ledger.get_total_supply ledger)));
      run (S.set_meta store "emission_remaining" "0");
      Source_proposal.initialize ~store ~ledger ~env
    end;
    let backend = E.make_live_backend ~fold ~math:true store ledger in
    let submit ~success tx =
      C.Tx_staging.clear ();
      let picked = select ledger tx in
      need "block signature verification failed" (Result.is_ok (bundle ledger env picked));
      if engine = "proposal" && tx.T.op_type = T.ProgramExec && tx.nonce = 2 then
        Source_proposal.check ~store ~ledger ~env tx;
      run (backend.begin_batch R.Prior);
      let result, receipt = if engine = "leader" then sender path backend env tx else validator backend env tx in
      need (label ^ " " ^ snd result) (fst result = success);
      let receipt = Option.get receipt in
      need "receipt success differs" ((receipt |> member "success" |> to_bool) = success);
      run (backend.flush_dirty ());
      run (backend.commit_batch ());
      save (Filename.concat path ("tx_" ^ string_of_int tx.T.nonce ^ "_" ^ tx.from ^ ".json")) (T.to_yojson tx);
      receipt in
    let deploy = transaction ~timestamp ~public ~sender:owner ~target:parent ~nonce:1 ~fee:400_000
      ~operation:T.ProgramDeploy ~payload:(Base64.encode_exn factory.package) ~params:(`List [`String owner]) |> sign key in
    let signatures = signature_cases ledger env deploy in
    let deploy_receipt = submit ~success:true deploy in
    let parent_meta = run (S.get_contract_meta store parent) |> Option.get in
    need "factory source metadata" (parent_meta.check = "source" && parent_meta.version = "2.0 AML-preview");
    need "factory operator differs" (List.assoc "operator" (fields store parent) = owner);
    need "factory record missing" (Option.is_some (run (S.read store (V.Program_record.key parent))));
    let params = if label = "constructor" then List.mapi (fun index value ->
      if index = 10 then `String (Base64.encode_exn (String.make 32 '\000')) else value) params else params in
    let call_params = `String (Base64.encode_exn (if label = "code" then factory.envelope else pool.envelope)) :: params in
    let payload = `Assoc ["program_create", `String (Base64.encode_exn pool.package); "params", `List call_params] in
    let caller_key, caller_public, caller, nonce = if label = "owner"
      then other_key, other_public, other, 1 else key, public, owner, 2 in
    let fee = if label = "work" then 1_000_000 else 4_000_000 in
    let tx = transaction ~timestamp ~public:caller_public ~sender:caller ~target:parent ~nonce ~fee
      ~operation:T.ProgramExec ~payload:"create" ~params:payload |> sign caller_key in
    let signatures = signatures @ signature_cases ledger env tx in
    let before = fields store parent in
    let account = Option.get (C.Ledger.find_opt ledger caller) in
    let balance, used_nonce = account.balance, account.nonce in
    let host = label = "host" in
    let receipt = if host then begin
      C.Tx_staging.clear ();
      let picked = select ledger tx in
      need "host block signature failed" (Result.is_ok (bundle ledger env picked));
      run (backend.begin_batch R.Prior);
      Unix.putenv "OCTRA_CHAOS_FAIL_AT" "vm_commit_after_program";
      Unix.putenv "OCTRA_CHAOS_FAIL_KIND" "host";
      let failed = Fun.protect ~finally:(fun () -> Unix.putenv "OCTRA_CHAOS_FAIL_AT" "";
        Unix.putenv "OCTRA_CHAOS_FAIL_KIND" "") (fun () ->
        try ignore (if engine = "leader" then sender path backend env tx else validator backend env tx); false
        with C.Exec_resource.Unavailable Host -> true) in
      need "host error did not propagate" failed;
      need "host error charged fee" ((Option.get (C.Ledger.find_opt ledger caller)).balance = balance);
      need "host error consumed nonce" ((Option.get (C.Ledger.find_opt ledger caller)).nonce = used_nonce);
      run (backend.flush_dirty ()); run (backend.commit_batch ());
      `Null
    end else submit ~success:(label = "pool") tx in
    if label = "pool" then begin
      let meta = run (S.get_contract_meta store child) |> Option.get in
      need "pool metadata differs" (meta.check = "source" && meta.version = "2.0 AML-preview" && meta.owner = parent);
      need "pool record missing" (Option.is_some (run (S.read store (V.Program_record.key child))));
      let storage = fields store child in
      need "pool state count differs" (List.length storage = 53);
      List.iter (fun (index, name) -> need ("pool state " ^ name)
        (List.assoc name storage = to_string (List.nth params index)))
        [0,"scope"; 1,"asset"; 6,"image"; 8,"config"; 9,"checkpoint"];
      need "factory not closed" (List.assoc "created" (fields store parent) = "true");
      C.Tx_staging.clear ();
      need "RPC replay accepted" (Result.is_error (ingress ledger tx));
      let repeat = sign key {tx with T.nonce = 3; signature = ""} in
      ignore (submit ~success:false repeat);
      need "repeat changed pool" (fields store child = storage);
      need "repeat changed owner" ((run (S.get_contract_meta store child) |> Option.get).owner = parent)
    end else begin
      need "failed creation retained parent writes" (fields store parent = before);
      need "failed creation retained child" (run (S.get_contract_meta store child) = None);
      need "failed creation retained record" (run (S.read store (V.Program_record.key child)) = None);
      if not host then begin
        let account = Option.get (C.Ledger.find_opt ledger caller) in
        need "refusal fee differs" (Z.equal account.balance (Z.sub balance tx.ou));
        need "refusal nonce differs" (account.nonce = nonce)
      end
    end;
    let root = run (S.get_head_hash store) |> Option.get in
    `Assoc ["engine", `String engine; "case", `String label; "root", `String root;
      "factory", `String parent; "program", `String child; "signer", `String owner;
      "signature_controls", `Int (List.length signatures * 2); "deploy_receipt", deploy_receipt;
      "pool_receipt", receipt; "signed_rpc", `Bool true; "proposal_checked", `Bool true])

let () =
  need "use test_source_entry MODE OUTPUT PLAN" (Array.length Sys.argv = 4);
  let active = match Sys.argv.(1) with "active" | "proposal" -> true | "prior" -> false | _ -> failwith "mode invalid" in
  policy active;
  let folder = Sys.argv.(2) in
  need "output exists" (not (Sys.file_exists folder));
  Unix.mkdir folder 0o700;
  let rows = if not active then [] else begin
    let plan = Yojson.Safe.from_file Sys.argv.(3) in
    if Sys.argv.(1) = "proposal" then [flow folder plan "proposal" "pool"] else
    let rows engine = List.map (flow folder plan engine) ["pool"; "owner"; "code"; "constructor"; "work"; "host"] in
    let left = rows "leader" in
    let right = rows "validator" in
    List.iter2 (fun left right ->
      need "signed path state roots differ" (member "root" left = member "root" right);
      let effort row = match member "pool_receipt" row with
        | `Null -> `Null
        | receipt -> member "effort" receipt in
      need "signed path effort differs" (effort left = effort right)) left right;
    left @ right
  end in
  save (Filename.concat folder "result.json") (`Assoc ["status", `String "passed";
    "active", `Bool active; "cases", `List rows; "network_state", `Bool false;
    "profile", `String (Consensus_profile.hash ~chain_id:chain ~epoch (fun _ -> None)
      |> Digestif.SHA256.of_raw_string |> Digestif.SHA256.to_hex);
    "deployment_ready", `Bool false]);
  print_endline "event = signed_source status = passed"