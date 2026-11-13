(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Source_case

let check input operation =
  let source = In_channel.with_open_bin input In_channel.input_all in
  let code = V.Oct_compile.compile source in
  Option.iter failwith code.error;
  let key, public, owner = identity 30 in
  let _, _, guardian = identity 31 in
  let _, _, recipient = identity 32 in
  Test_workspace.with_dir "bridge_path" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> C.Tx_staging.clear (); run (S.close store)) (fun () ->
      let ledger = C.Ledger.create store in
      unwrap (C.Ledger.add_account_with_pubkey ledger owner initial public);
      unwrap (C.Ledger.add_account ledger recipient Z.zero);
      run (C.Ledger.flush_dirty_lwt ledger);
      run (S.begin_epoch_batch store);
      let effects = V.Tx_effects.create ~ledger ~store in
      let vault, created = V.Contract.deploy ~journal:(V.Tx_effects.program effects)
        ~params:[`String guardian] store owner "CUSTOM" [||] code.bytecode 1 in
      need "bridge installation refused" created.success;
      unwrap (V.Tx_effects.ensure_account effects vault);
      V.Tx_effects.commit_exn effects;
      run (S.commit_epoch_batch store "bridge path");
      let timestamp = Float.floor (Unix.gettimeofday ()) in
      let env = environment timestamp owner in
      run (S.set_meta store "last_epoch" (string_of_int (epoch - 1)));
      run (S.set_meta store "total_supply" (Z.to_string (C.Ledger.get_total_supply ledger)));
      run (S.set_meta store "emission_remaining" "0");
      Source_proposal.initialize ~store ~ledger ~env;
      let backend = E.make_live_backend ~fold ~proof_mode:R.Active ~math:true store ledger in
      let execute nonce payload params amount =
        let tx = transaction ~timestamp ~public ~sender:owner ~target:vault ~nonce ~fee:1000
          ~operation ~payload ~params:(`List (List.map (fun value -> `String value) params)) in
        let tx = sign key {tx with T.amount} in
        List.iter (fun changed ->
          need "altered bridge signature accepted" (Result.is_error (ingress ledger changed));
          need "altered bridge block accepted" (Result.is_error (bundle ledger env [changed])))
          [{tx with T.amount = Z.succ tx.amount}; {tx with T.nonce = nonce + 1};
           {tx with T.ou = Z.succ tx.ou}; {tx with T.encrypted_data = Some "changed"}];
        ignore (select ledger tx);
        need "signed bridge bundle refused" (Result.is_ok (bundle ledger env [tx]));
        run (C.Ledger.flush_dirty_lwt ledger);
        ignore (Source_proposal.check_many ~progress:false ~store ~ledger ~env ~expected:[tx] [tx]);
        run (S.begin_epoch_batch store);
        let (confirmed, reason), receipt = sender path backend env tx in
        need ("signed bridge call refused: " ^ reason) confirmed;
        run (C.Ledger.flush_dirty_lwt ledger);
        run (S.commit_epoch_batch store "bridge call");
        C.Tx_staging.remove_processed [T.hash tx];
        need "bridge transaction replay accepted" (Result.is_error (ingress ledger tx));
        match receipt with Some value -> `Assoc ["op", `String (T.op_type_to_string operation);
          "vault", `String vault; "method", `String payload; "receipt", value]
        | None -> failwith "bridge receipt absent" in
      let locked = execute 1 "lock_to_eth" ["0x1234567890123456789012345678901234567890"] (Z.of_int 3_000_000) in
      let unlocked = execute 2 "unlock_trusted" [recipient; "1000000"; "burn-signed"] Z.zero in
      need "signed bridge reserve differs"
        ((Option.get (C.Ledger.find_opt ledger vault)).balance = Z.of_int 2_000_000);
      need "signed bridge payout differs"
        ((Option.get (C.Ledger.find_opt ledger recipient)).balance = Z.of_int 1_000_000);
      [locked; unlocked]))

let () =
  need "bridge sources and receipt output required" (Array.length Sys.argv = 4);
  let rows = [Sys.argv.(1); Sys.argv.(2)] |> List.concat_map (fun input ->
    [T.ContractCall; T.ProgramExec] |> List.concat_map (check input)) in
  save Sys.argv.(3) (`List rows);
  Printf.printf "event = bridge_path status = passed actions = %d\n" (List.length rows)