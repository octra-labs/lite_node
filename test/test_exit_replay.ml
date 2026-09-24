(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Exit_case

let exit = transaction ~epoch:exit_epoch ~nonce:2 T.ValidatorExit
let withdraw = transaction ~epoch:mature ~nonce:3 T.ValidatorWithdraw
let initial = [step first_epoch [bond ()]; step exit_epoch [exit]]

let count expected (artifacts, _) =
  expect "confirmation count" (List.length artifacts.X.confirmed = expected)

let refused reason (artifacts, _) =
  expect "refused without fee" (Z.equal artifacts.X.confirmed_fees Z.zero);
  expect "exact refusal"
    (match artifacts.X.confirmed, artifacts.rejected with
     | [], [entry] -> entry.X.reason = reason
     | _ -> false)

let registered view =
  view.registry |> Option.get |> R.of_string |> get "registry"
  |> R.find owner.address

let trace = initial @ [
  step (exit_epoch + 1) [transaction ~epoch:(exit_epoch + 1) ~nonce:3 T.ValidatorExit];
  step (mature - 1) [transaction ~epoch:(mature - 1) ~nonce:3 T.ValidatorWithdraw];
  step ~active:true mature [withdraw];
  step (mature + 1) [withdraw];
  step (mature + 2) [withdraw];
]

let check_lifecycle () =
  let path = path "exit_cycle" in
  seed path;
  let rows = run path trace in
  count 1 (List.nth rows 0);
  count 1 (List.nth rows 1);
  let exited = snd (List.nth rows 1) in
  expect "exit recorded"
    ((Option.get (registered exited)).exit_epoch = Some (Int64.of_int exit_epoch));
  refused "validator exit already requested" (List.nth rows 2);
  refused "validator unbonding period is not complete" (List.nth rows 3);
  refused "active validator cannot withdraw" (List.nth rows 4);
  count 1 (List.nth rows 5);
  let paid = snd (List.nth rows 5) in
  expect "bond removed" (registered paid = None);
  expect "escrow released" (Z.equal paid.escrow Z.zero);
  expect "only three fees" Z.(equal paid.account.balance (sub balance (mul fee (of_int 3))));
  expect "withdraw nonce" (paid.account.nonce = 3);
  refused "validator bond not found" (List.nth rows 6);
  let repeated = snd (List.nth rows 6) in
  expect "duplicate does not pay" (paid.account = repeated.account && paid.escrow = repeated.escrow);
  with_store path (fun store ledger ->
    expect "withdraw survives reload" (view store ledger = repeated));
  rows

let split count values =
  List.filteri (fun index _ -> index < count) values,
  List.filteri (fun index _ -> index >= count) values

let check_restarts expected =
  for cut = 0 to List.length trace do
    let path = path "exit_restart" in
    seed path;
    let before, after = split cut trace in
    let prefix = run path before in
    let suffix = run path after in
    expect "restart preserves trace" (prefix @ suffix = expected)
  done

let check_maturity () =
  let path = path "exit_maturity" in
  seed path;
  ignore (run path initial);
  let row = run path [step mature [withdraw]] |> List.hd in
  count 1 row;
  expect "exact maturity pays" (registered (snd row) = None)

let check_no_exit () =
  let path = path "exit_missing" in
  seed path;
  ignore (run path [List.hd initial]);
  let tx = transaction ~epoch:mature ~nonce:2 T.ValidatorWithdraw in
  let row = run path [step mature [tx]] |> List.hd in
  refused "validator exit was not requested" row;
  expect "missing exit keeps bond" (registered (snd row) <> None);
  expect "missing exit keeps nonce" ((snd row).account.nonce = 1)

let check_fee_balance () =
  let path = path "exit_fee" in
  seed path;
  ignore (run path initial);
  let before = with_store path view in
  let tx = transaction ~cost:balance ~epoch:mature ~nonce:3 T.ValidatorWithdraw in
  let row = run path [step mature [tx]] |> List.hd in
  count 0 row;
  expect "insufficient fee classified"
    (match (fst row).X.rejected with
     | [entry] -> entry.X.error_type = "insufficient_balance"
     | _ -> false);
  let state = snd row in
  expect "fee failure keeps account" (state.account = before.account);
  expect "fee failure keeps bond" (registered state <> None);
  expect "fee failure keeps escrow" (Z.equal state.escrow P.min_bond);
  expect "fee failure keeps nonce" (state.account.nonce = 2);
  count 1 (run path [step (mature + 1) [withdraw]] |> List.hd)

let check_epoch_limit () =
  let module A = Octra_core.Validator_admission in
  let last_exit = Int64.sub Int64.max_int P.unbonding_epochs in
  let candidate = A.{
    address = owner.address;
    pubkey = owner.public;
    bond = P.min_bond;
    bonded_epoch = 0L;
    ready_epoch = None;
    exit_epoch = Some last_exit;
  } in
  expect "last representable maturity"
    (A.withdraw_epoch P.parameters candidate = Ok Int64.max_int);
  expect "maturity before last epoch"
    (A.can_withdraw P.parameters ~current_epoch:(Int64.pred Int64.max_int)
       candidate = Ok false);
  expect "maturity at last epoch"
    (A.can_withdraw P.parameters ~current_epoch:Int64.max_int candidate = Ok true);
  let overflow = { candidate with exit_epoch = Some (Int64.succ last_exit) } in
  expect "maturity overflow refused"
    (A.withdraw_epoch P.parameters overflow = Error "epoch arithmetic overflow")

let slash ~voted ~epoch ~nonce =
  let vote proposal =
    let value = C.{
      chain_id; epoch_id = Int64.of_int voted; round = 0;
      vote_type = Precommit; proposal_id = String.make 32 proposal;
      validator = owner.address; signature = String.make 64 '\000';
    } in
    { value with signature =
      H.sign_ed25519 ~priv_raw:owner.secret ~msg:(H.vote_sign_bytes value) }
  in
  let proof =
    Octra_consensus.C_evidence.vote_conflict (vote 'a') (vote 'b')
    |> Option.get
  in
  transaction ~epoch ~nonce
    ~message:(Octra_core.Validator_evidence.message proof) T.ValidatorEvidence

let check_slash_after_exit () =
  let tx = slash ~voted:first_epoch ~epoch:(exit_epoch + 1) ~nonce:3 in
  let path = path "exit_slash" in
  seed path;
  ignore (run path initial);
  let before = with_store path view in
  let row = run path [step (exit_epoch + 1) [tx]] |> List.hd in
  count 1 row;
  let slashed = snd row in
  expect "exit does not avoid slash" (registered slashed = None);
  expect "slashed escrow empty" (Z.equal slashed.escrow Z.zero);
  let fee_split = Octra_core.Fee_policy.split ~active:true fee |> get "fee split" in
  expect "slashed bond retired"
    Z.(equal slashed.retired (add before.retired (add P.min_bond fee_split.burned)));
  let row = run path [step mature [withdraw]] |> List.hd in
  refused "validator bond not found" row;
  expect "slashed bond not paid after restart" ((snd row).account = slashed.account)

let check_credit_failure () =
  let path = path "exit_credit" in
  seed path;
  ignore (run path initial);
  with_store path (fun store ledger ->
    let before = view store ledger in
    let backend, env, reward = backend store ledger (step mature [withdraw]) in
    let ops = { backend.X.ops with credit = (fun address amount ->
      if address = owner.address then Error "credit unavailable"
      else backend.ops.credit address amount) } in
    let result = execute { backend with ops } env reward [withdraw] |> get "credit failure" in
    refused "credit unavailable" (result.artifacts, view store ledger);
    let after = view store ledger in
    expect "credit failure rolls back account" (after.account = before.account);
    expect "credit failure rolls back escrow" (after.escrow = before.escrow);
    expect "credit failure rolls back registry" (after.registry = before.registry));
  count 1 (run path [step (mature + 1) [withdraw]] |> List.hd)

let check_commit_failure () =
  let path = path "exit_commit" in
  seed path;
  ignore (run path initial);
  let before = with_store path view in
  with_store path (fun store ledger ->
    let backend, env, reward = backend store ledger (step mature [withdraw]) in
    let backend = { backend with commit_batch = (fun () -> Lwt.fail_with "commit interrupted") } in
    expect "commit failure reported" (Result.is_error (execute backend env reward [withdraw]));
    expect "failed commit preserves state" (view store ledger = before));
  expect "failed commit reload" (with_store path view = before);
  count 1 (run path [step mature [withdraw]] |> List.hd)

let check_prefix expected =
  let path = path "exit_prefix" in
  seed path;
  let early, _ = split 4 trace in
  let future = [step (mature + 1) []] in
  let actual = run path (early @ future) in
  expect "future does not change prefix" (fst (split 4 actual) = fst (split 4 expected))

let rec wait_child pid =
  try Unix.waitpid [] pid with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait_child pid

let check_process_exit committed =
  let expected =
    let reference = path "exit_reference" in
    seed reference;
    let rows = run reference (initial @ [step mature [withdraw]]) in
    snd (List.nth rows 2)
  in
  let path = path "exit_process" in
  seed path;
  ignore (run path initial);
  let before = with_store path view in
  let pid = Unix.fork () in
  if pid = 0 then begin
    with_store path (fun store ledger ->
      let backend, env, reward = backend store ledger (step mature [withdraw]) in
      let commit_batch () =
        if not committed then Unix._exit 75;
        let open Lwt.Syntax in
        let* () = backend.X.commit_batch () in
        Unix._exit 75
      in
      ignore (execute { backend with commit_batch } env reward [withdraw]));
    Unix._exit 76
  end;
  let _, status = wait_child pid in
  expect "process exited at commit" (status = Unix.WEXITED 75);
  let restored = with_store path view in
  if committed then begin
    expect "process recovery matches replay" (restored = expected);
    expect "committed exit removes bond" (registered restored = None);
    expect "committed exit removes escrow" (Z.equal restored.escrow Z.zero);
    expect "committed exit credits once"
      Z.(equal restored.account.balance (add before.account.balance (sub P.min_bond fee)));
    let retry = run path [step (mature + 1) [withdraw]] |> List.hd in
    refused "validator bond not found" retry;
    expect "process retry keeps payment" ((snd retry).account = restored.account)
  end else begin
    expect "uncommitted exit restores prefix" (restored = before);
    count 1 (run path [step mature [withdraw]] |> List.hd)
  end

let check_exit_switch () =
  let gate = (Option.get (G.exit_activation rules)).activation_epoch in
  let start = gate - 9_001 in
  let path = path "exit_switch" in
  seed path;
  let exit = transaction ~epoch:(start + 1) ~nonce:2 T.ValidatorExit in
  let tx = transaction ~epoch:(gate - 1) ~nonce:3 T.ValidatorWithdraw in
  let rows = run path [
    step start [bond ~epoch:start ()];
    step (start + 1) [exit];
    step (gate - 1) [tx];
  ] in
  count 1 (List.nth rows 0);
  count 1 (List.nth rows 1);
  refused "validator unbonding period is not complete" (List.nth rows 2);
  let before = with_store path view in
  let row = run path [step ~active:true gate [tx]] |> List.hd in
  refused "active validator cannot withdraw" row;
  let after = snd row in
  expect "activation cannot erase active bond"
    (after.account = before.account && after.registry = before.registry
     && Z.equal after.escrow before.escrow);
  let row = run path [step gate [tx]] |> List.hd in
  count 1 row;
  let paid = snd row in
  expect "old exit released after switch" (registered paid = None && Z.equal paid.escrow Z.zero);
  expect "switch pays once" Z.(equal paid.account.balance (add before.account.balance (sub P.min_bond fee)));
  expect "switch state survives reopen" (with_store path view = paid);
  let row = run path [step (gate + 1) [tx]] |> List.hd in
  refused "validator bond not found" row;
  expect "switch retry does not pay" ((snd row).account = paid.account)

let check_evidence_switch () =
  let gate = (Option.get (G.exit_activation rules)).activation_epoch in
  let start = gate - 9_001 in
  List.iteri (fun index (epoch, voted, accepted) ->
    let path = path ("exit_proof_" ^ string_of_int index) in
    seed path;
    ignore (run path [step start [bond ~epoch:start ()]]);
    let before = with_store path view in
    let tx = slash ~voted ~epoch ~nonce:2 in
    let row = run path [step epoch [tx]] |> List.hd in
    if accepted then begin
      count 1 row;
      expect "accepted proof removes bond" (registered (snd row) = None)
    end else begin
      refused "validator evidence expired" row;
      let after = snd row in
      expect "expired proof keeps bond and funds"
        (after.account = before.account && after.registry = before.registry
         && Z.equal after.escrow before.escrow)
    end;
    expect "proof decision survives restart" (with_store path view = snd row)
  ) [
    gate - 1, gate - 8_192, true;
    gate, gate - 8_192, false;
    gate, gate - 4_096, false;
    gate, gate - 4_095, true;
    gate + 1, gate - 4_095, false;
    gate + 1, gate - 4_094, true;
  ]

let () =
  check_exit_switch ();
  check_evidence_switch ();
  let rows = check_lifecycle () in
  check_restarts rows;
  check_maturity ();
  check_no_exit ();
  check_fee_balance ();
  check_epoch_limit ();
  check_slash_after_exit ();
  check_credit_failure ();
  check_commit_failure ();
  check_prefix rows;
  check_process_exit false;
  check_process_exit true;
  Exit_operator.check ();
  print_endline "status = pass test = exit_replay"