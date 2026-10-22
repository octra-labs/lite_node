(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module N = Octra_node_runtime
module Store = Octra_core.Store_irmin
module Data = Octra_core.Store_chaindata
module Origin = Octra_core.Store_origin
module Genesis = N.Genesis_start

let expect reason value = if not value then failwith reason

let wait pid =
  let deadline = Unix.gettimeofday () +. 120. in
  let rec poll () =
    if Unix.gettimeofday () >= deadline then begin
      Unix.kill pid Sys.sigkill;
      failwith "child did not finish"
    end;
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ -> Unix.sleepf 0.01; poll ()
    | _, status -> status
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> poll ()
  in
  poll ()

let member =
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 'g')
    |> function Ok key -> key | Error _ -> failwith "key refused" in
  let pub = Mirage_crypto_ec.Ed25519.pub_of_priv key
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  Octra_core.Crypto.Address.address_from_pubkey pub, pub

let write path text =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out output) (fun () -> output_string output text)

let with_stores dir run =
  let data, store = N.Startup_store_shell.open_stores dir in
  Fun.protect ~finally:(fun () ->
    Fun.protect ~finally:(fun () -> Data.close data)
      (fun () -> Lwt_main.run (Store.close store)))
    (fun () -> run data store)

let boot dir data store =
  let address, pub = member in
  N.Startup_node_boot_shell.run_node {
    data_dir = dir; store; ledger = Octra_core.Ledger.create store; chaindata = data;
    total_tx_count = ref 0; observer_mode = false; wallet = {address; pub};
    consensus_mode = true; voting_consensus_mode = true;
    consensus_port_configured = (fun () -> true);
    validators = (fun () -> [member]); int_value = (fun _ value -> value);
    env = (fun name -> if name = "OCTRA_CHAIN_ID" then Some "octra-devnet-9871-cluster" else None);
    exit_fatal = (fun () -> failwith "boot refused");
  }

let consume ?(chain = "octra-devnet-9871-cluster") ?(epoch = 0) ?(head = -1) dir value =
  Genesis.consume value ~data_dir:dir ~chain ~epoch ~head

let fresh dir run =
  with_stores dir (fun data store ->
    expect "fresh origin missing" (Option.is_some data.Data.origin);
    let epoch, value = boot dir data store in
    expect "genesis epoch differs" (epoch = 0);
    expect "genesis wrote HEAD" (Octra_core.Head_manifest.load_result dir = Missing);
    let value = Option.get value in
    run data store value)

let rejected_origin dir make =
  make dir;
  let data = Data.open_chaindata (Filename.concat dir "chaindata") in
  Fun.protect ~finally:(fun () -> Data.close data)
    (fun () -> expect "old state received origin" (data.Data.origin = None))

let account_fault fault =
  let writes = ref 0 in
  let action name = if name = fault then failwith name in
  let validators = match fault with
    | "empty" -> [] | "duplicate" -> [member; member] | _ -> [member] in
  let deps = N.Startup_account_shell.{
    ledger_empty = (fun () -> true); observer_mode = false;
    wallet_addr = fst member; wallet_pub = snd member;
    wallet_present = (fun () -> false); consensus_port_configured = (fun () -> true);
    validators = (fun () -> validators);
    add_account = (fun ~addr:_ ~pub:_ ~amount:_ ->
      incr writes; if fault = "account" then Error "write refused" else Ok ());
    set_meta = (fun ~key ~value:_ -> action key);
    flush_dirty = (fun () -> action "flush");
  } in
  let refused = try ignore (N.Startup_account_shell.run deps); false
    with Failure _ | Invalid_argument _ -> true in
  expect "genesis write failure accepted" refused;
  if fault = "empty" || fault = "duplicate" then
    expect "invalid validators wrote accounts" (!writes = 0)

let () =
  Test_workspace.with_dir "genesis_start" (fun root ->
    let cases = [
      "fresh", (fun dir -> fresh dir (fun _ _ value ->
        expect "genesis refused" (consume dir value);
        expect "genesis reused" (not (consume dir value))));
      "changed", (fun dir -> fresh dir (fun _ store value ->
        Lwt_main.run (Store.set_meta store "different" "1");
        expect "changed state accepted" (not (consume dir value))));
      "path_slash", (fun dir -> fresh (dir ^ "/") (fun _ _ value ->
        expect "directory spelling rejected" (consume dir value)));
      "epoch", (fun dir -> fresh dir (fun _ _ value ->
        expect "different epoch accepted" (not (consume ~epoch:1 ~head:0 dir value))));
      "chain", (fun dir -> fresh dir (fun _ _ value ->
        expect "different chain accepted" (not (consume ~chain:"different" dir value))));
      "journal", (fun dir -> fresh dir (fun _ _ value ->
        write (Filename.concat dir "commit_journal.log") "malformed";
        expect "new journal accepted" (not (consume dir value))));
      "fork", (fun dir -> fresh dir (fun _ _ value ->
        flush_all ();
        match Unix.fork () with
        | 0 -> Unix._exit (if consume dir value then 1 else 0)
        | pid ->
          expect "child reused origin" (wait pid = Unix.WEXITED 0);
          expect "parent lost origin" (consume dir value)));
      "closed", (fun dir -> fresh dir (fun data _ value ->
        Data.close data;
        expect "closed origin accepted" (not (consume dir value))));
      "replaced", (fun dir -> fresh dir (fun _ _ value ->
        let moved = dir ^ ".moved" in
        Unix.rename dir moved;
        Unix.mkdir dir 0o700;
        Fun.protect ~finally:(fun () -> Unix.rmdir dir; Unix.rename moved dir)
          (fun () -> expect "replaced directory accepted" (not (consume dir value)))));
      "head_link", (fun dir -> rejected_origin dir (fun dir ->
        Unix.symlink "missing" (Filename.concat dir "HEAD.json")));
      "finality", (fun dir -> rejected_origin dir (fun dir ->
        Unix.mkdir (Filename.concat dir "finality") 0o700));
      "vote", (fun dir -> rejected_origin dir (fun dir ->
        Unix.mkdir (Filename.concat dir "vote_log") 0o700));
      "empty_irmin", (fun dir -> rejected_origin dir (fun dir ->
        Unix.mkdir (Filename.concat dir "irmin_store") 0o700));
      "old_journal", (fun dir -> rejected_origin dir (fun dir ->
        write (Filename.concat dir "commit_journal.log") ""));
      "unknown", (fun dir -> rejected_origin dir (fun dir ->
        write (Filename.concat dir "unknown") ""));
      "reopen", (fun dir ->
        fresh dir (fun _ _ _ -> ());
        with_stores dir (fun data store ->
          expect "reopen received origin" (data.Data.origin = None);
          let refused = try ignore (boot dir data store); false with Failure _ -> true in
          expect "missing HEAD reopened" refused));
    ] in
    List.iter (fun (name, run) ->
      let dir = Filename.concat root name in
      Unix.mkdir dir 0o700;
      run dir;
      Printf.printf "event = passed case = %s\n%!" name) cases;
    List.iter account_fault
      ["empty"; "duplicate"; "account"; "emission_remaining"; "total_supply"; "flush"]);
  Printf.printf "status = pass test = genesis_start\n%!"