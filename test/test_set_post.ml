(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Post = Octra_node_runtime.Set_post
module Tx = Octra_core.Transaction
module Staging = Octra_core.Tx_staging
module Rule = Octra_core.Rule_graph

let expect label value = if not value then failwith ("set_post: " ^ label)
let flush () = Lwt_main.run (Lwt.pause ())

let rec remove_tree path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_DIR; _ } ->
    Sys.readdir path
    |> Array.iter (fun name -> remove_tree (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let rec test_directory attempt =
  if attempt >= 1000 then failwith "set_post: test directory limit";
  let path = Filename.concat "runtime_data"
    (Printf.sprintf "set_post_%d_%d" (Unix.getpid ()) attempt) in
  try Unix.mkdir path 0o755; path
  with Unix.Unix_error (Unix.EEXIST, _, _) -> test_directory (attempt + 1)

let transaction head = Tx.{
  from = "sender"; to_ = "sender"; amount = Z.zero; nonce = 7;
  ou = Z.of_int 1_000; timestamp = 0.; signature = "signed";
  public_key = Some "key"; encrypted_data = None; op_type = ValidatorReady;
  message = Some (Yojson.Safe.to_string (`Assoc [
    "consensus_pubkey", `String "key";
    "head_epoch", `String (Int64.to_string head);
    "head_proposal_id", `String (String.make 64 'a');
    "state_root", `String (String.make 64 'b');
  ]));
}

let wire tx = Yojson.Safe.to_string (Tx.to_yojson tx)

type sample = {
  mutable clock : float;
  mutable head : int64;
  mutable staged : bool;
  mutable consumed : bool;
  mutable control : bool;
  mutable stage_ok : bool;
  mutable eligible : bool;
  mutable calls : string list;
  mutable guards : int;
  mutable timers : (float * unit Lwt.t * unit Lwt.u) list;
  mutable response : (unit, Post.failure) result Lwt.t;
}

let setup () =
  let sample = {
    clock = 0.; head = 100L; staged = true; consumed = false;
    control = true; stage_ok = true; eligible = true;
    calls = []; guards = 0; timers = []; response = Lwt.return_ok ();
  } in
  let post = Post.create Post.{
    now = (fun () -> sample.clock);
    wait = (fun delay ->
      let promise, reply = Lwt.task () in
      sample.timers <- (sample.clock +. delay, promise, reply) :: sample.timers;
      promise);
    staged = (fun _ -> sample.staged);
    landed = (fun _ -> sample.consumed);
    post = (fun _ -> failwith "retained item used prior submission");
    warn = (fun _ -> ());
  } in
  let retry = Post.{
    retain = true;
    eligible = (fun tx ->
      if Staging.duty_expired ~mode:Rule.Active ~head:(Some sample.head) tx then Expired
      else if sample.eligible then Eligible else Paused);
    post = (fun ~current tx ->
      sample.guards <- sample.guards + 1;
      if not (current ()) || not sample.control then
        Lwt.return_error (Wait "control paused")
      else if not sample.stage_ok then Lwt.return_error (Wait "staging unavailable")
      else begin
        sample.staged <- true;
        sample.calls <- wire tx :: sample.calls;
        sample.response
      end);
  } in
  sample, post, retry

let fire sample at =
  let timers = sample.timers
    |> List.filter (fun (_, promise, _) -> Lwt.is_sleeping promise)
    |> List.sort (fun (left, _, _) (right, _, _) -> Float.compare left right) in
  match timers with
  | [] -> failwith "set_post: timer missing"
  | (due, _, reply) :: rest ->
    expect "retry deadline" (Float.equal due at);
    sample.timers <- rest;
    sample.clock <- due;
    Lwt.wakeup reply ();
    flush ()

let check_window () =
  let tx = transaction 100L in
  List.iter (fun head ->
    expect "additional delay accepted"
      (not (Staging.duty_expired ~mode:Rule.Active ~head:(Some head) tx)))
    [100L; 101L; 102L];
  expect "third additional delay expires"
    (Staging.duty_expired ~mode:Rule.Active ~head:(Some 103L) tx);
  expect "prior still expires after one head"
    (Staging.duty_expired ~head:(Some 101L) tx)

let enrollment () =
  Octra_node_runtime.Status_read_rpc.{
    head_epoch = 100; head_proposal_id = Some (String.make 64 'a');
    state_root = String.make 64 'b'; chain_id = "test"; config_hash = String.make 64 'c';
    candidate = Some Octra_core.Validator_admission.{
      address = "sender"; pubkey = "key"; bond = Z.one; bonded_epoch = 1L;
      ready_epoch = None; exit_epoch = None;
    };
    duty = None; sets = None, None;
  }

let check_enrollment () =
  let module Control = Octra_node_runtime.Set_control in
  let snapshot = enrollment () in
  let eligible snapshot = Control.eligible ~mode:Rule.Active ~head:100L
    ~bonded_epoch:1L ~snapshot (transaction 100L) in
  expect "same bond cycle is eligible" (eligible (Ok snapshot) = Post.Eligible);
  expect "absent candidate retires duty"
    (eligible (Ok { snapshot with candidate = None }) = Post.Expired);
  let candidate = Option.get snapshot.candidate in
  expect "new bond cycle retires duty"
    (eligible (Ok { snapshot with candidate = Some { candidate with bonded_epoch = 2L } })
     = Post.Expired);
  expect "committed exit retires duty"
    (eligible (Ok { snapshot with candidate = Some { candidate with exit_epoch = Some 100L } })
     = Post.Expired);
  expect "old enrollment cannot retire current duty"
    (eligible (Ok { snapshot with head_epoch = 99; candidate = None }) = Post.Eligible);
  expect "unavailable enrollment permits guarded refresh"
    (eligible (Error "enrollment unavailable") = Post.Eligible)

let check_refresh_gate () =
  let module Control = Octra_node_runtime.Set_control in
  let snapshot = enrollment () in
  List.iter (fun prior ->
    let sample, post, retry = setup () in
    sample.staged <- false;
    let cached = ref prior in
    let refresh = ref (Lwt.return prior) in
    let retry = { retry with
      Post.eligible = (fun tx -> Control.eligible ~mode:Rule.Active ~head:sample.head
        ~bonded_epoch:1L ~snapshot:!cached tx);
      post = (fun ~current tx ->
        let open Lwt.Syntax in
        let* fresh = !refresh in
        cached := fresh;
        match fresh with
        | Ok { head_epoch; candidate = Some candidate; _ }
          when Int64.of_int head_epoch = sample.head
               && candidate.bonded_epoch = 1L && candidate.exit_epoch = None ->
          retry.post ~current tx
        | _ -> Lwt.return_error (Post.Wait "committed enrollment refresh required"));
    } in
    Fun.protect ~finally:(fun () -> Post.stop post) (fun () ->
      let tx = transaction 100L in
      Post.put ~retry post ~hash:"owned" tx;
      flush ();
      expect "missing or old refresh cannot stage or send"
        (not sample.staged && sample.calls = [] && sample.guards = 0 && Post.pending post);
      let ready, resume = Lwt.wait () in
      refresh := ready;
      fire sample 30.;
      expect "pending refresh has no delivery effects"
        (not sample.staged && sample.calls = [] && sample.guards = 0);
      sample.control <- false;
      Lwt.wakeup resume (Ok snapshot);
      flush ();
      expect "successful refresh still checks control"
        (not sample.staged && sample.calls = [] && sample.guards = 1 && Post.pending post);
      sample.control <- true;
      refresh := Lwt.return_ok snapshot;
      fire sample 60.;
      expect "fresh enrollment and guard permit exact retry"
        (sample.staged && sample.calls = [wire tx] && sample.guards = 2)))
    [Error "enrollment unavailable"; Ok { snapshot with head_epoch = 99; candidate = None }]

let check_head_root () =
  let module C = Octra_consensus.C_types in
  let module Hash = Octra_consensus.C_hash in
  let module Config = Octra_consensus.C_config in
  let module Head = Octra_core.Head_manifest in
  let module Chain = Octra_core.Store_chaindata in
  let module Source = Octra_node_runtime.Consensus_parent_commit in
  let module Status = Octra_node_runtime.Status_read_rpc in
  let module Text = Octra_node_runtime.Text in
  let module Guard = Octra_node_runtime.Consensus_epoch_apply_guard in
  Mirage_crypto_rng_unix.use_default ();
  let key, pub = Mirage_crypto_ec.Ed25519.generate () in
  let pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets pub in
  let validator_set = C.make_validator_set [C.{ address = "octA"; pubkey }] in
  let ledger_root = String.make 64 'b' in
  let epoch_index_root = String.make 64 'c' in
  let state_root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:ledger_root ~epoch_index_root in
  let header = C.{
    proto_version = proto_version_current; chain_id = "set-post-test"; epoch_id = 100L;
    prev_state_root = String.make 32 'a'; tx_list_hash = Hash.tx_list_hash [];
    receipt_root = Hash.receipt_root []; proposed_state_root = Guard.raw32_of_pre_root state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = "octA";
    txid_hi = 0L; ts = 1.;
  } in
  let proposal_id = Hash.proposal_id header in
  let vote = C.{
    chain_id = header.chain_id; epoch_id = header.epoch_id; round = 0;
    vote_type = Precommit; proposal_id; validator = "octA"; signature = "";
  } in
  let vote = { vote with signature = Mirage_crypto_ec.Ed25519.sign
    ~key (Hash.vote_sign_bytes vote) } in
  let parent_commit = C.{
    validator_set;
    certificate = {
      chain_id = header.chain_id; epoch_id = header.epoch_id; commit_round = 0;
      header; proposal_id; precommits = [vote];
    };
  } in
  let floor = Octra_core.History_floor.create ~chain_id:header.chain_id ~epoch:100
    ~state_root ~ledger_state_root:ledger_root ~txid_hi:0L ~config_hash:(String.make 64 'd')
    ~validator_set_hash:(Text.raw_to_hex (Config.validator_set_hash validator_set))
    ~epoch_index_hash:(String.make 64 'e') ~epoch_index_root ~parent_commit
    |> Result.get_ok in
  (try Unix.mkdir "runtime_data" 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let data_dir = test_directory 0 in
  Fun.protect ~finally:(fun () -> remove_tree data_dir) (fun () ->
    let chaindata = Chain.open_chaindata (Filename.concat data_dir "chaindata") in
    Fun.protect ~finally:(fun () -> Chain.close chaindata) (fun () ->
    Chain.seed_history_floor chaindata floor |> Result.get_ok;
    expect "test head has no archived header" (Chain.get_epoch_header chaindata 100 = None);
    let source = Source.create ~chain_id:header.chain_id ~data_dir ~chaindata
      (fun name -> if name = "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH" then Some "0" else None) in
    let head = Head.{
      schema_version; generation = 1; epoch_id = 100; state_root;
      ledger_state_root = Some ledger_root; irmin_commit = None;
      txid_hi = 0L; txlog_seg = None; txlog_off = None; epochlog_off = None;
      commit_id = "not-a-proposal-id"; ts = 1.; quorum_cert_hash = None;
      epoch_index_hash = None; epoch_index_root = None;
    } in
    let read head = Status.head_proposal_id ~source ~chain_id:header.chain_id ~head in
    List.iter (fun root ->
      expect "verified floor matches normalized current head"
        (read { head with state_root = root } = Ok (Text.raw_to_hex proposal_id)))
      [state_root; state_root ^ String.make 64 'f'; String.uppercase_ascii state_root];
    List.iter (fun root ->
      expect "invalid or different current root refused"
        (Result.is_error (read { head with state_root = root })))
      [""; String.make 63 'a'; String.make 65 'a'; String.make 128 '0';
       state_root ^ String.make 63 'f' ^ "g"];
    expect "head epoch must match verified floor" (Result.is_error (read { head with epoch_id = 101 }));
    expect "head chain must match certificate"
      (Result.is_error (Status.head_proposal_id ~source ~chain_id:"another-chain" ~head))));
  expect "test storage removed" (not (Sys.file_exists data_dir))

let check_duty_state () =
  let module Store = Octra_core.Store_irmin in
  let module Head = Octra_core.Head_manifest in
  let module Fold = Octra_core.Set_fold in
  let module Status = Octra_node_runtime.Status_read_rpc in
  let open Lwt.Syntax in
  (try Unix.mkdir "runtime_data" 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let data_dir = test_directory 0 in
  Fun.protect ~finally:(fun () -> remove_tree data_dir) (fun () ->
    Lwt_main.run (
      let* store = Store.open_store ~fresh:true (Filename.concat data_dir "store") in
      Lwt.finalize (fun () ->
        let* () = Store.set_meta store "last_epoch" "0" in
        let* () = Store.tag_epoch store 0 in
        let* ledger_root = Store.state_hash store in
        let head = Head.{
          schema_version; generation = 1; epoch_id = 0; state_root = String.make 64 'f';
          ledger_state_root = Some ledger_root; irmin_commit = None;
          txid_hi = 0L; txlog_seg = None; txlog_off = None; epochlog_off = None;
          commit_id = "duty-state-test"; ts = 1.; quorum_cert_hash = None;
          epoch_index_hash = None; epoch_index_root = None;
        } in
        let read head =
          let* state = Status.duty_state ~store ~head in
          Lwt.return (Result.map Fold.to_string state) in
        let* empty = read head in
        expect "verified absent duty metadata yields empty state"
          (empty = Ok (Fold.to_string Fold.empty));
        let* missing_root = read { head with ledger_state_root = None } in
        expect "missing ledger root cannot yield empty duty" (Result.is_error missing_root);
        let* missing_epoch = read { head with epoch_id = 1 } in
        expect "missing epoch snapshot cannot yield empty duty" (Result.is_error missing_epoch);
        let committed = Fold.note_pulse Fold.standard ~epoch:100L ~active:false
          ~address:"sender" Fold.empty |> Result.get_ok in
        let committed_raw = Fold.to_string committed in
        let* () = Store.set_meta store Fold.meta_key committed_raw in
        let* () = Store.set_meta store "last_epoch" "100" in
        let* () = Store.tag_epoch store 100 in
        let* ledger_root = Store.state_hash store in
        let head = { head with epoch_id = 100; ledger_state_root = Some ledger_root } in
        let* before = read head in
        expect "committed duty metadata decoded" (before = Ok committed_raw);
        let changed = Fold.note_pulse Fold.standard ~epoch:101L ~active:false
          ~address:"sender" committed |> Result.get_ok |> Fold.to_string in
        expect "batch duty metadata differs" (changed <> committed_raw);
        let* () = Store.begin_epoch_batch store in
        let* () = Store.set_meta store Fold.meta_key changed in
        let* () = Store.set_meta store "last_epoch" "101" in
        let* () = Lwt.pause () in
        let* visible = Store.get_meta store Fold.meta_key in
        expect "ordinary metadata read observes open batch" (visible = Some changed);
        let* during = read head in
        expect "duty snapshot ignores open batch" (during = Ok committed_raw);
        let wrong_root =
          (if ledger_root.[0] = '0' then "1" else "0")
          ^ String.sub ledger_root 1 (String.length ledger_root - 1) in
        let* wrong = read { head with ledger_state_root = Some wrong_root } in
        expect "wrong head ledger root rejected during batch" (Result.is_error wrong);
        let* () = Store.set_meta store Fold.meta_key "invalid" in
        let* during_invalid = read head in
        expect "invalid batch duty cannot replace committed duty" (during_invalid = Ok committed_raw);
        Store.abort_epoch_batch store;
        let* () = Store.set_meta store Fold.meta_key "invalid" in
        let* () = Store.set_meta store "last_epoch" "101" in
        let* () = Store.tag_epoch store 101 in
        let* ledger_root = Store.state_hash store in
        let* invalid = read { head with epoch_id = 101; ledger_state_root = Some ledger_root } in
        expect "invalid committed duty cannot yield empty state" (Result.is_error invalid);
        let* retained = read head in
        expect "captured head remains isolated from newer commits" (retained = Ok committed_raw);
        Lwt.return_unit)
        (fun () -> Store.abort_epoch_batch store; Store.close store)));
  expect "duty test storage removed" (not (Sys.file_exists data_dir))

let check_pool_loss () =
  let sample, post, retry = setup () in
  let tx = transaction 100L in
  Post.put ~retry post ~hash:"owned" tx;
  flush ();
  expect "RPC success retains ownership" (Post.pending post);
  sample.clock <- 601.;
  sample.staged <- false;
  Post.tick post;
  flush ();
  expect "pool TTL does not forget signed duty"
    (sample.staged && List.length sample.calls = 2 && Post.pending post);
  sample.head <- 101L;
  Post.put ~retry post ~hash:"replacement" { tx with nonce = 8; timestamp = 601. };
  flush ();
  expect "new reference cannot replace owned bytes" (List.length sample.calls = 2);
  fire sample 631.;
  sample.head <- 102L;
  fire sample 661.;
  expect "all retries preserve signed serialization"
    (List.length sample.calls = 4 && List.for_all (( = ) (wire tx)) sample.calls);
  sample.head <- 103L;
  fire sample 691.;
  expect "window expiry releases owner" (not (Post.pending post));
  expect "expired item is not posted" (List.length sample.calls = 4);
  Post.stop post

let check_guard_stage () =
  let sample, post, retry = setup () in
  let tx = transaction 100L in
  Post.put ~retry post ~hash:"owned" tx;
  flush ();
  sample.control <- false;
  sample.staged <- false;
  fire sample 30.;
  expect "control checked on retry"
    (sample.guards = 2 && List.length sample.calls = 1 && not sample.staged);
  expect "exit intent preserves paused ownership" (Post.pending post);
  sample.control <- true;
  sample.stage_ok <- false;
  fire sample 60.;
  expect "failed staging prevents post" (List.length sample.calls = 1 && not sample.staged);
  sample.stage_ok <- true;
  fire sample 90.;
  expect "resume re-stages exact transaction"
    (sample.staged && sample.calls = [wire tx; wire tx]);
  sample.consumed <- true;
  Post.tick post;
  expect "consumed nonce releases owner" (not (Post.pending post));
  Post.stop post

let check_retry_pace () =
  let sample, post, retry = setup () in
  sample.response <- Lwt.return_error (Post.Retry "busy");
  let tx = transaction 100L in
  Post.put ~retry post ~hash:"owned" tx;
  flush ();
  fire sample 1.;
  fire sample 3.;
  sample.clock <- 8.;
  Post.put ~retry post ~hash:"owned" tx;
  Post.tick post;
  expect "same item keeps retry pace" (List.length sample.calls = 3);
  fire sample 33.;
  sample.eligible <- false;
  fire sample 63.;
  expect "unavailable eligibility pauses post" (List.length sample.calls = 4 && Post.pending post);
  sample.eligible <- true;
  fire sample 93.;
  expect "eligibility recovery resumes exact bytes"
    (List.length sample.calls = 5 && List.for_all (( = ) (wire tx)) sample.calls);
  Post.stop post

let check_refusal () =
  let sample, post, retry = setup () in
  sample.response <- Lwt.return_error (Post.Refused "invalid");
  let tx = transaction 100L in
  Post.put ~retry post ~hash:"owned" tx;
  flush ();
  fire sample 30.;
  Post.put ~retry post ~hash:"replacement" { tx with timestamp = 30. };
  expect "refusal cannot trigger re-signing"
    (Post.pending post && List.length sample.calls = 1);
  sample.head <- 103L;
  Post.tick post;
  expect "refused reference expires" (not (Post.pending post));
  Post.stop post

let check_late_reply () =
  let sample, post, retry = setup () in
  let response, reply = Lwt.wait () in
  sample.response <- response;
  Post.put ~retry post ~hash:"owned" (transaction 100L);
  sample.head <- 103L;
  expect "expired in-flight item blocks new signing" (Post.pending post);
  Lwt.wakeup reply (Ok ());
  flush ();
  expect "completed expired attempt releases slot" (not (Post.pending post));
  sample.response <- Lwt.return_ok ();
  Post.put ~retry post ~hash:"next" (transaction 103L);
  flush ();
  expect "next reference can start" (List.length sample.calls = 2);
  Post.stop post;
  sample.clock <- 1000.;
  Post.tick post;
  expect "stop cannot dispatch" (List.length sample.calls = 2)

let check_stop_prepare () =
  let sample, post, retry = setup () in
  let release, wake = Lwt.wait () in
  let retry = { retry with Post.post = (fun ~current tx ->
    let open Lwt.Syntax in
    let* () = release in
    retry.post ~current tx)
  } in
  Post.put ~retry post ~hash:"owned" (transaction 100L);
  Post.stop post;
  Lwt.wakeup wake ();
  flush ();
  expect "shutdown during preparation prevents dispatch" (sample.calls = [])

let () =
  check_window ();
  check_enrollment ();
  check_refresh_gate ();
  check_head_root ();
  check_duty_state ();
  check_pool_loss ();
  check_guard_stage ();
  check_retry_pace ();
  check_refusal ();
  check_late_reply ();
  check_stop_prepare ()