(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus

let fail check =
  failwith ("test_vote_log: " ^ check)

let expect check condition =
  if not condition then fail check

let value check = function
  | Ok result -> result
  | Error reason -> fail (check ^ ": " ^ reason)

let refused check = function
  | Ok _ -> fail check
  | Error _ -> ()

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
    end else
      Unix.unlink path

let work_dir () =
  let root = Filename.concat (Sys.getcwd ()) "runtime_data" in
  if not (Sys.file_exists root) then Unix.mkdir root 0o755;
  let name =
    Printf.sprintf "vote-log-unit-%d-%.0f"
      (Unix.getpid ())
      (Unix.gettimeofday () *. 1_000_000.)
  in
  let path = Filename.concat root name in
  Unix.mkdir path 0o700;
  path

let vote ~epoch_id ~round ~vote_type ~proposal_id ~signature =
  C_types.{
    chain_id = "octra-test-vote-log";
    epoch_id;
    round;
    vote_type;
    proposal_id;
    validator = "octVoteLog";
    signature;
  }

let round_of check = function
  | Some value -> value
  | None -> fail check

let () =
  let data_dir = work_dir () in
  Fun.protect
    ~finally:(fun () -> remove_tree data_dir)
    (fun () ->
      let store = C_vote_log.disk ~data_dir in
      let first =
        vote
          ~epoch_id:41L
          ~round:4
          ~vote_type:C_types.Prevote
          ~proposal_id:(String.make 32 '\x11')
          ~signature:(String.make 64 '\x21')
      in
      let stored = value "first write" (C_vote_log.keep store first) in
      expect "first signature preserved"
        (String.equal stored.signature first.signature);
      let replay = {
        first with
        signature = String.make 64 '\x22';
      } in
      let replayed = value "same statement replay" (C_vote_log.keep store replay) in
      expect "replay uses stored wire"
        (String.equal replayed.signature first.signature);
      let conflict = {
        first with
        proposal_id = String.make 32 '\x12';
        signature = String.make 64 '\x23';
      } in
      refused "conflicting statement is refused" (C_vote_log.keep store conflict);
      let second =
        vote
          ~epoch_id:42L
          ~round:1
          ~vote_type:C_types.Precommit
          ~proposal_id:(String.make 32 '\x31')
          ~signature:(String.make 64 '\x41')
      in
      ignore (value "next epoch write" (C_vote_log.keep store second));
      let found =
        C_vote_log.find_statement
          store
          ~chain_id:second.chain_id
          ~validator:second.validator
          ~epoch_id:second.epoch_id
          ~round:second.round
          ~vote_type:second.vote_type
          ~proposal_id:second.proposal_id
        |> value "exact statement lookup"
      in
      expect "exact statement survives reload" (found = Some second);
      let other =
        C_vote_log.find_statement
          store
          ~chain_id:second.chain_id
          ~validator:second.validator
          ~epoch_id:second.epoch_id
          ~round:second.round
          ~vote_type:second.vote_type
          ~proposal_id:(String.make 32 '\x32')
        |> value "different statement lookup"
      in
      expect "different statement is not returned" (other = None);
      let first_round =
        C_vote_log.max_round
          store
          ~chain_id:first.chain_id
          ~validator:first.validator
          ~epoch_id:first.epoch_id
        |> value "first round"
        |> round_of "first round missing"
      in
      expect "first maximum round" (first_round = 4);
      let second_round =
        C_vote_log.max_round
          store
          ~chain_id:second.chain_id
          ~validator:second.validator
          ~epoch_id:second.epoch_id
        |> value "second round"
        |> round_of "second round missing"
      in
      expect "second maximum round" (second_round = 1);
      ignore (value "prune finalized epoch" (C_vote_log.prune store ~through_epoch:41L));
      let removed =
        value "pruned epoch lookup"
          (C_vote_log.max_round
             store
             ~chain_id:first.chain_id
             ~validator:first.validator
             ~epoch_id:first.epoch_id)
      in
      expect "finalized record removed" (removed = None);
      let retained =
        C_vote_log.find_statement
          store
          ~chain_id:second.chain_id
          ~validator:second.validator
          ~epoch_id:second.epoch_id
          ~round:second.round
          ~vote_type:second.vote_type
          ~proposal_id:second.proposal_id
        |> value "retained statement lookup"
      in
      expect "next epoch statement is retained" (retained = Some second);
      let corrupt =
        Filename.concat
          (Filename.concat data_dir "vote_log")
          "00000000000000000042_00000003_bad.vote"
      in
      let channel = open_out_bin corrupt in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel "bad");
      refused "corrupt record holds local voting"
        (C_vote_log.max_round
           store
           ~chain_id:second.chain_id
           ~validator:second.validator
           ~epoch_id:second.epoch_id);
      Printf.printf "status = pass test = vote_log\n%!")