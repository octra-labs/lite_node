(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus
module E = C_engine
module C = C_types

let sign _ = String.make 64 's'
let chain_id = "octra-devnet-9871-cluster"
let members = Result.get_ok (C.make_weighted_validator_set (List.init 4 (fun index ->
  C.{address = "v" ^ string_of_int index; pubkey = String.make 32 (Char.chr (index + 1))}, Z.one)))

let run height restore =
  let engines = Array.init 4 (fun index -> E.create ~chain_id ~my_addr:("v" ^ string_of_int index)
    ~validator_set:members ~start_height:height ~can_vote:(fun () -> true)) in
  Array.iter (fun engine -> E.start_height engine height) engines;
  let output = ref [] in
  let snapshot () =
    Array.iter (fun engine ->
      output := (engine.E.my_addr, engine.state, E.drain_outputs engine) :: !output) engines in
  snapshot ();
  let leader = Array.find_opt E.am_i_leader engines |> Option.get in
  let header = C.{proto_version = C_protocol.version_for_epoch height; chain_id; epoch_id = height;
    prev_state_root = String.make 32 '\000'; tx_list_hash = E.tx_list_hash_for_header [];
    receipt_root = C_hash.receipt_root []; proposed_state_root = String.make 32 '\001';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = leader.my_addr;
    txid_hi = 0L; ts = 1.} in
  E.do_propose leader header [] ~sign_fn:sign;
  let offer = E.drain_outputs leader |> List.find_map (function E.SendPropose value -> Some value | _ -> None)
    |> Option.get in
  if restore then Array.iter (fun engine -> ignore (E.restore_precommit_lock engine offer)) engines
  else Array.iter (fun engine -> E.on_propose engine offer ~verify_fn:(fun _ _ _ -> true)
    ~execute_fn:(fun _ -> true) ~sign_fn:sign) engines;
  snapshot ();
  List.iter (fun kind ->
    Array.iteri (fun index sender ->
      let vote = C.{chain_id; epoch_id = height; round = sender.E.state.round;
        vote_type = kind; proposal_id = C_hash.proposal_id header;
        validator = "v" ^ string_of_int index; signature = sign ""} in
      Array.iter (fun engine -> E.on_vote engine vote ~sign_fn:sign) engines;
      snapshot ()) engines) C.[Prevote; Precommit];
  Array.iter (fun engine -> List.iter (fun step ->
    E.on_timeout engine ~step ~round:engine.E.state.round ~generation:engine.generation ~sign_fn:sign)
    C.[ProposeStep; PrevoteStep; PrecommitStep]) engines;
  snapshot ();
  List.rev !output

let () =
  Octra_log._set_level Octra_log.FATAL;
  let rows = List.concat_map (fun height -> List.map (fun restore ->
    let bytes = Marshal.to_string (run height restore) [Marshal.No_sharing] in
    `Assoc ["height", `String (Int64.to_string height); "restore", `Bool restore;
      "trace", `String (Base64.encode_exn bytes)]) [false; true])
    [1_737_332L; 1_737_333L; 1_739_999L] in
  print_endline (Yojson.Safe.to_string (`List rows))