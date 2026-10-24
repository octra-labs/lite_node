(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus

module E = C_engine
module C = C_types

let check message condition = if not condition then failwith message

let members = match C.make_weighted_validator_set (List.init 34 (fun i ->
  C.{address = "v" ^ string_of_int i; pubkey = String.make 32 (Char.chr (i + 1))}, Z.one)) with
  | Ok value -> value
  | Error reason -> failwith reason

let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L)

let node () =
  let engine = E.create_for_policy policy ~chain_id:"octra-test-round-wait" ~my_addr:"v0"
    ~validator_set:members ~start_height:1L ~can_vote:(fun () -> true) in
  E.start_height engine 1L;
  ignore (E.drain_outputs engine);
  engine

let sign _ = String.make 64 '\000'

let vote engine kind i pid = C.{
  chain_id = engine.E.chain_id; epoch_id = engine.state.height;
  round = engine.state.round; vote_type = kind; proposal_id = pid;
  validator = "v" ^ string_of_int i; signature = sign "";
}

let timers step engine = E.drain_outputs engine |> List.filter (function
  | E.ScheduleTimeout timer -> timer.step = step
  | _ -> false)

let timeout engine step =
  E.on_timeout engine ~step ~round:engine.E.state.round
    ~generation:engine.generation ~sign_fn:sign

let test_single_clock () =
  let engine = node () in
  timeout engine C.ProposeStep;
  check "single vote armed prevote timer" (timers C.PrevoteStep engine = []);
  for _ = 1 to 100 do timeout engine C.PrevoteStep done;
  check "single node advanced without quorum"
    (engine.state.round = 0 && engine.state.step = C.PrevoteStep)

let test_activation_clock () =
  let plan = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 10L) ~round_epoch:(Some 10L) in
  List.iter (fun (height, enabled) ->
    let engine = E.create_for_policy plan ~chain_id:"octra-test-clock-plan"
      ~my_addr:"v0" ~validator_set:members ~start_height:height
      ~can_vote:(fun () -> true) in
    E.start_height engine height;
    ignore (E.drain_outputs engine);
    timeout engine C.ProposeStep;
    check "clock activation changed the wrong height"
      ((timers C.PrevoteStep engine = []) = enabled)
  ) [9L, false; 10L, true; 11L, true]

let test_two_stages () =
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = C_quorum_policy.for_chain chain_id in
  let released = C_quorum_policy.with_round_epoch plan None in
  List.iter (fun (height, stake, waiting) ->
    check "stake stage moved" ((C_quorum_policy.select plan height = C_quorum_policy.Stake) = stake);
    check "round stage moved" (C_quorum_policy.round_wait plan height = waiting);
    let trace policy =
      let engine = E.create_for_policy policy ~chain_id ~my_addr:"v0"
        ~validator_set:members ~start_height:height ~can_vote:(fun () -> true) in
      E.start_height engine height;
      let first = E.drain_outputs engine in
      timeout engine C.ProposeStep;
      let second = E.drain_outputs engine in
      timeout engine C.PrevoteStep;
      let third = E.drain_outputs engine in
      engine.state.round, engine.state.step, first, second, third in
    let left = trace plan and right = trace released in
    check "published clock compatibility differs" ((left = right) = not waiting))
    [1_737_332L, false, false; 1_737_333L, true, false;
     1_739_999L, true, false; 1_740_000L, true, true];
  check "second stage omitted from profile plan"
    (C_quorum_policy.epochs plan = [1_737_333L; 1_740_000L]);
  check "rewind crossed second stage"
    (not (C_quorum_policy.rewind_allowed ~chain_id ~from_epoch:1_740_000L ~to_epoch:1_739_999L))

let test_quorum_timer () =
  let engine = node () in
  timeout engine C.ProposeStep;
  ignore (E.drain_outputs engine);
  let deliver i =
    E.on_vote engine (vote engine C.Prevote i
      (if i mod 2 = 0 then String.make 32 '\001' else String.make 32 '\002'))
      ~sign_fn:sign in
  for i = 1 to 21 do deliver i done;
  check "less than quorum armed timer" (timers C.PrevoteStep engine = []);
  deliver 22;
  check "quorum did not arm timer" (List.length (timers C.PrevoteStep engine) = 1);
  for i = 1 to 22 do deliver i done;
  deliver 23;
  check "duplicate or extra vote reset timer" (timers C.PrevoteStep engine = []);
  timeout engine C.PrevoteStep;
  check "quorum timeout did not precommit" (engine.state.step = C.PrecommitStep);
  check "single precommit armed timer" (timers C.PrecommitStep engine = []);
  let nil = Octra_net.Hash_domain.nil_hash in
  for i = 1 to 22 do E.on_vote engine (vote engine C.Precommit i nil) ~sign_fn:sign done;
  check "precommit quorum did not arm timer"
    (List.length (timers C.PrecommitStep engine) = 1);
  timeout engine C.PrecommitStep;
  check "precommit quorum did not advance round" (engine.state.round = 1)

let test_relief_clock () =
  let set count = match C.make_weighted_validator_set (List.init count (fun i ->
    C.{address = "r" ^ string_of_int i; pubkey = String.make 32 (Char.chr (i + 1))}, Z.one)) with
    | Ok value -> value | Error reason -> failwith reason in
  let current = set 7 in
  let height = Option.get (C_quorum_policy.linear_epoch ~chain_id:"octra-devnet-9871-cluster") in
  let limit chain_id target = C_relief.clock_limit ~chain_id ~height
    ~current ~activate_epoch:(Int64.succ height) ~target ~fingerprint:(String.make 64 'a') in
  check "valid relief plan lost its clock"
    (limit "octra-devnet-9871-cluster" (set 6) = Some C_relief.min_round);
  check "unchanged set enabled relief clock"
    (limit "octra-devnet-9871-cluster" current = None);
  check "wrong chain enabled relief clock" (limit "other" (set 6) = None);
  let engine = node () in
  E.set_relief_limit engine (Some C_relief.min_round);
  for round = 0 to C_relief.min_round - 1 do
    List.iter (timeout engine) C.[ProposeStep; PrevoteStep; PrecommitStep];
    check "checked relief clock stopped early" (engine.state.round = round + 1)
  done;
  timeout engine C.ProposeStep;
  timeout engine C.PrevoteStep;
  check "relief clock exceeded proof round" (engine.state.round = C_relief.min_round
    && engine.state.step = C.PrevoteStep);
  E.start_height engine 2L;
  check "new height retained relief clock" (engine.relief_limit = None)

let test_missing_offer () =
  let engine = node () in
  timeout engine C.ProposeStep;
  ignore (E.drain_outputs engine);
  let pid = String.make 32 '\003' in
  for i = 1 to 23 do E.on_vote engine (vote engine C.Prevote i pid) ~sign_fn:sign done;
  check "missing proposal blocked quorum timer"
    (List.length (timers C.PrevoteStep engine) = 1);
  timeout engine C.PrevoteStep;
  check "missing proposal blocked nil precommit" (engine.state.step = C.PrecommitStep);
  check "missing proposal created lock" (engine.state.locked_round = -1)

let test_relief_phase () =
  List.iter (fun step ->
    List.iter (fun kind ->
      let engine = node () in
      E.set_relief_limit engine (Some C_relief.min_round);
      if step = C.PrevoteStep then timeout engine C.ProposeStep;
      E.on_vote engine (vote engine kind 1 Octra_net.Hash_domain.nil_hash) ~sign_fn:sign;
      check "relief vote skipped the voting phase" (engine.state.step = step)
    ) C.[Prevote; Precommit]
  ) C.[ProposeStep; PrevoteStep]

let test_weighted_wait () =
  let members = match C.make_weighted_validator_set (List.init 4 (fun i ->
    C.{address = "w" ^ string_of_int i; pubkey = String.make 32 (Char.chr (i + 1))},
    Z.of_int (if i = 3 then 8 else 1))) with
    | Ok value -> value | Error reason -> failwith reason in
  let engine = E.create_for_policy policy ~chain_id:"octra-test-weighted-clock"
    ~my_addr:"w0" ~validator_set:members ~start_height:1L ~can_vote:(fun () -> true) in
  E.start_height engine 1L;
  ignore (E.drain_outputs engine);
  timeout engine C.ProposeStep;
  ignore (E.drain_outputs engine);
  let deliver i = E.on_vote engine C.{chain_id = engine.chain_id; epoch_id = 1L;
    round = 0; vote_type = Prevote; proposal_id = String.make 32 (Char.chr (i + 1));
    validator = "w" ^ string_of_int i; signature = sign ""} ~sign_fn:sign in
  deliver 1;
  deliver 2;
  check "signer count bypassed required weight" (timers C.PrevoteStep engine = []);
  deliver 3;
  check "dual quorum did not arm timer" (List.length (timers C.PrevoteStep engine) = 1)

type event =
  | Boot of int
  | Timer of int * C.round_step * int * int
  | Vote of int * C.vote
  | Offer of int * C.propose
  | Watch of int
  | Sync of int * int * string

module Events = Map.Make (struct
  type t = int * int
  let compare = compare
end)

let test_delayed_quorum ?(relief = false) ?(ahead = false) offset =
  let count = 23 in
  let engines = Array.init count (fun i ->
    E.create_for_policy policy ~chain_id:"octra-test-round-wait" ~my_addr:("v" ^ string_of_int (i + offset))
      ~validator_set:members ~start_height:1L ~can_vote:(fun () -> true)) in
  let awake = Array.make count false in
  let start = Array.init count (fun i -> if relief || ahead then 0 else i * 12_000) in
  let progress = Array.make count (0, C.ProposeStep, 0) in
  let offers = Hashtbl.create 16 in
  let finals = Array.make count None in
  let events = ref Events.empty and serial = ref 0 and now = ref 0 in
  let schedule at event =
    incr serial;
    events := Events.add (at, !serial) event !events in
  let send source event =
    for destination = 0 to count - 1 do
      if source <> destination && start.(destination) <= !now then
        schedule (!now + 10 + destination mod 7)
          (event destination)
    done in
  let rec drain index =
    E.drain_outputs engines.(index) |> List.iter (function
      | E.ScheduleTimeout timer -> schedule (!now + timer.delay_ms)
          (Timer (index, timer.step, timer.round, timer.generation))
      | E.SendVote (vote, _) -> send index (fun destination -> Vote (destination, vote))
      | E.SendPropose offer ->
        Hashtbl.replace offers offer.C.round offer;
        send index (fun destination -> Offer (destination, offer))
      | E.Finalized value -> finals.(index) <- Some value.finalize.proposal_id
      | E.SendFinalize _ | E.RequestProposal _ | E.RequestRoundEvidence _ -> ());
    let engine = engines.(index) in
    if engine.state.step = C.ProposeStep && finals.(index) = None then begin
      match Hashtbl.find_opt offers engine.state.round with
      | Some offer ->
        E.on_propose engine offer ~verify_fn:(fun _ _ _ -> true)
          ~execute_fn:(fun _ -> true) ~sign_fn:sign;
        if engine.state.step <> C.ProposeStep then drain index
      | None when E.am_i_leader engine ->
        let header = C.{proto_version = C_protocol.version_for_epoch 1L;
          chain_id = engine.chain_id; epoch_id = 1L;
          prev_state_root = String.make 32 '\000';
          tx_list_hash = E.tx_list_hash_for_header []; receipt_root = C_hash.receipt_root [];
          proposed_state_root = String.make 32 '\001';
          parent_commit_hash = Octra_net.Hash_domain.nil_hash;
          creator_addr = engine.my_addr; txid_hi = 0L; ts = 1.} in
        E.do_propose engine header [] ~sign_fn:sign;
        if engine.state.step <> C.ProposeStep then drain index
      | None -> ()
    end in
  Array.iteri (fun i at -> schedule at (Boot i)) start;
  let processed = ref 0 in
  while not (Array.for_all Option.is_some finals) && not (Events.is_empty !events)
      && !now < 1_200_000 && !processed < 100_000 do
    let ((at, _) as key), event = Events.min_binding !events in
    events := Events.remove key !events;
    now := at;
    incr processed;
    let index = match event with
      | Boot i ->
        awake.(i) <- true;
        E.start_height engines.(i) 1L;
        if relief then E.set_relief_limit engines.(i) (Some C_relief.min_round);
        if ahead && i = count - 1 then E.realign_round engines.(i) 2;
        schedule (!now + 15_000) (Watch i);
        i
      | Watch i ->
        let engine = engines.(i) in
        let round, step, since = progress.(i) in
        if round <> engine.state.round || step <> engine.state.step then
          progress.(i) <- engine.state.round, engine.state.step, !now
        else if !now - since >= 180_000 then begin
          ignore (E.try_round_skip engine || E.try_round_step engine);
          progress.(i) <- engine.state.round, engine.state.step, !now;
          send i (fun destination -> Sync (destination, engine.state.round, engine.my_addr));
          List.iter (fun votes -> Hashtbl.iter (fun _ (vote : C.vote) ->
            if vote.C.validator = engine.my_addr then
              send i (fun destination -> Vote (destination, vote))) votes.E.votes)
            [engine.prevotes; engine.precommits]
        end;
        schedule (!now + 15_000) (Watch i);
        i
      | Sync (i, round, validator) ->
        ignore (E.on_round_sync engines.(i) ~round ~validator); i
      | Timer (i, step, round, generation) ->
        E.on_timeout engines.(i) ~step ~round ~generation ~sign_fn:sign; i
      | Vote (i, vote) -> E.on_vote engines.(i) vote ~sign_fn:sign; i
      | Offer (i, offer) ->
        if offer.C.round = engines.(i).state.round then
          E.on_propose engines.(i) offer ~verify_fn:(fun _ _ _ -> true)
            ~execute_fn:(fun _ -> true) ~sign_fn:sign;
        i in
    if awake.(index) then drain index
  done;
  check (Printf.sprintf "delayed quorum did not finalize ahead = %b relief = %b time = %d rounds = %s"
    ahead relief !now (String.concat "," (Array.to_list (Array.map (fun engine ->
      string_of_int engine.E.state.round) engines)))) (Array.for_all Option.is_some finals);
  check "delayed quorum finalized different blocks"
    (Array.for_all (fun value -> value = finals.(0)) finals);
  if relief && offset = 0 then
    check "relief prevented round zero finalization"
      (Array.for_all (fun engine -> engine.E.state.round = 0) engines);
  check "late startup was skipped" (!now >= start.(count - 1))

let () =
  test_single_clock ();
  test_activation_clock ();
  test_two_stages ();
  test_quorum_timer ();
  test_relief_clock ();
  test_relief_phase ();
  test_missing_offer ();
  test_weighted_wait ();
  test_delayed_quorum 0;
  test_delayed_quorum 2;
  test_delayed_quorum ~relief:true 0;
  test_delayed_quorum ~ahead:true 0;
  print_endline "status = pass suite = round_wait"