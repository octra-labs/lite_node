(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_consensus.C_types
module D = Octra_consensus.C_driver
module M = Octra_consensus.C_measure
module P = Octra_consensus.C_quorum_policy
module W = Octra_node_runtime.Consensus_pending_commit_recovery
module K = Quorum_pinned

let expect label value = if not value then failwith ("test_reply_rows: " ^ label)
let seen = Hashtbl.create 64
let note label = Hashtbl.replace seen label ()
let noted label = expect ("case was not reached " ^ label) (Hashtbl.mem seen label)

let same label where left right =
  note (label ^ " = " ^ string_of_bool right);
  if left <> right then
    failwith (Printf.sprintf "test_reply_rows: %s differs library = %b pinned = %b %s"
      label left right (where ()))

let both label = List.iter (fun value -> noted (label ^ " = " ^ string_of_bool value)) [true; false]

let chain_id = "octra-test-replies"

let member index =
  T.{address = "v" ^ string_of_int index; pubkey = String.make 32 (Char.chr (index + 1))}

let range first last = List.init (max 0 (last - first + 1)) (fun index -> first + index)

let positive n = [
  (fun _ -> 1);
  (fun index -> index + 1);
  (fun index -> if index = 0 then 3 * n else 1);
  (fun index -> if index < 2 then 5 else 1);
]

let entries n weight = List.init n (fun index -> member index, Z.of_int (weight index))

let raw_sets =
  List.concat_map (fun n ->
    T.make_validator_set (List.init n member)
    :: List.map (fun weight -> T.build_validator_set ~weighted:true (entries n weight))
      (positive n @ [fun _ -> 0]))
    (range 0 8)

let sets = raw_sets @ List.map T.count_capped_set raw_sets

let weighted_set members = match T.make_weighted_validator_set members with
  | Ok value -> value | Error reason -> failwith reason

let describe (vs : T.validator_set) () =
  Printf.sprintf "n = %d weighted = %b weights = %s" vs.n vs.weighted
    (String.concat "," (List.map (fun (_, weight) -> Z.to_string weight) vs.weights))

let rules = [P.Prior; P.Capped; P.Stake]
let rule_name = function P.Prior -> "prior" | P.Capped -> "capped" | P.Stake -> "stake"

let planned = P.make ~count_epoch:10L ~stake_epoch:(Some 20L) ~round_epoch:(Some 30L)
let rule_epochs = [9L; 10L; 20L]

let rows () =
  List.iter (fun (vs : T.validator_set) ->
    List.iter (fun rule ->
      let name = rule_name rule in
      let space = M.ruled rule ~declared:vs.weighted vs.weights in
      let pinned = K.C_driver.root_weights rule vs ~epoch_id:0L ~stamp:""
        ~weight:(fun _ -> None) in
      let minority = pinned.weight_required in
      let start = K.C_driver.min_peers rule ~n_validators:2 ~quorum:vs.quorum in
      let promise = K.Pending.peer_quorum ~quorum:vs.quorum in
      let where () = describe vs () ^ " rule = " ^ name in
      List.iter (fun seats ->
        List.iter (fun weight ->
          let at () = Printf.sprintf "%s seats = %d weight = %d" (where ()) seats weight in
          let weight = Z.of_int weight in
          same ("row ahead " ^ name) at
            (M.meets space M.Ahead ~seats ~weight)
            (Z.sign minority > 0 && Z.geq weight minority);
          same ("row start " ^ name) at
            (M.meets space M.Start ~seats ~weight) (seats >= start);
          same ("row promise " ^ name) at
            (M.meets space M.Promise ~seats ~weight) (seats >= promise))
          (range (-1) (Z.to_int vs.total_weight + 2));
        List.iter (fun required ->
          same ("row attested " ^ name)
            (fun () -> Printf.sprintf "%s seats = %d required = %d" (where ()) seats required)
            (M.attested space ~required ~seats ~weight:Z.zero) (seats >= required))
          (range (-1) (vs.n + 2)))
        (range (-1) (vs.n + 2));
      List.iter (fun configured ->
        expect (Printf.sprintf "row replies differs %s configured = %d" (where ()) configured)
          (M.replies space ~configured = K.Probe.state_attest_required pinned ~configured))
        (max_int :: min_int :: range (-1) (vs.n + 2));
      let peers = List.init 24 string_of_int in
      expect ("row wave differs " ^ where ())
        (List.map List.length (K.C_driver.catchup_query_waves vs peers)
          = List.map List.length (D.catchup_query_waves space peers)
        && M.wave space = List.length (List.hd (K.C_driver.catchup_query_waves vs peers)));
      expect ("row fanout differs " ^ where ()) (M.fanout space = max 1 (vs.f + 2));
      expect ("row text differs " ^ where ())
        (M.start_text space = string_of_int start
          && M.promise_text space = string_of_int promise))
      rules)
    sets;
  List.iter (fun rule ->
    List.iter (fun row -> both (row ^ rule_name rule))
      ["row ahead "; "row start "; "row promise "; "row attested "])
    rules

let edges () =
  let unit_set n = T.make_validator_set (List.init n member) in
  let replies n configured =
    List.map (fun rule ->
      M.replies (M.ruled rule ~declared:false (unit_set n).weights) ~configured) rules in
  expect "empty set asks for replies" (replies 0 3 = [0; 0; 0]);
  expect "single seat asks for replies" (replies 1 3 = [0; 0; 0]);
  expect "two seats reply count differs" (replies 2 3 = [1; 1; 1]);
  expect "four seats reply count differs"
    (replies 4 0 = [2; 2; 2] && replies 4 3 = [3; 3; 3] && replies 4 max_int = [3; 3; 3]);
  expect "seven seats reply count differs"
    (replies 7 0 = [3; 3; 3] && replies 7 5 = [5; 5; 5] && replies 7 9 = [6; 6; 6]);
  List.iter (fun rule ->
    let name = rule_name rule in
    let four = M.ruled rule ~declared:false (unit_set 4).weights in
    expect ("own root passed above the peer count rule = " ^ name)
      (not (M.attested four ~required:4 ~seats:3 ~weight:Z.zero)
        && M.attested four ~required:3 ~seats:3 ~weight:Z.zero
        && M.attested four ~required:0 ~seats:0 ~weight:Z.zero);
    let closed = M.ruled rule ~declared:true ["v0", Z.of_int (-3)] in
    expect ("closed minority reported peers ahead rule = " ^ name)
      (Z.sign (M.minority (M.shape ~seats:1 ~total:(Z.of_int (-3)))) <= 0
        && List.for_all (fun weight ->
          not (M.meets closed M.Ahead ~seats:1 ~weight:(Z.of_int weight))) (range (-4) 4));
    let empty = M.ruled rule ~declared:false [] in
    expect ("empty set promise differs rule = " ^ name)
      (M.meets empty M.Promise ~seats:0 ~weight:Z.zero
        && not (M.meets empty M.Start ~seats:0 ~weight:Z.zero)
        && M.meets empty M.Start ~seats:1 ~weight:Z.zero))
    rules;
  let heavy = weighted_set (entries 4 (fun index -> if index = 0 then 100 else 1)) in
  let ask rule row ~seats ~weight =
    M.meets (M.ruled rule ~declared:true heavy.weights) row ~seats ~weight:(Z.of_int weight) in
  expect "one heavy seat is not ahead under the count rule"
    (ask P.Capped M.Ahead ~seats:1 ~weight:100 && not (ask P.Capped M.Skip ~seats:1 ~weight:100)
      && ask P.Stake M.Ahead ~seats:1 ~weight:35 && not (ask P.Stake M.Ahead ~seats:4 ~weight:34));
  expect "start peer count differs"
    (List.map (fun (rule, seats) -> ask rule M.Start ~seats ~weight:0) [
      P.Prior, 0; P.Prior, 1; P.Capped, 1; P.Capped, 2; P.Stake, 1; P.Stake, 2;
    ] = [false; true; false; true; false; true]);
  let seven = weighted_set (entries 7 (fun _ -> 1)) in
  expect "start peer text differs"
    (List.map (fun rule -> M.start_text (M.ruled rule ~declared:true seven.weights)) rules
      = ["1"; "4"; "4"]
    && List.map (fun rule -> M.promise_text (M.ruled rule ~declared:true seven.weights)) rules
      = ["4"; "4"; "4"])

let record promised responder_addr =
  D.{responder_addr; responder_head_epoch = 7L; state_root = Some promised}

let promise () =
  let boot = weighted_set (entries 4 (fun index -> if index = 0 then 100 else 1)) in
  let other = weighted_set (entries 7 (fun _ -> 1)) in
  expect "promise sets differ" (boot.n = 4 && boot.quorum = 3 && other.n = 7 && other.quorum = 5);
  let lists = [
    []; ["v1"]; ["v0"]; ["v1"; "v2"]; ["v2"; "v3"]; ["v1"; "v1"]; ["absent0"; "absent1"];
    ["absent0"; "absent0"]; ["v1"; "absent0"]; ["v0"; "v1"; "v2"]; ["v4"; "v5"; "v6"];
    ["v0"; "v1"; "v2"; "v3"]; ["v1"; "v2"; "v3"; "v4"];
  ] in
  List.iter (fun epoch_id ->
    let rule = rule_name (P.select planned epoch_id) in
    let reached set responders = T.promise_at ~policy:planned ~chain_id ~epoch_id set responders in
    expect ("light peers did not keep the promise rule = " ^ rule) (reached boot ["v1"; "v2"]);
    expect ("one peer kept the promise rule = " ^ rule) (not (reached boot ["v1"]));
    expect ("one heavy peer kept the promise rule = " ^ rule) (not (reached boot ["v0"]));
    expect ("promise read the responder set rule = " ^ rule)
      (reached boot ["absent0"; "absent1"]);
    expect ("promise merged repeated records rule = " ^ rule) (reached boot ["v1"; "v1"]);
    expect ("promise used another set rule = " ^ rule)
      (not (reached other ["v1"; "v2"]) && not (reached other ["v1"; "v2"; "v3"])
        && reached other ["v1"; "v2"; "v3"; "v4"]);
    expect ("promise level differs rule = " ^ rule)
      (M.promise_text (T.space ~policy:planned ~chain_id ~epoch_id boot) = "2"
        && M.promise_text (T.space ~policy:planned ~chain_id ~epoch_id other) = "4");
    List.iter (fun (set : T.validator_set) ->
      List.iter (fun responders ->
        same ("promise " ^ rule)
          (fun () -> describe set () ^ " responders = " ^ String.concat "," responders)
          (reached set responders)
          (List.length responders >= K.Pending.peer_quorum ~quorum:set.quorum))
        lists)
      (boot :: other :: sets))
    rule_epochs;
  List.iter (fun rule -> both ("promise " ^ rule_name rule)) rules;
  let promised = String.make 32 'p' in
  let decide matched =
    W.decide ~validator_count:boot.n
      ~reached:(T.promise_at ~chain_id ~epoch_id:0L boot)
      ~level:(M.promise_text (T.space ~chain_id ~epoch_id:0L boot))
      ~promised_root_raw:(Some promised) (List.map (record promised) matched) in
  expect "two matching peers did not confirm"
    (decide ["v2"; "v3"] = W.Confirmed_elsewhere {agreed = 2; level = "2"}
      && decide ["absent0"; "absent1"] = W.Confirmed_elsewhere {agreed = 2; level = "2"});
  expect "one matching peer confirmed"
    (decide ["v0"] = W.Inconclusive {agreed = 1; peers = 3; responses = 1; level = "2"})

let contains text part =
  let size = String.length part in
  List.exists (fun start -> String.equal (String.sub text start size) part)
    (range 0 (String.length text - size))

let body text name =
  let head = "\nlet " ^ name ^ " " in
  match List.find_opt (fun start -> String.equal (String.sub text start (String.length head)) head)
    (range 0 (String.length text - String.length head)) with
  | None -> failwith ("test_reply_rows: function is absent name = " ^ name)
  | Some start ->
    let rest = String.sub text (start + 1) (String.length text - start - 1) in
    let stop = List.find_opt (fun index -> String.equal (String.sub rest index 5) "\nlet ")
      (range 0 (String.length rest - 5)) in
    String.sub rest 0 (Option.value stop ~default:(String.length rest))

let flat text =
  String.split_on_char '\n' text
  |> List.concat_map (String.split_on_char ' ')
  |> List.filter (fun word -> word <> "")
  |> String.concat " "

let holds file name code parts =
  List.iter (fun part ->
    expect (Printf.sprintf "site does not ask the measure file = %s function = %s call = %s"
      file name part)
      (contains (flat code) part))
    parts

let lacks file name code parts =
  List.iter (fun part ->
    expect (Printf.sprintf "threshold is compared beside the module call file = %s function = %s text = %s"
      file name part)
      (not (contains (flat code) part)))
    parts

let compared = [">="; "<="; "Z.geq"; "Z.gt"; "Z.leq"; "Z.lt"; "/ 3"; "C_quorum_policy.Prior"]
let space = "C_types.space ~policy:t.policy ~chain_id:t.config.chain_id ~epoch_id"

let sites () =
  let read index = In_channel.with_open_bin Sys.argv.(index) In_channel.input_all in
  let driver = read 1 in
  let site name present absent =
    let code = body driver name in
    holds "c_driver.ml" name code present;
    lacks "c_driver.ml" name code absent in
  site "catchup_agreement_reached"
    ["C_measure.meets (" ^ space ^ " (catchup_agreement_validator_set t ~epoch_id)) C_measure.Source ~seats:count ~weight"]
    compared;
  site "planned_agreement_reached"
    ["C_measure.meets (" ^ space ^ " (planned_set t ~epoch:epoch_id plan)) C_measure.Source ~seats:count ~weight"]
    compared;
  site "epoch_root_weights" [
    space ^ " validator_set in";
    "C_measure.label space ^";
    "C_measure.meets space C_measure.Source ~seats:count ~weight);";
    "ahead = (fun weight -> C_measure.meets space C_measure.Ahead ~seats:0 ~weight);";
    "required = (fun ~configured -> C_measure.replies space ~configured);";
    "C_measure.attested space ~required ~seats:count ~weight);";
  ] compared;
  let range_code = flat (body driver "query_catchup_range") in
  let guard = "when planned_agreement_reached t validator_set_plan ~epoch_id ~count ~weight ->" in
  let first = List.find_opt (fun start ->
    String.equal (String.sub range_code start (String.length guard)) guard)
    (range 0 (String.length range_code - String.length guard)) in
  expect "range agreement does not ask the planned site twice"
    (match first with
     | None -> false
     | Some start ->
       let rest = String.sub range_code (start + 1) (String.length range_code - start - 1) in
       contains rest guard);
  site "query_catchup_range"
    ["catchup_query_waves (" ^ space ^ ":from_epoch (agreement_validator_set from_epoch)) peer_ids"]
    ["C_quorum_policy.Prior"; ".f + "; "agreement_reached epoch_id"];
  site "catchup_query_waves" ["let width = C_measure.wave space in"] [".f + "; "max 1"];
  site "finality_query_peer_ids" ["let limit = C_measure.fanout space in"] [".f + "; "max 1"];
  site "request_missing_finalize"
    ["finality_query_peer_ids (" ^ space ^ " validator_set) validator_set ~epoch_id records"] [];
  site "start" [
    "C_types.space ~policy:t.policy ~chain_id:t.config.chain_id ~epoch_id:t.engine.state.height t.engine.vs in";
    "if alone || C_measure.meets space C_measure.Start ~seats:pc ~weight:Z.zero || tries <= 0";
    "let need = if alone then \"0\" else C_measure.start_text space in";
  ] ["quorum - 1"; "C_quorum_policy.Prior"; "pc >="];
  lacks "c_driver.ml" "all" driver
    ["catchup_source_agreement_reached"; "weight_required"; "count_required"; "count_limit"];
  let probe = read 2 in
  holds "consensus_health_probe.ml" "peer_root_quorum" (body probe "peer_root_quorum")
    ["| [], Some own when weights.attested ~required ~count:own.count ~weight:own.weight ->"];
  lacks "consensus_health_probe.ml" "peer_root_quorum" (body probe "peer_root_quorum")
    ["count >="; "required <="];
  holds "consensus_health_probe.ml" "peer_ahead" (body probe "peer_ahead") [
    "if not (weights.ahead below) || weights.ahead current then None";
    "&& weights.ahead (snd (head_support target replies)))";
  ];
  lacks "consensus_health_probe.ml" "peer_ahead" (body probe "peer_ahead")
    ["Z.geq"; "Z.lt"; "Z.sign"; "Z.gt"; "Z.leq"];
  let shell = read 3 in
  holds "consensus_health_shell.ml" "all" shell
    ["let state_attest_required = weights.required ~configured:cfg.state_attest_configured in"];
  lacks "consensus_health_shell.ml" "all" shell ["count_limit"; "count_required"];
  let saved = read 4 in
  holds "consensus_pending_commit_recovery.ml" "decide" (body saved "decide")
    ["if reached matched then Confirmed_elsewhere { agreed; level }"];
  lacks "consensus_pending_commit_recovery.ml" "decide" (body saved "decide") ["agreed >="];
  lacks "consensus_pending_commit_recovery.ml" "all" saved
    ["peer_quorum"; ".quorum"; "quorum - 1"; "C_measure."];
  let boot = read 5 in
  let code = body boot "pending_runtime" in
  holds "consensus_driver_boot_shell.ml" "pending_runtime" code [
    "Result.map (fun validator_set -> Consensus_pending_commit_recovery.{";
    "Octra_consensus.C_types.promise_at ~chain_id:deps.chain_id ~epoch_id:epoch validator_set responders);";
    "Octra_consensus.C_measure.promise_text (Octra_consensus.C_types.space ~chain_id:deps.chain_id ~epoch_id:epoch validator_set);";
    "}) (Consensus_validator_anchor.expected_set ~eviction_source:(eviction_source deps) anchor ~epoch)";
  ];
  lacks "consensus_driver_boot_shell.ml" "pending_runtime" code (".quorum" :: compared)

let () =
  rows ();
  edges ();
  promise ();
  sites ();
  print_endline "status = pass test = reply_rows"