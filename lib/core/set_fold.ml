(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module String_map = Map.Make (String)
module Int64_set = Set.Make (Int64)
module C_types = Octra_consensus.C_types

type pulse = {
  first : int64;
  last : int64;
  count : int;
}

type phase =
  | Live of int64
  | Shadow of pulse option

type marks = {
  floor : int64;
  bits : Z.t;
}

type member = {
  address : string;
  phase : phase;
  marks : marks;
}

type final = {
  epoch : int64;
  proposal_id : string;
  set_hash : string;
}

type t = {
  members : member list;
  finals : final list;
  safe_after : int64;
  applied : int64 option;
  seats : int option;
}

type cfg = {
  window : int64;
  challenge : int64;
  rejoin_span : int64;
  pulse_gap : int64;
  cadence : int64;
  delay : int64;
  max_members : int;
  minimum : Validator_participation.minimum;
}

type proof = {
  vote : C_types.vote;
  commit : C_types.parent_commit;
}

type counts = {
  live : int;
  shadow : int;
  allowed : int;
}

type cap_mode = Reject | Prune

type final_step =
  | Final_applied of t
  | Final_held of t * string

let meta_key = "bft.validator_duty"

let standard = {
  window = 96L;
  challenge = 16L;
  rejoin_span = 64L;
  pulse_gap = 8L;
  cadence = 64L;
  delay = 64L;
  max_members = 200;
  minimum = Validator_participation.Any;
}

let participating = {
  window = 32L;
  challenge = 16L;
  rejoin_span = 64L;
  pulse_gap = 8L;
  cadence = 4L;
  delay = 8L;
  max_members = 200;
  minimum = Validator_participation.Share {
    numerator = 1;
    denominator = 2;
  };
}

let empty = {
  members = [];
  finals = [];
  safe_after = 0L;
  applied = None;
  seats = None;
}

let validate_cfg cfg =
  if Int64.compare cfg.window 8L < 0 || Int64.compare cfg.window 128L > 0 then
    Error "validator duty window is outside limits"
  else if Int64.compare cfg.challenge 1L < 0 then
    Error "validator duty challenge is not positive"
  else if Int64.compare cfg.challenge cfg.window >= 0 then
    Error "validator duty challenge is not below window"
  else if Int64.compare cfg.rejoin_span 128L > 0 then
    Error "validator duty rejoin span exceeds limit"
  else if Int64.compare cfg.pulse_gap 1L < 0 then
    Error "validator duty pulse gap is not positive"
  else if Int64.compare cfg.pulse_gap cfg.rejoin_span > 0 then
    Error "validator duty pulse gap exceeds rejoin span"
  else if Int64.compare cfg.cadence 1L < 0 then
    Error "validator duty cadence is not positive"
  else if Int64.compare cfg.delay 1L < 0 then
    Error "validator duty activation delay is not positive"
  else if cfg.max_members < 4 || cfg.max_members > 200 then
    Error "validator duty member limit is outside limits"
  else
    Validator_participation.validate cfg.minimum

let consensus_id cfg =
  String.concat ":" [
    Int64.to_string cfg.window;
    Int64.to_string cfg.challenge;
    Int64.to_string cfg.rejoin_span;
    Int64.to_string cfg.pulse_gap;
    Int64.to_string cfg.cadence;
    Int64.to_string cfg.delay;
    string_of_int cfg.max_members;
    Validator_participation.consensus_id cfg.minimum;
  ]

let replacement_limit cfg =
  Int64.add
    cfg.window
    (Int64.add
       cfg.challenge
       (Int64.add (Int64.pred cfg.cadence) cfg.delay))

let member_cap cfg = cfg.max_members * 4

let phase_rank = function
  | Live _ -> 0
  | Shadow _ -> 1

let compare_member left right =
  let by_address = String.compare left.address right.address in
  if by_address <> 0 then by_address
  else compare (phase_rank left.phase) (phase_rank right.phase)

let compare_final left right = Int64.compare left.epoch right.epoch

let sort_members members = List.sort compare_member members
let sort_finals finals = List.sort compare_final finals

let floor_epoch cfg at =
  let span = Int64.add cfg.window (Int64.add cfg.challenge 2L) in
  if Int64.compare at span < 0 then 0L else Int64.sub at span

let empty_marks = { floor = 0L; bits = Z.zero }

let prune_marks cfg at marks =
  let floor = floor_epoch cfg at in
  if Int64.compare floor marks.floor <= 0 then marks
  else
    let shift = Int64.sub floor marks.floor in
    let bits =
      if Int64.compare shift (Int64.of_int (Z.numbits marks.bits)) >= 0 then
        Z.zero
      else
        Z.shift_right marks.bits (Int64.to_int shift)
    in
    { floor; bits }

let add_mark cfg epoch marks =
  let marks = prune_marks cfg epoch marks in
  let index = Int64.sub epoch marks.floor in
  if Int64.compare index 0L < 0 then marks
  else
    let bit = Z.shift_left Z.one (Int64.to_int index) in
    { marks with bits = Z.logor marks.bits bit }

let table_of_members members =
  List.fold_left
    (fun table member -> String_map.add member.address member table)
    String_map.empty
    members

let live_member epoch address = {
  address;
  phase = Live epoch;
  marks = empty_marks;
}

let shadow_member address = {
  address;
  phase = Shadow None;
  marks = empty_marks;
}

let pulse_fresh cfg ~epoch pulse =
  Int64.compare pulse.last epoch > 0
  || Int64.compare (Int64.sub epoch pulse.last) cfg.pulse_gap <= 0

let retain_member cfg ~epoch member =
  match member.phase with
  | Live _ -> true
  | Shadow None -> not (Z.equal member.marks.bits Z.zero)
  | Shadow (Some pulse) ->
    not (Z.equal member.marks.bits Z.zero)
    || pulse_fresh cfg ~epoch pulse

let last_mark marks =
  if Z.equal marks.bits Z.zero then None
  else
    Some
      (Int64.add
         marks.floor
         (Int64.of_int (Z.numbits marks.bits - 1)))

let shadow_epoch member =
  match member.phase, last_mark member.marks with
  | Live _, _ -> None
  | Shadow None, mark -> mark
  | Shadow (Some pulse), None -> Some pulse.last
  | Shadow (Some pulse), Some mark -> Some (Int64.max pulse.last mark)

let compare_shadow (left_address, left) (right_address, right) =
  let by_epoch =
    match shadow_epoch left, shadow_epoch right with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some left_epoch, Some right_epoch ->
      Int64.compare left_epoch right_epoch
  in
  if by_epoch <> 0 then by_epoch
  else String.compare left_address right_address

let rec remove_oldest count table = function
  | _ when count <= 0 -> Ok table
  | [] -> Error "validator duty live state exceeds member limit"
  | (address, _) :: rest ->
    remove_oldest (count - 1) (String_map.remove address table) rest

let prune_table cfg ~epoch table =
  let table =
    String_map.map
      (fun member ->
        { member with marks = prune_marks cfg epoch member.marks })
      table
    |> String_map.filter (fun _ member -> retain_member cfg ~epoch member)
  in
  let overflow = String_map.cardinal table - member_cap cfg in
  if overflow <= 0 then Ok table
  else
    table
    |> String_map.bindings
    |> List.filter (fun (_, member) ->
      match member.phase with
      | Live _ -> false
      | Shadow _ -> true)
    |> List.sort compare_shadow
    |> remove_oldest overflow table

let limit_table cfg ~cap_mode ~epoch table =
  match cap_mode with
  | Reject when String_map.cardinal table > member_cap cfg ->
    Error "validator duty state exceeds member limit"
  | Reject -> Ok table
  | Prune -> prune_table cfg ~epoch table

let sync cfg ~epoch ~active state =
  let active = List.sort_uniq String.compare active in
  if List.length active > cfg.max_members then
    Error "validator duty active set exceeds limit"
  else
    let active_set =
      List.fold_left
        (fun set address -> String_map.add address () set)
        String_map.empty
        active
    in
    let update member =
      let present = String_map.mem member.address active_set in
      let phase =
        match present, member.phase with
        | true, Live since -> Live since
        | true, Shadow _ -> Live epoch
        | false, Live _ -> Shadow None
        | false, Shadow pulse -> Shadow pulse
      in
      { member with phase; marks = prune_marks cfg epoch member.marks }
    in
    let table =
      state.members
      |> List.map update
      |> table_of_members
    in
    let table =
      List.fold_left
        (fun items address ->
          if String_map.mem address items then items
          else String_map.add address (live_member epoch address) items)
        table
        active
    in
    let members =
      String_map.bindings table
      |> List.map snd
      |> List.filter (retain_member cfg ~epoch)
    in
    Ok { state with members }

let note_set cfg ~epoch ~active state =
  match validate_cfg cfg with
  | Error _ as error -> error
  | Ok () when Int64.compare epoch 0L < 0 ->
    Error "validator duty set epoch is negative"
  | Ok () -> sync cfg ~epoch ~active state

let lock cfg ~active state =
  match validate_cfg cfg with
  | Error _ as error -> error
  | Ok () ->
    begin
      match state.seats with
      | Some seats when seats < 4 || seats > cfg.max_members ->
        Error "validator seat limit is outside limits"
      | Some _ -> Ok (state, false)
      | None ->
        let seats = active |> List.sort_uniq String.compare |> List.length in
        if seats < 4 || seats > cfg.max_members then
          Error "validator seat limit is outside limits"
        else
          Ok ({ state with seats = Some seats }, true)
    end

let seats state = state.seats

let delay cfg ~at state =
  match validate_cfg cfg with
  | Error _ as error -> error
  | Ok () when Int64.compare at 0L < 0 ->
    Error "validator duty delay epoch is negative"
  | Ok () ->
    let safe_after = Int64.add at (Int64.add cfg.window cfg.challenge) in
    Ok { state with safe_after = Int64.max state.safe_after safe_after }

let add_signer cfg ~at signer table =
  let member =
    match String_map.find_opt signer table with
    | Some item -> item
    | None -> shadow_member signer
  in
  let marks = add_mark cfg at member.marks in
  String_map.add signer { member with marks } table

let retain_finals cfg ~at finals =
  let span = Int64.add cfg.challenge 1L in
  let floor = if Int64.compare at span < 0 then 0L else Int64.sub at span in
  List.filter (fun item -> Int64.compare item.epoch floor >= 0) finals

let put_final cfg ~at final finals =
  match List.find_opt (fun item -> Int64.equal item.epoch final.epoch) finals with
  | Some item when item = final -> Ok (retain_finals cfg ~at finals)
  | Some _ -> Error "validator duty finality conflict"
  | None -> Ok (retain_finals cfg ~at (final :: finals) |> sort_finals)

let note_final_step ~cap_mode cfg ~at ~active ~final ~signers state =
  match validate_cfg cfg with
  | Error error -> Final_held (state, error)
  | Ok () when Int64.compare at 0L <= 0 ->
    Final_held (state, "validator duty apply epoch is not positive")
  | Ok () when not (Int64.equal (Int64.succ final.epoch) at) ->
    Final_held (state, "validator duty finality epoch is not parent")
  | Ok () ->
    begin
      match sync cfg ~epoch:at ~active state with
      | Error error -> Final_held (state, error)
      | Ok state ->
        let signers = List.sort_uniq String.compare signers in
        let table = table_of_members state.members in
        let table =
          List.fold_left
            (fun table signer -> add_signer cfg ~at:final.epoch signer table)
            table
            signers
        in
        begin
          match limit_table cfg ~cap_mode ~epoch:at table with
          | Error error -> Final_held (state, error)
          | Ok table ->
            let state = {
              state with
              members = String_map.bindings table |> List.map snd;
            } in
          match put_final cfg ~at final state.finals with
          | Error error -> Final_held (state, error)
          | Ok finals ->
            Final_applied {
              members = String_map.bindings table |> List.map snd;
              finals;
              safe_after = state.safe_after;
              applied = state.applied;
              seats = state.seats;
            }
        end
    end

let note_final ?(cap_mode=Reject) cfg ~at ~active ~final ~signers state =
  match note_final_step ~cap_mode cfg ~at ~active ~final ~signers state with
  | Final_applied state -> Ok state
  | Final_held (_, error) -> Error error

let advance_pulse cfg epoch = function
  | None -> Ok { first = epoch; last = epoch; count = 1 }
  | Some pulse when Int64.compare epoch pulse.last < 0 ->
    Error "validator duty pulse epoch moved backward"
  | Some pulse when Int64.equal epoch pulse.last -> Ok pulse
  | Some pulse ->
    let gap = Int64.sub epoch pulse.last in
    if Int64.compare gap cfg.pulse_gap <= 0 then
      Ok { pulse with last = epoch; count = pulse.count + 1 }
    else
      Ok { first = epoch; last = epoch; count = 1 }

let note_pulse ?(cap_mode=Reject) cfg ~epoch ~active ~address state =
  match validate_cfg cfg with
  | Error _ as error -> error
  | Ok () when address = "" -> Error "validator duty pulse address is empty"
  | Ok () when Int64.compare epoch 0L < 0 ->
    Error "validator duty pulse epoch is negative"
  | Ok () when active -> Ok state
  | Ok () ->
    let table = table_of_members state.members in
    let member =
      match String_map.find_opt address table with
      | Some item -> item
      | None -> shadow_member address
    in
    let prior =
      match member.phase with
      | Live _ -> None
      | Shadow pulse -> pulse
    in
    begin
      match advance_pulse cfg epoch prior with
      | Error _ as error -> error
      | Ok pulse ->
        let member = {
          member with
          phase = Shadow (Some pulse);
          marks = prune_marks cfg epoch member.marks;
        } in
        let table = String_map.add address member table in
        limit_table cfg ~cap_mode ~epoch table
        |> Result.map (fun table ->
          { state with members = String_map.bindings table |> List.map snd })
    end

let find_final epoch state =
  List.find_opt (fun item -> Int64.equal item.epoch epoch) state.finals

let verify_vote ~chain_id ~address (commit : C_types.parent_commit)
    (vote : C_types.vote) =
  let cert = commit.C_types.certificate in
  if vote.C_types.chain_id <> chain_id then
    Error "validator duty vote chain mismatch"
  else if vote.validator <> address then
    Error "validator duty vote owner mismatch"
  else if vote.vote_type <> C_types.Precommit then
    Error "validator duty vote is not precommit"
  else if not (Int64.equal vote.epoch_id cert.epoch_id) then
    Error "validator duty vote epoch mismatch"
  else if vote.round <> cert.commit_round then
    Error "validator duty vote round mismatch"
  else if vote.proposal_id <> cert.proposal_id then
    Error "validator duty vote proposal mismatch"
  else
    match C_types.pubkey_of_addr commit.validator_set address with
    | None -> Error "validator duty vote signer is not in set"
    | Some pubkey ->
      if Octra_consensus.C_hash.verify_vote ~pubkey_raw:pubkey vote then Ok ()
      else Error "validator duty vote signature is invalid"

let verify_commit ~chain_id commit =
  let verify_vote (vote : C_types.vote) =
    match C_types.pubkey_of_addr commit.C_types.validator_set vote.C_types.validator with
    | None -> false
    | Some pubkey -> Octra_consensus.C_hash.verify_vote ~pubkey_raw:pubkey vote
  in
  match
    Octra_consensus.C_qc.validate_certificate
      ~chain_id
      ~validator_set:commit.validator_set
      ~verify_vote
      commit.certificate
  with
  | Octra_consensus.C_qc.Valid -> Ok ()
  | Octra_consensus.C_qc.Invalid reason ->
    Error ("validator duty commit is invalid: " ^ reason)

let read_parent ~chain_id (commit : C_types.parent_commit) =
  match verify_commit ~chain_id commit with
  | Error _ as error -> error
  | Ok () ->
    let cert = commit.certificate in
    let final = {
      epoch = cert.epoch_id;
      proposal_id = cert.proposal_id;
      set_hash = Octra_consensus.C_config.validator_set_hash commit.validator_set;
    } in
    let signers =
      cert.precommits
      |> List.map (fun (vote : C_types.vote) -> vote.validator)
      |> List.sort_uniq String.compare
    in
    let active =
      commit.validator_set.validators
      |> List.map (fun (item : C_types.validator_info) -> item.address)
      |> List.sort_uniq String.compare
    in
    Ok (final, signers, active)

let advance ?(cap_mode=Reject) cfg ~chain_id ~start ~at ~parent state =
  match state.applied with
  | Some epoch when Int64.equal epoch at -> Ok (state, None, false)
  | Some epoch when Int64.compare epoch at > 0 ->
    Error "validator duty apply epoch moved backward"
  | prior ->
    begin
      let hold state reason =
        match delay cfg ~at state with
        | Error _ as error -> error
        | Ok state -> Ok (state, Some reason)
      in
      let gap =
        match prior with
        | None -> Int64.compare at start > 0
        | Some epoch -> Int64.compare at (Int64.succ epoch) > 0
      in
      let evidence =
        match parent with
        | None -> Error "validator duty parent commit missing"
        | Some parent ->
          begin
            match read_parent ~chain_id parent with
            | Error _ as error -> error
            | Ok (final, signers, active) ->
              begin
                match
                  note_final_step
                    ~cap_mode
                    cfg
                    ~at
                    ~active
                    ~final
                    ~signers
                    state
                with
                | Final_applied state -> Ok (state, None)
                | Final_held (state, error) -> hold state error
              end
          end
      in
      begin
        match evidence with
        | Error _ as error -> error
        | Ok (state, reason) ->
          let next = if gap then hold state "epoch_gap" else Ok (state, reason) in
          Result.map
            (fun (state, reason) ->
              { state with applied = Some at }, reason, true)
            next
      end
    end

let verify_proof cfg ~chain_id ~epoch ~address proof state =
  let vote = proof.vote in
  let commit = proof.commit in
  if Int64.compare vote.epoch_id epoch > 0 then
    Error "validator duty proof is from the future"
  else if Int64.compare (Int64.sub epoch vote.epoch_id) cfg.challenge > 0 then
    Error "validator duty proof challenge expired"
  else
    match find_final vote.epoch_id state with
    | None -> Error "validator duty finalized reference is missing"
    | Some final ->
      let cert = commit.C_types.certificate in
      let set_hash = Octra_consensus.C_config.validator_set_hash commit.validator_set in
      if cert.proposal_id <> final.proposal_id then
        Error "validator duty finalized proposal mismatch"
      else if set_hash <> final.set_hash then
        Error "validator duty finalized set mismatch"
      else
        match verify_vote ~chain_id ~address commit vote with
        | Error _ as error -> error
        | Ok () -> verify_commit ~chain_id commit

let apply_proof ?(cap_mode=Reject) cfg ~chain_id ~epoch ~active ~address proof state =
  match verify_proof cfg ~chain_id ~epoch ~address proof state with
  | Error _ as error -> error
  | Ok () ->
    let table = table_of_members state.members in
    let member =
      match String_map.find_opt address table with
      | Some item -> item
      | None when active -> live_member proof.vote.epoch_id address
      | None -> shadow_member address
    in
    let marks = add_mark cfg proof.vote.epoch_id member.marks in
    let table = String_map.add address { member with marks } table in
    limit_table cfg ~cap_mode ~epoch table
    |> Result.map (fun table ->
      { state with members = String_map.bindings table |> List.map snd })

let warm_end cfg start = Int64.add start (Int64.add cfg.window cfg.challenge)

let evidence_bounds cfg source =
  let high = Int64.sub source cfg.challenge in
  let low = Int64.sub (Int64.succ high) cfg.window in
  low, high

let mark_count ~low ~high marks =
  let low = Int64.max low marks.floor in
  let rec loop total epoch =
    if Int64.compare epoch high > 0 then total
    else
      let index = Int64.sub epoch marks.floor |> Int64.to_int in
      let total = if Z.testbit marks.bits index then total + 1 else total in
      loop total (Int64.succ epoch)
  in
  if Int64.compare low high > 0 then 0 else loop 0 low

let participated cfg ~low ~high marks =
  let epochs = Int64.sub high low |> Int64.succ |> Int64.to_int in
  let signed = mark_count ~low ~high marks in
  Validator_participation.admits cfg.minimum ~epochs ~signed

let pulse_ready cfg ~source pulse =
  Int64.compare pulse.last source <= 0
  && Int64.compare (Int64.sub source pulse.last) cfg.pulse_gap <= 0
  && Int64.compare (Int64.sub pulse.last pulse.first) cfg.rejoin_span >= 0

let member_allows cfg ~start ~source ~safe_after member =
  match member.phase with
  | Live _ when Int64.compare source safe_after < 0 -> true
  | Live _ when Int64.compare source (warm_end cfg start) < 0 -> true
  | Live since ->
    let low, high = evidence_bounds cfg source in
    Int64.compare since low > 0 || participated cfg ~low ~high member.marks
  | Shadow None -> false
  | Shadow (Some pulse) -> pulse_ready cfg ~source pulse

let allows cfg ~start ~source ~address state =
  match List.find_opt (fun member -> member.address = address) state.members with
  | None -> false
  | Some member ->
    member_allows cfg ~start ~source ~safe_after:state.safe_after member

let filter cfg ~start ~source candidates state =
  let table = table_of_members state.members in
  let allowed, live, shadow =
    List.fold_left
      (fun (items, live, shadow) (candidate : Validator_admission.candidate) ->
        let address = candidate.Validator_admission.address in
        match String_map.find_opt address table with
        | Some member
          when member_allows
                 cfg
                 ~start
                 ~source
                 ~safe_after:state.safe_after
                 member ->
          let live, shadow =
            match member.phase with
            | Live _ -> live + 1, shadow
            | Shadow _ -> live, shadow + 1
          in
          candidate :: items, live, shadow
        | Some _ -> items, live, shadow + 1
        | None -> items, live, shadow)
      ([], 0, 0)
      candidates
  in
  List.rev allowed, { live; shadow; allowed = List.length allowed }

let decode_b64 label limit encoded =
  if String.length encoded > limit then Error (label ^ " exceeds size limit")
  else
    match Base64.decode encoded with
    | Error _ -> Error (label ^ " is not base64")
    | Ok raw when Base64.encode_exn raw <> encoded ->
      Error (label ^ " base64 is not exact")
    | Ok raw -> Ok raw

let decode_proof ~vote_b64 ~commit_b64 =
  match decode_b64 "validator duty vote" 4_096 vote_b64 with
  | Error _ as error -> error
  | Ok vote_raw ->
    begin
      match decode_b64 "validator duty commit" 1_000_000 commit_b64 with
      | Error _ as error -> error
      | Ok commit_raw ->
        try
          Ok {
            vote = Octra_consensus.C_codec.decode_vote vote_raw;
            commit = Octra_consensus.C_parent_commit.decode commit_raw;
          }
        with _ -> Error "validator duty proof encoding is invalid"
    end

let duplicate_field fields =
  let rec loop seen = function
    | [] -> None
    | (name, _) :: _ when String_map.mem name seen -> Some name
    | (name, _) :: rest -> loop (String_map.add name () seen) rest
  in
  loop String_map.empty fields

let proof_of_message = function
  | None -> Error "validator duty message is missing"
  | Some raw ->
    begin
      try
        match Yojson.Safe.from_string raw with
        | `Assoc fields ->
          begin
            match duplicate_field fields with
            | Some name -> Error ("validator duty field is duplicated: " ^ name)
            | None ->
              match List.assoc_opt "duty_vote" fields,
                    List.assoc_opt "duty_commit" fields with
              | None, None -> Ok None
              | Some (`String vote_b64), Some (`String commit_b64) ->
                decode_proof ~vote_b64 ~commit_b64 |> Result.map Option.some
              | Some _, Some _ -> Error "validator duty proof fields have wrong type"
              | _ -> Error "validator duty proof is incomplete"
          end
        | _ -> Error "validator duty message has wrong type"
      with _ -> Error "validator duty message encoding is invalid"
    end

let b64_raw value = Base64.encode_exn value

let raw_b64 label value =
  match Base64.decode value with
  | Error _ -> Error (label ^ " is not base64")
  | Ok raw when Base64.encode_exn raw <> value -> Error (label ^ " is not exact")
  | Ok raw when String.length raw <> 32 -> Error (label ^ " length is invalid")
  | Ok raw -> Ok raw

let pulse_to_json pulse =
  `Assoc [
    "first", `String (Int64.to_string pulse.first);
    "last", `String (Int64.to_string pulse.last);
    "count", `Int pulse.count;
  ]

let phase_to_json = function
  | Live since ->
    `Assoc ["kind", `String "live"; "since", `String (Int64.to_string since)]
  | Shadow None -> `Assoc ["kind", `String "shadow"]
  | Shadow (Some pulse) ->
    `Assoc ["kind", `String "shadow"; "pulse", pulse_to_json pulse]

let marks_to_json marks =
  let rec loop index values =
    if index >= Z.numbits marks.bits then List.rev values
    else
      let values =
        if Z.testbit marks.bits index then
          `String (Int64.add marks.floor (Int64.of_int index) |> Int64.to_string)
          :: values
        else
          values
      in
      loop (index + 1) values
  in
  `List (loop 0 [])

let member_to_json member =
  `Assoc [
    "address", `String member.address;
    "phase", phase_to_json member.phase;
    "marks", marks_to_json member.marks;
  ]

let final_to_json item =
  `Assoc [
    "epoch", `String (Int64.to_string item.epoch);
    "proposal_id", `String (b64_raw item.proposal_id);
    "set_hash", `String (b64_raw item.set_hash);
  ]

let to_yojson state =
  let fields = [
    "standard", `String "octra-validator-duty";
    "safe_after", `String (Int64.to_string state.safe_after);
    "applied",
      Option.fold
        ~none:`Null
        ~some:(fun epoch -> `String (Int64.to_string epoch))
        state.applied;
    "members", `List (List.map member_to_json (sort_members state.members));
    "finals", `List (List.map final_to_json (sort_finals state.finals));
  ] in
  let fields =
    match state.seats with
    | None -> fields
    | Some seats -> fields @ ["seats", `Int seats]
  in
  `Assoc fields

let int64_json label = function
  | `String raw ->
    begin
      try Ok (Int64.of_string raw) with _ -> Error (label ^ " is invalid")
    end
  | `Int value -> Ok (Int64.of_int value)
  | _ -> Error (label ^ " has wrong type")

let int_json label = function
  | `Int value -> Ok value
  | `String raw ->
    begin
      try Ok (int_of_string raw) with _ -> Error (label ^ " is invalid")
    end
  | _ -> Error (label ^ " has wrong type")

let assoc label = function
  | `Assoc fields -> Ok fields
  | _ -> Error (label ^ " has wrong type")

let list label = function
  | `List items -> Ok items
  | _ -> Error (label ^ " has wrong type")

let field name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (name ^ " is missing")

let optional_int64 label = function
  | `Null -> Ok None
  | json -> int64_json label json |> Result.map Option.some

let bind f result = Result.bind result f

let optional_seats fields =
  match List.assoc_opt "seats" fields with
  | None
  | Some `Null -> Ok None
  | Some json ->
    int_json "validator duty seats" json
    |> bind (fun seats ->
      if seats < 4 || seats > standard.max_members then
        Error "validator duty seats are outside limits"
      else Ok (Some seats))

let pulse_of_json json =
  assoc "validator duty pulse" json
  |> bind (fun fields ->
    field "first" fields |> bind (int64_json "validator duty pulse first")
    |> bind (fun first ->
      field "last" fields |> bind (int64_json "validator duty pulse last")
      |> bind (fun last ->
        field "count" fields |> bind (int_json "validator duty pulse count")
        |> bind (fun count ->
          if Int64.compare first 0L < 0 || Int64.compare last first < 0 || count < 1 then
            Error "validator duty pulse is invalid"
          else Ok { first; last; count }))))

let phase_of_json json =
  assoc "validator duty phase" json
  |> bind (fun fields ->
    match List.assoc_opt "kind" fields with
    | Some (`String "live") ->
      field "since" fields
      |> bind (int64_json "validator duty live epoch")
      |> Result.map (fun since -> Live since)
    | Some (`String "shadow") ->
      begin
        match List.assoc_opt "pulse" fields with
        | None -> Ok (Shadow None)
        | Some json -> pulse_of_json json |> Result.map (fun pulse -> Shadow (Some pulse))
      end
    | _ -> Error "validator duty phase kind is invalid")

let marks_of_json json =
  list "validator duty marks" json
  |> bind (fun items ->
    let rec loop set = function
      | [] ->
        let epochs = Int64_set.elements set in
        begin
          match epochs with
          | [] -> Ok empty_marks
          | floor :: _ ->
            begin
              match List.rev epochs with
              | [] -> Ok empty_marks
              | last :: _ when Int64.compare (Int64.sub last floor) 255L > 0 ->
                Error "validator duty marks exceed bounds"
              | _ ->
                let bits =
                  List.fold_left
                    (fun bits epoch ->
                      let index = Int64.sub epoch floor |> Int64.to_int in
                      Z.logor bits (Z.shift_left Z.one index))
                    Z.zero
                    epochs
                in
                Ok { floor; bits }
            end
        end
      | item :: rest ->
        int64_json "validator duty mark" item
        |> bind (fun epoch ->
          if Int64.compare epoch 0L < 0 then Error "validator duty mark is negative"
          else if Int64_set.mem epoch set then Error "validator duty mark is duplicated"
          else loop (Int64_set.add epoch set) rest)
    in
    loop Int64_set.empty items)

let member_of_json json =
  assoc "validator duty member" json
  |> bind (fun fields ->
    match List.assoc_opt "address" fields with
    | Some (`String address) when address <> "" ->
      field "phase" fields |> bind phase_of_json
      |> bind (fun phase ->
        field "marks" fields |> bind marks_of_json
        |> Result.map (fun marks -> { address; phase; marks }))
    | _ -> Error "validator duty member address is invalid")

let final_of_json json =
  assoc "validator duty final" json
  |> bind (fun fields ->
    field "epoch" fields |> bind (int64_json "validator duty final epoch")
    |> bind (fun epoch ->
      match List.assoc_opt "proposal_id" fields, List.assoc_opt "set_hash" fields with
      | Some (`String proposal_id), Some (`String set_hash) ->
        raw_b64 "validator duty proposal id" proposal_id
        |> bind (fun proposal_id ->
          raw_b64 "validator duty set hash" set_hash
          |> bind (fun set_hash ->
            if Int64.compare epoch 0L < 0 then Error "validator duty final epoch is negative"
            else Ok { epoch; proposal_id; set_hash }))
      | _ -> Error "validator duty final hash is missing"))

let decode_list parse items =
  let rec loop values = function
    | [] -> Ok (List.rev values)
    | item :: rest -> parse item |> bind (fun value -> loop (value :: values) rest)
  in
  loop [] items

let unique_members members =
  let rec loop prior = function
    | [] -> Ok ()
    | member :: _ when Some member.address = prior ->
      Error "validator duty member is duplicated"
    | member :: rest -> loop (Some member.address) rest
  in
  loop None (sort_members members)

let unique_finals finals =
  let rec loop prior = function
    | [] -> Ok ()
    | item :: _ when Some item.epoch = prior ->
      Error "validator duty final is duplicated"
    | item :: rest -> loop (Some item.epoch) rest
  in
  loop None (sort_finals finals)

let of_yojson json =
  assoc "validator duty state" json
  |> bind (fun fields ->
    match List.assoc_opt "standard" fields with
    | Some (`String "octra-validator-duty") ->
      field "safe_after" fields |> bind (int64_json "validator duty safe epoch")
      |> bind (fun safe_after ->
      field "applied" fields |> bind (optional_int64 "validator duty applied epoch")
      |> bind (fun applied ->
      optional_seats fields |> bind (fun seats ->
      field "members" fields |> bind (list "validator duty members")
      |> bind (decode_list member_of_json)
      |> bind (fun members ->
        field "finals" fields |> bind (list "validator duty finals")
        |> bind (decode_list final_of_json)
        |> bind (fun finals ->
          unique_members members |> bind (fun () ->
            unique_finals finals |> Result.map (fun () ->
              {
                members = sort_members members;
                finals = sort_finals finals;
                safe_after;
                applied;
                seats;
              })))))))
    | _ -> Error "validator duty standard is invalid")

let to_string state = Yojson.Safe.to_string (to_yojson state)

let of_string raw =
  try Yojson.Safe.from_string raw |> of_yojson
  with _ -> Error "validator duty state encoding is invalid"