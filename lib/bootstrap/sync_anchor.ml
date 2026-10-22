(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Checkpoint = State_sync_checkpoint
module C_codec = Octra_consensus.C_codec
module C_config = Octra_consensus.C_config
module C_hash = Octra_consensus.C_hash
module C_qc = Octra_consensus.C_qc
module C_relief = Octra_consensus.C_relief
module C_relief_log = Octra_consensus.C_relief_log
module C_quorum_policy = Octra_consensus.C_quorum_policy
module C_types = Octra_consensus.C_types
module Eic = Octra_core.Epoch_index_commitment
module Irmin = Octra_core.Store_irmin
module O = Octra_net.Oce1
module Update = Octra_core.Validator_set_update

type source =
  | Pending
  | Active
  | Relief of C_relief.mark

type step = {
  source : source;
  finalize : C_types.finalize;
  ledger_state_root : string;
  epoch_index_root : string;
  update : string;
  proof : string;
}

type t = {
  steps : step list;
  finalize : C_types.finalize;
  validator_set : C_types.validator_set;
  stake : (int64 * C_types.validator_set) option;
}

type origin = {
  chain_id : string;
  epoch_id : int64;
  state_root : string;
  validators : C_types.validator_set;
}

let from_input (root : Octra_core.Compute_input.root) =
  {chain_id = root.chain_id; epoch_id = root.epoch; state_root = root.state;
    validators = root.validators}

let max_steps = 1_024
let max_finalize_bytes = 4_000_000
let max_set_bytes = 1_000_000
let max_update_bytes = 1_000_000
let max_proof_bytes = 4_000_000
let max_root_bytes = 256

let make ~steps ~finalize ~validator_set =
  { steps; finalize; validator_set; stake = None }

let steps t =
  t.steps

let validator_set t =
  t.validator_set

let transition_base t =
  match t.stake with
  | Some stake -> Ok stake
  | None -> Error "validator transition path is not verified"

let live_relief t =
  let live step = match step.source with
    | Relief mark when t.finalize.C_types.epoch_id < mark.activate_epoch -> Some mark
    | _ -> None in
  List.find_map live (List.rev t.steps)

let relief t =
  match t.stake with
  | None -> Error "validator transition path is not verified"
  | Some _ -> Ok (live_relief t)

let path_epoch t =
  match List.rev t.steps with
  | [] -> -1L
  | step :: _ -> step.finalize.C_types.epoch_id

let finality t =
  t.finalize

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let protect f =
  try Ok (f ()) with exn -> Error (Printexc.to_string exn)

let source_to_u8 = function
  | Pending -> 1
  | Active -> 2
  | Relief _ -> 3

let source_of_u8 = function
  | 1 -> Pending
  | 2 -> Active
  | _ -> failwith "validator transition source is invalid"

let put_step buffer (step : step) =
  O.put_u8 buffer (source_to_u8 step.source);
  (match step.source with
   | Relief mark -> O.put_bytes buffer (C_relief_log.encode mark)
   | Pending | Active -> ());
  O.put_bytes buffer (C_codec.encode_finalize step.finalize);
  O.put_string buffer step.ledger_state_root;
  O.put_string buffer step.epoch_index_root;
  O.put_bytes buffer step.update;
  O.put_bytes buffer step.proof

let get_step cursor =
  let source = match O.get_u8 cursor with
    | 3 ->
      let wire = O.get_bytes_bounded ~max:C_relief_log.max_wire_bytes cursor in
      (match C_relief_log.decode wire with
       | Ok mark -> Relief mark
       | Error reason -> failwith reason)
    | tag -> source_of_u8 tag in
  let finalize =
    O.get_bytes_bounded ~max:max_finalize_bytes cursor
    |> C_codec.decode_finalize
  in
  let ledger_state_root = O.get_string_bounded ~max:max_root_bytes cursor in
  let epoch_index_root = O.get_string_bounded ~max:max_root_bytes cursor in
  let update = O.get_bytes_bounded ~max:max_update_bytes cursor in
  let proof = O.get_bytes_bounded ~max:max_proof_bytes cursor in
  { source; finalize; ledger_state_root; epoch_index_root; update; proof }

let raw t =
  O.encode (fun buffer ->
    O.put_list put_step buffer t.steps;
    O.put_bytes buffer (C_codec.encode_finalize t.finalize);
    O.put_bytes buffer (C_codec.encode_validator_set t.validator_set))

let encode t =
  raw t |> Base64.encode_exn

let decode encoded =
  let* bytes = protect (fun () -> Base64.decode_exn encoded) in
  if Base64.encode_exn bytes <> encoded then Error "finality encoding is not exact"
  else
    let* value =
      protect (fun () ->
        O.decode
          (fun cursor ->
            let steps =
              O.get_list_bounded ~max:max_steps get_step cursor
            in
            let finalize =
              O.get_bytes_bounded ~max:max_finalize_bytes cursor
              |> C_codec.decode_finalize
            in
            let validator_set =
              O.get_bytes_bounded ~max:max_set_bytes cursor
              |> C_codec.decode_validator_set
            in
            { steps; finalize; validator_set; stake = None })
          bytes)
    in
    if raw value <> bytes then Error "finality encoding is not exact"
    else Ok value

let verify_vote validator_set (vote : C_types.vote) =
  match C_types.pubkey_of_addr validator_set vote.C_types.validator with
  | None -> false
  | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote

let raw_validator_set validator_set =
  let* entries =
    List.fold_left
      (fun state (validator : C_types.validator_info) ->
        let* items = state in
        let* pubkey = Checkpoint.raw_pubkey validator.pubkey in
        let* weight =
          match C_types.weight_of_addr validator_set validator.address with
          | Some value -> Ok value
          | None -> Error "trusted validator weight is missing"
        in
        Ok ((C_types.{ validator with pubkey }, weight) :: items))
      (Ok [])
      validator_set.C_types.validators
    |> Result.map List.rev
  in
  let* result = if validator_set.weighted then
    C_types.make_weighted_validator_set entries
  else
    Ok (C_types.make_validator_set (List.map fst entries)) in
  let* authors = match validator_set.authors with
    | None -> Ok None
    | Some (height, entries) ->
      let* entries = List.fold_left (fun result (member, weight) ->
        let* result = result in
        let* pubkey = Checkpoint.raw_pubkey member.C_types.pubkey in
        Ok ((C_types.{member with pubkey}, weight) :: result)) (Ok []) entries in
      Ok (Some (height, List.rev entries)) in
  Ok {result with authors}

let encoded_validator_set validator_set =
  let* validators =
    List.fold_left
      (fun state (validator : C_types.validator_info) ->
        let* items = state in
        if String.length validator.pubkey <> 32 then
          Error "finality validator public key is invalid"
        else
          let* _ =
            match C_types.weight_of_addr validator_set validator.address with
            | Some value -> Ok value
            | None -> Error "finality validator weight is missing"
          in
          let encoded =
            C_types.{ validator with pubkey = Base64.encode_exn validator.pubkey }
          in
          Ok (encoded :: items))
      (Ok [])
      validator_set.C_types.validators
    |> Result.map List.rev
  in
  if validators = [] then
    Error "finality validator set is empty"
  else
    let authors = Option.map (fun (height, entries) -> height,
      List.map (fun (member, weight) ->
        C_types.{member with pubkey = Base64.encode_exn member.pubkey}, weight)
        entries) validator_set.authors in
    Ok C_types.{ validator_set with validators; authors }

let finalize_hash finalize =
  C_codec.encode_finalize finalize
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_raw_string
  |> Checkpoint.raw_to_hex

let same_set left right =
  C_config.validator_set_hash left = C_config.validator_set_hash right

let lower_hex lengths value =
  List.mem (String.length value) lengths
  && String.for_all
       (function
         | '0'..'9' | 'a'..'f' -> true
         | _ -> false)
       value

let verify_finalize ?policy ~chain_id ~validator_set finalize =
  if finalize.C_types.chain_id <> chain_id
     || finalize.header.chain_id <> chain_id then
    Error "finality chain mismatch"
  else if finalize.epoch_id <> finalize.header.epoch_id then
    Error "finality epoch mismatch"
  else
    match
      C_qc.validate_persisted_finalize_strict
        ?policy
        ~chain_id
        ~validator_set
        ~verify_vote:(verify_vote validator_set)
        finalize
    with
    | C_qc.Valid -> Ok ()
    | C_qc.Invalid reason -> Error ("finality qc " ^ reason)

let verify_anchor_finalize ?policy ~chain_id ~validator_set finalize =
  let* () = verify_finalize ?policy ~chain_id ~validator_set finalize in
  let vote_set =
    C_types.validator_set_for_epoch
      ?policy
      ~chain_id
      ~epoch_id:finalize.C_types.epoch_id
      validator_set
  in
  let signers =
    List.map (fun (vote : C_types.vote) -> vote.C_types.validator) finalize.C_types.precommits
  in
  if C_types.has_quorum vote_set signers then Ok ()
  else Error "finality qc signer quorum"

let step_input ?policy ~chain_id ~prior_epoch validator_set (step : step) =
  let epoch = step.finalize.C_types.epoch_id in
  match prior_epoch with
  | Some prior when Int64.compare epoch prior <= 0 ->
      Error
        (Printf.sprintf
           "validator transition order is invalid epoch = %Ld prior_epoch = %Ld"
           epoch
           prior)
  | _ when Int64.compare epoch Int64.max_int = 0 ->
    Error "validator transition epoch overflows"
  | _ when not (lower_hex [64; 128] step.ledger_state_root) ->
    Error "validator transition ledger root is invalid"
  | _ when not (lower_hex [64] step.epoch_index_root) ->
    Error "validator transition index root is invalid"
  | _ ->
    let* update = Update.of_string step.update in
    if Update.to_string update <> step.update then
      Error "validator transition update is not exact"
    else if not update.Update.weighted then
      Error "validator transition update is not weighted"
    else if update.source_epoch = None then
      Error "validator transition source epoch is missing"
    else
      let path, activation_valid =
        match step.source with
        | Pending ->
            ["meta"; Update.pending_meta_key],
            update.activate_epoch = Int64.succ epoch
        | Active ->
            ["meta"; Update.active_meta_key],
            Int64.compare update.activate_epoch epoch <= 0
        | Relief mark ->
            ["meta"; Update.pending_meta_key],
            mark.height = Int64.succ epoch
            && mark.activate_epoch = update.activate_epoch
            && mark.fingerprint = update.fingerprint
            && C_quorum_policy.linear_weight ~chain_id ~epoch_id:mark.height
      in
      if not activation_valid then
        Error "validator transition activation epoch mismatch"
      else
      let* () = verify_anchor_finalize ?policy ~chain_id ~validator_set step.finalize in
      let folded =
        Eic.folded_state_root
          ~ledger_state_root:step.ledger_state_root
          ~epoch_index_root:step.epoch_index_root
      in
      if Checkpoint.raw_to_hex step.finalize.header.proposed_state_root <> folded then
        Error "validator transition folded root mismatch"
      else
        let* next = Update.validator_set update in
        let* next = match step.source with
          | Pending | Active -> Ok next
          | Relief mark ->
            let current = C_types.validator_set_for_epoch ?policy ~chain_id
              ~epoch_id:mark.height validator_set in
            let target = C_types.validator_set_for_epoch ?policy ~chain_id
              ~epoch_id:update.activate_epoch next in
            let* selected = C_relief.restore ~chain_id ~height:mark.height ~current
              ~activate_epoch:update.activate_epoch ~target
              ~fingerprint:update.fingerprint mark in
            (match selected with Some set -> Ok set
             | None -> Error "validator relief step is expired") in
        Ok (epoch, next, path)

let step_output step (epoch, next, path) =
  let* value = Irmin.verify_merkle_proof_det
    ~ledger_state_root:step.ledger_state_root ~path ~proof:step.proof in
  if value <> Some step.update then Error "validator transition state proof mismatch"
  else
    Ok (Some epoch, next)

let verify_step ?policy ~chain_id ~prior_epoch validator_set step =
  let* input = step_input ?policy ~chain_id ~prior_epoch validator_set step in
  step_output step input

let effective ?policy value stake =
  C_types.validator_set_for_epoch ?policy ~chain_id:value.finalize.chain_id
    ~epoch_id:value.finalize.epoch_id stake

let derived_set ?policy value stake =
  let policy = Option.value policy ~default:(C_quorum_policy.for_chain value.finalize.chain_id) in
  if C_quorum_policy.select policy value.finalize.epoch_id = C_quorum_policy.Stake then
    effective ~policy value stake
  else match List.rev value.steps with
    | [] -> stake
    | step :: _ ->
      let update = Update.of_string step.update |> Result.get_ok in
      C_types.validator_set_for_epoch ~policy ~chain_id:value.finalize.chain_id
        ~epoch_id:update.activate_epoch stake

let derive_stake ?policy ~validator_set:trusted value =
  let* trusted = raw_validator_set trusted in
  let rec loop prior_epoch current = function
    | [] ->
        if same_set (derived_set ?policy value current) value.validator_set then Ok current
        else Error "finality validator transition is incomplete"
    | (step : step) :: rest ->
        if Int64.compare step.finalize.epoch_id value.finalize.epoch_id >= 0 then
          Error "validator transition is not before checkpoint"
        else
          let* prior_epoch, next =
            verify_step
              ?policy
              ~chain_id:value.finalize.chain_id
              ~prior_epoch
              current
              step
          in
          loop prior_epoch next rest
  in
  loop None trusted value.steps

let derive ?policy ~validator_set value =
  Result.map (derived_set ?policy value) (derive_stake ?policy ~validator_set value)

let verify_path ?policy ~validator_set encoded =
  let* value = decode encoded in
  let* stake = derive_stake ?policy ~validator_set value in
  let* () = verify_anchor_finalize ?policy ~chain_id:value.finalize.chain_id
    ~validator_set:value.validator_set value.finalize in
  Ok {value with stake = Some (path_epoch value, stake)}

let verify_checkpoint ?policy checkpoint value =
  let* () = Checkpoint.validate checkpoint in
  let finalize = value.finalize in
  let header = finalize.C_types.header in
  let set_hash =
    C_config.validator_set_hash value.validator_set
    |> Checkpoint.raw_to_hex
  in
  let* index_root =
    match checkpoint.Checkpoint.epoch_index_root with
    | Some value -> Ok value
    | None -> Error "finality checkpoint index root is missing"
  in
  let folded =
    Eic.folded_state_root
      ~ledger_state_root:checkpoint.ledger_state_root
      ~epoch_index_root:index_root
  in
  if folded <> checkpoint.state_root then
    Error "finality checkpoint folded root mismatch"
  else if set_hash <> checkpoint.Checkpoint.validator_set_hash then
    Error "finality validator set mismatch"
  else if finalize.chain_id <> checkpoint.chain_id then
    Error "finality chain mismatch"
  else if finalize.epoch_id <> checkpoint.epoch
       || header.epoch_id <> checkpoint.epoch then
    Error "finality epoch mismatch"
  else if Checkpoint.raw_to_hex header.proposed_state_root
          <> checkpoint.state_root then
    Error "finality state root mismatch"
  else if header.txid_hi <> checkpoint.txid_hi then
    Error "finality transaction high-water mismatch"
  else if
    Option.fold
      ~none:false
      ~some:(fun expected -> finalize_hash finalize <> expected)
      checkpoint.quorum_cert_hash
  then
    Error "finality certificate hash mismatch"
  else
    verify_anchor_finalize
      ?policy
      ~chain_id:checkpoint.chain_id
      ~validator_set:value.validator_set
      finalize

let verify ?policy ~validator_set checkpoint encoded =
  let* value = decode encoded in
  let* stake = derive_stake ?policy ~validator_set value in
  if not (same_set (derived_set ?policy value stake) value.validator_set) then
    Error "finality validator transition mismatch"
  else
    let* () = verify_checkpoint ?policy checkpoint value in
    Ok {value with stake = Some (path_epoch value, stake)}

let origin_shape ~max_bytes ~chain_id ~epoch_id ~state_root ~set_root encoded =
  if max_bytes <= 0 || String.length encoded > max_bytes then
    Error "origin proof size exceeds limit"
  else if chain_id = "" || String.length chain_id > 128 || epoch_id < 0L
      || String.length state_root <> 32 || String.length set_root <> 32 then
    Error "origin context is invalid"
  else Ok ()

let check_origin ?policy ~chain_id ~epoch_id ~state_root ~set_root validators value =
  let finalize = value.finalize in
  if finalize.C_types.chain_id <> chain_id || finalize.header.chain_id <> chain_id then
    Error "origin chain differs"
  else if finalize.epoch_id <> epoch_id || finalize.header.epoch_id <> epoch_id then
    Error "origin epoch differs"
  else if finalize.header.proposed_state_root <> state_root then
    Error "origin state root differs"
  else if C_config.validator_set_hash validators <> set_root then
    Error "origin validator set differs"
  else
    let* () = verify_anchor_finalize ?policy ~chain_id ~validator_set:validators finalize in
    Ok {chain_id; epoch_id; state_root; validators}

let verify_origin ?policy ~max_bytes ~trusted ~chain_id ~epoch_id ~state_root ~set_root encoded =
  let* () = origin_shape ~max_bytes ~chain_id ~epoch_id ~state_root ~set_root encoded in
  let* value = decode encoded in
  let* validators = derive ?policy ~validator_set:trusted value in
  check_origin ?policy ~chain_id ~epoch_id ~state_root ~set_root validators value

let derive_stake_lwt ?policy ?(cancelled = fun () -> false) ~validator_set value =
  let open Lwt.Syntax in
  if cancelled () then Lwt.return_error "state sync verification cancelled" else
  match raw_validator_set validator_set with
  | Error reason -> Lwt.return_error reason
  | Ok trusted ->
      let rec loop prior current = function
        | [] ->
          let* () = Lwt.pause () in
          if cancelled () then Lwt.return_error "state sync verification cancelled"
          else if not (same_set (derived_set ?policy value current) value.validator_set) then
            Lwt.return_error "finality validator transition is incomplete"
          else Lwt.return_ok current
        | (step : step) :: rest ->
          let* () = Lwt.pause () in
          if cancelled () then Lwt.return_error "state sync verification cancelled"
          else if Int64.compare step.finalize.epoch_id value.finalize.epoch_id >= 0 then
            Lwt.return_error "validator transition is not before checkpoint"
          else
            let chain_id = value.finalize.chain_id in
            let* checked = Lwt_preemptive.detach (fun () ->
              Result.bind (step_input ?policy ~chain_id ~prior_epoch:prior current step)
                (step_output step)) () in
            if cancelled () then Lwt.return_error "state sync verification cancelled"
            else match checked with
            | Error reason -> Lwt.return_error reason
            | Ok (prior, current) -> loop prior current rest
      in
      loop None trusted value.steps

let derive_lwt ?policy ?cancelled ~validator_set value =
  Lwt.map (Result.map (derived_set ?policy value))
    (derive_stake_lwt ?policy ?cancelled ~validator_set value)

module Paths = Map.Make(struct
  type t = string * string
  let compare = Stdlib.compare
end)

type path = {
  bytes : int;
  prior : int64 option;
  validators : C_types.validator_set;
  reversed : step list;
}

let restrict validators (finalize : C_types.finalize) =
  if not (C_types.is_author ~chain_id:finalize.chain_id ~epoch_id:finalize.epoch_id
      validators finalize.header.creator_addr) then None
  else Some C_types.{finalize with precommits = List.filter
    (fun (vote : C_types.vote) -> C_types.is_validator validators vote.validator)
    finalize.precommits}

let step_bytes (step : step) =
  let source = match step.source with
    | Pending | Active -> 1
    | Relief mark -> 5 + String.length (C_relief_log.encode mark) in
  source + 20 + String.length (C_codec.encode_finalize step.finalize)
  + String.length step.ledger_state_root + String.length step.epoch_index_root
  + String.length step.update + String.length step.proof

let raw_size value =
  12 + List.fold_left (fun bytes step -> bytes + step_bytes step) 0 value.steps
  + String.length (C_codec.encode_finalize value.finalize)
  + String.length (C_codec.encode_validator_set value.validator_set)

let encoded_size value = 4 * ((raw_size value + 2) / 3)

let extend_path ?policy ~chain_id path (step : step) =
  match restrict path.validators step.finalize with
  | None -> None
  | Some finalize ->
    let voters = C_types.validator_set_for_epoch ?policy ~chain_id
      ~epoch_id:finalize.epoch_id path.validators in
    if not (C_types.has_quorum voters
      (List.map (fun (vote : C_types.vote) -> vote.validator) finalize.precommits)) then None
    else
      let step = {step with finalize} in
      Some (path.bytes + step_bytes step, path, step)

let compact_lwt ?policy ?(cancelled = fun () -> false) ~validator_set value =
  let open Lwt.Syntax in
  let* checked = derive_stake_lwt ?policy ~cancelled ~validator_set value in
  match checked with
  | Error _ as error -> Lwt.return error
  | Ok _ ->
      begin match raw_validator_set validator_set with
      | Error reason -> Lwt.return_error reason
      | Ok initial ->
          let chain_id = value.finalize.chain_id in
          let retained = live_relief value in
          let empty = {bytes = 0; prior = None; validators = initial; reversed = []} in
          let path_key path =
            let steps = match path.reversed with [] -> [] | step :: _ -> [step] in
            let ending = derived_set ?policy {value with steps} path.validators in
            C_config.validator_set_hash path.validators, C_config.validator_set_hash ending in
          let rec loop paths selected prior current = function
            | [] ->
              let selected = Paths.fold (fun _ path best ->
                let short = {value with steps = List.rev path.reversed; stake = None} in
                if same_set (derived_set ?policy short path.validators) value.validator_set
                  && (path.bytes < best.bytes
                    || (path.bytes = best.bytes && compare path.prior best.prior < 0))
                then path else best) paths selected in
              Lwt.return_ok {value with steps = List.rev selected.reversed; stake = None}
            | step :: rest ->
                let* () = Lwt.pause () in
                if cancelled () then Lwt.return_error "state sync verification cancelled"
                else
                  let* next = Lwt_preemptive.detach (fun () ->
                    step_input ?policy ~chain_id ~prior_epoch:prior current step) () in
                  match next with
                  | Error reason -> Lwt.return_error reason
                  | Ok (epoch, next, _) ->
                    let* choices = Lwt_preemptive.detach (fun () ->
                      Paths.fold (fun _ path acc ->
                        match extend_path ?policy ~chain_id path step with
                        | None -> acc | Some item -> item :: acc) paths []
                      |> List.stable_sort (fun (left, _, _) (right, _, _) -> compare left right)) () in
                    let rec choose = function
                      | [] -> Lwt.return_error "validator path has no checked link"
                      | (bytes, path, link) :: rest ->
                        let* () = Lwt.pause () in
                        if cancelled () then Lwt.return_error "state sync verification cancelled"
                        else
                          let* checked = Lwt_preemptive.detach (fun () ->
                            step_input ?policy ~chain_id ~prior_epoch:path.prior path.validators link) () in
                          match checked with
                          | Ok (_, reached, _) when same_set reached next ->
                            Lwt.return_ok {bytes; prior = Some epoch; validators = next;
                              reversed = link :: path.reversed}
                          | _ -> choose rest in
                    let* extended = choose choices in
                    match extended with
                    | Error _ as error -> Lwt.return error
                    | Ok extended ->
                      let paths = match step.source with
                        | Relief mark when retained = Some mark -> Paths.empty
                        | _ -> paths in
                      let key = path_key extended in
                      let selected = match Paths.find_opt key paths with
                        | Some old when old.bytes <= extended.bytes -> old
                        | _ -> extended in
                      loop (Paths.add key selected paths) selected (Some epoch) next rest
          in
          let* result = loop (Paths.singleton (path_key empty) empty)
            empty None initial value.steps in
          match result with
          | Error _ as error -> Lwt.return error
          | Ok reduced ->
              let* verified = derive_stake_lwt ?policy ~cancelled ~validator_set reduced in
              Lwt.return (Result.map (fun _ ->
                if raw_size reduced < raw_size value then reduced else value) verified)
      end

let verify_lwt ?policy ?(cancelled = fun () -> false) ~validator_set checkpoint encoded =
  let open Lwt.Syntax in
  if cancelled () then Lwt.return_error "state sync verification cancelled" else
  let* decoded = Lwt_preemptive.detach decode encoded in
  match decoded with
  | Error reason -> Lwt.return_error reason
  | Ok value ->
      let* derived = derive_stake_lwt ?policy ~cancelled ~validator_set value in
      match derived with
      | Error reason -> Lwt.return_error reason
      | Ok stake ->
          Lwt_preemptive.detach (fun () ->
            Result.map (fun () -> {value with stake = Some (path_epoch value, stake)})
              (verify_checkpoint ?policy checkpoint value)) ()

let verify_origin_lwt ?policy ?(cancelled = fun () -> false) ~max_bytes ~trusted
    ~chain_id ~epoch_id ~state_root ~set_root encoded =
  let open Lwt.Syntax in
  if cancelled () then Lwt.return_error "state sync verification cancelled"
  else match origin_shape ~max_bytes ~chain_id ~epoch_id ~state_root ~set_root encoded with
  | Error reason -> Lwt.return_error reason
  | Ok () ->
    let* decoded = Lwt_preemptive.detach decode encoded in
    match decoded with
    | Error reason -> Lwt.return_error reason
    | Ok value ->
      let* derived = derive_lwt ?policy ~cancelled ~validator_set:trusted value in
      match derived with
      | Error reason -> Lwt.return_error reason
      | Ok validators ->
        if cancelled () then Lwt.return_error "state sync verification cancelled"
        else
          let* checked = Lwt_preemptive.detach (fun () ->
            check_origin ?policy ~chain_id ~epoch_id ~state_root ~set_root validators value) () in
          if cancelled () then Lwt.return_error "state sync verification cancelled"
          else Lwt.return checked

let read_state ~max_bytes ~(origin : origin) ~ledger_root ~index_root ~path ~proof =
  if max_bytes <= 0 || String.length proof > max_bytes then Error "state proof size exceeds limit"
  else if not (lower_hex [64; 128] ledger_root && lower_hex [64] index_root)
      || path = [] || List.exists (String.equal "") path then
    Error "state proof context is invalid"
  else if Eic.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root:index_root
      <> Checkpoint.raw_to_hex origin.state_root then
    Error "state proof root differs"
  else
    match Lwt.state (Irmin.verify_merkle_proof_lwt ~ledger_state_root:ledger_root ~path ~proof) with
    | Lwt.Return result -> result
    | Lwt.Fail error -> raise error
    | Lwt.Sleep -> failwith "state proof verification did not settle"

let read_state_lwt ?(cancelled = fun () -> false) ~max_bytes ~origin
    ~ledger_root ~index_root ~path ~proof () =
  let open Lwt.Syntax in
  if cancelled () then Lwt.return_error "state sync verification cancelled"
  else if max_bytes <= 0 || String.length proof > max_bytes then
    Lwt.return_error "state proof size exceeds limit"
  else
    let* result = Lwt_preemptive.detach (fun () ->
      read_state ~max_bytes ~origin ~ledger_root ~index_root ~path ~proof) () in
    if cancelled () then Lwt.return_error "state sync verification cancelled"
    else Lwt.return result