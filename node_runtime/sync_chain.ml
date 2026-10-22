(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

module Anchor = Octra_bootstrap.Sync_anchor
module C_config = Octra_consensus.C_config
module C_types = Octra_consensus.C_types
module Irmin = Octra_core.Store_irmin
module Manifest = Octra_bootstrap.State_sync_manifest
module State_sync = Octra_bootstrap.State_sync
module Store = Octra_core.Store_chaindata
module Update = Octra_core.Validator_set_update
module S = Set.Make(String)

type deps = {
  data_dir : string;
  chain_id : string;
  store : Irmin.t;
  chaindata : Store.t;
  read_finality : int64 -> Consensus_finality_journal.read_result;
  certificate_path : unit -> string;
}

type t = {
  rev_steps : Anchor.step list;
  validator_set : C_types.validator_set;
  epoch : int64;
  witness : (C_types.finalize * string) option;
}

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let same_set left right =
  C_config.validator_set_hash left = C_config.validator_set_hash right

let retain_stake ?policy ~chain_id epoch =
  let policy = Option.value policy ~default:(Octra_consensus.C_quorum_policy.for_chain chain_id) in
  Octra_consensus.C_quorum_policy.select policy epoch = Octra_consensus.C_quorum_policy.Stake

let of_certificate ?policy ~head_epoch trusted certificate =
  match Manifest.finality certificate with
  | None -> Lwt.return_error "state sync certificate has no finality anchor"
  | Some encoded ->
      Anchor.verify_lwt
        ?policy
        ~validator_set:trusted
        certificate.Manifest.checkpoint
        encoded >>= (function
        | Error reason -> Lwt.return_error reason
        | Ok anchor -> Anchor.compact_lwt ?policy ~validator_set:trusted anchor >>= function
          | Error reason -> Lwt.return_error reason
          | Ok anchor -> Anchor.verify_lwt ?policy ~validator_set:trusted
              certificate.Manifest.checkpoint (Anchor.encode anchor))
        >|= fun checked -> Result.bind checked (fun anchor ->
          if not (retain_stake ?policy ~chain_id:certificate.Manifest.checkpoint.chain_id head_epoch) then
            Ok {rev_steps = List.rev (Anchor.steps anchor);
                validator_set = Anchor.validator_set anchor;
                epoch = certificate.Manifest.checkpoint.epoch; witness = None}
          else
          Result.map (fun (epoch, validator_set) ->
            let validator_set = match validator_set.C_types.authors with
              | Some (height, _) when height <> head_epoch -> {validator_set with authors = None}
              | _ -> validator_set in {
            rev_steps = List.rev (Anchor.steps anchor);
            validator_set;
            epoch;
            witness = Option.map (fun index -> Anchor.finality anchor, index)
              certificate.Manifest.checkpoint.epoch_index_root;
          }) (Anchor.transition_base anchor))

let load ?policy ~head_epoch trusted path =
  Lwt_preemptive.detach Manifest.load_certificate path >>= function
  | Error _ -> Lwt.return_none
  | Ok certificate ->
      of_certificate ?policy ~head_epoch trusted certificate >|= Result.to_option

let relief_path data_dir = Filename.concat data_dir "relief_chain"

let load_relief ?policy ~head_epoch deps trusted =
  Lwt_preemptive.detach (fun () ->
    try
      let* json = Manifest.load_limited (fun wire -> Ok (Yojson.Safe.from_string wire))
        (relief_path deps.data_dir) in
      match json with
      | `String wire ->
        let* anchor = Anchor.verify_path ?policy ~validator_set:trusted wire in
        let* epoch, validator_set = Anchor.transition_base anchor in
        if (Anchor.finality anchor).C_types.epoch_id > head_epoch then
          Error "relief chain is ahead"
        else
          let validator_set = match validator_set.authors with
            | Some (height, _) when height <> head_epoch -> {validator_set with authors = None}
            | _ -> validator_set in
          Ok {rev_steps = List.rev (Anchor.steps anchor); validator_set; epoch; witness = None}
      | _ -> Error "relief chain is not text"
    with exn -> Error (Printexc.to_string exn)) () >|= Result.to_option

let stored ?policy ~head_epoch deps trusted =
  Lwt_list.filter_map_s (load ?policy ~head_epoch trusted)
    [
      State_sync.anchor_path deps.data_dir;
      deps.certificate_path ();
    ] >>= fun chains ->
  load_relief ?policy ~head_epoch deps trusted >|= fun relief ->
  let chains = Option.to_list relief @ chains in
  match List.sort (fun left right -> Int64.compare right.epoch left.epoch) chains with
  | first :: _ -> Some first
  | [] -> None

let update ?policy ~chain_id ~head_epoch raw =
  let* update = Update.of_string raw in
  if Update.to_string update <> raw then
    Error "state sync validator update is not exact"
  else
    let* validator_set = Update.validator_set update in
    let validator_set = if retain_stake ?policy ~chain_id head_epoch then validator_set else
      C_types.validator_set_for_epoch ?policy ~chain_id ~epoch_id:update.activate_epoch validator_set in
    Ok (update, validator_set)

let transition_epoch update =
  let epoch = Int64.pred update.Update.activate_epoch in
  if Int64.compare epoch 0L < 0
     || Int64.compare epoch (Int64.of_int max_int) > 0 then
    Error "state sync validator transition epoch is invalid"
  else
    Ok (epoch, Int64.to_int epoch)

type transition_failure =
  | Finality_missing
  | Transition_invalid of string

let transition_record deps epoch epoch_int =
  match deps.read_finality epoch with
  | Consensus_finality_journal.Missing -> Error Finality_missing
  | Consensus_finality_journal.Invalid reason ->
      Error
        (Transition_invalid
           ("state sync validator transition finality is invalid: " ^ reason))
  | Consensus_finality_journal.Valid record ->
      begin
        match Store.get_epoch_index_commitment deps.chaindata epoch_int with
        | _, None ->
            Error
              (Transition_invalid
                 "state sync validator transition index root is missing")
        | _, Some epoch_index_root -> Ok (record, epoch_index_root)
      end

let transition_step deps raw update =
  match transition_epoch update with
  | Error reason -> Lwt.return_error (Transition_invalid reason)
  | Ok (epoch, epoch_int) ->
      Lwt_preemptive.detach
        (fun () -> transition_record deps epoch epoch_int)
        () >>= function
      | Error reason -> Lwt.return_error reason
      | Ok (record, epoch_index_root) ->
          Irmin.merkle_proof_at_epoch
            deps.store
            epoch_int
            ["meta"; Update.pending_meta_key] >>= function
          | Error reason -> Lwt.return_error (Transition_invalid reason)
          | Ok proof when proof.Irmin.value <> Some raw ->
              Lwt.return_error
                (Transition_invalid
                   "state sync validator transition proof value differs")
          | Ok proof ->
              Lwt.return_ok Anchor.{
                source = Pending;
                finalize = record.finalize;
                ledger_state_root = proof.ledger_state_root;
                epoch_index_root;
                update = raw;
                proof = proof.proof;
              }

let restrict = Anchor.restrict

let bridge_finalize ?policy deps validator_set record =
  let finalize = record.Consensus_finality_journal.finalize in
  let source_set = record.validator_set in
  let* () =
    Anchor.verify_finalize
      ?policy
      ~chain_id:deps.chain_id
      ~validator_set:source_set
      finalize
    |> Result.map_error (fun reason ->
      "state sync bridge source finality is invalid: " ^ reason)
  in
  match restrict validator_set finalize with
  | None -> Ok None
  | Some restricted ->
      begin
        match
          Anchor.verify_finalize
            ?policy
            ~chain_id:deps.chain_id
            ~validator_set
            restricted
        with
        | Ok () -> Ok (Some restricted)
        | Error _ -> Ok None
      end

let bridge_record ?policy deps validator_set epoch =
  match deps.read_finality epoch with
  | Consensus_finality_journal.Missing -> Ok None
  | Consensus_finality_journal.Invalid reason ->
      Error ("state sync bridge finality is invalid: " ^ reason)
  | Consensus_finality_journal.Valid record ->
      let* finalize = bridge_finalize ?policy deps validator_set record in
      begin
        match finalize with
        | None -> Ok None
        | Some finalize ->
            if Int64.compare epoch (Int64.of_int max_int) > 0 then
              Error "state sync bridge epoch exceeds platform limit"
            else
              match
                Store.get_epoch_index_commitment
                  deps.chaindata
                  (Int64.to_int epoch)
              with
              | _, None -> Error "state sync bridge index root is missing"
              | _, Some epoch_index_root ->
                  Ok (Some (finalize, epoch_index_root))
      end

type bridge_failure =
  | Bridge_unavailable
  | Bridge_invalid of string

let bridge_range ~head_epoch ~after_epoch ~before_epoch ~activate_epoch =
  if Int64.compare head_epoch 0L <= 0
     || Int64.compare before_epoch 0L <= 0
     || Int64.equal after_epoch Int64.max_int then
    None
  else
    let span = Int64.pred Consensus_finality_journal.history_limit in
    let floor =
      Int64.max
        (Int64.succ after_epoch)
        (Int64.max activate_epoch (Int64.sub head_epoch span))
    in
    let ceiling =
      Int64.min (Int64.pred before_epoch) (Int64.pred head_epoch)
    in
    if Int64.compare ceiling floor < 0 then None else Some (floor, ceiling)

let bridge_step_at ?policy deps ~validator_set raw epoch =
  Lwt_preemptive.detach
    (fun () -> bridge_record ?policy deps validator_set epoch)
    () >>= function
  | Error reason -> Lwt.return_error (Bridge_invalid reason)
  | Ok None -> Lwt.return_ok None
  | Ok (Some (finalize, epoch_index_root)) ->
      Irmin.merkle_proof_at_epoch
        deps.store
        (Int64.to_int epoch)
        ["meta"; Update.active_meta_key] >>= function
      | Error reason -> Lwt.return_error (Bridge_invalid reason)
      | Ok proof when proof.Irmin.value <> Some raw -> Lwt.return_ok None
      | Ok proof ->
          Lwt.return_ok
            (Some Anchor.{
              source = Active;
              finalize;
              ledger_state_root = proof.ledger_state_root;
              epoch_index_root;
              update = raw;
              proof = proof.proof;
            })

let bridge_step ?policy deps ~head_epoch ~after_epoch ~before_epoch ~validator_set raw
    update =
  if Int64.equal after_epoch Int64.max_int then
    Lwt.return_error
      (Bridge_invalid "state sync validator bridge order overflows")
  else if Int64.compare head_epoch 0L <= 0 then
    Lwt.return_error
      (Bridge_invalid "state sync validator bridge head is invalid")
  else
    match
      bridge_range
        ~head_epoch
        ~after_epoch
        ~before_epoch
        ~activate_epoch:update.Update.activate_epoch
    with
    | None -> Lwt.return_error Bridge_unavailable
    | Some (floor, ceiling) ->
        let rec scan epoch =
          if Int64.compare epoch floor < 0 then
            Lwt.return_error Bridge_unavailable
          else
            bridge_step_at ?policy deps ~validator_set raw epoch >>= function
            | Error _ as error -> Lwt.return error
            | Ok (Some step) -> Lwt.return_ok step
            | Ok None -> scan (Int64.pred epoch)
        in
        scan ceiling

let prior_update deps update =
  match transition_epoch update with
  | Error reason -> Lwt.return_error reason
  | Ok (_, epoch) ->
      Irmin.read_at_epoch
        deps.store
        epoch
        ["meta"; Update.active_meta_key] >>= fun value ->
      Lwt.return_ok value

let walk ?policy deps ~head_epoch ~base ~stop raw =
  let rec loop depth seen before_epoch raw =
    if depth >= Anchor.max_steps then
      Lwt.return_error "state sync validator transition chain is too long"
    else
      let digest = Digestif.SHA256.(digest_string raw |> to_hex) in
      if S.mem digest seen then
        Lwt.return_error "state sync validator transition chain has a cycle"
      else
        match update ?policy ~chain_id:deps.chain_id ~head_epoch raw with
        | Error reason -> Lwt.return_error reason
        | Ok (_, next_set) when same_set next_set stop.validator_set ->
            Lwt.return_ok stop
        | Ok (item, next_set) ->
            let append prior (step : Anchor.step) =
              Lwt.return_ok {
                rev_steps = step :: prior.rev_steps;
                validator_set = next_set;
                epoch = (if retain_stake ?policy ~chain_id:deps.chain_id head_epoch
                  then step.finalize.epoch_id else Int64.max item.activate_epoch step.finalize.epoch_id);
                witness = None;
              }
            in
            let history () =
              prior_update deps item >>= function
              | Error reason -> Lwt.return_error reason
              | Ok prior ->
                  let prior_chain =
                    match prior with
                    | None when same_set base.validator_set stop.validator_set ->
                        Lwt.return_ok stop
                    | None ->
                        Lwt.return_error
                          "state sync stored validator chain does not join history"
                    | Some prior_raw ->
                        begin
                          match update ?policy ~chain_id:deps.chain_id ~head_epoch prior_raw with
                          | Error reason -> Lwt.return_error reason
                          | Ok (_, prior_set)
                            when same_set prior_set stop.validator_set ->
                              Lwt.return_ok stop
                          | Ok _ ->
                              begin
                                match transition_epoch item with
                                | Error reason -> Lwt.return_error reason
                                | Ok (prior_before, _) ->
                                    loop
                                      (depth + 1)
                                      (S.add digest seen)
                                      prior_before
                                      prior_raw
                              end
                        end
                  in
                  prior_chain >>= function
                  | Error reason -> Lwt.return_error reason
                  | Ok prior -> Lwt.return_ok prior
            in
            let append_history step =
              history () >>= function
              | Error reason -> Lwt.return_error reason
              | Ok prior when same_set prior.validator_set next_set ->
                  Lwt.return_ok prior
              | Ok prior -> append prior step
            in
            let bridge_history () =
              history () >>= function
              | Error reason -> Lwt.return_error reason
              | Ok prior when same_set prior.validator_set next_set ->
                  Lwt.return_ok prior
              | Ok prior ->
                  bridge_step
                    ?policy
                    deps
                    ~head_epoch
                    ~after_epoch:prior.epoch
                    ~before_epoch
                    ~validator_set:prior.validator_set
                    raw
                    item >>= function
                  | Error Bridge_unavailable ->
                      Lwt.return_error
                        (Printf.sprintf
                           "state sync validator bridge is unavailable activate_epoch = %Ld after_epoch = %Ld head_epoch = %Ld"
                           item.activate_epoch
                           prior.epoch
                           head_epoch)
                  | Error (Bridge_invalid reason) -> Lwt.return_error reason
                  | Ok step -> append prior step
            in
            transition_step deps raw item >>= function
            | Ok step -> append_history step
            | Error (Transition_invalid reason) -> Lwt.return_error reason
            | Error Finality_missing ->
                bridge_step
                  ?policy
                  deps
                  ~head_epoch
                  ~after_epoch:stop.epoch
                  ~before_epoch
                  ~validator_set:stop.validator_set
                  raw
                  item >>= function
                | Ok step -> append stop step
                | Error (Bridge_invalid reason) -> Lwt.return_error reason
                | Error Bridge_unavailable -> bridge_history ()
  in
  if Int64.compare head_epoch 0L <= 0 then
    Lwt.return_error "state sync checkpoint epoch is invalid"
  else
    loop 0 S.empty head_epoch raw

let saved_step ?policy deps ~head_epoch stop raw update =
  match stop.witness with
  | Some (finalize, epoch_index_root)
    when finalize.C_types.epoch_id > stop.epoch
      && finalize.epoch_id < head_epoch
      && finalize.epoch_id <= Int64.of_int max_int
      && update.Update.activate_epoch <= finalize.epoch_id ->
    Irmin.merkle_proof_at_epoch deps.store (Int64.to_int finalize.epoch_id)
      ["meta"; Update.active_meta_key] >|= (function
      | Ok proof when proof.Irmin.value = Some raw
        && Octra_core.Epoch_index_commitment.folded_state_root
             ~ledger_state_root:proof.ledger_state_root ~epoch_index_root
           = Manifest.raw_to_hex finalize.header.proposed_state_root ->
        (match Anchor.verify_finalize ?policy ~chain_id:deps.chain_id
          ~validator_set:stop.validator_set finalize with
         | Error _ -> None
         | Ok () -> Some Anchor.{source = Active; finalize;
             ledger_state_root = proof.ledger_state_root; epoch_index_root;
             update = raw; proof = proof.proof})
      | _ -> None)
  | _ -> Lwt.return_none

let build_plain ?policy ?saved deps ~head_epoch trusted active =
  match Anchor.raw_validator_set trusted with
  | Error reason -> Lwt.return_error reason
  | Ok trusted_raw ->
      let base = {
        rev_steps = [];
        validator_set = trusted_raw;
        epoch = -1L;
        witness = None;
      } in
      if same_set trusted_raw active then Lwt.return_ok []
      else if Int64.compare head_epoch (Int64.of_int max_int) > 0 then
        Lwt.return_error "state sync checkpoint epoch exceeds platform limit"
      else
        Irmin.read_at_epoch
          deps.store
          (Int64.to_int head_epoch)
          ["meta"; Update.active_meta_key] >>= function
        | None -> Lwt.return_error "state sync active validator update is missing"
        | Some raw ->
            let run stop =
              let resume = match update ?policy ~chain_id:deps.chain_id ~head_epoch raw with
                | Error reason -> Lwt.return_error reason
                | Ok (_, next_set) when same_set next_set stop.validator_set ->
                  walk ?policy deps ~head_epoch ~base ~stop raw
                | Ok (item, next_set) ->
                  (if retain_stake ?policy ~chain_id:deps.chain_id head_epoch
                    then saved_step ?policy deps ~head_epoch stop raw item else Lwt.return_none) >>= function
                  | Some step -> Lwt.return_ok {rev_steps = step :: stop.rev_steps;
                      validator_set = next_set; epoch = step.finalize.epoch_id; witness = None}
                  | None -> walk ?policy deps ~head_epoch ~base ~stop raw in
              resume >>= function
              | Error _ as error -> Lwt.return error
              | Ok chain when List.length chain.rev_steps > Anchor.max_steps ->
                  Lwt.return_error "state sync validator transition chain is too long"
              | Ok chain when same_set
                  (if retain_stake ?policy ~chain_id:deps.chain_id head_epoch then
                    C_types.validator_set_for_epoch ?policy ~chain_id:deps.chain_id
                      ~epoch_id:head_epoch chain.validator_set
                  else chain.validator_set) active ->
                  Lwt.return_ok (List.rev chain.rev_steps)
              | Ok _ ->
                  Lwt.return_error
                    "state sync active validator set differs from history"
            in
            begin
              (match saved with Some chain -> Lwt.return_some chain
               | None -> stored ?policy ~head_epoch deps trusted) >>= function
              | Some stop when retain_stake ?policy ~chain_id:deps.chain_id head_epoch
                  || not (same_set stop.validator_set trusted_raw) ->
                  begin
                    run stop >>= function
                    | Ok _ as result -> Lwt.return result
                    | Error stored_reason ->
                        run base >|= Result.map_error (fun base_reason ->
                          Printf.sprintf
                            "state sync validator chain failed stored = %s base = %s"
                            stored_reason
                            base_reason)
                  end
              | Some _ | None -> run base
            end

let build_relief ?policy ?saved deps ~head_epoch trusted active =
  let module Relief = Octra_consensus.C_relief in
  let module Log = Octra_consensus.C_relief_log in
  let plain () = build_plain ?policy ?saved deps ~head_epoch trusted active in
  if not (Octra_consensus.C_quorum_policy.linear_weight
    ~chain_id:deps.chain_id ~epoch_id:head_epoch) then plain () else
  match Log.latest (Log.disk ~data_dir:deps.data_dir) ~through_height:head_epoch with
  | Error reason -> Lwt.return_error reason
  | Ok None -> plain ()
  | Ok (Some mark) when head_epoch >= mark.Relief.activate_epoch -> plain ()
  | Ok (Some mark) ->
    let epoch = Int64.pred mark.height in
    if epoch < 0L || epoch > Int64.of_int max_int then
      Lwt.return_error "state sync relief epoch is invalid"
    else Lwt_preemptive.detach
      (fun () -> transition_record deps epoch (Int64.to_int epoch)) () >>= function
    | Error Finality_missing -> Lwt.return_error "state sync relief parent is missing"
    | Error (Transition_invalid reason) -> Lwt.return_error reason
    | Ok (record, epoch_index_root) ->
      Irmin.merkle_proof_at_epoch deps.store (Int64.to_int epoch)
        ["meta"; Update.pending_meta_key] >>= function
      | Error reason -> Lwt.return_error reason
      | Ok proof ->
        match proof.Irmin.value with
        | None -> Lwt.return_error "state sync relief plan is missing"
        | Some raw ->
          build_plain ?policy ?saved deps ~head_epoch:epoch trusted record.validator_set >>= function
          | Error reason -> Lwt.return_error reason
          | Ok steps ->
          let parent = Anchor.make ~steps ~finalize:record.finalize
            ~validator_set:record.validator_set in
          Lwt_preemptive.detach (fun () ->
            let* parent = Anchor.verify_path ?policy ~validator_set:trusted (Anchor.encode parent) in
            let* _, stake = Anchor.transition_base parent in
            let current = C_types.validator_set_for_epoch ?policy ~chain_id:deps.chain_id
              ~epoch_id:mark.height stake in
            let* plan = Update.of_string raw in
            let* target = Update.validator_set plan in
            Relief.restore ~chain_id:deps.chain_id ~height:head_epoch
              ~current ~activate_epoch:plan.activate_epoch
              ~target ~fingerprint:plan.fingerprint mark) () >>= function
          | Error reason -> Lwt.return_error reason
          | Ok None -> Lwt.return_error "state sync relief is expired"
          | Ok (Some selected) when not (same_set selected active) ->
            Lwt.return_error "state sync relief set differs"
          | Ok (Some _) ->
            Lwt.return_ok (steps @ [Anchor.{
              source = Relief mark; finalize = record.finalize;
              ledger_state_root = proof.ledger_state_root; epoch_index_root;
              update = raw; proof = proof.proof;
            }])

let local_dir data_dir = Filename.concat data_dir "relief_steps"

let local_name epoch = Printf.sprintf "%020Ld.proof" epoch

let local_epochs data_dir ~head_epoch =
  let path = local_dir data_dir in
  if not (Sys.file_exists path) then [] else
  Sys.readdir path |> Array.to_list |> List.filter_map (fun name ->
    try
      let epoch = Int64.of_string (Filename.remove_extension name) in
      if epoch >= 0L && epoch <= head_epoch && name = local_name epoch
      then Some epoch else None
    with _ -> None)
  |> List.sort (fun left right -> Int64.compare right left)

let read_local ?policy deps epoch =
  try
    let* json = Manifest.load_limited (fun wire -> Ok (Yojson.Safe.from_string wire))
      (Filename.concat (local_dir deps.data_dir) (local_name epoch)) in
    let open Yojson.Safe.Util in
    let source = json |> member "source" |> to_string |> Base64.decode_exn
      |> Octra_consensus.C_codec.decode_validator_set in
    let* trusted = Anchor.encoded_validator_set source in
    let* anchor = Anchor.verify_path ?policy ~validator_set:trusted
      (json |> member "proof" |> to_string) in
    match Anchor.steps anchor with
    | [{source = Relief mark; finalize = parent; _}]
      when mark.height = epoch && (Anchor.finality anchor).epoch_id = epoch
        && parent.epoch_id = Int64.pred epoch && parent.chain_id = deps.chain_id ->
      Ok (source, anchor)
    | _ -> Error "local relief proof height differs"
  with exn -> Error (Printexc.to_string exn)

let build ?policy deps ~head_epoch trusted active =
  let rec gather depth head_epoch active =
    if depth > Anchor.max_steps then Lwt.return_error "local relief path is too long" else
    stored ?policy ~head_epoch deps trusted >>= fun saved ->
    let matches saved = same_set (C_types.validator_set_for_epoch ?policy
      ~chain_id:deps.chain_id ~epoch_id:head_epoch saved.validator_set) active in
    match saved with
    | Some saved when matches saved -> Lwt.return_ok (List.rev saved.rev_steps)
    | _ ->
      Lwt_preemptive.detach (fun () ->
        match local_epochs deps.data_dir ~head_epoch with
        | epoch :: _ when Option.fold ~none:true
            ~some:(fun saved -> epoch > saved.epoch) saved ->
          Result.map Option.some (read_local ?policy deps epoch)
        | _ -> Ok None) () >>= function
      | Error reason -> Lwt.return_error reason
      | Ok None -> build_relief ?policy ?saved deps ~head_epoch trusted active
      | Ok (Some (source, local)) ->
        let epoch = (Anchor.finality local).epoch_id in
        gather (depth + 1) (Int64.pred epoch) source >>= function
        | Error reason -> Lwt.return_error reason
        | Ok prefix ->
          let anchor = Anchor.make ~steps:(prefix @ Anchor.steps local)
            ~finalize:(Anchor.finality local) ~validator_set:(Anchor.validator_set local) in
          Lwt_preemptive.detach (fun () ->
            let* checked = Anchor.verify_path ?policy ~validator_set:trusted (Anchor.encode anchor) in
            let* epoch, validator_set = Anchor.transition_base checked in
            let validator_set = match validator_set.C_types.authors with
              | Some (height, _) when height <> head_epoch -> {validator_set with authors = None}
              | _ -> validator_set in
            Ok {rev_steps = List.rev (Anchor.steps checked); validator_set; epoch; witness = None}) () >>= function
          | Error reason -> Lwt.return_error reason
          | Ok saved when matches saved -> Lwt.return_ok (List.rev saved.rev_steps)
          | Ok saved -> build_relief ?policy ~saved deps ~head_epoch trusted active
  in
  gather 0 head_epoch active

let keep_new_relief deps validator_set finalize =
  match validator_set.C_types.authors with
  | None -> Lwt.return_ok ()
  | Some (height, _) when height <> finalize.C_types.epoch_id ->
    Lwt.return_error "relief chain height differs"
  | Some _ ->
    let epoch = Int64.pred finalize.epoch_id in
    if epoch < 0L || epoch > Int64.of_int max_int then
      Lwt.return_error "local relief parent height is invalid" else
    Lwt_preemptive.detach (fun () -> transition_record deps epoch (Int64.to_int epoch)) () >>= function
    | Error Finality_missing -> Lwt.return_error "local relief parent is missing"
    | Error (Transition_invalid reason) -> Lwt.return_error reason
    | Ok (parent, _) ->
      (match Anchor.encoded_validator_set parent.validator_set with
       | Error reason -> Lwt.return_error reason
       | Ok trusted -> build_relief deps ~head_epoch:finalize.epoch_id trusted validator_set)
      >>= function
      | Error reason -> Lwt.return_error reason
      | Ok steps when List.length steps <> 1 -> Lwt.return_error "local relief step count differs"
      | Ok steps ->
      let anchor = Anchor.make ~steps ~finalize ~validator_set in
      Lwt_preemptive.detach (fun () ->
        let* trusted = Anchor.encoded_validator_set parent.validator_set in
        let wire = Anchor.encode anchor in
        let* _ = Anchor.verify_path ~validator_set:trusted wire in
        let json = `Assoc ["source", `String (Octra_consensus.C_codec.encode_validator_set
          parent.validator_set |> Base64.encode_exn); "proof", `String wire] in
        if String.length (Yojson.Safe.to_string json) > Manifest.manifest_limit then
          Error "local relief proof size exceeds limit" else
        try
          Manifest.write_json (Filename.concat (local_dir deps.data_dir)
            (local_name finalize.epoch_id)) json;
          Ok ()
        with exn -> Error (Printexc.to_string exn)) ()

let keep_relief deps validator_set finalize =
  let path = Filename.concat (local_dir deps.data_dir) (local_name finalize.C_types.epoch_id) in
  if validator_set.C_types.authors = None then Lwt.return_ok ()
  else if not (Sys.file_exists path) then keep_new_relief deps validator_set finalize
  else Lwt_preemptive.detach (fun () ->
    let* _, saved = read_local deps finalize.epoch_id in
    if (Anchor.finality saved).proposal_id = finalize.proposal_id
      && same_set (Anchor.validator_set saved) validator_set then Ok ()
    else Error "local relief proof conflicts with applied value") ()

let prune_local data_dir ~through_epoch =
  List.iter (fun epoch -> Unix.unlink (Filename.concat (local_dir data_dir) (local_name epoch)))
    (local_epochs data_dir ~head_epoch:through_epoch)