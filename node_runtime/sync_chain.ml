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
}

let ( let* ) value f =
  match value with
  | Ok item -> f item
  | Error _ as error -> error

let same_set left right =
  C_config.validator_set_hash left = C_config.validator_set_hash right

let of_certificate trusted certificate =
  let* encoded =
    match Manifest.finality certificate with
    | Some value -> Ok value
    | None -> Error "state sync certificate has no finality anchor"
  in
  let* anchor =
    Anchor.verify
      ~validator_set:trusted
      certificate.Manifest.checkpoint
      encoded
  in
  Ok {
    rev_steps = List.rev (Anchor.steps anchor);
    validator_set = Anchor.validator_set anchor;
    epoch = certificate.Manifest.checkpoint.epoch;
  }

let load trusted path =
  if not (Sys.file_exists path) then None
  else
    match Manifest.load_certificate path with
    | Error _ -> None
    | Ok certificate ->
        begin
          match of_certificate trusted certificate with
          | Error _ -> None
          | Ok chain -> Some chain
        end

let stored deps trusted =
  match
    [
      State_sync.anchor_path deps.data_dir;
      deps.certificate_path ();
    ]
    |> List.filter_map (load trusted)
    |> List.sort (fun left right -> Int64.compare right.epoch left.epoch)
  with
  | first :: _ -> Some first
  | [] -> None

let update ~chain_id raw =
  let* update = Update.of_string raw in
  if Update.to_string update <> raw then
    Error "state sync validator update is not exact"
  else
    let* validator_set = Update.validator_set update in
    Ok
      ( update,
        C_types.validator_set_for_epoch
          ~chain_id
          ~epoch_id:update.activate_epoch
          validator_set )

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

let restrict validator_set (finalize : C_types.finalize) =
  if not (C_types.is_validator validator_set finalize.header.creator_addr) then
    None
  else
    Some C_types.{
      finalize with
      precommits =
        List.filter
          (fun (vote : C_types.vote) ->
            C_types.is_validator validator_set vote.validator)
          finalize.precommits;
    }

let bridge_finalize deps validator_set record =
  let finalize = record.Consensus_finality_journal.finalize in
  let source_set = record.validator_set in
  let* () =
    Anchor.verify_finalize
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
            ~chain_id:deps.chain_id
            ~validator_set
            restricted
        with
        | Ok () -> Ok (Some restricted)
        | Error _ -> Ok None
      end

let bridge_record deps validator_set epoch =
  match deps.read_finality epoch with
  | Consensus_finality_journal.Missing -> Ok None
  | Consensus_finality_journal.Invalid reason ->
      Error ("state sync bridge finality is invalid: " ^ reason)
  | Consensus_finality_journal.Valid record ->
      let* finalize = bridge_finalize deps validator_set record in
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

let bridge_range ~head_epoch ~after_epoch ~through_epoch ~activate_epoch =
  if Int64.compare head_epoch 0L <= 0
     || Int64.equal after_epoch Int64.max_int then
    None
  else
    let span = Int64.pred Consensus_finality_journal.history_limit in
    let floor =
      Int64.max
        (Int64.succ after_epoch)
        (Int64.max activate_epoch (Int64.sub head_epoch span))
    in
    let ceiling = Int64.min through_epoch (Int64.pred head_epoch) in
    if Int64.compare ceiling floor < 0 then None else Some (floor, ceiling)

let bridge_step_at deps ~validator_set raw epoch =
  Lwt_preemptive.detach
    (fun () -> bridge_record deps validator_set epoch)
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

let bridge_step deps ~head_epoch ~after_epoch ~through_epoch ~validator_set raw
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
        ~through_epoch
        ~activate_epoch:update.Update.activate_epoch
    with
    | None -> Lwt.return_error Bridge_unavailable
    | Some (floor, ceiling) ->
        let rec scan epoch =
          if Int64.compare epoch floor < 0 then
            Lwt.return_error Bridge_unavailable
          else
            bridge_step_at deps ~validator_set raw epoch >>= function
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

let walk deps ~head_epoch ~base ~stop raw =
  let rec loop depth seen through_epoch raw =
    if depth >= 1_024 then
      Lwt.return_error "state sync validator transition chain is too long"
    else
      let digest = Digestif.SHA256.(digest_string raw |> to_hex) in
      if S.mem digest seen then
        Lwt.return_error "state sync validator transition chain has a cycle"
      else
        match update ~chain_id:deps.chain_id raw with
        | Error reason -> Lwt.return_error reason
        | Ok (_, next_set) when same_set next_set stop.validator_set ->
            Lwt.return_ok stop
        | Ok (item, next_set) ->
            let append prior (step : Anchor.step) =
              Lwt.return_ok {
                rev_steps = step :: prior.rev_steps;
                validator_set = next_set;
                epoch =
                  Int64.max item.activate_epoch step.finalize.epoch_id;
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
                          match update ~chain_id:deps.chain_id prior_raw with
                          | Error reason -> Lwt.return_error reason
                          | Ok (_, prior_set)
                            when same_set prior_set stop.validator_set ->
                              Lwt.return_ok stop
                          | Ok _ ->
                              begin
                                match transition_epoch item with
                                | Error reason -> Lwt.return_error reason
                                | Ok (prior_through, _) ->
                                    loop
                                      (depth + 1)
                                      (S.add digest seen)
                                      prior_through
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
                    deps
                    ~head_epoch
                    ~after_epoch:prior.epoch
                    ~through_epoch
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
                  deps
                  ~head_epoch
                  ~after_epoch:stop.epoch
                  ~through_epoch
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
    loop 0 S.empty (Int64.pred head_epoch) raw

let build deps ~head_epoch trusted active =
  match Anchor.raw_validator_set trusted with
  | Error reason -> Lwt.return_error reason
  | Ok trusted_raw ->
      let base = {
        rev_steps = [];
        validator_set = trusted_raw;
        epoch = -1L;
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
              walk deps ~head_epoch ~base ~stop raw >>= function
              | Error _ as error -> Lwt.return error
              | Ok chain when same_set chain.validator_set active ->
                  Lwt.return_ok (List.rev chain.rev_steps)
              | Ok _ ->
                  Lwt.return_error
                    "state sync active validator set differs from history"
            in
            begin
              match stored deps trusted with
              | Some stop when not (same_set stop.validator_set trusted_raw) ->
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
              | Some _
              | None -> run base
            end