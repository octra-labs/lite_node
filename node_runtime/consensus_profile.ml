(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let int_value getenv name fallback =
  match getenv name with
  | None -> fallback
  | Some raw ->
    try int_of_string raw with _ -> fallback

let bool_value getenv name =
  match getenv name with
  | Some "1" | Some "true" -> true
  | _ -> false

let admit_ops getenv =
  Option.value ~default:"" (getenv "OCTRA_BFT_ADMIT_OPS")
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun value -> value <> "")
  |> List.sort_uniq String.compare

let receipt_activation getenv =
  Octra_core.Preverify_receipt_policy.activation_epoch_of getenv

let private_result_activation getenv =
  match Octra_core.Private_result_policy.activation_epoch_of getenv with
  | Ok epoch -> epoch
  | Error _ -> 0

let crypto_profile getenv =
  Octra_core.Transaction.bft_crypto_profile_of getenv
  |> Octra_core.Transaction.bft_crypto_profile_name

let release_profile getenv =
  Octra_core.Transaction.bft_release_profile_of getenv
  |> Octra_core.Transaction.bft_release_profile_name

let emission_schedule getenv =
  match Octra_core.Emission_schedule.of_env getenv with
  | Ok value -> value
  | Error _ -> Octra_core.Emission_schedule.Inactive

let required_receipt_activation getenv profile =
  match getenv "OCTRA_PREVERIFY_RECEIPT_ACTIVATION_EPOCH" with
  | None | Some "" ->
    Error (profile ^ " requires preverify receipt activation epoch")
  | Some raw ->
    begin
      try
        let epoch = int_of_string raw in
        if epoch < 0 then
          Error (profile ^ " requires nonnegative preverify receipt activation epoch")
        else
          Ok epoch
      with _ ->
        Error (profile ^ " requires valid preverify receipt activation epoch")
    end

let required_private_result_activation getenv profile =
  match getenv Octra_core.Private_result_policy.env_name with
  | None | Some "" ->
    Error (profile ^ " requires private result activation epoch")
  | Some _ ->
    begin
      match Octra_core.Private_result_policy.activation_epoch_of getenv with
      | Ok epoch -> Ok epoch
      | Error reason -> Error reason
    end

let migration_root getenv =
  Option.value ~default:"" (getenv "OCTRA_PVAC_MIGRATION_ROOT")

let valid_migration_root value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9' | 'a' .. 'f' -> true
         | _ -> false)
       value

let validate_migration getenv profile =
  match getenv "OCTRA_PVAC_MIGRATION_ENTITLEMENTS",
        getenv "OCTRA_PVAC_MIGRATION_ROOT" with
  | Some path, Some root
      when path <> "" && valid_migration_root root ->
    Ok ()
  | _ ->
    Error (profile ^ " requires migration entitlement path and root")

let validate_validator_schedule validator_policy schedule =
  match validator_policy, schedule with
  | Octra_core.Validator_policy.Inactive, _ -> Ok ()
  | Octra_core.Validator_policy.Bonded _,
    Octra_core.Emission_schedule.Inactive ->
    Error "bonded validator admission requires security_curve emission"
  | Octra_core.Validator_policy.Bonded validator,
    Octra_core.Emission_schedule.Security_curve emission ->
    if emission.activation_epoch > validator.activation_epoch then
      Error "validator admission activation precedes emission activation"
    else
      Ok ()

let validate getenv =
  match Octra_consensus.C_protocol.activation_epoch_of getenv with
  | Error error -> Error error
  | Ok _ ->
    match Octra_core.Validator_policy.of_env getenv with
    | Error error -> Error error
    | Ok validator_policy ->
    match Octra_core.Private_result_policy.activation_epoch_of getenv with
    | Error error -> Error error
    | Ok _ ->
      match Octra_core.Emission_schedule.of_env getenv with
      | Error error -> Error error
      | Ok schedule ->
        begin
          match validate_validator_schedule validator_policy schedule with
          | Error _ as error -> error
          | Ok () ->
        match getenv "OCTRA_BFT_RELEASE_PROFILE" with
        | None | Some "" ->
          Ok ()
        | Some value ->
          if value <> "devnet_private_v1" && value <> "devnet_full_v1" then
            Error ("unknown BFT release profile = " ^ value)
          else if admit_ops getenv <> [] then
            Error (value ^ " rejects OCTRA_BFT_ADMIT_OPS")
          else
            match required_receipt_activation getenv value with
            | Error error -> Error error
            | Ok receipt_epoch ->
              match required_private_result_activation getenv value with
              | Error error -> Error error
              | Ok _ ->
              match getenv "OCTRA_BFT_CRYPTO_PROFILE" with
              | Some raw when raw <> "" ->
                Error (value ^ " rejects OCTRA_BFT_CRYPTO_PROFILE")
              | _ ->
                if value = "devnet_private_v1" then
                  if getenv "OCTRA_EMISSION_GUARD" <> Some "1" then
                    Error (value ^ " requires OCTRA_EMISSION_GUARD = 1")
                  else
                    begin
                      match schedule with
                      | Octra_core.Emission_schedule.Inactive -> Ok ()
                      | Octra_core.Emission_schedule.Security_curve _ ->
                        Error (value ^ " rejects active emission")
                    end
                else if getenv "OCTRA_EMISSION_GUARD" = Some "1" then
                  Error (value ^ " rejects OCTRA_EMISSION_GUARD = 1")
                else
                  begin
                    match schedule with
                    | Octra_core.Emission_schedule.Security_curve active
                        when active.activation_epoch = receipt_epoch ->
                      validate_migration getenv "devnet_full_v1"
                    | Octra_core.Emission_schedule.Security_curve _ ->
                      Error
                        (value
                         ^ " requires matching emission and preverify activation epochs")
                    | Octra_core.Emission_schedule.Inactive ->
                      Error (value ^ " requires security_curve emission")
                  end
        end

let crypto_engine () =
  Octra_core.Pvac_registry.expected_kat ()

let circle_hfhe_abi = "bound_range_commitment_v2"
let program_fhe_abi = "proof_verify_view_only_v1"

let runtime_env getenv = Startup_runtime_limits.{
  int_value = (fun name fallback -> int_value getenv name fallback);
  opt = getenv;
}

let put_budget buf lane =
  let budget = Octra_core.Resource_lanes.default_budget lane in
  Octra_net.Oce1.put_string buf (Octra_core.Resource_lanes.to_string lane);
  Octra_net.Oce1.put_u32_int buf budget.max_txs;
  Octra_net.Oce1.put_u32_int buf budget.max_bytes;
  Octra_net.Oce1.put_string buf (Z.to_string budget.max_ou);
  Octra_net.Oce1.put_u32_int buf budget.max_proof

let put_int buf value =
  Octra_net.Oce1.put_string buf (string_of_int value)

let hash getenv =
  let env = runtime_env getenv in
  let schedule = emission_schedule getenv in
  let validator_policy =
    Octra_core.Validator_policy.of_env_exn getenv
  in
  let private_limits = Startup_runtime_limits.private_limits env in
  let epoch_min_ms = Epoch_cadence.minimum_ms_of getenv in
  let proposal_limits = Startup_runtime_limits.proposal_limits env
    ~default_max_ou:Octra_core.Tx_staging.max_ou_per_epoch in
  Octra_net.Hash_domain.hash_encoded "octra:consensus_profile:v11" (fun buf ->
    put_int buf proposal_limits.max_txs;
    put_int buf epoch_min_ms;
    put_int buf proposal_limits.max_bytes;
    Octra_net.Oce1.put_string buf
      (Z.to_string proposal_limits.max_ou);
    put_int buf private_limits.max_stealth_defer;
    put_int buf private_limits.max_stealth_per_epoch;
    put_int buf private_limits.max_fhe_per_epoch;
    Octra_net.Oce1.put_bool buf
      (bool_value getenv "OCTRA_STEALTH_INLINE_VERIFY");
    Octra_net.Oce1.put_string buf
      (Octra_core.Transaction.stealth_min_ou_of getenv |> Z.to_string);
    put_int buf (receipt_activation getenv);
    put_int buf (private_result_activation getenv);
    put_int buf (Startup_runtime_limits.multi_exec_max env);
    Octra_net.Oce1.put_bool buf
      (getenv "OCTRA_EMISSION_GUARD" = Some "1");
    Octra_net.Oce1.put_string buf (release_profile getenv);
    Octra_net.Oce1.put_string buf (crypto_profile getenv);
    Octra_net.Oce1.put_string buf
      (Octra_core.Emission_schedule.consensus_id schedule);
    Octra_net.Oce1.put_string buf
      (Octra_core.Validator_policy.consensus_id validator_policy);
    Octra_net.Oce1.put_string buf
      (Octra_consensus.C_protocol.consensus_id getenv);
    Octra_net.Oce1.put_string buf Octra_core.Fee_policy.consensus_id;
    Octra_net.Oce1.put_string buf
      (match Octra_core.Emission_schedule.activation_epoch schedule with
       | Some value -> string_of_int value
       | None -> "");
    Octra_net.Oce1.put_string buf (crypto_engine ());
    Octra_net.Oce1.put_string buf circle_hfhe_abi;
    Octra_net.Oce1.put_string buf program_fhe_abi;
    Octra_net.Oce1.put_string buf Octra_vm.Program_package.compiler_profile_id;
    Octra_net.Oce1.put_string buf (migration_root getenv);
    Octra_net.Oce1.put_list Octra_net.Oce1.put_string buf (admit_ops getenv);
    List.iter (put_budget buf) Octra_core.Resource_lanes.all)