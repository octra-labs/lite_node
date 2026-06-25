(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type rollback = {
  rollback_epoch : int64;
  rollback_binary_hash : string;
  rollback_config_hash : string;
}

type t = {
  activate_epoch : int64;
  target_binary_hash : string;
  target_config_hash : string;
  rollback : rollback option;
}

type readiness =
  | Ready
  | Not_ready of string

let normalize_hash label h =
  if String.length h = 32 then Ok h
  else
  let h = String.trim h in
  if String.length h = 32 then Ok h
  else if String.length h = 64 then
    try
      Ok (String.init 32 (fun i ->
        Char.chr (int_of_string ("0x" ^ String.sub h (i * 2) 2))))
    with _ -> Error (label ^ " must be 32 raw bytes or 64 hex chars")
  else Error (label ^ " must be 32 raw bytes or 64 hex chars")

let rollback_of_env ~current_config_hash ~activate_epoch =
  match Sys.getenv_opt "OCTRA_P2P_ROLLBACK_ACTIVATE_EPOCH",
        Sys.getenv_opt "OCTRA_P2P_ROLLBACK_BINARY_HASH" with
  | None, None -> Ok None
  | Some _, None -> Error "OCTRA_P2P_ROLLBACK_BINARY_HASH is required"
  | None, Some _ -> Error "OCTRA_P2P_ROLLBACK_ACTIVATE_EPOCH is required"
  | Some epoch_raw, Some binary_raw ->
    try
      let rollback_epoch = Int64.of_string (String.trim epoch_raw) in
      if Int64.compare rollback_epoch activate_epoch <= 0 then
        Error "OCTRA_P2P_ROLLBACK_ACTIVATE_EPOCH must be greater than upgrade activation"
      else
        match normalize_hash "OCTRA_P2P_ROLLBACK_BINARY_HASH" binary_raw with
        | Error e -> Error e
        | Ok rollback_binary_hash ->
          let config_raw =
            match Sys.getenv_opt "OCTRA_P2P_ROLLBACK_CONFIG_HASH" with
            | Some h -> h
            | None -> current_config_hash
          in
          match normalize_hash "OCTRA_P2P_ROLLBACK_CONFIG_HASH" config_raw with
          | Error e -> Error e
          | Ok rollback_config_hash ->
            Ok (Some { rollback_epoch; rollback_binary_hash; rollback_config_hash })
    with _ -> Error "OCTRA_P2P_ROLLBACK_ACTIVATE_EPOCH is invalid"

let of_env ~current_config_hash =
  match Sys.getenv_opt "OCTRA_P2P_UPGRADE_ACTIVATE_EPOCH",
        Sys.getenv_opt "OCTRA_P2P_UPGRADE_BINARY_HASH" with
  | None, None -> Ok None
  | Some _, None -> Error "OCTRA_P2P_UPGRADE_BINARY_HASH is required"
  | None, Some _ -> Error "OCTRA_P2P_UPGRADE_ACTIVATE_EPOCH is required"
  | Some epoch_raw, Some binary_raw ->
    try
      let activate_epoch = Int64.of_string (String.trim epoch_raw) in
      if Int64.compare activate_epoch 0L <= 0 then
        Error "OCTRA_P2P_UPGRADE_ACTIVATE_EPOCH must be positive"
      else
        match normalize_hash "OCTRA_P2P_UPGRADE_BINARY_HASH" binary_raw with
        | Error e -> Error e
        | Ok target_binary_hash ->
          let config_raw =
            match Sys.getenv_opt "OCTRA_P2P_UPGRADE_CONFIG_HASH" with
            | Some h -> h
            | None -> current_config_hash
          in
          match normalize_hash "OCTRA_P2P_UPGRADE_CONFIG_HASH" config_raw with
          | Error e -> Error e
          | Ok target_config_hash ->
            match rollback_of_env ~current_config_hash ~activate_epoch with
            | Error e -> Error e
            | Ok rollback ->
              Ok (Some { activate_epoch; target_binary_hash; target_config_hash; rollback })
    with _ -> Error "OCTRA_P2P_UPGRADE_ACTIVATE_EPOCH is invalid"

let active ~epoch = function
  | None -> false
  | Some plan -> Int64.compare epoch plan.activate_epoch >= 0

let rollback_active ~epoch = function
  | None -> false
  | Some plan ->
    match plan.rollback with
    | None -> false
    | Some rb -> Int64.compare epoch rb.rollback_epoch >= 0

let schedule_hash ~base_config_hash plan =
  Hash_domain.hash_encoded "octra:p2p_upgrade_schedule:v1" (fun buf ->
    Oce1.put_hash32 buf base_config_hash;
    Oce1.put_u64 buf plan.activate_epoch;
    Oce1.put_hash32 buf plan.target_binary_hash;
    Oce1.put_hash32 buf plan.target_config_hash;
    match plan.rollback with
    | None -> Oce1.put_u8 buf 0
    | Some rb ->
      Oce1.put_u8 buf 1;
      Oce1.put_u64 buf rb.rollback_epoch;
      Oce1.put_hash32 buf rb.rollback_binary_hash;
      Oce1.put_hash32 buf rb.rollback_config_hash)

let transport_hash ~config_hash ~binary_hash =
  P2p_peer_record.transport_config_hash ~config_hash ~binary_hash

let handshake_hash ~epoch ~config_hash ~binary_hash ~require_binary_hash plan =
  match plan with
  | Some p when Int64.compare epoch p.activate_epoch < 0 ->
    schedule_hash ~base_config_hash:config_hash p
  | Some _ ->
    transport_hash ~config_hash ~binary_hash
  | None ->
    if require_binary_hash then transport_hash ~config_hash ~binary_hash
    else config_hash

let binary_required ~epoch ~require_binary_hash plan =
  require_binary_hash || active ~epoch plan

let ready ~epoch ~config_hash ~binary_hash = function
  | None -> Ready
  | Some p when Int64.compare epoch p.activate_epoch < 0 -> Ready
  | Some p ->
    let target_binary_hash, target_config_hash =
      match p.rollback with
      | Some rb when Int64.compare epoch rb.rollback_epoch >= 0 ->
        rb.rollback_binary_hash, rb.rollback_config_hash
      | _ -> p.target_binary_hash, p.target_config_hash
    in
    if binary_hash <> target_binary_hash then Not_ready "binary_hash"
    else if config_hash <> target_config_hash then Not_ready "config_hash"
    else Ready

let expected_binary_hash ~epoch ~binary_hash plan =
  match plan with
  | Some p ->
    (match p.rollback with
     | Some rb when Int64.compare epoch rb.rollback_epoch >= 0 ->
       rb.rollback_binary_hash
     | _ when Int64.compare epoch p.activate_epoch >= 0 ->
       p.target_binary_hash
     | _ -> binary_hash)
  | _ -> binary_hash