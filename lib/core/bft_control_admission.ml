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


let validate_message op_type message =
  match op_type with
  | Transaction.ValidatorSetUpdate ->
    begin
      match Validator_set_update.of_message message with
      | Ok _ -> Ok ()
      | Error e -> Error e
    end
  | Transaction.ValidatorReady ->
    begin
      match Validator_set_update.ready_ext_of_message message with
      | Ok _ -> Ok ()
      | Error e -> Error e
    end
  | _ -> Ok ()