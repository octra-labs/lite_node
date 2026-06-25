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


type t =
  | Invalid_signature
  | Invalid_nonce
  | Nonce_too_far
  | Insufficient_balance
  | Sender_not_found
  | Malformed_transaction
  | Duplicate_transaction
  | Staging_full
  | Self_transfer
  | Invalid_address
  | Node_rejected
  | Invalid_commit
  | Invalid_tree_signature
  | Supply_violation
  | Internal of string

let to_string = function
  | Invalid_signature -> "invalid_signature"
  | Invalid_nonce -> "invalid_nonce"
  | Nonce_too_far -> "nonce_too_far"
  | Insufficient_balance -> "insufficient_balance"
  | Sender_not_found -> "sender_not_found"
  | Malformed_transaction -> "malformed_transaction"
  | Duplicate_transaction -> "duplicate_transaction"
  | Staging_full -> "staging_full"
  | Self_transfer -> "self_transfer"
  | Invalid_address -> "invalid_address"
  | Node_rejected -> "node_rejected"
  | Invalid_commit -> "invalid_commit"
  | Invalid_tree_signature -> "invalid_tree_signature"
  | Supply_violation -> "supply_violation"
  | Internal _ -> "internal_error"

let to_status = function
  | Invalid_signature | Invalid_nonce | Nonce_too_far
  | Malformed_transaction | Self_transfer | Invalid_address -> `Bad_request
  | Insufficient_balance | Sender_not_found -> `Forbidden
  | Duplicate_transaction | Staging_full -> `Conflict
  | Node_rejected | Invalid_commit | Invalid_tree_signature | Supply_violation -> `Forbidden
  | Internal _ -> `Internal_server_error

let respond ?(reason="") e =
  let json = `Assoc [
    "status", `String "error";
    "error", `Assoc [
      "type", `String (to_string e);
      "reason", `String reason
    ]
  ] |> Yojson.Safe.to_string in
  Cohttp_lwt_unix.Server.respond_string ~status:(to_status e) ~body:json ()