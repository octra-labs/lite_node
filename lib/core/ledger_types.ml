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


type account = {
  balance : Z.t;
  nonce : int;
  public_key : string option;
  encrypted_balance : string option;
  decrypt_allowance : Z.t;
}

let empty_account = {
  balance = Z.zero;
  nonce = 0;
  public_key = None;
  encrypted_balance = None;
  decrypt_allowance = Z.zero;
}

let account_to_yojson a =
  let opt_str = function None -> `Null | Some s -> `String s in
  `Assoc [
    "balance", `String (Z.to_string a.balance);
    "nonce", `Int a.nonce;
    "public_key", opt_str a.public_key;
    "encrypted_balance", opt_str a.encrypted_balance;
    "decrypt_allowance", `String (Z.to_string a.decrypt_allowance);
  ]

let account_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let balance = json |> member "balance" |> to_string |> Z.of_string in
    let nonce = json |> member "nonce" |> to_int in
    let public_key = (try Some (json |> member "public_key" |> to_string) with _ -> None) in
    let encrypted_balance = (try Some (json |> member "encrypted_balance" |> to_string) with _ -> None) in
    let decrypt_allowance =
      (try json |> member "decrypt_allowance" |> to_string |> Z.of_string
       with _ -> Z.zero) in
    Ok { balance; nonce; public_key; encrypted_balance; decrypt_allowance }
  with e -> Error (Printexc.to_string e)

type stealth_output = {
  id : int;
  stealth_tag : string;
  eph_pub : string;
  enc_amount : string;
  amount : string;
  epoch_id : int;
  tx_hash : string;
  sender_addr : string;
  claimed : int;
  claim_pub : string;
  delta_cipher_stored : string;
  amount_hash : string;
  amount_commitment : string;
}

let stealth_output_to_yojson s =
  `Assoc [
    "id", `Int s.id;
    "stealth_tag", `String s.stealth_tag;
    "eph_pub", `String s.eph_pub;
    "enc_amount", `String s.enc_amount;
    "amount", `String s.amount;
    "epoch_id", `Int s.epoch_id;
    "tx_hash", `String s.tx_hash;
    "sender_addr", `String s.sender_addr;
    "claimed", `Int s.claimed;
    "claim_pub", `String s.claim_pub;
    "delta_cipher_stored", `String s.delta_cipher_stored;
    "amount_hash", `String s.amount_hash;
    "amount_commitment", `String s.amount_commitment;
  ]

let stealth_output_of_yojson json =
  let open Yojson.Safe.Util in
  try
    Ok {
      id = json |> member "id" |> to_int;
      stealth_tag = json |> member "stealth_tag" |> to_string;
      eph_pub = json |> member "eph_pub" |> to_string;
      enc_amount = json |> member "enc_amount" |> to_string;
      amount = json |> member "amount" |> to_string;
      epoch_id = json |> member "epoch_id" |> to_int;
      tx_hash = json |> member "tx_hash" |> to_string;
      sender_addr = json |> member "sender_addr" |> to_string;
      claimed = json |> member "claimed" |> to_int;
      claim_pub = (try json |> member "claim_pub" |> to_string with _ -> "");
      delta_cipher_stored = (try json |> member "delta_cipher_stored" |> to_string with _ -> "");
      amount_hash = (try json |> member "amount_hash" |> to_string with _ -> "");
      amount_commitment = (try json |> member "amount_commitment" |> to_string with _ -> "");
    }
  with e -> Error (Printexc.to_string e)

type rejected_tx = {
  hash : string;
  from_addr : string;
  to_addr : string;
  amount : string;
  nonce : int;
  error_type : string;
  reason : string;
  epoch_id : int;
  ts : float;
}

let rejected_tx_to_yojson r =
  `Assoc [
    "hash", `String r.hash;
    "from_addr", `String r.from_addr;
    "to_addr", `String r.to_addr;
    "amount", `String r.amount;
    "nonce", `Int r.nonce;
    "error_type", `String r.error_type;
    "reason", `String r.reason;
    "epoch_id", `Int r.epoch_id;
    "ts", `Float r.ts;
  ]

let rejected_tx_of_yojson json =
  let open Yojson.Safe.Util in
  try
    Ok {
      hash = json |> member "hash" |> to_string;
      from_addr = json |> member "from_addr" |> to_string;
      to_addr = json |> member "to_addr" |> to_string;
      amount = json |> member "amount" |> to_string;
      nonce = json |> member "nonce" |> to_int;
      error_type = json |> member "error_type" |> to_string;
      reason = json |> member "reason" |> to_string;
      epoch_id = json |> member "epoch_id" |> to_int;
      ts = json |> member "ts" |> to_number;
    }
  with e -> Error (Printexc.to_string e)