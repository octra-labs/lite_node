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


module Validator = struct
  type t = {
    address : string;
    score : float;
    online : bool;
    last_seen : float;
  }

  let create addr score =
    { address = addr; score; online = true; last_seen = 0.0 }
end

module Difficulty = struct
  let challenge_string = "octra_useful_work"

  let meets_difficulty ~hash ~zero_bits =
    let bytes = Digestif.SHA256.to_raw_string hash in
    let rec go i acc =
      if acc >= zero_bits then true
      else if i >= String.length bytes then false
      else
        let byte = int_of_char bytes.[i] in
        if byte = 0 then go (i + 1) (acc + 8)
        else
          let bits = 8 - (int_of_float (log (float (byte + 1)) /. log 2.)) in
          bits + acc >= zero_bits
    in
    go 0 0

  let verify ~nonce ~address ~required_bits =
    challenge_string ^ address ^ string_of_int nonce
    |> Digestif.SHA256.digest_string
    |> fun hash -> meets_difficulty ~hash ~zero_bits:required_bits
end

let select_leader ?(epoch_id=0) validators =
  let online = List.filter (fun v -> v.Validator.online) validators in
  let sorted = List.sort (fun a b -> String.compare a.Validator.address b.Validator.address) online in
  match sorted with
  | [] -> failwith "no online validators"
  | _ ->
    let idx = epoch_id mod List.length sorted in
    List.nth sorted idx

let quorum threshold total =
  float_of_int total *. threshold |> int_of_float

let validate_commit_signatures signatures required quorum =
  List.length signatures >= quorum &&
  List.exists (fun (addr, _) -> addr = required) signatures

let verify_useful_work ~address ~nonce ~required_bits =
  Difficulty.verify ~nonce ~address ~required_bits