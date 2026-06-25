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


module C_driver = Octra_consensus.C_driver

type majority = {
  root : string;
  count : int;
}

let required_attesters ~active_f ~validator_count ~configured =
  let max_peer_attesters = max 0 (validator_count - 1) in
  let base = max (active_f + 1) configured in
  if max_peer_attesters = 0 then 0 else min max_peer_attesters base

let peer_root_majority responses =
  let counts = Hashtbl.create 8 in
  List.iter
    (fun (r : C_driver.epoch_root_response_record) ->
      match r.state_root with
      | Some root ->
        let cur = try Hashtbl.find counts root with Not_found -> 0 in
        Hashtbl.replace counts root (cur + 1)
      | None -> ())
    responses;
  Hashtbl.fold
    (fun root count best ->
      match best with
      | None -> Some { root; count }
      | Some best when count > best.count -> Some { root; count }
      | Some _ -> best)
    counts
    None