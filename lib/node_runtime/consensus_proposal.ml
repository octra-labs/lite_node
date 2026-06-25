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


module Transaction = Octra_core.Transaction

type limits = {
  max_txs : int;
  max_bytes : int;
  max_ou : Z.t;
}

type totals = {
  count : int;
  bytes : int;
  ou : Z.t;
}

type capped = {
  txs : Transaction.t list;
  skipped : int;
  totals : totals;
}

let limits ~max_txs ~max_bytes ~max_ou =
  {
    max_txs = max 1 max_txs;
    max_bytes = max 1024 max_bytes;
    max_ou;
  }

let wire_size tx =
  String.length (Yojson.Safe.to_string (Transaction.to_yojson tx))

let totals txs =
  let bytes, ou =
    List.fold_left
      (fun (bytes, ou) tx ->
         bytes + wire_size tx, Z.add ou (Transaction.ou_cost tx))
      (0, Z.zero)
      txs
  in
  {
    count = List.length txs;
    bytes;
    ou;
  }

let within_limits ~limits txs =
  let t = totals txs in
  t.count <= limits.max_txs
  && t.bytes <= limits.max_bytes
  && Z.leq t.ou limits.max_ou

let cap ~limits txs =
  let rec loop count bytes ou acc skipped = function
    | [] ->
      {
        txs = List.rev acc;
        skipped;
        totals = { count; bytes; ou };
      }
    | tx :: rest ->
      let next_count = count + 1 in
      let next_bytes = bytes + wire_size tx in
      let next_ou = Z.add ou (Transaction.ou_cost tx) in
      if next_count > limits.max_txs
         || next_bytes > limits.max_bytes
         || Z.gt next_ou limits.max_ou then
        {
          txs = List.rev acc;
          skipped = skipped + 1 + List.length rest;
          totals = { count; bytes; ou };
        }
      else
        loop next_count next_bytes next_ou (tx :: acc) skipped rest
  in
  loop 0 0 Z.zero [] 0 txs