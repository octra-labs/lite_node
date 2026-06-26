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


type t = {
  epoch : int64;
  root : string;
  declared_count : int32;
  producer : string;
  txs : Octra_core.Transaction.t list;
}

type observer_event =
  | Silent
  | Ignored of {
      epoch : int;
      tx_count : int;
      root : string;
      producer : string;
    }

let tx_count t =
  List.length t.txs

let root_short root =
  if String.length root >= 16 then
    String.sub root 0 8 ^ ".." ^ String.sub root (String.length root - 8) 8
  else root

let producer_short t =
  String.sub t.producer 0 (min 14 (String.length t.producer))

let tx_of_json tx_json =
  match Octra_core.Transaction.of_yojson (Yojson.Safe.from_string tx_json) with
  | Ok tx -> tx
  | Error e -> failwith (Printf.sprintf "tx decode: %s" e)

let parse payload =
  let c = Octra_net.Oce1.make_cursor payload in
  let epoch = Octra_net.Oce1.get_u64 c in
  let root = Octra_net.Oce1.get_string c in
  let declared_count = Octra_net.Oce1.get_u32 c in
  let producer = Octra_net.Oce1.get_string c in
  let tx_count = Int32.to_int (Octra_net.Oce1.get_u32 c) in
  let txs =
    List.init tx_count (fun _ ->
      Octra_net.Oce1.get_string c |> tx_of_json)
  in
  {
    epoch;
    root;
    declared_count;
    producer;
    txs;
  }

let observer_event ~observer payload =
  try
    let broadcast = parse payload in
    if observer then
      Ok (Ignored {
        epoch = Int64.to_int broadcast.epoch;
        tx_count = tx_count broadcast;
        root = root_short broadcast.root;
        producer = producer_short broadcast;
      })
    else
      Ok Silent
  with exn ->
    Error (Printexc.to_string exn)