(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source = {
  bucket : string;
  records : int;
  connected : int;
}

type rejection = {
  reason : string;
  count : int;
}

type relayed_records = {
  accepted : int;
  unchanged : int;
  rejected : int;
  rejections : rejection list;
}

type t = {
  connected : int;
  known : int;
  public_sources : int;
  sources : source list;
  scores : P2p_peer_guard.score_record list;
  relayed_records : relayed_records;
  risks : string list;
}

let empty_relayed_records = {
  accepted = 0;
  unchanged = 0;
  rejected = 0;
  rejections = [];
}

let bump tbl key f =
  let value =
    match Hashtbl.find_opt tbl key with
    | Some v -> v
    | None -> 0
  in
  Hashtbl.replace tbl key (f value)

let source_rows records connected =
  let rec_counts = Hashtbl.create 8 in
  let conn_counts = Hashtbl.create 8 in
  List.iter
    (fun r -> bump rec_counts (P2p_peer_record.bucket r) (fun n -> n + 1))
    records;
  List.iter
    (fun endpoint ->
      endpoint
      |> P2p_peer_guard.host_of_addr
      |> P2p_peer_record.bucket_of_host
      |> fun bucket -> bump conn_counts bucket (fun n -> n + 1))
    connected;
  let buckets =
    Hashtbl.fold (fun k _ acc -> k :: acc) rec_counts []
    |> List.fold_left
      (fun acc k -> if List.mem k acc then acc else k :: acc)
      (Hashtbl.fold (fun k _ acc -> k :: acc) conn_counts [])
    |> List.sort_uniq String.compare
  in
  List.map
    (fun bucket -> {
      bucket;
      records = Option.value ~default:0 (Hashtbl.find_opt rec_counts bucket);
      connected = Option.value ~default:0 (Hashtbl.find_opt conn_counts bucket);
    })
    buckets

let risk_rows ~min_public_sources public_sources (sources : source list) =
  let known =
    List.fold_left (fun n (row : source) -> n + row.records) 0 sources
  in
  let connected =
    List.fold_left (fun n (row : source) -> n + row.connected) 0 sources
  in
  let local_only =
    known + connected > 0
    && List.for_all (fun (row : source) -> row.bucket = "local") sources
  in
  if local_only then ["local_only"]
  else if known + connected > 0 && public_sources = 0 then ["non_public_only"]
  else if known + connected >= min_public_sources && public_sources < min_public_sources then
    ["low_public_source_diversity"]
  else
    []

let make
    ?(min_public_sources = 2)
    ?(relayed_records = empty_relayed_records)
    ~connected
    ~records
    ~scores
    () =
  let sources = source_rows records connected in
  let public_sources =
    (sources : source list)
    |> List.filter (fun (row : source) ->
      P2p_peer_record.public_bucket row.bucket && row.records + row.connected > 0)
    |> List.length
  in
  {
    connected = List.length connected;
    known = List.length records;
    public_sources;
    sources;
    scores;
    relayed_records;
    risks = risk_rows ~min_public_sources public_sources sources;
  }