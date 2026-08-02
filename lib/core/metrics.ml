(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type address_stat = {
  mutable sent_count : int;
  mutable received_count : int;
  mutable total_sent : Z.t;
  mutable total_received : Z.t;
  mutable last_active : float;
}

type stats = {
  mutable total_txs : int;
  mutable total_volume : Z.t;
  mutable total_fees : Z.t;
  mutable avg_tx_time : float;
  mutable peak_tps : float;
  mutable last_epoch_txs : int;
  mutable last_epoch_time : float;
  hourly_volume : (int, Z.t) Hashtbl.t;
  address_stats : (string, address_stat) Hashtbl.t;
  mutable top_addresses : (string * address_stat) list;
}

let global_stats = {
  total_txs = 0;
  total_volume = Z.zero;
  total_fees = Z.zero;
  avg_tx_time = 0.;
  peak_tps = 0.;
  last_epoch_txs = 0;
  last_epoch_time = 0.;
  hourly_volume = Hashtbl.create 24;
  address_stats = Hashtbl.create 1000;
  top_addresses = [];
}

let address_limit = 4096

let take n lst =
  let rec go n = function
    | _ when n <= 0 -> []
    | [] -> []
    | h :: t -> h :: go (n - 1) t
  in
  go n lst

let address_volume stat =
  Z.add stat.total_sent stat.total_received

let compare_address (left_addr, left) (right_addr, right) =
  let by_volume = Z.compare (address_volume right) (address_volume left) in
  if by_volume <> 0 then by_volume else String.compare left_addr right_addr

let update_top addr stat =
  let others =
    List.filter (fun (known, _) -> known <> addr) global_stats.top_addresses
  in
  global_stats.top_addresses <-
    (addr, stat) :: others
    |> List.sort compare_address
    |> take 10

let ranked addr =
  List.exists
    (fun (known, _) -> String.equal known addr)
    global_stats.top_addresses

let rec first_unranked seq =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (addr, rest) ->
    if ranked addr then first_unranked rest else Some addr

let reserve_address () =
  if Hashtbl.length global_stats.address_stats < address_limit then true
  else
    match first_unranked (Hashtbl.to_seq_keys global_stats.address_stats) with
    | None -> false
    | Some addr ->
      Hashtbl.remove global_stats.address_stats addr;
      true

let tracked_count () =
  Hashtbl.length global_stats.address_stats

let update_address addr is_sender amount =
  let stat =
    match Hashtbl.find_opt global_stats.address_stats addr with
    | Some s -> s
    | None ->
      let s = {
        sent_count = 0; received_count = 0;
        total_sent = Z.zero; total_received = Z.zero;
        last_active = Unix.gettimeofday ()
      } in
      if reserve_address () then Hashtbl.add global_stats.address_stats addr s;
      s
  in
  stat.last_active <- Unix.gettimeofday ();
  if is_sender then (
    stat.sent_count <- stat.sent_count + 1;
    stat.total_sent <- Z.add stat.total_sent amount
  ) else (
    stat.received_count <- stat.received_count + 1;
    stat.total_received <- Z.add stat.total_received amount
  );
  update_top addr stat

let record_tx (tx : Transaction.t) =
  global_stats.total_txs <- global_stats.total_txs + 1;
  global_stats.total_volume <- Z.add global_stats.total_volume tx.amount;
  global_stats.total_fees <- Z.add global_stats.total_fees (Transaction.ou_cost tx);
  update_address tx.from true tx.amount;
  update_address tx.to_ false tx.amount

let record_epoch_complete tx_count duration =
  global_stats.last_epoch_txs <- tx_count;
  global_stats.last_epoch_time <- duration;
  if tx_count > 0 && duration > 0. then
    let tps = float_of_int tx_count /. duration in
    if tps > global_stats.peak_tps then
      global_stats.peak_tps <- tps

let address_stat_to_yojson (addr, stat) =
  let fb = Denomination.format_balance in
  `Assoc [
    "address", `String addr;
    "sent_count", `Int stat.sent_count;
    "received_count", `Int stat.received_count;
    "total_sent", `String (fb stat.total_sent);
    "total_received", `String (fb stat.total_received);
    "total_volume", `String (fb (Z.add stat.total_sent stat.total_received));
    "last_active", `Float stat.last_active;
  ]

let get_metrics () =
  let fb = Denomination.format_balance in
  `Assoc [
    "total_transactions", `Int global_stats.total_txs;
    "total_volume", `String (fb global_stats.total_volume);
    "total_fees_collected", `String (fb global_stats.total_fees);
    "average_tx_time", `Float global_stats.avg_tx_time;
    "peak_tps", `Float global_stats.peak_tps;
    "last_epoch_txs", `Int global_stats.last_epoch_txs;
    "last_epoch_duration", `Float global_stats.last_epoch_time;
    "top_addresses",
      `List (List.map address_stat_to_yojson global_stats.top_addresses);
  ]