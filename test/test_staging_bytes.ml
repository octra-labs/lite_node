(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_core

module B = Staging_bytes
module Pool = Tx_staging

let check condition reason = if not condition then failwith reason
let get = function Ok value -> value | Error reason -> failwith reason
let lookup _ = Some (Z.of_int 1_000_000_000, 0)

let tx ?(fee = 1_000) ?encrypted_data ?message ?(duty = false) sender nonce = Transaction.{
  from = Printf.sprintf "oct%044d" sender;
  to_ = "oct99999999999999999999999999999999999999999";
  amount = Z.zero;
  nonce;
  ou = Z.of_int fee;
  timestamp = 0.;
  signature = string_of_int sender ^ ":" ^ string_of_int nonce;
  public_key = None;
  message;
  op_type = if duty then ValidatorReady else Standard;
  encrypted_data;
}

let measured item = get (B.measure item)
let usage item =
  if item.Transaction.op_type = ValidatorReady && item.nonce <> 1 then
    B.{regular = measured item; duty = 0}
  else B.of_tx item (measured item)
let rules ~regular ~sender ~duty ~duty_sender = get (B.limits ~regular ~sender ~duty ~duty_sender)
let roomy = rules ~regular:1_000_000 ~sender:500_000 ~duty:100_000 ~duty_sender:50_000
let add ?(byte_limits = roomy) ?tx_limit ?ou_limit item =
  get (Pool.add_smart ~byte_limits ?tx_limit ?ou_limit ~lookup item)

let audit () =
  let entries = Pool.all () in
  let expected = List.fold_left (fun sum item -> B.add sum (usage item)) B.zero entries in
  check (Pool.staging_bytes () = expected) "total byte counter differs";
  let senders = List.map (fun item -> item.Transaction.from) entries |> List.sort_uniq String.compare in
  check (Hashtbl.length Pool.sender_byte_usage = List.length senders) "sender byte table differs";
  List.iter (fun sender ->
    let expected = List.filter (fun item -> item.Transaction.from = sender) entries
      |> List.fold_left (fun sum item -> B.add sum (usage item)) B.zero in
    check (Pool.sender_bytes sender = expected) "sender byte counter differs") senders;
  check (List.length entries = Pool.staging_size ()) "staging size differs";
  check (List.length entries = Hashtbl.length Pool.hash_index) "hash index size differs";
  check (List.length entries = Pool.Index.cardinal !(Pool.view_index)) "view index size differs";
  check (List.length entries = Pool.Evict_index.cardinal !(Pool.evict_index)) "eviction index size differs";
  check (List.length (List.filter (fun item -> (usage item).regular > 0) entries)
    = Pool.Byte_index.cardinal !(Pool.byte_index)) "byte index size differs";
  check (List.length entries = Pool.Recent_index.cardinal !(Pool.recent_index)) "recent index size differs";
  Hashtbl.iter (fun key (entry : Pool.entry) ->
    check (key = entry.tx.from ^ string_of_int entry.tx.nonce) "staging key differs";
    check (entry.bytes = measured entry.tx) "entry measurement differs";
    check (entry.usage = usage entry.tx) "entry lane differs";
    check (entry.hash = Transaction.hash entry.tx) "entry hash differs") Pool.staging;
  let cost = List.fold_left (fun sum item -> Z.add sum (Transaction.ou_cost item)) Z.zero entries in
  check (Pool.staging_total_ou () = cost) "OU counter differs"

let check_measure () =
  let item = tx 1 1 in
  let large = String.make 4_096 'x' in
  List.iter (fun grown -> check (measured grown > measured item) "retained field not measured") [
    {item with from = large}; {item with to_ = large};
    {item with signature = large}; {item with public_key = Some large};
    {item with message = Some large}; {item with encrypted_data = Some large};
    {item with amount = Z.shift_left Z.one 4_096};
    {item with ou = Z.shift_left Z.one 4_096};
  ];
  let raw char = {item with encrypted_data = Some (String.make 128 char)} in
  check (measured (raw '\000') = measured (raw '"')) "measurement depends on JSON escaping";
  let shared = {item with message = Some large; encrypted_data = Some large} in
  let copied = {shared with encrypted_data = Some (String.make 4_096 'x')} in
  check (measured shared = measured copied) "measurement depends on sharing";
  let key = {item with from = String.make 6 'a'; nonce = 1} in
  let longer = {key with nonce = 10} in
  check (measured longer - measured key = Sys.word_size / 8) "nonce key bytes missing";
  let no_separator = {item with from = String.make 5 'a'; nonce = 1} in
  check (measured {no_separator with nonce = 10} = measured no_separator) "key separator was counted";
  check (measured {item with amount = Z.neg (Z.shift_left Z.one 4_096)}
    = measured {item with amount = Z.shift_left Z.one 4_096}) "integer sign changed byte charge"

let check_config () =
  let defaults = get (B.of_env (fun _ -> None)) in
  let plan total added = B.plan defaults ~total ~sender:B.zero ~removed:B.zero ~added in
  let mib = 1_024 * 1_024 in
  check (plan B.zero {regular = 16 * mib; duty = 0} = Ok ()) "default sender exact capacity";
  check (Result.is_error (plan B.zero {regular = 16 * mib + 1; duty = 0})) "default sender excess";
  check (plan {regular = 1_024 * mib; duty = 0} B.zero = Ok ()) "default global exact capacity";
  check (Result.is_error (plan {regular = 1_024 * mib; duty = 0} {regular = 1; duty = 0})) "default global excess";
  check (plan {regular = 0; duty = 128 * mib} B.zero = Ok ()) "default duty exact capacity";
  check (Result.is_error (plan {regular = 0; duty = 128 * mib} {regular = 0; duty = 1})) "default duty excess";
  check (plan B.zero {regular = 0; duty = 65_536} = Ok ()) "default duty sender exact capacity";
  check (Result.is_error (plan B.zero {regular = 0; duty = 65_537})) "default duty sender excess";
  let names = ["OCTRA_STAGING_MAX_BYTES", "2147483648";
    "OCTRA_STAGING_SENDER_BYTES", "1024";
    "OCTRA_STAGING_DUTY_BYTES", "268435456";
    "OCTRA_STAGING_DUTY_SENDER_BYTES", "1024"] in
  List.iter (fun (name, value) ->
    let get_env raw key = if key = name then Some raw else None in
    check (Result.is_ok (B.of_env (get_env value))) "independent override refused";
    List.iter (fun raw -> check (Result.is_error (B.of_env (get_env raw))) "invalid override accepted")
      [""; "0"; "-1"; "+1"; " 1"; "1_000"; "1.0"; "99999999999999999999999"]) names;
  check (Result.is_error (B.of_env (fun name ->
    if name = "OCTRA_STAGING_MAX_BYTES" then Some "1" else None))) "inconsistent override accepted";
  check (Result.is_error (B.limits ~regular:max_int ~sender:1 ~duty:1 ~duty_sender:1)) "combined integer overflow accepted"

let check_plan () =
  let limits = rules ~regular:100 ~sender:40 ~duty:50 ~duty_sender:20 in
  let plan = B.plan limits in
  check (plan ~total:{regular = 60; duty = 30} ~sender:B.zero ~removed:B.zero
    ~added:{regular = 40; duty = 20} = Ok ()) "exact capacities refused";
  List.iter (fun (total, sender, added) ->
    check (Result.is_error (plan ~total ~sender ~removed:B.zero ~added)) "capacity excess accepted") [
    {B.regular = 100; duty = 0}, B.zero, {B.regular = 1; duty = 0};
    {B.regular = 40; duty = 0}, {B.regular = 40; duty = 0}, {B.regular = 1; duty = 0};
    {B.regular = 0; duty = 50}, B.zero, {B.regular = 0; duty = 1};
    {B.regular = 0; duty = 20}, {B.regular = 0; duty = 20}, {B.regular = 0; duty = 1};
  ];
  let total = {B.regular = 100; duty = 30} and sender = {B.regular = 40; duty = 0} in
  let removed = sender and added = {B.regular = 0; duty = 20} in
  check (plan ~total ~sender ~removed ~added = Ok ()) "cross-lane replacement refused";
  check (plan ~total ~sender ~removed ~added = plan ~total ~sender ~removed ~added) "capacity decision differs";
  check (Result.is_error (plan ~total:B.zero ~sender ~removed:B.zero ~added:B.zero)) "inconsistent sender accepted";
  check (Result.is_error (plan ~total ~sender ~removed:{regular = 41; duty = 0} ~added:B.zero)) "removal underflow accepted";
  check (Result.is_error (plan ~total ~sender ~removed:B.zero ~added:{regular = -1; duty = 0})) "negative bytes accepted";
  check (Result.is_error (plan ~total ~sender ~removed:B.zero ~added:{regular = max_int; duty = 0})) "addition overflow accepted"

let check_capacity () =
  Pool.clear ();
  let first = tx 1 1 in
  let size = measured first in
  let byte_limits = rules ~regular:(4 * size) ~sender:size ~duty:(2 * size) ~duty_sender:size in
  ignore (add ~byte_limits first);
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup (tx 1 2))) "sender excess entered";
  List.iter (fun sender -> ignore (add ~byte_limits (tx sender 1))) [2; 3; 4];
  let fifth = tx ~fee:2_000 5 1 in
  check (List.length (add ~byte_limits fifth) = 1) "higher byte fee did not displace cheap entry";
  let equal = tx 6 1 in
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup equal)) "equal byte fee displaced entry";
  check (not (Hashtbl.mem Pool.virtual_balances equal.from)) "byte refusal retained sender state";
  ignore (add ~byte_limits (tx ~duty:true 7 1));
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup
    (tx ~duty:true ~encrypted_data:(String.make size 'x') ~fee:2_000 7 1))) "duty sender excess entered";
  ignore (add ~byte_limits (tx ~duty:true 8 1));
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup (tx ~duty:true 9 1))) "duty excess entered";
  audit ();
  Pool.clear ();
  audit ()

let check_byte_bid () =
  Pool.clear ();
  let cheap = tx ~encrypted_data:(String.make 1_024 'x') 21 1 in
  let second = tx ~encrypted_data:(String.make 1_024 'x') 22 1 in
  let small = tx ~fee:600 23 1 in
  let size = measured cheap in
  let byte_limits = rules ~regular:(2 * size) ~sender:(2 * size)
    ~duty:10_000 ~duty_sender:10_000 in
  ignore (add ~byte_limits cheap);
  ignore (add ~byte_limits second);
  check (List.length (add ~byte_limits small) = 1) "byte fee compared absolute fee";
  audit ();
  Pool.clear ();
  let first = tx 31 1 and next = tx 31 2 and other = tx 32 1 in
  let byte_limits = rules ~regular:(3 * measured first) ~sender:(3 * measured first)
    ~duty:10_000 ~duty_sender:10_000 in
  List.iter (fun item -> ignore (add ~byte_limits item)) [first; next; other];
  let incoming = tx ~fee:20_000 ~encrypted_data:(String.make 600 'x') 33 1 in
  ignore (add ~byte_limits incoming);
  check (Pool.find_by_hash (Transaction.hash next) = None
    || Pool.find_by_hash (Transaction.hash first) <> None) "byte eviction left nonce gap";
  check ((Pool.staging_bytes ()).regular <= 3 * measured first) "byte eviction exceeded capacity";
  audit ();
  Pool.clear ();
  let costly = tx ~fee:100_000 31 2 in
  List.iter (fun item -> ignore (add ~byte_limits item)) [first; costly; other];
  let before = Pool.sample 20, Pool.recent 20, Pool.staging_bytes (), Pool.staging_total_ou () in
  let drops = List.map (fun item -> Pool.lookup_dropped (Transaction.hash item)) [first; costly; other] in
  let incoming = tx ~fee:10_000 ~encrypted_data:(String.make 1_024 'x') 34 1 in
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup incoming)) "high fee successor displaced";
  check (before = (Pool.sample 20, Pool.recent 20, Pool.staging_bytes (), Pool.staging_total_ou ()))
    "failed byte plan changed pool";
  check (drops = List.map (fun item -> Pool.lookup_dropped (Transaction.hash item)) [first; costly; other])
    "failed byte plan published drops";
  audit ();
  Pool.clear ();
  let prefix = tx 61 1 and successor = tx ~fee:100_000 61 2 in
  let occupied = tx ~fee:100_000 62 1 in
  let size = measured prefix in
  let byte_limits = rules ~regular:(3 * size) ~sender:(3 * size)
    ~duty:size ~duty_sender:size in
  List.iter (fun item -> ignore (add ~byte_limits item)) [prefix; successor; occupied];
  let incoming = tx ~fee:10_000 63 1 in
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup incoming))
    "terminal selection removed protected prefix";
  check (Pool.find_by_hash (Transaction.hash prefix) = Some prefix)
    "terminal refusal removed prefix";
  audit ();
  Pool.clear ();
  let byte_limits = rules ~regular:(2 * size) ~sender:(2 * size)
    ~duty:size ~duty_sender:size in
  List.iter (fun item -> ignore (add ~byte_limits item)) [prefix; occupied];
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup {prefix with nonce = 2; ou = Z.of_int 10_000}))
    "sender displaced own predecessor";
  audit ();
  Pool.clear ();
  let duty = tx ~duty:true 41 1 in
  let full = tx ~encrypted_data:(String.make 1_024 'x') 42 1 in
  let byte_limits = rules ~regular:(measured full) ~sender:(measured full)
    ~duty:(measured duty) ~duty_sender:(measured duty) in
  ignore (add ~byte_limits duty);
  ignore (add ~byte_limits full);
  ignore (add ~byte_limits (tx ~fee:10_000 43 1));
  check (Pool.find_by_hash (Transaction.hash duty) = Some duty) "byte eviction consumed duty reserve";
  audit ();
  Pool.clear ()

let check_acceptance () =
  List.iter (fun pressure ->
    Pool.clear ();
    let first = tx 51 1 in
    let size = measured first in
    let byte_limits = rules ~regular:(if pressure = 0 then size else 10 * size)
      ~sender:size ~duty:size ~duty_sender:size in
    ignore (add ~byte_limits first);
    let incoming = if pressure = 3 then {first with Transaction.ou = Z.of_int 2_000}
      else tx ~fee:2_000 52 1 in
    let tx_limit = if pressure = 1 then 1 else 10 in
    let ou_limit = Z.of_int (if pressure = 2 then 1_000 else 10_000) in
    let before = Pool.sample 20, Pool.recent 20, Pool.staging_bytes (), Pool.staging_total_ou () in
    let calls = ref 0 in
    let accept () = incr calls; Error "pre_verify_busy" in
    check (Pool.add_smart ~byte_limits ~tx_limit ~ou_limit ~accept ~lookup incoming
      = Error "pre_verify_busy") "acceptance refusal ignored";
    check (!calls = 1) "acceptance not checked once";
    check (before = (Pool.sample 20, Pool.recent 20, Pool.staging_bytes (), Pool.staging_total_ou ()))
      "acceptance refusal changed staging";
    check (Hashtbl.length Pool.virtual_balances = 1 && Hashtbl.length Pool.virtual_nonces = 1)
      "acceptance refusal retained sender";
    audit ()) [0; 1; 2; 3; 4];
  Pool.clear ();
  for sender = 100 to 300 do
    check (Result.is_error (Pool.add_smart ~lookup:(fun _ -> None) (tx sender 1)))
      "unfunded sender accepted"
  done;
  check (Hashtbl.length Pool.virtual_balances = 0 && Hashtbl.length Pool.virtual_nonces = 0)
    "failed submissions retained sender rows";
  audit ()

let check_duty_reserve () =
  Pool.clear ();
  let prefix = tx 71 2 in
  let future = tx ~duty:true ~message:"{\"consensus_pubkey\":\"key\",\"head_epoch\":\"11\",\"state_root\":\"root\"}" 71 3 in
  let capacity = measured prefix + measured future in
  let byte_limits = rules ~regular:capacity ~sender:capacity ~duty:capacity ~duty_sender:capacity in
  ignore (add ~byte_limits prefix);
  ignore (add ~byte_limits future);
  check ((Pool.staging_bytes ()).duty = 0) "future duty consumed reserve";
  let incoming = tx ~fee:50_000 ~encrypted_data:(String.make 800 'x') 72 1 in
  check (List.length (add ~byte_limits incoming) = 2) "future duty protected nonce gap";
  check (Pool.find_by_hash (Transaction.hash prefix) = None) "gap prefix retained";
  audit ();
  Pool.clear ()

let check_replace () =
  Pool.clear ();
  let original = tx ~encrypted_data:(String.make 2_048 'a') 6 1 in
  let size = measured original in
  let byte_limits = rules ~regular:size ~sender:size ~duty:size ~duty_sender:size in
  ignore (add ~byte_limits original);
  let before = Pool.staging_bytes (), Pool.staging_total_ou (),
    Hashtbl.find Pool.virtual_balances original.from, Pool.get_expected_nonce original.from,
    Pool.sample 10, Pool.recent 10 in
  let large = {original with encrypted_data = Some (String.make 4_096 'b'); ou = Z.of_int 2_000} in
  check (Result.is_error (Pool.add_smart ~byte_limits ~lookup large)) "oversized RBF accepted";
  check (before = (Pool.staging_bytes (), Pool.staging_total_ou (),
    Hashtbl.find Pool.virtual_balances original.from, Pool.get_expected_nonce original.from,
    Pool.sample 10, Pool.recent 10)) "RBF refusal changed pool";
  check (Pool.lookup_dropped (Transaction.hash original) = None) "RBF refusal recorded drop";
  let small = {original with encrypted_data = None; ou = Z.of_int 2_000} in
  check (List.length (add ~byte_limits small) = 1) "shrinking RBF refused";
  audit ();
  check (Pool.staging_bytes () = usage small) "shrinking RBF retained bytes";
  let duty = {small with op_type = Transaction.ValidatorReady; ou = Z.of_int 3_000} in
  ignore (add ~byte_limits duty);
  audit ();
  check ((Pool.staging_bytes ()).regular = 0) "cross-lane RBF retained general bytes";
  check (Pool.add_smart ~byte_limits ~lookup {duty with ou = Z.of_int 3_001}
    = Error "duplicate nonce (fee rate bump < 10%)") "RBF fee bump changed";
  let ordinary = {duty with op_type = Transaction.Standard; ou = Z.of_int 4_000} in
  ignore (add ~byte_limits ordinary);
  audit ();
  check ((Pool.staging_bytes ()).duty = 0) "cross-lane RBF retained duty bytes";
  Pool.clear ()

let check_lifecycle () =
  Pool.clear ();
  let first = tx 7 1 and second = tx 7 2 and third = tx 8 1 in
  List.iter (fun item -> ignore (add item)) [first; second; third];
  ignore (Pool.remove_by_hash (Transaction.hash first));
  check (not (Pool.remove_by_hash (Transaction.hash first))) "repeated remove succeeded";
  audit ();
  Pool.remove_processed [Transaction.hash second; Transaction.hash second];
  audit ();
  ignore (Pool.drop_preview third);
  audit ();
  let old = tx ~duty:true ~message:"{\"consensus_pubkey\":\"key\",\"head_epoch\":\"1\",\"state_root\":\"root\"}" 9 1 in
  ignore (add old);
  check (List.length (Pool.expire_duty ~head:(Some 2L) ()) = 1) "duty expiry failed";
  audit ();
  ignore (add first);
  let received = (Hashtbl.find Pool.hash_index (Transaction.hash first)).added_at in
  check (Pool.expire_old ~now:(received +. Pool.staging_ttl) () = []) "TTL expired at exact limit";
  check (List.length (Pool.expire_old ~now:(received +. Pool.staging_ttl +. 1.) ()) = 1) "TTL expiry failed";
  audit ();
  let dependent = {second with Transaction.ou = Z.of_int 2_000} in
  List.iter (fun item -> ignore (add ~tx_limit:2 item)) [first; dependent];
  let better = tx ~fee:3_000 8 1 in
  check (List.length (add ~tx_limit:2 better) = 2) "count eviction retained nonce dependents";
  check (Pool.sender_bytes first.from = B.zero) "count eviction retained sender bytes";
  audit ();
  Pool.clear ();
  let ou_limit = Z.of_int 2_000 in
  List.iter (fun item -> ignore (add ~ou_limit item)) [first; dependent];
  check (List.length (add ~ou_limit better) = 2) "OU eviction retained nonce dependents";
  check (Pool.sender_bytes first.from = B.zero) "OU eviction retained sender bytes";
  audit ();
  Pool.clear ();
  let duty = tx ~duty:true 9 1 in
  ignore (add ~tx_limit:1 duty);
  check (List.length (add ~tx_limit:1 better) = 1) "count eviction policy changed";
  audit ();
  for index = 1 to 100 do
    let item = tx ~duty:(index mod 2 = 0) ~encrypted_data:(String.make index 'x') (100 + index) 1 in
    ignore (add item);
    audit ();
    Pool.remove_processed [Transaction.hash item];
    audit ()
  done;
  Pool.clear ();
  audit ()

let () =
  check_measure ();
  check_config ();
  check_plan ();
  check_capacity ();
  check_byte_bid ();
  check_acceptance ();
  check_duty_reserve ();
  check_replace ();
  check_lifecycle ();
  print_endline "status = pass test = staging_bytes"