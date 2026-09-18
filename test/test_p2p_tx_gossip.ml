(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let fail name =
  failwith ("test failed: " ^ name)

let eq name a b =
  if a <> b then fail name

let h c = String.make 64 c

let expect_fail name f =
  try
    ignore (f ());
    fail name
  with _ -> ()

let test_roundtrip () =
  let open Octra_net.P2p_tx_gossip in
  eq "inv"
    (Inv [h 'a'; h 'b'])
    (decode (encode (Inv [h 'A'; h 'b'])));
  eq "get"
    (Get [h 'c'])
    (decode (encode (Get [h 'c'])));
  eq "tx"
    (Tx { hash = h 'd'; tx_json = "{\"x\":1}" })
    (decode (encode (Tx { hash = h 'd'; tx_json = "{\"x\":1}" })))

let test_rejects () =
  let open Octra_net.P2p_tx_gossip in
  expect_fail "short hash" (fun () -> encode (Inv ["abc"]));
  expect_fail "non-hex hash" (fun () -> encode (Get [String.make 64 'z']));
  expect_fail "too many hashes" (fun () ->
    encode (Inv (List.init (max_hashes + 1) (fun _ -> h 'e'))));
  expect_fail "bad version" (fun () -> decode "\000\001");
  expect_fail "too large tx json" (fun () ->
    encode (Tx { hash = h 'f'; tx_json = String.make (max_tx_json + 1) 'x' }))

let test_circle_asset_size () =
  let open Octra_net.P2p_tx_gossip in
  let tx_json = String.make 3_000_000 'x' in
  match decode (encode (Tx { hash = h 'a'; tx_json })) with
  | Tx decoded ->
    eq "circle asset hash" (h 'a') decoded.hash;
    eq "circle asset size" (String.length tx_json) (String.length decoded.tx_json);
    eq "circle asset body" tx_json decoded.tx_json
  | Inv _ | Get _ -> fail "circle asset kind"

let accept = function
  | Octra_net.P2p_tx_gossip_guard.Accept -> true
  | Octra_net.P2p_tx_gossip_guard.Drop _ -> false

let drop_reason = function
  | Octra_net.P2p_tx_gossip_guard.Accept -> ""
  | Octra_net.P2p_tx_gossip_guard.Drop s -> s

let test_guard_budget () =
  let open Octra_net.P2p_tx_gossip_guard in
  let cfg = {
    default with
    window_s = 10.0;
    max_msgs = 2;
    max_bytes = 10;
    max_payload_bytes = 8;
  } in
  let g = create () in
  eq "budget accept 1" true (accept (admit ~cfg g ~now:0.0 ~peer:"p" ~bytes:3));
  eq "budget accept 2" true (accept (admit ~cfg g ~now:1.0 ~peer:"p" ~bytes:3));
  eq "budget msg drop" "message_budget" (drop_reason (admit ~cfg g ~now:2.0 ~peer:"p" ~bytes:1));
  eq "budget reset" true (accept (admit ~cfg g ~now:11.0 ~peer:"p" ~bytes:3));
  eq "budget byte accept" true (accept (admit ~cfg g ~now:12.0 ~peer:"q" ~bytes:6));
  eq "budget byte drop" "byte_budget" (drop_reason (admit ~cfg g ~now:13.0 ~peer:"q" ~bytes:5));
  eq "budget payload drop" "payload_too_large" (drop_reason (admit ~cfg g ~now:0.0 ~peer:"r" ~bytes:9))

let test_guard_prune () =
  let open Octra_net.P2p_tx_gossip_guard in
  let g = create () in
  eq "prune old accept" true (accept (admit g ~now:0.0 ~peer:"old" ~bytes:1));
  eq "prune fresh accept" true (accept (admit g ~now:9.0 ~peer:"fresh" ~bytes:1));
  prune g ~now:10.0 ~ttl:5.0 ~max_entries:10;
  eq "prune ttl size" 1 (size g);
  let capped = create () in
  List.iter
    (fun i ->
      ignore (admit capped ~now:(float_of_int i) ~peer:("p" ^ string_of_int i) ~bytes:1))
    [0; 1; 2; 3; 4];
  prune capped ~now:10.0 ~ttl:100.0 ~max_entries:2;
  eq "prune cap size" 2 (size capped)

let test_guard_plans () =
  let open Octra_net.P2p_tx_gossip_guard in
  let cfg = { default with max_inv_request = 2; max_get_reply = 1 } in
  let has_hash hash = hash = h 'b' || hash = h 'd' in
  eq "inv plan"
    [h 'a'; h 'c']
    (plan_inv ~cfg ~has:has_hash [h 'a'; h 'a'; h 'b'; h 'c'; h 'd'; h 'e']);
  eq "get plan"
    [h 'b']
    (plan_get ~cfg ~has:has_hash [h 'a'; h 'b'; h 'b'; h 'd'])

let () =
  test_roundtrip ();
  test_rejects ();
  test_circle_asset_size ();
  test_guard_budget ();
  test_guard_prune ();
  test_guard_plans ();
  print_endline "p2p_tx_gossip tests passed"