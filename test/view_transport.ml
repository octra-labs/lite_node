(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module C = Octra_core.Circles
module Read = Octra_node_runtime.Circle_read_rpc
module Auth = Octra_node_runtime.Circle_auth
module Policy = Octra_core.Circle_transport_policy
module Key = Octra_core.Circle_key_policy
module V = Octra_vm

let run path =
  let expect = View_binding.expect in
  let circle_id = View_binding.circle_id in
  let first = View_binding.identity '\056' in
  let second = View_binding.identity '\057' in
  let owner, _, _ = first in
  let relay, _, _ = second in
  let intent_id = String.make 64 'a' in
  let key_id = "delivery" in
  let hash epoch = Printf.sprintf "%064x" epoch in
  let intent epoch = C.{
    intent_id;
    created_epoch = Int64.of_int epoch;
    expiry_epoch = Int64.of_int (epoch + 70);
    relay_policy_hash = hash epoch;
    payload_hash = hash (epoch + 1);
    ciphertext_blob_hash = Some (hash (epoch + 2));
    delivery_key_id = Some key_id;
    max_response_bytes = Int64.of_int epoch;
    fee_budget = Int64.of_int epoch;
    route_hint = Some (string_of_int epoch);
    callback_policy_hash = Some (hash (epoch + 3));
  } in
  let claim epoch relay_id = C.{
    intent_id;
    relay_id;
    claim_epoch = Int64.of_int (epoch + 1);
    claim_expiry_epoch = Int64.of_int (epoch + 20);
    signature = Base64.encode_exn (String.make 64 (Char.chr epoch));
  } in
  let packet epoch : C.ingress_packet = C.{
    intent_id;
    relay_id = relay;
    ingress_nonce = Int64.of_int epoch;
    result_code = epoch;
    response_payload_hash = hash epoch;
    response_size_bytes = Int64.of_int epoch;
    response_ciphertext_blob_hash = Some (hash (epoch + 1));
    external_receipt_hash = Some (hash (epoch + 2));
    signature = Base64.encode_exn (String.make 64 (Char.chr epoch));
  } in
  let info = C.{
    circle_id;
    runtime = Octb;
    version = 1L;
    owner;
    code_hash = zero_hash_hex;
    stable_root = zero_hash_hex;
    assets_root = zero_hash_hex;
    privacy_class = Scoped;
    browser_mode = Native_sealed;
    resource_mode = Public_resources;
    policy_hash = None;
    members_root = None;
    export_policy = None;
    limits = default_limits;
  } in
  let signed ?subject identity name id =
    let addr, public_key, secret = identity in
    let subject = Option.value subject ~default:id in
    let signature = Auth.message ~op:("octra_circle_" ^ name) ~circle_id
      ~addr ~subject
      |> Mirage_crypto_ec.Ed25519.sign ~key:secret
      |> Base64.encode_exn in
    `List (List.map (fun value -> `String value)
      [circle_id; id; addr; public_key; signature]) in
  let clock = ref 51 in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let put epoch identity =
      let addr, _, _ = identity in
      let data = Hashtbl.create 8 in
      List.iter (fun (key, value) -> Hashtbl.add data key value) [
        Policy.claim_topology_key, "quorum_relays";
        Policy.quorum_mode_key, "weighted_quorum";
        Policy.quorum_threshold_key, "2";
        Policy.quorum_weight_threshold_key, "3";
        Policy.max_active_claims_key, "2";
        Policy.ingress_strategy_key, "primary_heaviest";
        Policy.relay_weights_key, Yojson.Safe.to_string (`Assoc [owner, `Int 1; relay, `Int 2]);
        Key.activate_after_key key_id, string_of_int (epoch + 1);
        Key.expire_after_key key_id, string_of_int (epoch + 60);
      ];
      let open Lwt.Syntax in
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
      let* () = S.deploy_circle store {info with owner = addr} in
      let* _ = S.save_circle_stable_storage store circle_id data in
      let* () = S.save_circle_outbox_intent store circle_id (intent epoch) in
      let* () = S.set_circle_outbox_status store circle_id intent_id C.Claimed in
      let* () = S.save_circle_outbox_claim store circle_id (claim epoch owner) in
      let* () = S.save_circle_outbox_claim_for_relay store circle_id (claim epoch relay) in
      S.save_circle_ingress_packet store circle_id (packet epoch) in
    let load ?snapshot name id epoch =
      match name with
      | "outbox_claim" -> Read.outbox_claim ?snapshot store ~circle_id ~intent_id:id ~current_epoch:epoch
      | "outbox_intent" -> Read.outbox_intent ?snapshot store ~circle_id ~intent_id:id
      | "outbox_status" -> Read.outbox_status ?snapshot store ~circle_id ~intent_id:id ~current_epoch:epoch
      | "ingress_packet" -> Read.ingress_packet ?snapshot store ~circle_id ~intent_id:id
      | _ -> assert false in
    let read ?snapshot name id epoch = V.Contract.run_s (load ?snapshot name id epoch) in
    let routes = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      epoch_read = (fun action params store -> action store params ~current_epoch:!clock);
      program_read = (fun _ _ _ -> failwith "transport test invoked source route");
      public_asset_read = (fun _ _ _ -> failwith "transport test invoked asset route");
      circle_view = (fun _ _ -> failwith "transport test invoked view route");
      circle_view_auth = (fun _ _ -> failwith "transport test invoked signed view route");
    } in
    let query route params = Lwt_main.run ((List.assoc route routes) params store) in
    let cases = [
      "outbox_claim", "OutboxClaim";
      "outbox_intent", "OutboxIntent";
      "outbox_status", "OutboxStatus";
      "ingress_packet", "IngressPacket";
    ] in
    let commit reason = V.Contract.run_s (S.commit_epoch_batch store reason) in
    let capture () = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
    let member name result = Yojson.Safe.Util.member name (Result.get_ok result) in
    let resolution reason actor_id = C.{
      intent_id;
      status = Cancelled;
      reason;
      resolved_epoch = 51L;
      actor_id;
      related_key_id = Some key_id;
    } in
    let stable key value = S.save_circle_stable_entry store circle_id
      (C.make_stable_entry key (C.Inline value)) in
    List.iter (fun (name, alias) ->
      List.iter (fun route ->
        clock := 51;
        V.Contract.run_s (put 50 first);
        V.Contract.run_s (S.commit_epoch_batch store "transport seed");
        let saved = read name intent_id 51 in
        expect "transport saved control failed" (Result.is_ok saved);
        expect "transport saved RPC failed" (query route (signed first name intent_id) = saved);
        expect "transport RPC accepted another operation" (Result.is_error
          (query route (signed first "another" intent_id)));
        expect "transport RPC accepted another signed intent" (Result.is_error
          (query route (signed ~subject:"another" first name intent_id)));
        expect "transport RPC changed intent spelling" (Result.is_error
          (query route (signed first name (String.uppercase_ascii intent_id))));
        List.iter (fun epoch ->
          clock := epoch;
          expect "transport RPC used external clock" (query route (signed first name intent_id) = saved))
          [-1; 999];
        clock := 51;
        V.Contract.run_s (put 80 first);
        expect "transport live control did not change data" (read name intent_id 51 <> saved);
        let snapshot = capture () in
        expect "transport direct snapshot read pending data" (read ~snapshot name intent_id 51 = saved);
        expect "transport RPC read pending root" (query route (signed first name intent_id) = saved);
        S.abort_epoch_batch store;
        V.Contract.run_s (put 80 second);
        expect "transport RPC refused saved owner" (query route (signed first name intent_id) = saved);
        expect "transport RPC accepted pending owner" (Result.is_error
          (query route (signed second name intent_id)));
        V.Contract.run_s (S.commit_epoch_batch store "transport next");
        clock := 81;
        let next = read name intent_id 81 in
        V.Contract.run_s (put 90 first);
        expect "transport RPC reused old owner" (Result.is_error
          (query route (signed first name intent_id)));
        expect "transport RPC missed committed data"
          (query route (signed second name intent_id) = next);
        S.abort_epoch_batch store;
        V.Contract.run_s (put 50 first);
        commit "transport race seed";
        let snapshot = capture () in
        let changed = Read.with_owner_subject_auth ~snapshot store (signed first name intent_id)
          ~field:"intent_id" ~op:("octra_circle_" ^ name) (fun _ id ->
            let open Lwt.Syntax in
            let* () = put 80 second in
            let* () = S.commit_epoch_batch store "transport race commit" in
            load ~snapshot name id 51) in
        expect "transport read crossed commit after owner check" (V.Contract.run_s changed = saved);
        expect "transport race did not commit" (read name intent_id 51 <> saved);
        List.iter (fun public ->
          expect "public transport RPC disclosed data" (Result.is_error
            (query public (`List [`String circle_id; `String intent_id]))))
          ["circle_" ^ name; "octra_circle" ^ alias])
        ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"]) cases;
    V.Contract.run_s (put 50 first);
    commit "transport policy seed";
    let saved = read "outbox_status" intent_id 51 in
    expect "transport quorum control failed" (member "claim_ready" saved = `Bool true);
    expect "transport primary control failed" (member "selected_ingress_relay" saved = `String relay);
    expect "transport lease control failed"
      (member "active_claims" (read "outbox_claim" intent_id 51)
        |> Yojson.Safe.Util.to_list |> List.length |> (=) 2);
    expect "transport expired lease control failed"
      (member "active_claims" (read "outbox_claim" intent_id 70) = `List []);
    expect "transport key expiry control failed" (member "delivery_key_state"
      (read "outbox_status" intent_id 110) = `String "expired");
    List.iter (fun change ->
      V.Contract.run_s (S.begin_epoch_batch store);
      V.Contract.run_s (change ());
      expect "transport policy live control did not change" (read "outbox_status" intent_id 51 <> saved);
      List.iter (fun route ->
        expect "transport RPC mixed policy roots"
          (query route (signed first "outbox_status" intent_id) = saved))
        ["circle_outbox_status_auth"; "octra_circleOutboxStatusAuth"];
      S.abort_epoch_batch store) [
        (fun () -> S.set_circle_outbox_status store circle_id intent_id C.Fulfilled);
        (fun () -> S.save_circle_outbox_resolution store circle_id (resolution C.Owner_cancelled (Some owner)));
        (fun () -> S.save_circle_outbox_claim_resolution store circle_id owner (resolution C.Relay_cancelled (Some owner)));
        (fun () -> stable Policy.quorum_weight_threshold_key "4");
        (fun () -> stable (Key.revoked_key key_id) "true");
      ];
    let snapshot = capture () in
    V.Contract.run_s (put 80 first);
    expect "transport intent default missed batch"
      (V.Contract.run_s (S.get_circle_outbox_intent store circle_id intent_id) = Some (intent 80));
    expect "transport intent snapshot missed root"
      (V.Contract.run_s (S.get_circle_outbox_intent ~snapshot store circle_id intent_id) = Some (intent 50));
    expect "transport packet default missed batch"
      (V.Contract.run_s (S.get_circle_ingress_packet store circle_id intent_id) = Some (packet 80));
    expect "transport packet snapshot missed root"
      (V.Contract.run_s (S.get_circle_ingress_packet ~snapshot store circle_id intent_id) = Some (packet 50));
    V.Contract.run_s (S.save_circle_outbox_resolution store circle_id (resolution C.Owner_cancelled (Some owner)));
    let module State = Octra_core.Circle_transport_state in
    expect "transport resolution default missed batch"
      (V.Contract.run_s (State.get_outbox_resolution store circle_id intent_id) = Some (resolution C.Owner_cancelled (Some owner)));
    expect "transport resolution snapshot missed root"
      (V.Contract.run_s (State.get_outbox_resolution ~snapshot store circle_id intent_id) = None);
    V.Contract.run_s (stable (Key.revoked_key key_id) "true");
    expect "transport key default missed batch" ((V.Contract.run_s
      (State.delivery_key_status store circle_id 51 (intent 80))).state = Octra_core.Circle_key_state.Revoked);
    expect "transport key snapshot missed root" ((V.Contract.run_s
      (State.delivery_key_status ~snapshot store circle_id 51 (intent 50))).state = Octra_core.Circle_key_state.Live);
    S.abort_epoch_batch store;
    List.iter (fun status ->
      V.Contract.run_s (S.begin_epoch_batch store);
      V.Contract.run_s (S.set_circle_outbox_status store circle_id intent_id status);
      V.Contract.run_s (S.save_circle_outbox_resolution store circle_id (resolution C.Owner_cancelled (Some owner)));
      V.Contract.run_s (stable (Key.revoked_key key_id) "true");
      commit "transport terminal state";
      let saved = read "outbox_status" intent_id 51 in
      expect "transport terminal precedence changed" (member "status" saved = `String (C.string_of_outbox_status status));
      List.iter (fun route ->
        expect "transport RPC changed terminal record"
          (query route (signed first "outbox_status" intent_id) = saved))
        ["circle_outbox_status_auth"; "octra_circleOutboxStatusAuth"]) [C.Fulfilled; C.Cancelled];
    List.iter (fun (fields, state) ->
      V.Contract.run_s (put 50 first);
      let data = Hashtbl.create 4 in
      List.iter (fun (key, value) -> Hashtbl.add data key value) fields;
      V.Contract.run_s (S.save_circle_stable_storage store circle_id data) |> ignore;
      commit "transport key policy";
      let saved = read "outbox_status" intent_id 51 in
      expect "transport key option changed state" (member "delivery_key_state" saved = `String state);
      List.iter (fun route ->
        expect "transport RPC changed key option"
          (query route (signed first "outbox_status" intent_id) = saved))
        ["circle_outbox_status_auth"; "octra_circleOutboxStatusAuth"]) [
        [], "live";
        [Key.expire_after_key key_id, "0"], "expired";
        [Key.expire_after_key key_id, "-1"], "expired";
        [Key.erased_key key_id, "true"], "erased";
        [Key.revoked_key key_id, "true"], "revoked";
        [Key.activate_after_key key_id, "52"], "scheduled";
      ];
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.save_circle_outbox_intent store circle_id {(intent 50) with delivery_key_id = None});
    V.Contract.run_s (stable (Key.erased_key key_id) "true");
    commit "transport no delivery key";
    let saved = read "outbox_status" intent_id 51 in
    expect "transport absent key changed state" (member "delivery_key_state" saved = `String "live");
    expect "transport absent key gained identity" (member "delivery_key_id" saved = `Null);
    List.iter (fun route ->
      expect "transport RPC changed absent key"
        (query route (signed first "outbox_status" intent_id) = saved))
      ["circle_outbox_status_auth"; "octra_circleOutboxStatusAuth"];
    V.Contract.run_s (put 50 first);
    let raw = " Parcel | Case/01 " in
    V.Contract.run_s (S.save_circle_outbox_intent store circle_id {(intent 50) with intent_id = raw});
    V.Contract.run_s (S.set_circle_outbox_status store circle_id raw C.Claimed);
    V.Contract.run_s (S.save_circle_outbox_claim store circle_id {(claim 50 owner) with intent_id = raw});
    V.Contract.run_s (S.save_circle_ingress_packet store circle_id {(packet 50) with intent_id = raw});
    commit "transport raw subject";
    List.iter (fun (name, alias) ->
      let saved = read name raw 51 in
      expect "transport raw control failed" (Result.is_ok saved);
      List.iter (fun route ->
        expect "transport RPC changed raw subject" (query route (signed first name raw) = saved);
        let absent = "not a stored intent" in
        let reason = match name with
          | "outbox_claim" -> "circle outbox claim not found"
          | "outbox_intent" -> "circle outbox intent not found"
          | "outbox_status" -> "circle outbox status not found"
          | "ingress_packet" -> "circle ingress packet not found"
          | _ -> assert false in
        expect "transport missing record changed error"
          (query route (signed first name absent) = Error (Octra_core.Rpc.not_found reason));
        expect "transport RPC accepted normalized signature" (Result.is_error
          (query route (signed ~subject:(String.trim raw) first name raw))))
        ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"]) cases;
    List.iter (fun epoch ->
      V.Contract.run_s (S.begin_epoch_batch store);
      V.Contract.run_s (S.set_meta store "last_epoch" epoch);
      commit "transport epoch metadata";
      List.iter (fun (name, alias) ->
        List.iter (fun route ->
          let result = query route (signed first name intent_id) in
          if epoch = Int64.to_string Int64.max_int &&
            (name = "outbox_intent" || name = "ingress_packet") then
            expect "transport record rejected unused epoch size" (Result.is_ok result)
          else
            expect "transport RPC accepted invalid policy epoch" (Result.is_error result))
          ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"]) cases)
      ["invalid"; "-1"; Int64.to_string Int64.max_int];
    V.Contract.run_s (S.begin_epoch_batch store);
    V.Contract.run_s (S.remove_path store ["meta"; "last_epoch"]);
    commit "transport missing epoch";
    List.iter (fun (name, alias) ->
      List.iter (fun route ->
        expect "transport RPC guessed missing epoch" (Result.is_error
          (query route (signed first name intent_id))))
        ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"]) cases)