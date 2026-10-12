(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module C = Octra_core.Circles
module Read = Octra_node_runtime.Circle_read_rpc
module Auth = Octra_node_runtime.Circle_auth
module View = Octra_node_runtime.Circle_view
module Balance = Octra_core.Circle_balance_binding
module Register = Octra_core.Circle_register_binding
module Flow = Octra_core.Circle_balance_workflow
module Update = Octra_core.Circle_register_workflow
module V = Octra_vm

let expect reason value =
  if not value then failwith reason
let circle_id = "oct" ^ String.make 44 '1'

let identity seed =
  let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
    (String.make 32 seed)) in
  let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv secret
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  Octra_core.Crypto.Address.address_from_pubkey public_key, public_key, secret

let run path =
  let first = identity '\054' in
  let second = identity '\055' in
  let subject, _, _ = first in
  let recipient, _, _ = second in
  let register_ref = String.make 64 'a' in
  let workflow_ref = String.make 64 'f' in
  let old_ref = String.make 64 'd' in
  let next_ref = String.make 64 'b' in
  let info = C.{
    circle_id; runtime = Octb; version = 1L; owner = subject;
    code_hash = zero_hash_hex; stable_root = zero_hash_hex;
    assets_root = zero_hash_hex; privacy_class = Scoped;
    browser_mode = Native_sealed; resource_mode = Public_resources;
    policy_hash = None; members_root = None; export_policy = None;
    limits = default_limits;
  } in
  let cases = [
    "balance_binding", "BalanceBinding", "subject_addr", subject;
    "register_binding", "RegisterBinding", "register_ref", register_ref;
    "balance_workflow", "BalanceWorkflow", "workflow_ref", workflow_ref;
    "register_workflow", "RegisterWorkflow", "workflow_ref", workflow_ref;
  ] in
  let values epoch =
    let values = Hashtbl.create 40 in
    let state_ref = if epoch = 50 then old_ref else next_ref in
    let status = Some (string_of_int epoch) in
    Balance.write_snapshot values subject Balance.{
      current_state_ref = Some state_ref; version = Some (Int64.of_int epoch);
      status; last_workflow_ref = Some workflow_ref;
    };
    Register.write_snapshot values register_ref Register.{
      current_state_ref = Some state_ref; version = Some (Int64.of_int epoch);
      status; last_workflow_ref = Some workflow_ref;
    };
    Flow.write_snapshot values workflow_ref {Flow.empty with
      flow_kind = Some "transfer"; debit_subject_addr = Some subject;
      credit_subject_addr = Some recipient; debit_state_ref = Some state_ref;
      credit_state_ref = Some next_ref; status;
      amount_commitment = Some (Base64.encode_exn (String.make 32 (Char.chr epoch)));
      proof_receipt_hash = Some (Printf.sprintf "%064x" epoch);
      intent_id = Some (Printf.sprintf "%064x" (epoch + 1));
    };
    Update.write_snapshot values workflow_ref {Update.empty with
      register_ref = Some register_ref; previous_state_ref = Some old_ref;
      next_state_ref = Some state_ref; workflow_kind = Some "update"; status;
      proof_receipt_hash = Some (Printf.sprintf "%064x" epoch);
      intent_id = Some (Printf.sprintf "%064x" (epoch + 1));
    };
    List.iter (fun state_ref ->
      let locator = View.state_locator state_ref |> Result.get_ok in
      List.iter (fun (key, value) -> Hashtbl.add values key value) [
        Octra_core.Circle_state_descriptor.state_class_key locator.path_key, "cipher_cell";
        Octra_core.Circle_state_descriptor.codec_key locator.path_key, string_of_int epoch;
        Octra_core.Circle_state_descriptor.schema_hash_key locator.path_key, Printf.sprintf "%064x" epoch;
        Octra_core.Circle_state_descriptor.subject_addr_key locator.path_key, subject;
        Octra_core.Circle_state_descriptor.hfhe_profile_key locator.path_key, "ciphertext_v1";
      ]) [old_ref; next_ref];
    values in
  let signed ?subject identity name key =
    let owner, public_key, secret = identity in
    let subject = Option.value subject ~default:key in
    let signature = Auth.message ~op:("octra_circle_" ^ name) ~circle_id
      ~addr:owner ~subject
      |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
    `List (List.map (fun value -> `String value) [circle_id; key; owner; public_key; signature]) in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let put ?data epoch identity =
      let owner, _, _ = identity in
      let open Lwt.Syntax in
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
      let* () = S.deploy_circle store {info with owner} in
      let data = Option.value data ~default:(values epoch) in
      let* _ = S.save_circle_stable_storage store circle_id data in
      Lwt.return_unit in
    let load ?snapshot name key =
      match name with
      | "balance_binding" ->
        Read.binding ?snapshot store
          ~circle_id
          ~binding_field:name
          ~binding_key_field:"subject_addr"
          ~binding_key_value:key
          ~empty_binding:Balance.empty
          ~load_binding:Octra_core.Circle_balance_binding_store.load_opt
          ~binding_json:Balance.yojson_of_t
          ~current_state_ref:(fun (value : Balance.t) -> value.current_state_ref)
      | "register_binding" ->
        Read.binding ?snapshot store
          ~circle_id
          ~binding_field:name
          ~binding_key_field:"register_ref"
          ~binding_key_value:key
          ~empty_binding:Register.empty
          ~load_binding:Octra_core.Circle_register_binding_store.load_opt
          ~binding_json:Register.yojson_of_t
          ~current_state_ref:(fun (value : Register.t) -> value.current_state_ref)
      | "balance_workflow" ->
        Read.workflow ?snapshot store
          ~circle_id
          ~workflow_field:name
          ~workflow_ref:key
          ~empty_workflow:Flow.empty
          ~load_workflow:Octra_core.Circle_balance_workflow_store.load_opt
          ~workflow_json:Flow.yojson_of_t
      | "register_workflow" ->
        Read.workflow ?snapshot store
          ~circle_id
          ~workflow_field:name
          ~workflow_ref:key
          ~empty_workflow:Update.empty
          ~load_workflow:Octra_core.Circle_register_workflow_store.load_opt
          ~workflow_json:Update.yojson_of_t
      | _ -> assert false in
    let read ?snapshot name key =
      V.Contract.run_s (load ?snapshot name key) in
    let direct name key =
      let module Exec = Octra_circle_runtime.Circle_exec in
      match name with
      | "balance_binding" ->
        V.Contract.run_s (Exec.read_balance_binding store circle_id key)
        |> Balance.yojson_of_t
      | "register_binding" ->
        V.Contract.run_s (Exec.read_register_binding store circle_id key)
        |> Register.yojson_of_t
      | "balance_workflow" ->
        V.Contract.run_s (Exec.read_balance_workflow store circle_id key)
        |> Flow.yojson_of_t
      | "register_workflow" ->
        V.Contract.run_s (Exec.read_register_workflow store circle_id key)
        |> Update.yojson_of_t
      | _ -> assert false in
    let routes = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      epoch_read = (fun _ _ _ -> failwith "binding test invoked epoch route");
      program_read = (fun _ _ _ -> failwith "binding test invoked source route");
      public_asset_read = (fun _ _ _ -> failwith "binding test invoked asset route");
      circle_view = (fun _ _ -> failwith "binding test invoked view route");
      circle_view_auth = (fun _ _ -> failwith "binding test invoked signed view route");
    } in
    let query route params = Lwt_main.run ((List.assoc route routes) params store) in
    List.iter (fun (name, alias, field, key) ->
      List.iter (fun route ->
        V.Contract.run_s (put 50 first);
        V.Contract.run_s (S.commit_epoch_batch store "binding seed");
        let saved = read name key in
        expect "binding saved read failed" (Result.is_ok saved);
        expect "binding saved RPC failed" (query route (signed first name key) = saved);
        expect "binding RPC accepted another operation" (Result.is_error
          (query route (signed first "another" key)));
        expect "binding RPC accepted another signed key" (Result.is_error
          (query route (signed ~subject:"another" first name key)));
        let spelling = " " ^ String.uppercase_ascii key ^ " " in
        if field = "subject_addr" then
          expect "binding RPC changed address spelling" (Result.is_error
            (query route (signed first name (" " ^ key ^ " "))))
        else begin
          expect "binding RPC changed reference normalization"
            (query route (signed ~subject:key first name spelling) = saved);
          expect "binding RPC accepted raw reference signature" (Result.is_error
            (query route (signed first name spelling)))
        end;
        V.Contract.run_s (put 80 first);
        expect "binding live control did not change data" (read name key <> saved);
        expect "binding direct batch reader changed default"
          (direct name key = Yojson.Safe.Util.member name (Result.get_ok (read name key)));
        expect "binding RPC read pending root" (query route (signed first name key) = saved);
        let snapshot = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
        expect "binding direct snapshot read pending data" (read ~snapshot name key = saved);
        S.abort_epoch_batch store;
        V.Contract.run_s (put 80 second);
        expect "binding RPC refused saved owner" (query route (signed first name key) = saved);
        expect "binding RPC accepted pending owner" (Result.is_error (query route (signed second name key)));
        V.Contract.run_s (S.commit_epoch_batch store "binding next");
        let next = read name key in
        V.Contract.run_s (put 90 first);
        expect "binding RPC reused old owner" (Result.is_error (query route (signed first name key)));
        expect "binding RPC missed committed data" (query route (signed second name key) = next);
        S.abort_epoch_batch store;
        V.Contract.run_s (put 50 first);
        V.Contract.run_s (S.commit_epoch_batch store "binding race seed");
        let snapshot = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
        let changed =
          Read.with_owner_validated_subject_auth ~snapshot store (signed first name key)
            ~field ~op:("octra_circle_" ^ name)
            ~validate:(if field = "subject_addr" then (fun value -> Ok value)
              else Octra_core.Circle_private_common.normalize_hex64 field)
            (fun _ subject ->
              let open Lwt.Syntax in
              let* () = put 80 second in
              let* () = S.commit_epoch_batch store "binding race commit" in
              load ~snapshot name subject) in
        expect "binding read crossed commit after owner check"
          (V.Contract.run_s changed = saved);
        expect "binding race did not commit" (read name key <> saved);
        List.iter (fun public ->
          expect "public binding RPC disclosed data" (Result.is_error
            (query public (`List [`String circle_id; `String key]))))
          ["circle_" ^ name; "octra_circle" ^ alias])
        ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"]) cases;
    List.iter (fun (name, alias, _, key) ->
      V.Contract.run_s (put 50 first);
      let _ = V.Contract.run_s
        (S.save_circle_stable_storage store circle_id (Hashtbl.create 0)) in
      V.Contract.run_s (S.commit_epoch_batch store "binding empty");
      let empty = read name key in
      let body = Result.get_ok empty in
      expect "binding absent record became materialized"
        (Yojson.Safe.Util.member "materialized" body = `Bool false);
      expect "binding direct reader changed empty record"
        (direct name key = Yojson.Safe.Util.member name body);
      List.iter (fun route ->
        expect "binding RPC changed empty response" (query route (signed first name key) = empty))
        ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"];
      V.Contract.run_s (put 80 first);
      expect "binding RPC exposed pending record after absent record"
        (query ("circle_" ^ name ^ "_auth") (signed first name key) = empty);
      S.abort_epoch_batch store) cases;
    let partial = Hashtbl.create 4 in
    List.iter (fun (key, value) -> Hashtbl.add partial key value) [
      Balance.status_key subject, "ready";
      Register.status_key register_ref, "ready";
      Flow.status_key workflow_ref, "ready";
      Update.status_key workflow_ref, "ready";
    ];
    V.Contract.run_s (put ~data:partial 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "binding partial");
    List.iter (fun (name, _, _, key) ->
      let saved = read name key in
      let body = Result.get_ok saved in
      expect "binding RPC lost partial record"
        (Yojson.Safe.Util.member "materialized" body = `Bool true);
      if name = "balance_binding" || name = "register_binding" then
        expect "binding RPC invented linked descriptor"
          (Yojson.Safe.Util.member "descriptor_materialized" body = `Bool false);
      expect "binding RPC changed partial response"
        (query ("circle_" ^ name ^ "_auth") (signed first name key) = saved)) cases;
    V.Contract.run_s (S.set_meta store "last_epoch" (Int64.to_string Int64.max_int));
    List.iter (fun (name, _, _, key) ->
      expect "binding RPC limited unused epoch conversion"
        (query ("circle_" ^ name ^ "_auth") (signed first name key) = read name key)) cases;
    V.Contract.run_s (S.set_meta store "last_epoch" "not an epoch");
    List.iter (fun (name, _, _, key) ->
      expect "binding RPC ignored missing committed epoch" (Result.is_error
        (query ("circle_" ^ name ^ "_auth") (signed first name key)))) cases)