(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_core.Store_irmin
module L = Octra_core.Ledger
module V = Octra_vm

let expect reason value = if not value then failwith reason
let parent = "oct" ^ String.make 44 '1'
let child = "oct" ^ String.make 44 '2'
let absent = "oct" ^ String.make 44 '3'

let profile ~epoch = Ok V.Contract_rpc.{
  epoch; point_ops = true; math = false; object_cost = false;
  int_work = V.Int_work.Active; fhe_work = Octra_core.Rule_graph.Prior;
  proof_exec = Octra_core.Rule_graph.Prior; wasm_float = Octra_core.Rule_graph.Prior;
}

let compile source =
  let result = V.Oct_compile.compile source in
  match result.error with
  | None -> result.bytecode
  | Some error -> failwith error

let deploy store address code =
  S.deploy_contract store ~address ~owner:parent ~ctype:"CUSTOM" ~version:"1"
    ~admission:"binary" ~code_hash:Digestif.SHA256.(digest_string code |> to_hex)
    ~bytecode_b64:(Base64.encode_exn code)

let put store mode epoch amount count key main nested =
  let open Lwt.Syntax in
  let* () = S.begin_epoch_batch ~mode store in
  let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
  let* () = S.set_account store child
    {Octra_core.Ledger_types.empty_account with balance = Z.of_int amount} in
  let* () = S.set_pvac_pubkey store child key in
  let* () = deploy store parent main in
  let* () = deploy store child nested in
  S.write store ["contracts"; parent; "storage"; "count"] (string_of_int count)

let field name = function
  | Ok json -> Yojson.Safe.Util.member name json
  | Error error -> failwith error.Octra_core.Rpc.message

let run mode path =
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let main = compile {|
program Read {
  state { count: int }
  public view fn read(target: address): int {
    return self.count + balance(target) + to_int(call(target, "read")) + epoch
  }
}
|} in
    let code value = compile (Printf.sprintf {|
program Value {
  public view fn read(): int {
    return %d
  }
}
|} value) in
    let seed = put store mode 50 100 10 "key-before" main (code 7) in
    Lwt_main.run (Lwt.bind seed (fun () -> S.commit_epoch_batch store "seed"));
    let snapshot = Lwt_main.run (S.capture_read_snapshot store) |> Result.get_ok in
    let ledger = L.create store in
    L.add_account ledger child (Z.of_int 777) |> Result.get_ok;
    let query profile args = V.Contract_rpc.call ~trusted:[] ~profile ~store ~ledger
      ~get_fhe_pubkey:(fun _ -> failwith "live key loader reached")
      ~storage_json:(fun values -> `Assoc (List.map (fun (key, value) -> key, `String value) values))
      ~addr:parent ~method_name:"read" ~call_params:args ~caller_addr:parent ~include_storage:true in
    let main_code value = compile (Printf.sprintf {|
program Value {
  public view fn read(target: address): int {
    return %d
  }
}
|} value) in
    let next = main_code 999 in
    let pending = main_code 777 in
    let change ~epoch =
      expect "view profile used live epoch" (epoch = 51);
      V.Contract.run_s (put store mode 70 1000 100 "key-after" next (code 70));
      V.Contract.run_s (S.commit_epoch_batch store "next");
      V.Contract.run_s (put store mode 90 9999 999 "key-pending" pending (code 700));
      profile ~epoch in
    let answer = Lwt_main.run (query change [`String child]) in
    expect "view mixed committed roots" (field "result" answer = `String "168");
    expect "view storage page used another root"
      (field "storage" answer = `Assoc ["count", `String "10"]);
    let current = Lwt_main.run (S.get_commit_hash store) in
    let next_profile ~epoch =
      expect "view profile used pending epoch" (epoch = 71);
      profile ~epoch in
    let answer = Lwt_main.run (query next_profile [`String child]) in
    expect "view ignored new commit" (field "result" answer = `String "999");
    expect "view exposed pending storage"
      (field "storage" answer = `Assoc ["count", `String "100"]);
    expect "view changed committed state" (Lwt_main.run (S.get_commit_hash store) = current);
    expect "view changed pending state"
      (Lwt_main.run (S.read store ["contracts"; parent; "storage"; "count"]) = Some "999");
    let ctx = V.Contract_rpc.make_view_ctx ~snapshot ~trusted:[]
      ~profile:(Result.get_ok (profile ~epoch:51)) ~store ~ledger
      ~get_fhe_pubkey:(fun _ -> failwith "live key loader reached") () in
    expect "view context lost root" (ctx.tree_hash = snapshot.state_root);
    expect "view balance used live ledger" (ctx.get_balance child = Z.of_int 100);
    expect "view key used live registry"
      (ctx.get_fhe_pubkey child = Some (V.Contract_vm.Key_bytes "key-before"));
    expect "missing snapshot key changed" (ctx.get_fhe_pubkey absent = None);
    S.ensure_pvac_dir store;
    let file = S.pvac_legacy_path store absent in
    let out = open_out_bin file in
    Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out "unrooted-key");
    let refused = try ignore (ctx.get_fhe_pubkey absent); false with
      | Octra_core.Exec_resource.Unavailable Host -> true in
    expect "view accepted unrooted key" refused;
    let file = S.pvac_blob_path store (S.pvac_hash "key-before") in
    Unix.unlink file;
    let refused = try ignore (ctx.get_fhe_pubkey child); false with
      | Octra_core.Exec_resource.Unavailable Host -> true in
    expect "view replaced missing key with live key" refused;
    let denied = query (fun ~epoch:_ -> Error (Octra_core.Rpc.err (-32005) "profile unavailable" None)) [`String child]
      |> Lwt_main.run in
    expect "view ignored profile refusal" (Result.is_error denied);
    expect "view retained slot" (not !(V.Contract_rpc.view_active));
    S.abort_epoch_batch store)

let fhe_view path =
  let native, secret = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\003') in
  let key = Pvac_ffi.serialize_pubkey native |> Bytes.to_string in
  let encrypted value seed =
    Pvac_ffi.enc_value_seeded native secret value (Bytes.make 32 seed)
    |> Pvac_ffi.serialize_cipher |> Bytes.to_string |> Base64.encode_exn in
  let first = encrypted 7L '\004' in
  let second = encrypted 11L '\005' in
  let code = compile {|
program Predict {
  public view fn read(key: string, first: string, second: string): string {
    let pk = fhe_load_pk(key)
    let a = fhe_scale(pk, fhe_deser(first), 2)
    let b = fhe_scale(pk, fhe_deser(second), 3)
    let dot = fhe_add_const(pk, fhe_add(pk, a, b), 5)
    return fhe_ser(fhe_add_const(pk, fhe_scale(pk, dot, 2), 1))
  }
  public view fn nested(target: address, first: string, second: string): string {
    return to_string(call(target, "read", target, first, second))
  }
  public view fn twice(target: address, first: string, second: string): string {
    let one = call(target, "read", target, first, second)
    return to_string(call(target, "read", target, first, second))
  }
}
|} in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    List.iter (fun epoch ->
      Lwt_main.run (let open Lwt.Syntax in
        let* () = put store Octra_core.Rule_graph.Active epoch 0 0 key code code in
        S.commit_epoch_batch store "predict");
      let root = Lwt_main.run (S.get_commit_hash store) in
      let select ~epoch =
        let value = Result.get_ok (profile ~epoch) in
        Ok {value with fhe_work = Octra_core.Rule_graph.Active;
          proof_exec = if epoch >= 1_663_000 then Active else Prior} in
      let query method_name = Lwt_main.run (V.Contract_rpc.call ~trusted:[] ~profile:select ~store
        ~ledger:(L.create store) ~get_fhe_pubkey:(fun _ -> failwith "live key read")
        ~storage_json:(fun _ -> `Null) ~addr:parent ~method_name
        ~call_params:[`String child; `String first; `String second]
        ~caller_addr:parent ~include_storage:false) in
      let answer = query "read" in
      begin match answer with
      | Error error -> failwith ("standard key view refused: " ^ error.Octra_core.Rpc.message)
      | Ok _ -> ()
      end;
      let encoded = field "result" answer |> Yojson.Safe.Util.to_string in
      let cipher = Base64.decode_exn encoded |> Bytes.of_string |> Pvac_ffi.deserialize_cipher in
      expect "private prediction differs" (Pvac_ffi.dec_value native secret cipher = 105L);
      expect "nested prediction differs" (field "result" (query "nested") = field "result" answer);
      expect "nested call reset effort" (Result.is_error (query "twice"));
      expect "private view changed state" (Lwt_main.run (S.get_commit_hash store) = root);
      expect "private view retained slot" (not !(V.Contract_rpc.view_active)))
      [1_662_998; 1_662_999])

let storage_root path =
  let module C = Octra_core.Circles in
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let identity seed =
    let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
      (String.make 32 seed)) in
    let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv secret
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
    Octra_core.Crypto.Address.address_from_pubkey public_key, public_key, secret in
  let first = identity '\046' in
  let second = identity '\047' in
  let owner, _, _ = first in
  let info = C.{
    circle_id = parent; runtime = Octb; version = 1L; owner;
    code_hash = zero_hash_hex; stable_root = zero_hash_hex;
    assets_root = zero_hash_hex; privacy_class = Scoped;
    browser_mode = Native_sealed; resource_mode = Public_resources;
    policy_hash = None; members_root = None; export_policy = None;
    limits = default_limits;
  } in
  let signed identity op key =
    let owner, public_key, secret = identity in
    let subject = Option.value ~default:"" key in
    let signature = Auth.message ~op ~circle_id:parent ~addr:owner ~subject
      |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
    `List ([`String parent] @ Option.to_list (Option.map (fun value -> `String value) key)
      @ [`String owner; `String public_key; `String signature]) in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let put epoch identity count value =
      let owner, _, _ = identity in
      let open Lwt.Syntax in
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
      let* () = S.deploy_circle store {info with owner} in
      let values = Hashtbl.create count in
      List.init count (fun key -> if key = 0 then "count" else string_of_int key)
      |> List.iter (fun key -> Hashtbl.add values key value);
      let* _ = S.save_circle_stable_storage store parent values in
      Lwt.return_unit in
    let routes = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      program_read = (fun _ _ _ -> failwith "storage test invoked source route");
      epoch_read = (fun _ _ _ -> failwith "storage test invoked epoch route");
      public_asset_read = (fun _ _ _ -> failwith "storage test invoked asset route");
      circle_view = (fun _ _ -> failwith "storage test invoked view route");
      circle_view_auth = (fun _ _ -> failwith "storage test invoked signed view route");
    } in
    let cases = [
      "circle_storage_auth", false;
      "octra_circleStorageAuth", false;
      "circle_storage_dump_auth", true;
      "octra_circleStorageDumpAuth", true;
    ] in
    let query identity route page =
      let op = if page then "octra_circle_storage_dump" else "octra_circle_storage" in
      let key = if page then None else Some "count" in
      Lwt_main.run ((List.assoc route routes) (signed identity op key) store) in
    let check answer page count value =
      if page then begin
        let pairs = field "storage" answer |> Yojson.Safe.Util.to_assoc in
        expect "storage page exceeds cap" (List.length pairs = min count 128);
        expect "storage page changed total" (field "total" answer = `Int count);
        expect "storage page changed count" (field "count" answer = `Int (min count 128));
        expect "storage page changed continuation" (field "has_more" answer = `Bool (count > 128));
        expect "storage page read another root"
          (List.for_all (fun (_, read) -> read = `String value) pairs)
      end else
        expect "storage key read pending root" (field "value" answer = `String value) in
    List.iter (fun (route, page) ->
      V.Contract.run_s (put 50 first 193 "10");
      V.Contract.run_s (S.commit_epoch_batch store "storage seed");
      let root = V.Contract.run_s (S.get_commit_hash store) in
      V.Contract.run_s (put 80 first 257 "999");
      check (query first route page) page 193 "10";
      expect "storage RPC changed head" (V.Contract.run_s (S.get_commit_hash store) = root);
      expect "storage RPC changed pending key"
        (V.Contract.run_s (S.read_circle_stable_inline_value store parent "count") = Some "999");
      S.abort_epoch_batch store;
      V.Contract.run_s (put 80 second 257 "1000");
      check (query first route page) page 193 "10";
      expect "storage RPC accepted pending owner" (Result.is_error (query second route page));
      V.Contract.run_s (S.commit_epoch_batch store "storage next");
      V.Contract.run_s (put 90 first 193 "777");
      expect "storage RPC reused old owner" (Result.is_error (query first route page));
      check (query second route page) page 257 "1000";
      S.abort_epoch_batch store;
      expect "storage RPC accepted malformed auth"
        (Result.is_error (Lwt_main.run ((List.assoc route routes) (`List [`String parent]) store)))) cases;
    List.iter (fun page ->
      V.Contract.run_s (put 80 second 257 "1000");
      V.Contract.run_s (S.commit_epoch_batch store "storage saved");
      let snapshot = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
      let change () =
        let open Lwt.Syntax in
        let* () = put 100 first 193 "777" in
        let* () = S.commit_epoch_batch store "storage moved" in
        put 110 second 257 "9999" in
      let answer = if page then
        Read.with_owner_scope_auth ~snapshot store
          (signed second "octra_circle_storage_dump" None)
          ~op:"octra_circle_storage_dump" (fun circle_id ->
            Lwt.bind (change ()) (fun () -> Read.storage_dump ~snapshot store ~circle_id))
        else
          Read.with_owner_subject_auth ~snapshot store
            (signed second "octra_circle_storage" (Some "count"))
            ~field:"key" ~op:"octra_circle_storage" (fun circle_id key ->
              Lwt.bind (change ()) (fun () -> Read.storage ~snapshot store ~circle_id ~key)) in
      check (Lwt_main.run answer) page 257 "1000";
      expect "storage overlap lost pending value"
        (V.Contract.run_s (S.read_circle_stable_inline_value store parent "count") = Some "9999");
      S.abort_epoch_batch store;
      let route = if page then "circle_storage_dump_auth" else "circle_storage_auth" in
      expect "storage overlap reused owner" (Result.is_error (query second route page));
      check (query first route page) page 193 "777") [false; true];
    V.Contract.run_s (put 80 second 257 "1000");
    V.Contract.run_s (S.commit_epoch_batch store "storage restored");
    let query_key key = Lwt_main.run (Read.storage_auth store
      (signed second "octra_circle_storage" (Some key))) in
    expect "storage missing key changed" (field "value" (query_key "absent") = `Null);
    let other_op = signed second "octra_circle_storage_dump" (Some "count") in
    expect "storage RPC accepted another operation"
      (Result.is_error (Lwt_main.run (Read.storage_auth store other_op)));
    let other_key = match signed second "octra_circle_storage" (Some "count") with
      | `List (circle :: _ :: rest) -> `List (circle :: `String "absent" :: rest)
      | _ -> assert false in
    expect "storage RPC accepted another key"
      (Result.is_error (Lwt_main.run (Read.storage_auth store other_key)));
    V.Contract.run_s (put 100 second 0 "empty");
    V.Contract.run_s (S.commit_epoch_batch store "storage empty");
    check (query second "circle_storage_dump_auth" true) true 0 "empty";
    expect "empty storage key changed" (field "value" (query_key "count") = `Null);
    V.Contract.run_s (put 110 second 0 "empty");
    V.Contract.run_s (S.save_circle_stable_entry store parent
      (C.make_stable_entry "blob" (C.Blob_ref {blob_hash = C.zero_hash_hex; value_size = 1L})));
    V.Contract.run_s (S.commit_epoch_batch store "storage blob");
    expect "storage dump accepted non-inline value"
      (Result.is_error (query second "circle_storage_dump_auth" true));
    expect "storage blob became inline" (field "value" (query_key "blob") = `Null);
    List.iter (fun route ->
      expect "storage public route disclosed data"
        (Result.is_error (Lwt_main.run ((List.assoc route routes)
          (`List [`String parent; `String "count"]) store))))
      ["circle_storage"; "octra_circleStorage"; "circle_storage_dump"; "octra_circleStorageDump"];
    V.Contract.run_s (S.remove_path store ["meta"; "last_epoch"]);
    List.iter (fun (route, page) ->
      expect "storage RPC accepted missing epoch" (Result.is_error (query second route page))) cases)

let object_root path =
  let module C = Octra_core.Circles in
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let module View = Octra_node_runtime.Circle_view in
  let module Query = Octra_core.Circle_object_query in
  let module Detail = Octra_core.Circle_object_detail_query in
  let module Find = Octra_core.Circle_object_discovery_query in
  let identity seed =
    let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
      (String.make 32 seed)) in
    let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv secret
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
    Octra_core.Crypto.Address.address_from_pubkey public_key, public_key, secret in
  let first = identity '\048' in
  let second = identity '\049' in
  let owner, _, _ = first in
  let object_ref = String.make 64 'a' in
  let state_ref = String.make 64 'd' in
  let transition_ref = String.make 64 'e' in
  let info = C.{
    circle_id = parent; runtime = Octb; version = 1L; owner;
    code_hash = zero_hash_hex; stable_root = zero_hash_hex;
    assets_root = zero_hash_hex; privacy_class = Scoped;
    browser_mode = Native_sealed; resource_mode = Public_resources;
    policy_hash = None; members_root = None; export_policy = None;
    limits = default_limits;
  } in
  let values epoch =
    let values = Hashtbl.create 20 in
    let status = string_of_int epoch in
    Octra_core.Circle_object_binding.write_snapshot values object_ref
      Octra_core.Circle_object_binding.{
        current_state_ref = Some state_ref; version = Some (Int64.of_int epoch);
        status = Some status; last_transition_ref = Some transition_ref};
    Octra_core.Circle_object_member.write_snapshot values object_ref "first"
      {Octra_core.Circle_object_member.empty with
        state_ref = Some state_ref; member_kind = Some "register"; status = Some status};
    let _, _, key = C.path_key_of_state_ref state_ref |> Result.get_ok in
    Hashtbl.add values (Octra_core.Circle_state_descriptor.codec_key key) status;
    Octra_core.Circle_object_transition.write_snapshot values transition_ref
      {Octra_core.Circle_object_transition.empty with
        object_ref = Some object_ref; next_state_ref = Some state_ref; status = Some status};
    if epoch > 50 then begin
      Octra_core.Circle_object_binding.write_snapshot values (String.make 64 'b')
        {Octra_core.Circle_object_binding.empty with version = Some (Int64.of_int epoch)};
      Octra_core.Circle_object_member.write_snapshot values object_ref "second"
        {Octra_core.Circle_object_member.empty with status = Some status}
    end;
    values in
  let cases = [
    "summary", "Summary";
    "members", "Members";
    "detail", "Detail";
    "member", "Member";
    "refs", "Refs";
    "list", "List";
  ] in
  let args name = match name with
    | "refs" | "list" -> []
    | "member" -> [object_ref; "first"]
    | _ -> [object_ref] in
  let signed identity name keys =
    let owner, public_key, secret = identity in
    let op = "octra_circle_object_" ^ name in
    let subject = String.concat "|" keys in
    let signature = Auth.message ~op ~circle_id:parent ~addr:owner ~subject
      |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
    `List ([`String parent] @ List.map (fun key -> `String key) keys
      @ [`String owner; `String public_key; `String signature]) in
  let expected name epoch =
    let values = values epoch in
    match name with
    | "summary" -> View.object_summary ~circle_id:parent
        (Query.load_summary_from_storage_tbl values object_ref)
    | "members" -> View.object_members ~circle_id:parent ~object_ref
        (Query.load_member_summaries_from_storage_tbl values object_ref)
    | "detail" -> View.object_detail ~circle_id:parent
        (Detail.load_detail_from_storage_tbl values object_ref)
    | "member" -> View.object_member_detail ~circle_id:parent ~object_ref
        (Detail.load_member_detail_from_storage_tbl values object_ref "first")
    | "refs" -> View.object_refs ~circle_id:parent
        (Find.object_refs_from_storage_tbl values)
    | "list" -> View.object_list ~circle_id:parent
        (Find.load_summaries_from_storage_tbl values)
    | _ -> assert false in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let put epoch identity =
      let owner, _, _ = identity in
      let open Lwt.Syntax in
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
      let* () = S.deploy_circle store {info with owner} in
      let* _ = S.save_circle_stable_storage store parent (values epoch) in
      Lwt.return_unit in
    let routes = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      program_read = (fun _ _ _ -> failwith "object test invoked source route");
      epoch_read = (fun _ _ _ -> failwith "object test invoked epoch route");
      public_asset_read = (fun _ _ _ -> failwith "object test invoked asset route");
      circle_view = (fun _ _ -> failwith "object test invoked view route");
      circle_view_auth = (fun _ _ -> failwith "object test invoked signed view route");
    } in
    let query route params = Lwt_main.run ((List.assoc route routes) params store) in
    let read ?snapshot name circle_id object_ref member_ref =
      match name with
      | "summary" -> Read.object_read ?snapshot store ~circle_id ~object_ref
          ~load:Query.load_summary
          ~view:(fun _ summary -> View.object_summary ~circle_id summary)
      | "members" -> Read.object_read ?snapshot store ~circle_id ~object_ref
          ~load:Query.load_member_summaries
          ~view:(fun object_ref members -> View.object_members ~circle_id ~object_ref members)
      | "detail" -> Read.object_read ?snapshot store ~circle_id ~object_ref
          ~load:Detail.load_detail
          ~view:(fun _ detail -> View.object_detail ~circle_id detail)
      | "member" -> Lwt.map (fun result ->
          Ok (View.object_member_detail ~circle_id ~object_ref (Result.get_ok result)))
          (Detail.load_member_detail ?snapshot store circle_id object_ref member_ref)
      | "refs" -> Read.object_discovery ?snapshot store ~circle_id
          ~load:Find.load_refs ~view:(View.object_refs ~circle_id)
      | "list" -> Read.object_discovery ?snapshot store ~circle_id
          ~load:Find.load_summaries ~view:(View.object_list ~circle_id)
      | _ -> assert false in
    List.iter (fun (name, camel) ->
      expect "object test lacks changed data" (expected name 50 <> expected name 80);
      List.iter (fun route ->
        let params identity = signed identity name (args name) in
        V.Contract.run_s (put 50 first);
        V.Contract.run_s (S.commit_epoch_batch store "object seed");
        expect "object RPC changed saved reply" (query route (params first) = Ok (expected name 50));
        let normalized = match params first with
          | `List fields -> `List (List.mapi (fun at field ->
              match at, field with
              | 1, `String key when args name <> [] -> `String (" " ^ String.uppercase_ascii key ^ " ")
              | 2, `String key when name = "member" -> `String (" " ^ key ^ " ")
              | _ -> field) fields)
          | _ -> assert false in
        expect "object RPC changed normalized subject" (query route normalized = Ok (expected name 50));
        let root = V.Contract.run_s (S.get_commit_hash store) in
        V.Contract.run_s (put 80 first);
        expect "object RPC read pending root" (query route (params first) = Ok (expected name 50));
        expect "object RPC changed head" (V.Contract.run_s (S.get_commit_hash store) = root);
        S.abort_epoch_batch store;
        V.Contract.run_s (put 80 second);
        expect "object RPC refused saved owner" (query route (params first) = Ok (expected name 50));
        expect "object RPC accepted pending owner" (Result.is_error (query route (params second)));
        V.Contract.run_s (S.commit_epoch_batch store "object next");
        V.Contract.run_s (put 90 first);
        expect "object RPC reused old owner" (Result.is_error (query route (params first)));
        expect "object RPC reused old data" (query route (params second) = Ok (expected name 80));
        S.abort_epoch_batch store;
        expect "object RPC accepted malformed auth"
          (Result.is_error (query route (`List [`String parent])));
        expect "object RPC accepted another operation"
          (Result.is_error (query route (signed second "storage" (args name))));
        List.iteri (fun index _ ->
          let changed = match params second with
            | `List fields -> `List (List.mapi (fun at field ->
                if at = index + 1 then `String (if index = 0 then String.make 64 'c' else "second")
                else field) fields)
            | _ -> assert false in
          expect "object RPC accepted another subject" (Result.is_error (query route changed))) (args name))
        ["circle_object_" ^ name ^ "_auth"; "octra_circleObject" ^ camel ^ "Auth"];
      List.iter (fun route ->
        expect "object public route disclosed data"
          (Result.is_error (query route (`List (`String parent :: List.map (fun key -> `String key) (args name))))))
        ["circle_object_" ^ name; "octra_circleObject" ^ camel];
      V.Contract.run_s (put 80 second);
      V.Contract.run_s (S.commit_epoch_batch store "object saved");
      let snapshot = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
      let after circle_id object_ref member_ref =
        let open Lwt.Syntax in
        let* () = put 100 first in
        let* () = S.commit_epoch_batch store "object moved" in
        let* () = put 110 second in
        read ~snapshot name circle_id object_ref member_ref in
      let params = signed second name (args name) in
      let op = "octra_circle_object_" ^ name in
      let answer = match name with
        | "refs" | "list" -> Read.with_owner_scope_auth ~snapshot store params ~op
            (fun circle_id -> after circle_id object_ref "first")
        | "member" -> Read.with_object_member_auth ~snapshot store params after
        | _ -> Read.with_owner_validated_subject_auth ~snapshot store params
            ~field:"object_ref" ~op
            ~validate:(Octra_core.Circle_private_common.normalize_hex64 "object_ref")
            (fun circle_id object_ref -> after circle_id object_ref "first") in
      expect "object overlap mixed roots" (Lwt_main.run answer = Ok (expected name 80));
      expect "object read changed live default"
        (Lwt_main.run (read name parent object_ref "first") = Ok (expected name 110));
      let details = Lwt_main.run (Detail.load_member_details store parent object_ref) |> Result.get_ok in
      expect "object member list changed live default"
        (details = Detail.load_member_details_from_storage_tbl (values 110) object_ref);
      let details = Lwt_main.run (Detail.load_member_details ~snapshot store parent object_ref) |> Result.get_ok in
      expect "object member list mixed roots"
        (details = Detail.load_member_details_from_storage_tbl (values 80) object_ref);
      S.abort_epoch_batch store;
      let route = "circle_object_" ^ name ^ "_auth" in
      expect "object overlap reused owner" (Result.is_error (query route params));
      expect "object overlap lost new data"
        (query route (signed first name (args name)) = Ok (expected name 100))) cases;
    let malformed = ["bad"; String.make 64 'g'] in
    List.iter (fun name ->
      List.iter (fun key ->
        expect "object RPC accepted malformed reference"
          (Result.is_error (query ("circle_object_" ^ name ^ "_auth")
            (signed first name (if name = "member" then [key; "first"] else [key]))))) malformed)
      ["summary"; "members"; "detail"; "member"];
    List.iter (fun member_ref ->
      expect "object RPC accepted malformed member"
        (Result.is_error (query "circle_object_member_auth"
          (signed first "member" [object_ref; member_ref])))) [""; "first:second"];
    V.Contract.run_s (put 120 second);
    let _ = V.Contract.run_s (S.save_circle_stable_storage store parent (Hashtbl.create 0)) in
    V.Contract.run_s (S.commit_epoch_batch store "object empty");
    expect "empty object list changed"
      (field "object_count" (query "circle_object_list_auth" (signed second "list" [])) = `Int 0);
    expect "empty object refs changed"
      (field "object_refs" (query "circle_object_refs_auth" (signed second "refs" [])) = `List []);
    expect "missing object binding changed"
      (field "binding_materialized" (query "circle_object_summary_auth"
        (signed second "summary" [object_ref])) = `Bool false);
    expect "missing object member changed"
      (field "materialized" (query "circle_object_member_auth"
        (signed second "member" [object_ref; "first"])) = `Bool false);
    V.Contract.run_s (S.remove_path store ["meta"; "last_epoch"]);
    List.iter (fun (name, _) ->
      expect "object RPC accepted missing epoch"
        (Result.is_error (query ("circle_object_" ^ name ^ "_auth")
          (signed second name (args name))))) cases)

let policy_root path =
  let module C = Octra_core.Circles in
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let module View = Octra_node_runtime.Circle_view in
  let module Key = Octra_core.Circle_key_policy in
  let identity seed =
    let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
      (String.make 32 seed)) in
    let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv secret
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
    Octra_core.Crypto.Address.address_from_pubkey public_key, public_key, secret in
  let first = identity '\050' in
  let second = identity '\051' in
  let owner, _, _ = first in
  let key_id = String.make 64 'f' in
  let slot_ref = String.make 64 'a' in
  let state_ref = String.make 64 'd' in
  let slot = View.slot_locator slot_ref |> Result.get_ok in
  let state = View.state_locator state_ref |> Result.get_ok in
  let info = C.{
    circle_id = parent; runtime = Octb; version = 1L; owner;
    code_hash = zero_hash_hex; stable_root = zero_hash_hex;
    assets_root = zero_hash_hex; privacy_class = Scoped;
    browser_mode = Native_sealed; resource_mode = Public_resources;
    policy_hash = None; members_root = None; export_policy = None;
    limits = default_limits;
  } in
  let values epoch =
    let values = Hashtbl.create 20 in
    Hashtbl.add values Octra_core.Circle_transport_policy.max_response_bytes_key
      (string_of_int epoch);
    Hashtbl.add values Octra_core.Circle_hfhe_policy.pk_allowlist_key
      (Yojson.Safe.to_string (`List [`String (Printf.sprintf "%064x" epoch)]));
    Key.write_snapshot values (Key.put_payload_of_t key_id
      {Key.default with activate_after_epoch = Some (Int64.of_int (epoch + 2));
        expire_after_epoch = Some (Int64.of_int (epoch + 20)); revoked = epoch >= 80});
    let subject, _, _ = if epoch = 50 then first else second in
    List.iter (fun (key, value) -> Hashtbl.add values key value) [
      C.slot_policy_delivery_key_key slot.path_key, key_id;
      C.slot_policy_activate_after_key slot.path_key, string_of_int epoch;
      C.slot_policy_expire_after_key slot.path_key, string_of_int (epoch + 20);
      C.slot_policy_tombstone_key slot.path_key, string_of_bool (epoch >= 80);
      C.slot_policy_revoked_key slot.path_key, string_of_bool (epoch >= 80);
      C.mailbox_activate_after_key slot.path_key, "999";
      C.state_policy_delivery_key_key state.path_key, key_id;
      C.state_policy_activate_after_key state.path_key, string_of_int epoch;
      C.state_policy_expire_after_key state.path_key, string_of_int (epoch + 20);
      C.state_policy_tombstone_key state.path_key, string_of_bool (epoch >= 80);
      C.state_policy_revoked_key state.path_key, string_of_bool (epoch >= 80);
      Octra_core.Circle_state_descriptor.state_class_key state.path_key,
        (if epoch = 50 then "cipher_cell" else "register_cell");
      Octra_core.Circle_state_descriptor.codec_key state.path_key, string_of_int epoch;
      Octra_core.Circle_state_descriptor.schema_hash_key state.path_key, Printf.sprintf "%064x" epoch;
      Octra_core.Circle_state_descriptor.subject_addr_key state.path_key, subject;
      Octra_core.Circle_state_descriptor.hfhe_profile_key state.path_key,
        (if epoch = 50 then "ciphertext_v1" else "register_cell_v1");
      Octra_core.Circle_state_descriptor.mutable_state_key state.path_key, string_of_bool (epoch >= 80);
    ];
    values in
  let cases = [
    "transport_policy", "TransportPolicy";
    "hfhe_policy", "HfhePolicy";
    "key_policy", "KeyPolicy";
    "slot_policy", "SlotPolicy";
    "state_policy", "StatePolicy";
    "state_descriptor", "StateDescriptor";
  ] in
  let args name = match name with
    | "key_policy" -> [key_id]
    | "slot_policy" -> [slot_ref]
    | "state_policy" | "state_descriptor" -> [state_ref]
    | _ -> [] in
  let signed identity name keys =
    let owner, public_key, secret = identity in
    let subject = match keys with [] -> "" | key :: _ -> key in
    let op = "octra_circle_" ^ name in
    let signature = Auth.message ~op ~circle_id:parent ~addr:owner ~subject
      |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
    `List (List.map (fun value -> `String value)
      ([parent] @ keys @ [owner; public_key; signature])) in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let put epoch identity =
      let owner, _, _ = identity in
      let open Lwt.Syntax in
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
      let* () = S.deploy_circle store {info with owner} in
      let* _ = S.save_circle_stable_storage store parent (values epoch) in
      Lwt.return_unit in
    let routes = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      epoch_read = (fun action params store -> action store params ~current_epoch:1000);
      program_read = (fun _ _ _ -> failwith "policy test invoked source route");
      public_asset_read = (fun _ _ _ -> failwith "policy test invoked asset route");
      circle_view = (fun _ _ -> failwith "policy test invoked view route");
      circle_view_auth = (fun _ _ -> failwith "policy test invoked signed view route");
    } in
    let read ?snapshot name epoch =
      let current_epoch = epoch + 1 in
      let result = match name with
        | "transport_policy" -> Read.transport_policy ?snapshot store ~circle_id:parent
        | "hfhe_policy" -> Read.hfhe_policy ?snapshot store ~circle_id:parent
        | "key_policy" -> Read.key_policy ?snapshot store ~circle_id:parent ~key_id ~current_epoch
        | "slot_policy" -> Read.slot_locator_policy ?snapshot store ~circle_id:parent ~locator:slot ~current_epoch
        | "state_policy" -> Read.state_locator_policy ?snapshot store ~circle_id:parent ~locator:state ~current_epoch
        | "state_descriptor" -> Read.state_descriptor ?snapshot store ~circle_id:parent ~locator:state
        | _ -> assert false in
      V.Contract.run_s result in
    let query identity name route =
      Lwt_main.run ((List.assoc route routes) (signed identity name (args name)) store) in
    List.iter (fun (name, alias) ->
      List.iter (fun route ->
        V.Contract.run_s (put 50 first);
        V.Contract.run_s (S.commit_epoch_batch store "policy seed");
        let saved = read name 50 in
        expect "policy saved read failed" (Result.is_ok saved);
        expect "policy saved RPC failed" (query first name route = saved);
        let root = V.Contract.run_s (S.get_commit_hash store) in
        V.Contract.run_s (put 80 first);
        expect "policy control did not change live data" (read name 80 <> saved);
        expect "policy RPC read pending root" (query first name route = saved);
        expect "policy RPC changed head" (V.Contract.run_s (S.get_commit_hash store) = root);
        expect "policy RPC changed pending data"
          (V.Contract.run_s (S.read_circle_stable_inline_value store parent
            Octra_core.Circle_transport_policy.max_response_bytes_key) = Some "80");
        S.abort_epoch_batch store;
        V.Contract.run_s (put 80 second);
        expect "policy RPC refused saved owner" (query first name route = saved);
        expect "policy RPC accepted pending owner" (Result.is_error (query second name route));
        V.Contract.run_s (S.commit_epoch_batch store "policy next");
        let next = read name 80 in
        V.Contract.run_s (put 90 first);
        expect "policy RPC reused old owner" (Result.is_error (query first name route));
        expect "policy RPC missed committed data" (query second name route = next);
        S.abort_epoch_batch store;
        let invoke params = Lwt_main.run ((List.assoc route routes) params store) in
        expect "policy RPC accepted malformed auth" (Result.is_error (invoke (`List [`String parent])));
        expect "policy RPC accepted another operation"
          (Result.is_error (invoke (signed second "storage_dump" (args name))));
        let changed = match signed second name (args name) with
          | `List keys -> `List (List.mapi (fun index value ->
              if index = List.length keys - 1 then `String (String.make 88 'A') else value) keys)
          | _ -> assert false in
        expect "policy RPC accepted changed signature" (Result.is_error (invoke changed));
        List.iter (fun public ->
          expect "policy public RPC disclosed data"
            (Result.is_error (Lwt_main.run ((List.assoc public routes)
              (`List (List.map (fun value -> `String value) ([parent] @ args name))) store))))
          ["circle_" ^ name; "octra_circle" ^ alias])
        ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"]) cases;
    List.iter (fun (name, _) ->
      V.Contract.run_s (put 50 first);
      V.Contract.run_s (S.commit_epoch_batch store "policy saved");
      let snapshot = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
      let saved = read ~snapshot name 50 in
      let change () =
        V.Contract.run_s (put 80 second);
        V.Contract.run_s (S.commit_epoch_batch store "policy moved");
        V.Contract.run_s (put 90 first);
        Lwt.return (read ~snapshot name 50) in
      let op = "octra_circle_" ^ name in
      let params = signed first name (args name) in
      let result = match name with
        | "transport_policy" | "hfhe_policy" ->
          Read.with_owner_scope_auth ~snapshot store params ~op (fun _ -> change ())
        | "key_policy" ->
          Read.with_owner_subject_auth ~snapshot store params ~field:"key_id" ~op (fun _ _ -> change ())
        | _ ->
          let field, parse_locator = if name = "slot_policy"
            then "slot_ref", View.slot_locator else "state_ref", View.state_locator in
          Read.with_owner_locator_auth ~snapshot store params ~field ~op ~parse_locator (fun _ _ -> change ()) in
      expect "policy overlap mixed roots" (Lwt_main.run result = saved);
      expect "policy overlap changed pending data"
        (V.Contract.run_s (S.read_circle_stable_inline_value store parent
          Octra_core.Circle_transport_policy.max_response_bytes_key) = Some "90");
      S.abort_epoch_batch store;
      let route = "circle_" ^ name ^ "_auth" in
      expect "policy overlap reused owner" (Result.is_error (query first name route));
      expect "policy overlap missed new root" (query second name route = read name 80)) cases;
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "policy clock seed");
    expect "slot policy changed mailbox priority"
      (field "activate_after_epoch" (query first "slot_policy" "circle_slot_policy_auth") = `String "50");
    List.iter (fun (name, _) ->
      let route = "circle_" ^ name ^ "_auth" in
      let params = match signed first name (args name) with
        | `List (_ :: rest) -> `List (`String absent :: rest)
        | _ -> assert false in
      let result = Lwt_main.run ((List.assoc route routes) params store) in
      expect "policy RPC accepted missing circle" (Result.is_error result)) cases;
    let check_key state_name epoch =
      List.iter (fun name ->
        let route = "circle_" ^ name ^ "_auth" in
        let result = query first name route in
        let state_field = if name = "key_policy" then "state" else "delivery_key_state" in
        expect "policy clock classification changed" (field state_field result = `String state_name);
        if name = "key_policy" then
          expect "policy clock did not use saved epoch" (field "current_epoch" result = `String (string_of_int epoch)))
        ["key_policy"; "slot_policy"; "state_policy"] in
    check_key "scheduled" 51;
    expect "policy direct read ignored supplied epoch"
      (field "state" (V.Contract.run_s (Read.key_policy store ~circle_id:parent ~key_id ~current_epoch:1000)) = `String "expired");
    List.iter (fun (epoch, status) ->
      V.Contract.run_s (S.begin_epoch_batch store);
      V.Contract.run_s (S.set_meta store "last_epoch" (string_of_int epoch));
      V.Contract.run_s (S.commit_epoch_batch store "policy clock");
      check_key status (epoch + 1)) [51, "live"; 69, "expired"];
    List.iter (fun (key, status) ->
      V.Contract.run_s (S.begin_epoch_batch store);
      V.Contract.run_s (S.save_circle_stable_entry store parent (C.make_stable_entry key (C.Inline "true")));
      V.Contract.run_s (S.commit_epoch_batch store "policy status");
      check_key status 70) [Key.revoked_key key_id, "revoked"; Key.erased_key key_id, "erased"];
    List.iter (fun name ->
      let key = List.hd (args name) in
      let upper = String.uppercase_ascii key in
      let route = "circle_" ^ name ^ "_auth" in
      let invoke params = Lwt_main.run ((List.assoc route routes) params store) in
      expect "policy RPC refused signed locator spelling"
        (Result.is_ok (invoke (signed first name [upper])));
      let changed = match signed first name [key] with
        | `List (circle :: _ :: rest) -> `List (circle :: `String upper :: rest)
        | _ -> assert false in
      expect "policy RPC accepted changed locator spelling" (Result.is_error (invoke changed));
      expect "policy RPC accepted invalid locator"
        (Result.is_error (invoke (signed first name ["invalid"]))))
      ["slot_policy"; "state_policy"; "state_descriptor"];
    V.Contract.run_s (S.begin_epoch_batch store);
    let _ = V.Contract.run_s (S.save_circle_stable_storage store parent (Hashtbl.create 0)) in
    V.Contract.run_s (S.commit_epoch_batch store "policy empty");
    List.iter (fun (name, _) ->
      expect "policy default RPC changed" (query first name ("circle_" ^ name ^ "_auth") = read name 69)) cases;
    expect "policy default key is not live"
      (field "state" (query first "key_policy" "circle_key_policy_auth") = `String "live");
    expect "policy absent delivery key changed"
      (field "delivery_key_state" (query first "slot_policy" "circle_slot_policy_auth") = `Null);
    expect "policy absent descriptor changed"
      (field "materialized" (query first "state_descriptor" "circle_state_descriptor_auth") = `Bool false);
    expect "policy transport default changed"
      (query first "transport_policy" "circle_transport_policy_auth" =
        Ok (View.transport_policy ~circle_id:parent Octra_core.Circle_transport_policy.default));
    expect "policy hfhe default changed"
      (query first "hfhe_policy" "circle_hfhe_policy_auth" =
        Ok (View.hfhe_policy ~circle_id:parent Octra_core.Circle_hfhe_policy.default));
    V.Contract.run_s (S.set_meta store "last_epoch" (string_of_int (max_int - 1)));
    List.iter (fun name ->
      expect "policy RPC refused final valid epoch"
        (query first name ("circle_" ^ name ^ "_auth") = read name (max_int - 1)))
      ["key_policy"; "slot_policy"; "state_policy"];
    V.Contract.run_s (S.set_meta store "last_epoch" (string_of_int max_int));
    List.iter (fun name ->
      expect "policy RPC accepted epoch overflow"
        (Result.is_error (query first name ("circle_" ^ name ^ "_auth"))))
      ["key_policy"; "slot_policy"; "state_policy"];
    V.Contract.run_s (S.remove_path store ["meta"; "last_epoch"]);
    List.iter (fun (name, _) ->
      expect "policy RPC accepted missing epoch"
        (Result.is_error (query first name ("circle_" ^ name ^ "_auth")))) cases)

let asset_root path =
  let module C = Octra_core.Circles in
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let module View = Octra_node_runtime.Circle_view in
  let identity seed =
    let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
      (String.make 32 seed)) in
    let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv secret
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
    Octra_core.Crypto.Address.address_from_pubkey public_key, public_key, secret in
  let first = identity '\052' in
  let second = identity '\053' in
  let owner, _, _ = first in
  let state_ref = String.make 64 'd' in
  let slot_ref = String.make 64 'a' in
  let state = View.state_locator state_ref |> Result.get_ok in
  let slot = View.slot_locator slot_ref |> Result.get_ok in
  let other = View.state_locator (String.make 64 'b') |> Result.get_ok in
  let canonical_path, path_key = C.path_key_of_raw_path "/record" |> Result.get_ok in
  let raw = View.{field = "path"; value = canonical_path; canonical_path; path_key} in
  let page, page_key = C.path_key_of_raw_path "/page" |> Result.get_ok in
  let key_id = String.make 64 'f' in
  let info = C.{
    circle_id = parent; runtime = Octb; version = 1L; owner;
    code_hash = zero_hash_hex; stable_root = zero_hash_hex;
    assets_root = zero_hash_hex; privacy_class = Scoped;
    browser_mode = Native_sealed; resource_mode = Public_resources;
    policy_hash = None; members_root = None; export_policy = None;
    limits = default_limits;
  } in
  let meta epoch mode path_key canonical_path = C.{
    path_key; canonical_path; content_type = "application/octet-stream";
    encoding = "identity"; size_bytes = Int64.of_int epoch; blob_hash = zero_hash_hex;
    body_mode = Sealed_read; plaintext_hash = Some (Printf.sprintf "%064x" epoch);
    key_id = Some key_id; padding_class = Some (string_of_int epoch);
    resource_key = "record"; locator_mode = mode;
    slot_ref = None; activate_after_epoch = None; expire_after_epoch = None;
    metadata_mode = Metadata_opaque;
  } in
  let values epoch =
    let values = Hashtbl.create 20 in
    let digest = Printf.sprintf "%064x" epoch in
    let commitment = Base64.encode_exn (String.make 32 (Char.chr epoch)) in
    List.iter (fun (key, value) -> Hashtbl.add values key value) [
      C.state_policy_delivery_key_key state.path_key, key_id;
      C.slot_policy_delivery_key_key slot.path_key, key_id;
      Octra_core.Circle_state_descriptor.state_class_key state.path_key, "cipher_cell";
      Octra_core.Circle_state_descriptor.codec_key state.path_key, string_of_int epoch;
      Octra_core.Circle_state_descriptor.schema_hash_key state.path_key, digest;
      Octra_core.Circle_state_descriptor.hfhe_profile_key state.path_key, "ciphertext_v1";
      Octra_core.Circle_balance_cell.ciphertext_commitment_key state.path_key, commitment;
      Octra_core.Circle_balance_cell.amount_commitment_key state.path_key, commitment;
      Octra_core.Circle_balance_cell.proof_receipt_hash_key state.path_key, digest;
      Octra_core.Circle_register_cell.ciphertext_commitment_key state.path_key, commitment;
      Octra_core.Circle_register_cell.proof_receipt_hash_key state.path_key, digest;
    ];
    values in
  let cases = [
    "asset_ciphertext", "AssetCiphertext", raw.canonical_path;
    "asset_ciphertext_by_resource_key", "AssetCiphertextByResourceKey", "record";
    "asset_ciphertext_by_slot_ref", "AssetCiphertextBySlotRef", slot_ref;
    "asset_ciphertext_by_state_ref", "AssetCiphertextByStateRef", state_ref;
    "balance_cell", "BalanceCell", state_ref;
    "register_cell", "RegisterCell", state_ref;
  ] in
  let subject name key = match name with
    | "asset_ciphertext" -> View.asset_subject ~kind:"path" ~subject:key
    | "asset_ciphertext_by_resource_key" -> View.asset_subject ~kind:"resource_key" ~subject:key
    | "asset_ciphertext_by_slot_ref" -> View.asset_subject ~kind:"slot_ref" ~subject:key
    | "asset_ciphertext_by_state_ref" -> View.asset_subject ~kind:"state_ref" ~subject:key
    | _ -> key in
  let signed identity name key =
    let owner, public_key, secret = identity in
    let signature = Auth.message ~op:("octra_circle_" ^ name) ~circle_id:parent
      ~addr:owner ~subject:(subject name key)
      |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
    `List (List.map (fun value -> `String value) [parent; key; owner; public_key; signature]) in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let put epoch identity =
      let owner, _, _ = identity in
      let open Lwt.Syntax in
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
      let* () = S.deploy_circle store {info with owner} in
      let* _ = S.save_circle_stable_storage store parent (values epoch) in
      let* () = Lwt_list.iter_s (fun (mode, (locator : View.locator)) ->
        let* () = S.save_circle_asset_meta store parent
          (meta epoch mode locator.View.path_key locator.canonical_path) in
        S.save_circle_asset_ciphertext_b64 store parent locator.path_key
          (Base64.encode_exn ("cipher" ^ string_of_int epoch ^ locator.value)))
        [C.State_locator, state; C.Slot_locator, slot; C.State_locator, other; C.Path_locator, raw] in
      let* () = S.save_circle_asset_resource_index store parent "record"
        (if epoch = 50 then state.path_key else other.path_key) in
      let* () = S.save_circle_asset_meta store parent
        {(meta epoch C.Path_locator page_key page) with body_mode = C.Public_resources} in
      S.save_circle_asset_body_b64 store parent page_key
        (Base64.encode_exn ("page" ^ string_of_int epoch)) in
    let routes = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      epoch_read = (fun action params store -> action store params ~current_epoch:1000);
      public_asset_read = (fun action params store ->
        action store params ~current_epoch:1000 ~reveal_sensitive_fields:false);
      program_read = (fun _ _ _ -> failwith "asset test invoked source route");
      circle_view = (fun _ _ -> failwith "asset test invoked view route");
      circle_view_auth = (fun _ _ -> failwith "asset test invoked signed view route");
    } in
    let read ?snapshot name epoch reveal =
      let current_epoch = epoch + 1 in
      let direct (locator : View.locator) = Read.asset_ciphertext ?snapshot store ~circle_id:parent
        ~path_key:locator.View.path_key ~canonical_path:locator.canonical_path
        ~current_epoch ~reveal_sensitive_fields:reveal in
      let result = match name with
        | "asset_ciphertext_by_resource_key" -> Read.asset_ciphertext_by_resource_key ?snapshot store
            ~circle_id:parent ~resource_key:"record" ~current_epoch ~reveal_sensitive_fields:reveal
        | "asset_ciphertext_by_slot_ref" -> direct slot
        | "asset_ciphertext" -> direct raw
        | "balance_cell" -> Read.private_state_cell ?snapshot store ~circle_id:parent ~locator:state
            ~current_epoch ~not_found:"circle balance cell not found" ~cell_field:"balance_cell"
            ~empty_cell:Octra_core.Circle_balance_cell.empty
            ~load_cell:Octra_core.Circle_balance_cell_store.load_opt
            ~cell_json:Octra_core.Circle_balance_cell.yojson_of_t
        | "register_cell" -> Read.private_state_cell ?snapshot store ~circle_id:parent ~locator:state
            ~current_epoch ~not_found:"circle register cell not found" ~cell_field:"register_cell"
            ~empty_cell:Octra_core.Circle_register_cell.empty
            ~load_cell:Octra_core.Circle_register_cell_store.load_opt
            ~cell_json:Octra_core.Circle_register_cell.yojson_of_t
        | _ -> direct state in
      V.Contract.run_s result in
    let query route params = Lwt_main.run ((List.assoc route routes) params store) in
    List.iter (fun (name, alias, key) ->
      List.iter (fun route ->
        V.Contract.run_s (put 50 first);
        V.Contract.run_s (S.commit_epoch_batch store "asset seed");
        let saved = read name 50 true in
        expect "asset saved read failed" (Result.is_ok saved);
        expect "asset saved RPC failed" (query route (signed first name key) = saved);
        V.Contract.run_s (put 80 first);
        expect "asset control did not change live data" (read name 80 true <> saved);
        expect "asset RPC read pending root" (query route (signed first name key) = saved);
        S.abort_epoch_batch store;
        V.Contract.run_s (put 80 second);
        expect "asset RPC refused saved owner" (query route (signed first name key) = saved);
        expect "asset RPC accepted pending owner" (Result.is_error (query route (signed second name key)));
        V.Contract.run_s (S.commit_epoch_batch store "asset next");
        let next = read name 80 true in
        V.Contract.run_s (put 90 first);
        expect "asset RPC reused old owner" (Result.is_error (query route (signed first name key)));
        expect "asset RPC missed committed data" (query route (signed second name key) = next);
        S.abort_epoch_batch store;
        expect "asset RPC accepted missing auth"
          (Result.is_error (query route (`List [`String parent; `String key])));
        expect "asset RPC accepted another operation"
          (Result.is_error (query route (signed second "storage_dump" key)));
        let changed = match signed second name key with
          | `List (circle :: _ :: rest) -> `List (circle :: `String "changed" :: rest)
          | _ -> assert false in
        expect "asset RPC accepted changed subject" (Result.is_error (query route changed));
        let changed = match signed second name key with
          | `List keys -> `List (List.mapi (fun index value ->
              if index = 4 then `String (String.make 88 'A') else value) keys)
          | _ -> assert false in
        expect "asset RPC accepted changed signature" (Result.is_error (query route changed)))
        ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"];
      List.iter (fun route ->
        V.Contract.run_s (put 50 first);
        V.Contract.run_s (S.commit_epoch_batch store "public asset seed");
        let params = `List [`String parent; `String key] in
        if name = "balance_cell" || name = "register_cell" then
          expect "public cell RPC disclosed data" (Result.is_error (query route params))
        else begin
          let saved = read name 50 false in
          expect "public asset saved RPC failed" (query route params = saved);
          List.iter (fun key -> expect "public opaque asset disclosed metadata"
            (field key saved = `Null))
            ["path_key"; "canonical_path"; "plaintext_hash"; "key_id";
             "delivery_key_id"; "state_descriptor"];
          V.Contract.run_s (put 80 second);
          expect "public asset RPC read pending root" (query route params = saved);
          S.abort_epoch_batch store
        end) ["circle_" ^ name; "octra_circle" ^ alias]) cases;
    List.iter (fun route ->
      V.Contract.run_s (put 50 first);
      V.Contract.run_s (S.commit_epoch_batch store "plain asset seed");
      let params = `List [`String parent; `String page] in
      let saved = query route params in
      expect "plain asset saved RPC failed" (Result.is_ok saved);
      V.Contract.run_s (put 80 second);
      expect "plain asset RPC read pending root" (query route params = saved);
      expect "plain asset live control failed"
        (V.Contract.run_s (Read.asset_plaintext store ~circle_id:parent
          ~path_key:page_key ~canonical_path:page) <> saved);
      S.abort_epoch_batch store) ["circle_asset"; "octra_circleAsset"];
    List.iter (fun (name, _, key) ->
      V.Contract.run_s (put 50 first);
      V.Contract.run_s (S.commit_epoch_batch store "asset saved");
      let snapshot = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
      let saved = read ~snapshot name 50 true in
      let change () =
        V.Contract.run_s (put 80 second);
        V.Contract.run_s (S.commit_epoch_batch store "asset moved");
        V.Contract.run_s (put 90 first);
        Lwt.return (read ~snapshot name 50 true) in
      let params = signed first name key in
      let op = "octra_circle_" ^ name in
      let result = if name = "balance_cell" || name = "register_cell" then
        Read.with_owner_locator_auth ~snapshot store params ~op ~field:"state_ref"
          ~parse_locator:View.state_locator (fun _ _ -> change ())
        else
          Read.with_owner_asset_auth ~snapshot store params ~circle_id:parent ~op
            ~subject:(subject name key) change in
      expect "asset overlap mixed roots" (Lwt_main.run result = saved);
      expect "asset overlap changed pending body"
        (V.Contract.run_s (S.get_circle_asset_ciphertext_b64 store parent state.path_key) =
          Some (Base64.encode_exn ("cipher90" ^ state.value)));
      expect "asset direct reader lost live data" (read name 90 true <> saved);
      S.abort_epoch_batch store;
      expect "asset overlap reused owner"
        (Result.is_error (query ("circle_" ^ name ^ "_auth") params));
      expect "asset overlap missed new data"
        (query ("circle_" ^ name ^ "_auth") (signed second name key) = read name 80 true)) cases;
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "asset visibility seed");
    let write key value = V.Contract.run_s (S.save_circle_stable_entry store parent
      (C.make_stable_entry key (C.Inline value))) in
    let changed_epoch epoch =
      V.Contract.run_s (S.begin_epoch_batch store);
      V.Contract.run_s (S.set_meta store "last_epoch" (string_of_int epoch)) in
    let commit () = V.Contract.run_s (S.commit_epoch_batch store "asset visibility") in
    let check_visible expected =
      List.iter (fun (name, alias, key) ->
        List.iter (fun route ->
          expect "asset visibility used another root or clock"
            (Result.is_ok (query route (signed first name key)) = expected))
          ["circle_" ^ name ^ "_auth"; "octra_circle" ^ alias ^ "Auth"];
        if name <> "balance_cell" && name <> "register_cell" then
          List.iter (fun route ->
            expect "public asset visibility used another root or clock"
              (Result.is_ok (query route (`List [`String parent; `String key])) = expected))
            ["circle_" ^ name; "octra_circle" ^ alias]) cases in
    changed_epoch 50;
    write (Octra_core.Circle_key_policy.activate_after_key key_id) "52";
    write (Octra_core.Circle_key_policy.expire_after_key key_id) "70";
    commit ();
    check_visible false;
    changed_epoch 51;
    commit ();
    check_visible true;
    expect "asset direct reader ignored explicit epoch"
      (Result.is_error (read "asset_ciphertext" 1000 true));
    changed_epoch 80;
    write (Octra_core.Circle_key_policy.revoked_key key_id) "true";
    check_visible true;
    S.abort_epoch_batch store;
    changed_epoch 69;
    commit ();
    check_visible false;
    changed_epoch 51;
    write (Octra_core.Circle_key_policy.revoked_key key_id) "true";
    commit ();
    check_visible false;
    changed_epoch 51;
    write (Octra_core.Circle_key_policy.revoked_key key_id) "false";
    write (Octra_core.Circle_key_policy.erased_key key_id) "true";
    commit ();
    check_visible false;
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "asset policy seed");
    changed_epoch 50;
    List.iter (fun (activate, expire) ->
      write activate "52";
      write expire "70") [
        C.slot_policy_activate_after_key raw.path_key, C.slot_policy_expire_after_key raw.path_key;
        C.slot_policy_activate_after_key slot.path_key, C.slot_policy_expire_after_key slot.path_key;
        C.state_policy_activate_after_key state.path_key, C.state_policy_expire_after_key state.path_key;
      ];
    commit ();
    check_visible false;
    changed_epoch 51;
    commit ();
    check_visible true;
    changed_epoch 69;
    commit ();
    check_visible false;
    changed_epoch 51;
    List.iter (fun key -> write key "true") [
      C.slot_policy_tombstone_key raw.path_key;
      C.slot_policy_revoked_key slot.path_key;
      C.state_policy_tombstone_key state.path_key;
    ];
    commit ();
    check_visible false;
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "asset plain guard seed");
    let sealed = `List [`String parent; `String raw.canonical_path] in
    expect "plain asset returned sealed body" (Result.is_error (query "circle_asset" sealed));
    changed_epoch 80;
    V.Contract.run_s (S.save_circle_asset_meta store parent
      {(meta 80 C.Path_locator raw.path_key raw.canonical_path) with body_mode = C.Public_resources});
    V.Contract.run_s (S.save_circle_asset_body_b64 store parent raw.path_key "pending plaintext");
    expect "plain asset guard used pending policy" (Result.is_error (query "circle_asset" sealed));
    expect "plain asset direct live guard failed"
      (Result.is_ok (V.Contract.run_s (Read.asset_plaintext store ~circle_id:parent
        ~path_key:raw.path_key ~canonical_path:raw.canonical_path)));
    S.abort_epoch_batch store;
    List.iter (fun name ->
      let key = if name = "asset_ciphertext_by_slot_ref" then slot_ref else state_ref in
      let upper = String.uppercase_ascii key in
      let route = "circle_" ^ name ^ "_auth" in
      expect "asset RPC refused signed locator spelling" (Result.is_ok (query route (signed first name upper)));
      let changed = match signed first name key with
        | `List (circle :: _ :: rest) -> `List (circle :: `String upper :: rest)
        | _ -> assert false in
      expect "asset RPC accepted changed locator spelling" (Result.is_error (query route changed));
      expect "asset RPC accepted malformed locator"
        (Result.is_error (query route (signed first name "invalid"))))
      ["asset_ciphertext_by_slot_ref"; "asset_ciphertext_by_state_ref"; "balance_cell"; "register_cell"];
    let normalized = match signed first "asset_ciphertext" raw.canonical_path with
      | `List (circle :: _ :: rest) -> `List (circle :: `String "/%72ecord//." :: rest)
      | _ -> assert false in
    expect "asset RPC changed normalized path signature"
      (query "circle_asset_ciphertext_auth" normalized = read "asset_ciphertext" 50 true);
    List.iter (fun metadata_mode ->
      changed_epoch 50;
      List.iter (fun (mode, (locator : View.locator)) ->
        V.Contract.run_s (S.save_circle_asset_meta store parent
          {(meta 50 mode locator.path_key locator.canonical_path) with
            metadata_mode; slot_ref = Some slot_ref;
            activate_after_epoch = Some 1L; expire_after_epoch = Some 70L}))
        [C.Path_locator, raw; C.Slot_locator, slot; C.State_locator, state];
      commit ();
      List.iter (fun (name, alias, key) ->
        if name <> "balance_cell" && name <> "register_cell" then begin
          let saved = read name 50 false in
          expect "public ciphertext read failed" (Result.is_ok saved);
          List.iter (fun (route, auth_route) ->
            let public = query route (`List [`String parent; `String key]) in
            expect "public ciphertext route changed" (public = saved);
            List.iter (fun key ->
              expect ("public ciphertext disclosed field = " ^ key)
                (match public with
                 | Ok (`Assoc fields) -> not (List.mem_assoc key fields)
                 | _ -> false))
              ["plaintext_hash"; "slot_ref"; "state_ref"; "activate_after_epoch";
               "expire_after_epoch"; "key_id"; "padding_class"; "delivery_key_id";
               "delivery_key_state"; "delivery_key_live"; "state_descriptor";
               "state_descriptor_materialized"];
            let signed = query auth_route (signed first name key) in
            expect "owner ciphertext read changed" (signed = read name 50 true);
            expect "owner ciphertext lost plaintext hash"
              (field "plaintext_hash" signed = `String (Printf.sprintf "%064x" 50));
            expect "owner ciphertext lost key metadata" (field "key_id" signed = `String key_id);
            expect "public ciphertext body changed"
              (field "ciphertext_b64" public = field "ciphertext_b64" signed);
            expect "public ciphertext lost mode"
              (field "metadata_mode" public = `String (C.string_of_metadata_mode metadata_mode));
            expect "public ciphertext locator disclosure changed"
              ((field "path_key" public <> `Null) = (metadata_mode = C.Metadata_reveal)))
            ["circle_" ^ name, "circle_" ^ name ^ "_auth";
             "octra_circle" ^ alias, "octra_circle" ^ alias ^ "Auth"]
        end) cases)
      [C.Metadata_reveal; C.Metadata_opaque];
    changed_epoch 80;
    V.Contract.run_s (S.deploy_circle store {info with resource_mode = C.Sealed_read});
    expect "plain asset guard used pending Circle mode"
      (Result.is_ok (query "circle_asset" (`List [`String parent; `String page])));
    S.abort_epoch_batch store;
    changed_epoch 50;
    V.Contract.run_s (S.deploy_circle store {info with resource_mode = C.Sealed_read});
    commit ();
    changed_epoch 80;
    V.Contract.run_s (S.deploy_circle store info);
    expect "plain asset returned sealed Circle body"
      (Result.is_error (query "circle_asset" (`List [`String parent; `String page])));
    S.abort_epoch_batch store;
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "asset missing seed");
    List.iter (fun (name, _, _) ->
      expect "asset RPC accepted missing resource"
        (Result.is_error (query ("circle_" ^ name ^ "_auth")
          (signed first name (if name = "asset_ciphertext" then "/missing"
            else String.make 64 'e'))));
      let params = match signed first name state_ref with
        | `List (_ :: rest) -> `List (`String absent :: rest)
        | _ -> assert false in
      expect "asset RPC accepted missing Circle"
        (Result.is_error (query ("circle_" ^ name ^ "_auth") params))) cases;
    changed_epoch 50;
    V.Contract.run_s (S.remove_path store
      ["circles"; parent; "assets"; "resource_keys"; "record"]);
    commit ();
    expect "asset RPC invented missing resource index"
      (Result.is_error (query "circle_asset_ciphertext_by_resource_key_auth"
        (signed first "asset_ciphertext_by_resource_key" "record")));
    expect "asset RPC lost direct state without index" (Result.is_ok
      (query "circle_asset_ciphertext_by_state_ref_auth"
        (signed first "asset_ciphertext_by_state_ref" state_ref)));
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "asset metadata clock seed");
    changed_epoch 50;
    List.iter (fun (mode, (locator : View.locator)) ->
      V.Contract.run_s (S.save_circle_asset_meta store parent
        {(meta 50 mode locator.path_key locator.canonical_path) with
          activate_after_epoch = Some 52L; expire_after_epoch = Some 70L}))
      [C.Path_locator, raw; C.State_locator, state; C.Slot_locator, slot];
    commit ();
    check_visible false;
    changed_epoch 51;
    commit ();
    check_visible true;
    changed_epoch 69;
    commit ();
    check_visible false;
    changed_epoch 50;
    List.iter (fun (activate, expire) -> write activate "1"; write expire "100") [
      C.slot_policy_activate_after_key raw.path_key, C.slot_policy_expire_after_key raw.path_key;
      C.slot_policy_activate_after_key slot.path_key, C.slot_policy_expire_after_key slot.path_key;
      C.state_policy_activate_after_key state.path_key, C.state_policy_expire_after_key state.path_key;
    ];
    let other_key = String.make 64 'e' in
    write (Octra_core.Circle_key_policy.revoked_key other_key) "true";
    V.Contract.run_s (S.save_circle_asset_meta store parent
      {(meta 50 C.State_locator state.path_key state.canonical_path) with key_id = Some other_key});
    commit ();
    check_visible true;
    expect "asset delivery key priority changed"
      (field "delivery_key_id" (query "circle_asset_ciphertext_by_state_ref_auth"
        (signed first "asset_ciphertext_by_state_ref" state_ref)) = `String key_id);
    V.Contract.run_s (put 50 first);
    V.Contract.run_s (S.commit_epoch_batch store "asset default seed");
    changed_epoch 50;
    let _ = V.Contract.run_s (S.save_circle_stable_storage store parent (Hashtbl.create 0)) in
    commit ();
    List.iter (fun name ->
      let answer = query ("circle_" ^ name ^ "_auth") (signed first name state_ref) in
      expect "asset absent cell changed" (field (name ^ "_materialized") answer = `Bool false);
      expect "asset absent descriptor changed" (field "descriptor_materialized" answer = `Bool false))
      ["balance_cell"; "register_cell"];
    V.Contract.run_s (S.set_meta store "last_epoch" (string_of_int (max_int - 1)));
    check_visible true;
    V.Contract.run_s (S.set_meta store "last_epoch" (string_of_int max_int));
    check_visible false;
    V.Contract.run_s (S.remove_path store ["meta"; "last_epoch"]);
    check_visible false;
    expect "plain asset RPC accepted missing epoch"
      (Result.is_error (query "circle_asset" (`List [`String parent; `String page]))))

let circle_root path =
  let module C = Octra_core.Circles in
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
    (String.make 32 '\045')) in
  let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv secret
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let owner = Octra_core.Crypto.Address.address_from_pubkey public_key in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let raw = compile {|
program Read {
  state { count: int }
  public view fn read(target: address): int {
    return self.count + balance(target) + to_int(call(target, "read"))
  }
}
|} in
    let info = C.{
      circle_id = parent; runtime = Octb; version = 1L; owner;
      code_hash = sha256_hex raw; stable_root = zero_hash_hex;
      assets_root = zero_hash_hex; privacy_class = Public;
      browser_mode = Native_sealed; resource_mode = Public_resources;
      policy_hash = None; members_root = None; export_policy = None;
      limits = default_limits;
    } in
    let put epoch count amount =
      let open Lwt.Syntax in
      let code = if epoch = 50 then raw else compile (Printf.sprintf {|
program Read {
  state { count: int }
  public view fn read(target: address): int {
    return self.count + balance(target) + to_int(call(target, "read")) + %d
  }
}
|} (epoch * 10)) in
      let info = {info with code_hash = C.sha256_hex code;
        version = Int64.of_int epoch;
        owner = if epoch >= 80 then absent else owner;
        privacy_class = if epoch >= 80 then C.Scoped else C.Public} in
      let* () = S.begin_epoch_batch store in
      let* () = S.set_meta store "last_epoch" (string_of_int epoch) in
      let* () = S.deploy_circle store info in
      let* () = S.save_circle_program_code_b64 store parent (Base64.encode_exn code) in
      let* () = Octra_core.Circle_deploy.allow_calls store info in
      let nested = compile (Printf.sprintf {|
program Value {
  public view fn read(): int {
    return %d
  }
}
|} (if epoch = 50 then 7 else epoch)) in
      let* () = deploy store child nested in
      let* () = S.set_account store child
        {Octra_core.Ledger_types.empty_account with balance = Z.of_int amount} in
      let values = Hashtbl.create 1 in
      Hashtbl.add values "count" (string_of_int count);
      let* _ = S.save_circle_stable_storage store parent values in
      Lwt.return_unit in
    V.Contract.run_s (put 50 10 100);
    V.Contract.run_s (S.commit_epoch_batch store "circle seed");
    let snapshot = V.Contract.run_s (S.capture_read_snapshot store) |> Result.get_ok in
    let ledger = L.create store in
    V.Contract.run_s (put 80 1000 10000);
    let code_params = `List [`String parent] in
    let answer = Lwt_main.run (Read.program_params store code_params) in
    expect "circle source used pending root"
      (field "code_b64" answer = `String (Base64.encode_exn raw));
    let answer = Lwt_main.run (Read.program_info_public_params store code_params) in
    expect "circle descriptor used pending root"
      (field "code_hash" answer = `String info.code_hash);
    expect "circle descriptor used pending owner" (field "owner" answer = `String owner);
    S.abort_epoch_batch store;
    let signed op =
      let signature = Auth.message ~op ~circle_id:parent ~addr:owner ~subject:""
        |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
      `List [`String parent; `String owner; `String public_key; `String signature] in
    let info_auth = signed "octra_circle_program_info" in
    let code_auth = signed "octra_circle_program" in
    let cases = [
      "circle_program_info", code_params, true;
      "octra_circleProgramInfo", code_params, true;
      "circle_program_info_auth", info_auth, true;
      "octra_circleProgramInfoAuth", info_auth, true;
      "octra_circleProgram", code_params, false;
      "circle_program_auth", code_auth, false;
      "octra_circleProgramAuth", code_auth, false;
    ] in
    let routes select = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      program_read = (fun action params store -> action ~profile:select store params);
      epoch_read = (fun action params store -> action store params ~current_epoch:0);
      public_asset_read = (fun action params store ->
        action store params ~current_epoch:0 ~reveal_sensitive_fields:false);
      circle_view = (fun _ _ -> failwith "source test invoked view route");
      circle_view_auth = (fun _ _ -> failwith "source test invoked signed view route");
    } in
    List.iter (fun (route, params, described) ->
      V.Contract.run_s (put 50 10 100);
      V.Contract.run_s (S.commit_epoch_batch store "source seed");
      V.Contract.run_s (put 90 999 9999);
      let selected = ref false in
      let change ~epoch =
        expect "source RPC used another epoch" (epoch = 51);
        expect "source RPC selected twice" (not !selected);
        selected := true;
        S.abort_epoch_batch store;
        V.Contract.run_s (put 80 1000 10000);
        V.Contract.run_s (S.commit_epoch_batch store "source next");
        V.Contract.run_s (put 100 9999 99999);
        profile ~epoch in
      let answer = Lwt_main.run ((List.assoc route (routes change)) params store) in
      expect "source RPC bypassed root profile" !selected;
      expect "source RPC changed code hash" (field "code_hash" answer = `String info.code_hash);
      if described then begin
        expect "descriptor RPC changed owner" (field "owner" answer = `String owner);
        expect "descriptor RPC changed version" (field "version" answer = `String "50")
      end else
        expect "source RPC disclosed another code"
          (field "code_b64" answer = `String (Base64.encode_exn raw));
      S.abort_epoch_batch store;
      let fresh ~epoch =
        expect "source RPC reused old epoch" (epoch = 81);
        profile ~epoch in
      let refused = Lwt_main.run ((List.assoc route (routes fresh)) params store) in
      expect "source RPC reused old access" (Result.is_error refused)) cases;
    let unavailable ~epoch:_ = Error (Octra_core.Rpc.err (-32005) "profile unavailable" None) in
    List.iter (fun described ->
      let refused = Lwt_main.run (Read.program_rpc ~authenticated:false ~describe:described
        ~profile:unavailable store code_params) in
      expect "source RPC ignored profile refusal" (Result.is_error refused)) [false; true];
    V.Contract.run_s (put 50 10 100);
    V.Contract.run_s (S.commit_epoch_batch store "view seed");
    let selected ~epoch = profile ~epoch |> Result.map (fun profile ->
      {profile with V.Contract_rpc.proof_exec = Octra_core.Rule_graph.Active}) in
    let view_ctx = V.Contract_rpc.make_view_ctx ~snapshot ~trusted:[]
      ~profile:(Result.get_ok (selected ~epoch:51)) ~store ~ledger
      ~get_fhe_pubkey:(fun _ -> failwith "live circle key loader reached") () in
    let changed = ref false in
    let running () =
      if not !changed then begin
        changed := true;
        V.Contract.run_s (put 70 100 1000);
        V.Contract.run_s (S.commit_epoch_batch store "circle next");
        V.Contract.run_s (put 90 999 9999)
      end;
      true in
    let call_params = [`String child] in
    let subject = Auth.view_subject ~method_name:"read" ~call_params ~include_storage:true in
    let signature = Auth.message ~op:"octra_circle_view" ~circle_id:parent ~addr:owner ~subject
      |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
    let params = `List [
      `String parent; `String "read"; `List call_params;
      `String owner; `String public_key; `String signature; `Bool true;
    ] in
    let answer = Lwt_main.run (Read.view_call_auth ~snapshot store params ~view_ctx
      ~running ~stop:(fun () -> ())) in
    expect "circle view did not overlap a commit" !changed;
    expect "circle view mixed committed roots" (field "result" answer = `String "117");
    expect "circle view response used another root"
      (field "storage" answer = `Assoc ["count", `String "10"]);
    expect "circle view changed pending state"
      (V.Contract.run_s (S.read_circle_stable_inline_value store parent "count") = Some "999");
    S.abort_epoch_batch store;
    let before = V.Contract.run_s (S.get_commit_hash store) in
    let change ~epoch =
      expect "circle RPC used pending epoch" (epoch = 71);
      V.Contract.run_s (put 80 1000 10000);
      V.Contract.run_s (S.commit_epoch_batch store "circle RPC next");
      V.Contract.run_s (put 100 9999 99999);
      selected ~epoch in
    let answer = Lwt_main.run (Read.view_rpc ~authenticated:true ~trusted:[]
      ~profile:change ~store ~ledger params) in
    expect "circle RPC mixed roots" (field "result" answer = `String "1870");
    expect "circle RPC response lost root"
      (field "storage" answer = `Assoc ["count", `String "100"]);
    expect "circle RPC did not overlap root change"
      (V.Contract.run_s (S.get_commit_hash store) <> before);
    S.abort_epoch_batch store;
    let fresh ~epoch =
      expect "circle RPC did not see new committed epoch" (epoch = 81);
      selected ~epoch in
    let refused = Lwt_main.run (Read.view_rpc ~authenticated:true ~trusted:[]
      ~profile:fresh ~store ~ledger params) in
    expect "circle RPC reused old owner" (Result.is_error refused);
    let public_params = `List [`String parent; `String "read"; `List call_params] in
    let refused = Lwt_main.run (Read.view_rpc ~authenticated:false ~trusted:[]
      ~profile:fresh ~store ~ledger public_params) in
    expect "circle RPC reused public access" (Result.is_error refused);
    let refused = Lwt_main.run (Read.view_rpc ~authenticated:true ~trusted:[]
      ~profile:(fun ~epoch:_ -> Error (Octra_core.Rpc.err (-32005) "profile unavailable" None))
      ~store ~ledger params) in
    expect "circle RPC ignored profile refusal" (Result.is_error refused);
    V.Contract.run_s (S.remove_path store ["meta"; "last_epoch"]);
    let unused ~epoch:_ = failwith "circle RPC selected profile without a root" in
    let refused = Lwt_main.run (Read.view_rpc ~authenticated:true ~trusted:[]
      ~profile:unused ~store ~ledger params) in
    expect "circle RPC accepted missing epoch" (Result.is_error refused);
    let refused = Lwt_main.run (Read.program_rpc ~authenticated:false ~describe:true
      ~profile:unused store code_params) in
    expect "source RPC accepted missing epoch" (Result.is_error refused);
    V.Contract.run_s (S.set_meta store "last_epoch" (string_of_int max_int));
    let refused = Lwt_main.run (Read.view_rpc ~authenticated:true ~trusted:[]
      ~profile:unused ~store ~ledger params) in
    expect "circle RPC accepted epoch overflow" (Result.is_error refused);
    let refused = Lwt_main.run (Read.program_rpc ~authenticated:false ~describe:false
      ~profile:unused store code_params) in
    expect "source RPC accepted epoch overflow" (Result.is_error refused))

let view_bytes path =
  let module C = Octra_core.Circles in
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
    (String.make 32 '\052')) in
  let public_key = Mirage_crypto_ec.Ed25519.pub_of_priv secret
    |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
  let owner = Octra_core.Crypto.Address.address_from_pubkey public_key in
  let store = Lwt_main.run (S.open_store ~fresh:true path) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
    let raw = compile {|
program WorkRead {
  public view fn copy(text: string, count: int): int {
    let index = 0
    while index < count {
      text = concat(text, text)
      index += 1
    }
    return len(text)
  }
  public view fn square(count: int): int {
    let number = 2
    let index = 0
    while index < count {
      number = number * number
      index += 1
    }
    return 7
  }
}
|} in
    let info = C.{
      circle_id = parent; runtime = Octb; version = 1L; owner;
      code_hash = sha256_hex raw; stable_root = zero_hash_hex;
      assets_root = zero_hash_hex; privacy_class = Public;
      browser_mode = Native_sealed; resource_mode = Public_resources;
      policy_hash = None; members_root = None; export_policy = None;
      limits = default_limits;
    } in
    let ledger = L.create store in
    V.Contract.run_s (S.set_meta store "last_epoch" "0");
    V.Contract.run_s (S.deploy_circle store info);
    V.Contract.run_s (S.save_circle_program_code_b64 store parent (Base64.encode_exn raw));
    let selected ~epoch = profile ~epoch |> Result.map (fun value ->
      {value with V.Contract_rpc.int_work = V.Int_work.Prior}) in
    let routes = Read.dispatch Read.{
      store_read = (fun action params store -> action store params);
      program_read = (fun _ _ _ -> failwith "work view invoked source route");
      epoch_read = (fun _ _ _ -> failwith "work view invoked epoch route");
      public_asset_read = (fun _ _ _ -> failwith "work view invoked asset route");
      circle_view = (fun params store -> Read.view_rpc ~authenticated:false
        ~trusted:[] ~profile:selected ~store ~ledger params);
      circle_view_auth = (fun params store -> Read.view_rpc ~authenticated:true
        ~trusted:[] ~profile:selected ~store ~ledger params);
    } in
    let params auth method_name call_params =
      if auth then
        let subject = Auth.view_subject ~method_name ~call_params ~include_storage:false in
        let signature = Auth.message ~op:"octra_circle_view" ~circle_id:parent ~addr:owner ~subject
          |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
        `List [`String parent; `String method_name; `List call_params;
          `String owner; `String public_key; `String signature; `Bool false]
      else `List [`String parent; `String method_name; `List call_params] in
    List.iter (fun (route, auth) ->
      V.Contract.run_s (S.deploy_circle store
        {info with C.privacy_class = if auth then C.Scoped else C.Public});
      let read method_name args = Lwt_main.run ((List.assoc route routes)
        (params auth method_name args) store) in
      expect "circle byte control changed"
        (field "result" (read "copy" [`String "abcd"; `Int 8]) = `String "1024");
      expect "circle integer control changed"
        (field "result" (read "square" [`Int 4]) = `String "7");
      let refused reason = function
        | Error error when error.Octra_core.Rpc.code = -32000 -> ()
        | Error _ -> failwith "circle work became technical error"
        | Ok _ -> failwith reason in
      refused "circle concat unpriced"
        (read "copy" [`String (String.make 128 'a'); `Int 18]);
      refused "circle effort unarmed" (read "square" [`Int 20]);
      expect "circle view did not recover"
        (field "result" (read "square" [`Int 4]) = `String "7"))
      ["circle_view", false; "octra_circleView", false;
       "circle_view_auth", true; "octra_circleViewAuth", true])

let () =
  if Array.exists (String.equal "--fhe") Sys.argv then begin
    Test_workspace.with_dir "view_fhe" fhe_view;
    print_endline "event = view_fhe status = pass";
    exit 0
  end;
  Test_workspace.with_dir "view_bytes" view_bytes;
  if Array.exists (String.equal "--bytes") Sys.argv then begin
    print_endline "event = circle_view_bytes status = pass";
    exit 0
  end;
  Test_workspace.with_dir "storage_root" storage_root;
  Test_workspace.with_dir "object_root" object_root;
  Test_workspace.with_dir "policy_root" policy_root;
  Test_workspace.with_dir "asset_root" asset_root;
  Test_workspace.with_dir "binding_root" View_binding.run;
  Test_workspace.with_dir "transport_root" View_transport.run;
  Test_workspace.with_dir "circle_root" circle_root;
  if not (Array.exists (String.equal "--circle") Sys.argv) then begin
    Test_workspace.with_dir "view_fhe" fhe_view;
    List.iter (fun mode -> Test_workspace.with_dir "view_root" (run mode))
      Octra_core.Rule_graph.[Prior; Active]
  end;
  print_endline "event = view_root status = pass"