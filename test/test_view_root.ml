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
      expect "standard key view refused" (Result.is_ok answer);
      let encoded = field "result" answer |> Yojson.Safe.Util.to_string in
      let cipher = Base64.decode_exn encoded |> Bytes.of_string |> Pvac_ffi.deserialize_cipher in
      expect "private prediction differs" (Pvac_ffi.dec_value native secret cipher = 105L);
      expect "nested prediction differs" (field "result" (query "nested") = field "result" answer);
      expect "nested call reset effort" (Result.is_error (query "twice"));
      expect "private view changed state" (Lwt_main.run (S.get_commit_hash store) = root);
      expect "private view retained slot" (not !(V.Contract_rpc.view_active)))
      [1_662_998; 1_662_999])

let () =
  Test_workspace.with_dir "view_fhe" fhe_view;
  List.iter (fun mode -> Test_workspace.with_dir "view_root" (run mode))
    Octra_core.Rule_graph.[Prior; Active];
  print_endline "event = view_root status = pass"