(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Exec = Octra_circle_runtime.Circle_exec
module Storage = Octra_circle_runtime.Circle_runtime_storage
module Transcript = Octra_core.Circle_hfhe_transcript
module Wire = Octra_core.Circle_wasm_codec

let need value reason =
  if not value then failwith reason

let table entries =
  let values = Hashtbl.create (List.length entries) in
  List.iter (fun (key, value) -> Hashtbl.replace values key value) entries;
  values

let cell_key prefix suffix =
  prefix ^ String.make 64 'a' ^ ":" ^ suffix

let expect_cell_guard before_tbl after_tbl =
  match
    Storage.validate_runtime_storage_delta
      ~proof_mode:Octra_core.Rule_graph.Active
      before_tbl
      after_tbl
  with
  | Error ("circle_runtime_cell_write_denied", _, _) -> ()
  | Error (code, _, _) -> failwith code
  | Ok () -> failwith "circle cell write accepted"

let rec remove_path path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name -> remove_path (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path

let commit_guard balance before_tbl changed_tbl =
  let data = Filename.concat (Sys.getcwd ()) "runtime_data" in
  let scope = Filename.concat data "wasm-guard" in
  if not (Sys.file_exists data) then Unix.mkdir data 0o700;
  if not (Sys.file_exists scope) then Unix.mkdir scope 0o700;
  let path = Filename.concat scope (string_of_int (Unix.getpid ())) in
  remove_path path;
  let store = Lwt_main.run (Octra_core.Store_irmin.open_store ~fresh:true path) in
  let circle_id = "circle-test" in
  let zero = Octra_core.Circles.zero_hash_hex in
  let info : Octra_core.Circles.circle_info = {
    circle_id;
    runtime = Octra_core.Circles.Octb;
    version = 1L;
    owner = "owner";
    code_hash = zero;
    stable_root = zero;
    assets_root = zero;
    privacy_class = Octra_core.Circles.Public;
    browser_mode = Octra_core.Circles.Gateway_allowed;
    resource_mode = Octra_core.Circles.Public_resources;
    policy_hash = None;
    members_root = None;
    export_policy = None;
    limits = Octra_core.Circles.default_limits;
  } in
  let receipt : Octra_vm.Contract.exec_result = {
    success = true;
    return_value = None;
    effort_used = 0;
    events = [];
    error = None;
    storage_writes = 1;
  } in
  let result storage_tbl : Exec.call_result = {
    receipt;
    storage_tbl;
    baseline_storage_tbl = before_tbl;
    spawns = [];
    assets = [];
    encrypted_assets = [];
    caller = "caller";
    tx_hash = zero;
    hfhe_binding = {
      circle_id;
      code_hash = zero;
      stable_root = zero;
      public_reads_hash = zero;
      context_hash = zero;
      transcript = [];
    };
  } in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      remove_path path)
    (fun () ->
      Lwt_main.run (Octra_core.Store_irmin.deploy_circle store info);
      ignore
        (Lwt_main.run
           (Octra_core.Store_irmin.save_circle_stable_storage
              store circle_id before_tbl));
      begin
        match
          Lwt_main.run
            (Exec.commit_call_result
               ~proof_mode:Octra_core.Rule_graph.Active
               store circle_id (result changed_tbl))
        with
        | Error reason ->
          need
            (String.starts_with
               ~prefix:"circle runtime attempted to write reserved stable key "
               reason)
            "circle commit guard reason changed"
        | Ok () -> failwith "circle commit guard accepted"
      end;
      let stored =
        Lwt_main.run
          (Octra_core.Store_irmin.load_circle_stable_storage store circle_id)
      in
      match stored with
      | Error reason -> failwith reason
      | Ok values ->
        need
          (Hashtbl.find_opt values balance = Hashtbl.find_opt before_tbl balance)
          "circle commit guard changed storage")

let () =
  need
    (String.equal Transcript.consensus_id "receipt_mode:amount_link_v1")
    "circle hfhe consensus id changed";
  let prior_context = Exec.hfhe_context_hash ~strict:false [] [] None in
  let active_context = Exec.hfhe_context_hash ~strict:true [] [] None in
  need
    (String.equal
       prior_context
       "971ef73959dc93c466ddf9dca85886299a1df18a7808a86695283471e45fe895")
    "prior hfhe context hash changed";
  need
    (not (String.equal prior_context active_context))
    "hfhe proof modes share a context hash";
  need
    (String.equal
       Storage.consensus_id
       "circle_storage:cell_owner:standard")
    "circle storage consensus id changed";
  begin
    match Exec.vm_response_value (Some (Wire.Resp_int "invalid")) with
    | Error "wasm response integer is invalid" -> ()
    | Error reason -> failwith reason
    | Ok _ -> failwith "invalid wasm integer accepted"
  end;
  begin
    match Exec.vm_response_value (Some (Wire.Resp_int "42")) with
    | Ok (Some (Octra_vm.Contract_vm.VInt value)) ->
      need (Z.equal value (Z.of_int 42)) "wasm integer changed"
    | Ok _ -> failwith "wasm integer type changed"
    | Error reason -> failwith reason
  end;
  let cleared = ref false in
  Lwt_main.run
    (Exec.run_preview_prefetch
       ~clear:(fun () -> cleared := true)
       (fun () -> Lwt.fail Exit));
  need !cleared "preview prefetch remained inflight";
  let balance = cell_key "balance_cell:" "ciphertext_commitment" in
  let register = cell_key "register_cell:" "ciphertext_commitment" in
  let first_commitment = Base64.encode_exn (String.make 32 '\000') in
  let second_commitment = Base64.encode_exn (String.make 32 '\001') in
  let changed_commitment = Base64.encode_exn (String.make 32 '\002') in
  let before_tbl =
    table [
      balance, first_commitment;
      register, second_commitment;
      "user:key", "value";
    ] in
  begin
    match
      Storage.validate_runtime_storage_delta
        ~proof_mode:Octra_core.Rule_graph.Active
        before_tbl
        (Hashtbl.copy before_tbl)
    with
    | Ok () -> ()
    | Error (code, _, _) -> failwith code
  end;
  let changed_balance = Hashtbl.copy before_tbl in
  Hashtbl.replace changed_balance balance changed_commitment;
  expect_cell_guard before_tbl changed_balance;
  commit_guard balance before_tbl changed_balance;
  let added_balance = Hashtbl.copy before_tbl in
  let new_balance = cell_key "balance_cell:" "proof" in
  Hashtbl.replace added_balance new_balance changed_commitment;
  expect_cell_guard before_tbl added_balance;
  let removed_register = Hashtbl.copy before_tbl in
  Hashtbl.remove removed_register register;
  expect_cell_guard before_tbl removed_register;
  let changed_user = Hashtbl.copy before_tbl in
  Hashtbl.replace changed_user "user:key" "changed";
  begin
    match
      Storage.validate_runtime_storage_delta
        ~proof_mode:Octra_core.Rule_graph.Active
        before_tbl
        changed_user
    with
    | Ok () -> ()
    | Error (code, _, _) -> failwith code
  end;
  begin
    match
      Storage.validate_runtime_storage_delta
        ~proof_mode:Octra_core.Rule_graph.Prior
        before_tbl
        changed_balance
    with
    | Ok () -> ()
    | Error (code, _, _) -> failwith code
  end;
  Printf.printf "status = pass test = wasm_guard\n%!"