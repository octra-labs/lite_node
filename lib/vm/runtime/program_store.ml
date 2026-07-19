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


let run task =
  match Lwt.state task with
  | Lwt.Return value -> value
  | Lwt.Fail error -> raise error
  | Lwt.Sleep ->
    let result = ref None in
    Lwt.on_any task
      (fun value -> result := Some (Ok value))
      (fun error -> result := Some (Error error));
    let rec pump steps =
      if steps > 100000 then failwith "program store I/O timeout"
      else
        match !result with
        | Some (Ok value) -> value
        | Some (Error error) -> raise error
        | None ->
          ignore (Lwt_engine.iter true);
          pump (steps + 1)
    in
    pump 0

let abi deploy =
  Yojson.Safe.to_string (`Assoc [
    "contract_type", `String deploy.Program_journal.ctype;
    "address", `String deploy.address;
    "deployer", `String deploy.owner;
    "version", `String Oct_compile.lang_version;
  ])

let stage_deploy store deploy =
  run
    (Octra_core.Store_irmin.deploy_contract store
       ~address:deploy.Program_journal.address
       ~code_hash:deploy.code_hash
       ~version:Oct_compile.lang_version
       ~owner:deploy.owner
       ~ctype:deploy.ctype
       ~bytecode_b64:deploy.bytecode_b64);
  run (Octra_core.Store_irmin.save_contract_abi store deploy.address (abi deploy))

let stage_storage store (address, storage) =
  run (Octra_core.Store_irmin.save_contract_storage store address storage)

let stage store journal =
  Program_journal.deploys journal |> List.iter (stage_deploy store);
  Program_journal.storage_entries journal |> List.iter (stage_storage store)