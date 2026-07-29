(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

let abi ~ctype ~address ~owner ~version =
  Yojson.Safe.to_string (`Assoc [
    "contract_type", `String ctype;
    "address", `String address;
    "deployer", `String owner;
    "version", `String version;
  ])

let stage_deploy store (deploy : Program_journal.deploy) =
  run
    (Octra_core.Store_irmin.deploy_contract store
       ~address:deploy.Program_journal.address
       ~code_hash:deploy.code_hash
       ~version:Oct_compile.lang_version
       ~owner:deploy.owner
       ~ctype:deploy.ctype
       ~admission:deploy.admission
       ~bytecode_b64:deploy.bytecode_b64);
  run
    (Octra_core.Store_irmin.save_contract_abi
       store
       deploy.address
       (abi
          ~ctype:deploy.ctype
          ~address:deploy.address
          ~owner:deploy.owner
          ~version:Oct_compile.lang_version))

let stage_upgrade store (upgrade : Program_journal.upgrade) =
  match
    run
      (Octra_core.Store_irmin.upgrade_contract
         store
         ~address:upgrade.Program_journal.address
         ~expected_code_hash:upgrade.expected_code_hash
         ~code_hash:upgrade.code_hash
         ~version:upgrade.version
         ~owner:upgrade.owner
         ~ctype:upgrade.ctype
         ~admission:upgrade.admission
         ~bytecode_b64:upgrade.bytecode_b64)
  with
  | Error error -> failwith error
  | Ok () ->
    run
      (Octra_core.Store_irmin.save_contract_abi
         store
         upgrade.address
         (abi
            ~ctype:upgrade.ctype
            ~address:upgrade.address
            ~owner:upgrade.owner
            ~version:upgrade.version))

let stage_storage store (address, storage) =
  run (Octra_core.Store_irmin.save_contract_storage store address storage)

let stage store journal =
  Program_journal.deploys journal |> List.iter (stage_deploy store);
  Program_journal.upgrades journal |> List.iter (stage_upgrade store);
  Program_journal.storage_entries journal |> List.iter (stage_storage store)