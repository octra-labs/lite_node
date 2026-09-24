(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Intent = Octra_core.Validator_intent
module Control = Octra_core.Validator_control

let expect name condition =
  if not condition then failwith ("validator_control: " ^ name)

let unwrap = function Ok value -> value | Error reason -> failwith reason

let rec wait_child child =
  try snd (Unix.waitpid [] child) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait_child child

let key byte =
  Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 byte)
  |> function Ok key -> key | Error _ -> failwith "test key"

let private_key = key 'i'
let privkey = Base64.encode_exn (String.make 32 'i')
let pubkey = Mirage_crypto_ec.Ed25519.pub_of_priv private_key
  |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn

let identity = Intent.{
  chain_id = "octra-control-test";
  address = Octra_core.Crypto.Address.address_from_pubkey pubkey;
  pubkey;
  bonded_epoch = 9L;
}

let check_codec () =
  let value = unwrap (Intent.create identity ~privkey) in
  let encoded = Intent.encode value in
  let decoded = unwrap (Intent.decode encoded) in
  expect "codec round trip" (Intent.encode decoded = encoded);
  expect "same registration" (Intent.applies identity decoded = Ok true);
  expect "next registration" (Intent.applies { identity with bonded_epoch = 10L } decoded = Ok false);
  expect "ahead registration"
    (Result.is_error (Intent.applies { identity with bonded_epoch = 8L } decoded));
  expect "different chain"
    (Result.is_error (Intent.applies { identity with chain_id = "other" } decoded));
  expect "different key"
    (Result.is_error (Intent.create identity ~privkey:(Base64.encode_exn (String.make 32 'j'))));
  let fields = match Yojson.Safe.from_string encoded with
    | `Assoc fields -> fields | _ -> failwith "test fields" in
  let changes = [
    ("chain_id", `String "other");
    ("address", `String "octOther");
    ("pubkey", `String "");
    ("bonded_epoch", `String "10");
    ("bonded_epoch", `String "09");
    ("bonded_epoch", `String "-1");
    ("signature", `String (Base64.encode_exn (String.make 64 's')));
  ] in
  List.iter (fun (name, value) ->
    let changed = (name, value) :: List.remove_assoc name fields in
    expect ("modified " ^ name)
      (Result.is_error (Intent.decode (Yojson.Safe.to_string (`Assoc changed))))) changes;
  List.iter (fun raw -> expect "invalid encoding" (Result.is_error (Intent.decode raw)))
    ["null"; "[]"; "{}"; String.make 4_097 'x';
     Yojson.Safe.to_string (`Assoc (("chain_id", `String "other") :: fields))]

let write path text =
  let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
  let channel = Unix.out_channel_of_descr fd in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let check_actor control =
  let module Actor = Octra_node_runtime.Set_actor in
  let waiting, release = Lwt.wait () in
  let entered = ref false in
  let sent = ref 0 in
  let faults = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> { epoch = 100L; active = false; bonded = Ok true });
    read = (fun ~epoch:_ ->
      entered := true;
      waiting);
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ ->
      Control.guard control identity (fun () -> incr sent; Ok ())
      |> Result.map_error (fun error -> Actor.Control, error) |> Lwt.return);
    warn = (fun fault _ -> faults := fault :: !faults);
  } in
  ignore (Actor.notify actor ~epoch:100L None);
  Lwt_main.run (Lwt_unix.sleep 0.01);
  expect "actor waiting for head read" !entered;
  ignore (unwrap (Control.request control identity ~privkey));
  Lwt.wakeup_later release (Ok Octra_core.Set_fold.{ marked = []; pulse = None });
  Lwt_main.run (Lwt_unix.sleep 0.01);
  expect "exit during read prevents signing" (!sent = 0);
  expect "exit during read is a control refusal" (!faults = [Actor.Control]);
  Lwt_main.run (Actor.shutdown actor);
  unwrap (Control.cancel control identity ~privkey)

let check_storage () =
  (try Unix.mkdir "runtime_data" 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let data_dir = "runtime_data/validator_control_" ^ string_of_int (Unix.getpid ()) in
  Unix.mkdir data_dir 0o700;
  let control = Control.create ~data_dir in
  let rpc_control () =
    let snapshot = Octra_node_runtime.Status_read_rpc.{
      head_epoch = 10; state_root = ""; chain_id = identity.chain_id;
      head_proposal_id = None;
      config_hash = ""; duty = None; sets = (None, None);
      candidate = Some Octra_core.Validator_admission.{
        address = identity.address; pubkey = Base64.decode_exn pubkey;
        bond = Z.one; bonded_epoch = identity.bonded_epoch;
        ready_epoch = None; exit_epoch = None;
      };
    } in
    Octra_node_runtime.Status_read_rpc.local_control
      ~data_dir ~snapshot ~address:identity.address ~pubkey
  in
  let module Tx = Octra_core.Transaction in
  let module Staging = Octra_core.Tx_staging in
  Staging.clear ();
  let transfer = Tx.{
    from = identity.address; to_ = identity.address; amount = Z.one; nonce = 1;
    ou = Z.of_int 1_000; timestamp = 1.; signature = ""; public_key = Some pubkey;
    message = None; op_type = Standard; encrypted_data = None;
  } |> fun tx -> Tx.sign_with_privkey tx privkey in
  ignore (Staging.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000, 0))
    transfer |> unwrap);
  let queued = Tx.hash transfer in
  expect "no intent" (Control.status control identity = Ok None);
  expect "RPC reports no pause"
    (Yojson.Safe.Util.member "exit_requested" (rpc_control ()) = `Bool false);
  let calls = ref 0 in
  let send () = incr calls; Ok () in
  unwrap (Control.guard control identity send);
  let id = unwrap (Control.request control identity ~privkey) in
  expect "idempotent request" (Control.request control identity ~privkey = Ok id);
  let restarted = Control.create ~data_dir in
  expect "RPC acknowledges exact intent"
    (Yojson.Safe.Util.member "intent_id" (rpc_control ()) = `String id);
  expect "survives restart" (Control.status restarted identity = Ok (Some id));
  expect "suppresses only guarded work" (Result.is_error (Control.guard restarted identity send));
  expect "no new duty" (!calls = 1);
  expect "ordinary transfer remains queued" (Staging.find_by_hash queued <> None);
  expect "old registration ignored"
    (Control.status restarted { identity with bonded_epoch = 10L } = Ok None);
  expect "wrong registration refuses"
    (Result.is_error (Control.status restarted { identity with bonded_epoch = 8L }));
  unwrap (Control.cancel control identity ~privkey);
  check_actor control;
  let lock = Filename.concat data_dir "validator-control/lock" in
  List.iter (fun mode ->
    Unix.chmod lock mode;
    expect "RPC exposes unusable lock"
      (Yojson.Safe.Util.member "error" (rpc_control ()) <> `Null);
    expect "status rejects unusable lock" (Result.is_error (Control.status control identity));
    Unix.chmod lock 0o600) [0o644; 0o400; 0o000];
  unwrap (Control.guard control identity (fun () ->
    expect "same owner reentry refuses"
      (Result.is_error (Control.request control identity ~privkey));
    let child = Unix.fork () in
    if child = 0 then begin
      let result = Control.request (Control.create ~data_dir) identity ~privkey in
      exit (if Result.is_error result then 0 else 1)
    end;
    expect "request cannot overtake signing" (wait_child child = Unix.WEXITED 0);
    send ()));
  let path = Filename.concat data_dir "validator-control/exit.json" in
  let next = Filename.concat data_dir "validator-control/exit.next" in
  write next "interrupted write";
  ignore (unwrap (Control.request control identity ~privkey));
  expect "orphan write recovered" (not (Sys.file_exists next));
  Unix.chmod path 0o644;
  expect "public intent refuses" (Result.is_error (Control.guard control identity send));
  Unix.chmod path 0o600;
  write path "{}";
  expect "RPC reports invalid intent"
    (Yojson.Safe.Util.member "error" (rpc_control ()) <> `Null);
  expect "invalid intent refuses" (Result.is_error (Control.guard control identity send));
  Unix.unlink path;
  write next "not an intent";
  Unix.symlink next path;
  expect "symlink refuses" (Result.is_error (Control.guard control identity send));
  Unix.unlink path;
  Unix.unlink next;
  unwrap (Control.guard control identity send);
  expect "cancel permits duty" (!calls = 3);
  Unix.unlink (Filename.concat data_dir "validator-control/lock");
  Unix.rmdir (Filename.concat data_dir "validator-control");
  Unix.rmdir data_dir;
  Staging.clear ()

let check_diagnostics () =
  let data_dir = Test_workspace.unique_dir "duty_control" in
  let control = Control.create ~data_dir in
  unwrap (Control.guard control identity (fun () -> Ok ()));
  let directory = Filename.concat data_dir "validator-control" in
  Unix.chmod directory 0o755;
  let warnings = ref [] in
  let faults = ref [] in
  let reads = ref 0 in
  let sent = ref 0 in
  let module Actor = Octra_node_runtime.Set_actor in
  let start () = Actor.create Actor.{
    sample = (fun () -> {
      epoch = 100L; active = false;
      bonded = Control.status control identity |> Result.map Option.is_none;
    });
    read = (fun ~epoch:_ ->
      incr reads;
      Lwt.return_ok Octra_core.Set_fold.{ marked = []; pulse = None });
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ ->
      Control.guard control identity (fun () -> incr sent; Ok ())
      |> Result.map_error (fun error -> Actor.Control, error) |> Lwt.return);
    warn = (fun fault reason ->
      faults := !faults @ [fault];
      warnings := !warnings @ [reason]);
  } in
  let wake actor =
    expect "notice accepted" (Actor.notify actor ~epoch:100L None = Actor.Accepted);
    Lwt_main.run (Lwt_unix.sleep 0.01)
  in
  let actor = start () in
  for _ = 1 to 20 do wake actor done;
  expect "private directory failure reported once"
    (!warnings = ["validator control directory is not private"]);
  expect "invalid control prevents reads and sends" (!reads = 0 && !sent = 0);
  Unix.chmod directory 0o700;
  ignore (unwrap (Control.request control identity ~privkey));
  wake actor;
  expect "requested pause is not an error" (List.length !warnings = 1 && !sent = 0);
  Unix.chmod directory 0o755;
  wake actor;
  expect "same error after recovery reported" (List.length !warnings = 2);
  Lwt_main.run (Actor.shutdown actor);
  let actor = start () in
  wake actor;
  expect "restart reports existing problem" (List.length !warnings = 3);
  Unix.chmod directory 0o700;
  let path = Filename.concat directory "exit.json" in
  Unix.chmod path 0o644;
  wake actor;
  expect "changed problem is reported"
    (List.nth !warnings 3 = "invalid validator exit intent file");
  Unix.chmod path 0o600;
  unwrap (Control.cancel control identity ~privkey);
  wake actor;
  expect "repaired control permits duty" (!reads = 1 && !sent = 1);
  let lock = Filename.concat directory "lock" in
  Unix.chmod lock 0o644;
  for _ = 1 to 20 do wake actor done;
  expect "lock failure reported once" (List.length !warnings = 5);
  expect "control failures classified" (List.for_all ((=) Actor.Control) !faults);
  expect "control event is not send event"
    (Actor.event Actor.Control = "set_actor_control_failed"
     && Actor.event Actor.Send = "set_actor_send_failed");
  Unix.chmod lock 0o600;
  Lwt_main.run (Actor.shutdown actor);
  Test_workspace.remove data_dir

let check_bonded () =
  let module Status = Octra_node_runtime.Status_read_rpc in
  let module Duty = Octra_node_runtime.Set_control in
  Test_workspace.with_dir "duty_bonded" (fun data_dir ->
    let control = Control.create ~data_dir in
    let entry = Octra_core.Validator_admission.{
      address = identity.address; pubkey = Base64.decode_exn pubkey;
      bond = Z.one; bonded_epoch = identity.bonded_epoch;
      ready_epoch = None; exit_epoch = None;
    } in
    let snapshot = Status.{
      head_epoch = 10; state_root = ""; chain_id = identity.chain_id;
      config_hash = ""; candidate = Some entry; duty = None; sets = (None, None);
      head_proposal_id = None;
    } in
    let read = Duty.bonded ~control ~address:identity.address ~pubkey in
    expect "read errors retained" (read (Error "read failed") = Error "read failed");
    expect "absent bond pauses" (read (Ok { snapshot with candidate = None }) = Ok false);
    expect "bond permits duty" (read (Ok snapshot) = Ok true);
    ignore (unwrap (Control.request control identity ~privkey));
    expect "exit intent pauses" (read (Ok snapshot) = Ok false);
    let newer = { entry with bonded_epoch = 10L } in
    expect "old intent cannot pause new bond"
      (read (Ok { snapshot with candidate = Some newer }) = Ok true);
    let exiting = { entry with exit_epoch = Some 10L } in
    expect "committed exit pauses"
      (read (Ok { snapshot with candidate = Some exiting }) = Ok false);
    Unix.chmod (Filename.concat data_dir "validator-control/lock") 0o644;
    expect "lock error retained" (Result.is_error (read (Ok snapshot))))

let () =
  check_codec ();
  check_storage ();
  check_diagnostics ();
  check_bonded ();
  print_endline "status = pass test = validator_control"