(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Manifest = Octra_bootstrap.State_sync_manifest
module Checkpoint = Octra_bootstrap.State_sync_checkpoint
module Anchor = Octra_bootstrap.Sync_anchor
module C = Octra_consensus.C_types

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let check reason value =
  if not value then failwith reason

let write path body =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output body)

let read path = In_channel.with_open_bin path In_channel.input_all

let run ~command ~root ~validators certificate =
  let draft = Filename.concat root "draft.json" in
  let authority = Filename.concat root "authority" in
  let signature = Filename.concat root "signature.json" in
  let output = Filename.concat root "assembled.json" in
  let log = Filename.concat root "assemble.log" in
  Manifest.write_json draft (Manifest.draft_json Manifest.{
    checkpoint = certificate.checkpoint;
    checkpoint_hash = certificate.checkpoint_hash;
    manifest = certificate.manifest;
    manifest_hash = certificate.manifest_hash;
  });
  let exporter = List.hd certificate.exporter_signatures in
  Manifest.write_json signature (Checkpoint.signature_json exporter);
  let encoded = Manifest.finality certificate |> Option.get in
  check "cli input does not exceed prior limit" (String.length encoded > 4_000_000);
  let signers flag = validators.C.validators |> List.concat_map (fun member ->
    [flag; member.C.address ^ ":" ^ member.pubkey]) in
  let args = Array.of_list ([command; "--command"; "assemble-finalized";
    "--draft"; draft; "--finality"; authority; "--exporter-signature"; signature;
    "--output"; output] @ signers "--validator" @ signers "--exporter") in
  let invoke () =
    let descriptor = Unix.openfile log [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    Fun.protect ~finally:(fun () -> Unix.close descriptor) (fun () ->
      let pid = Unix.create_process command args Unix.stdin descriptor descriptor in
      let rec wait () =
        try snd (Unix.waitpid [] pid) with
        | Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
      wait ()) in
  let accept name =
    let status = invoke () in
    check ("cli refused supported authority: " ^ name ^ ": " ^ read log)
      (status = Unix.WEXITED 0);
    let assembled = Manifest.load_certificate output |> get in
    check "cli changed certificate" (assembled = certificate);
    ignore (Manifest.verify_certificate ~validator_set:validators
      ~exporter_set:validators assembled |> get) in
  write log "";
  write authority encoded;
  accept "raw";
  Manifest.write_json authority (`String encoded);
  accept "json";
  write authority (String.make (Manifest.manifest_limit - String.length encoded) ' ' ^ encoded);
  accept "file limit";
  let preserved = read output in
  let reject reason =
    check "cli accepted invalid authority" (invoke () = Unix.WEXITED 1);
    check "cli refusal reason differs" (read log = "error = " ^ reason ^ "\n");
    check "cli replaced certificate after refusal" (read output = preserved) in
  write authority (String.make (Manifest.manifest_limit + 1) ' ');
  reject "input exceeds size limit";
  let anchor = Anchor.decode encoded |> get in
  let steps = List.mapi (fun index (step : Anchor.step) ->
    if index > 0 then step else
      let finalize = C.{ step.finalize with precommits = List.map (fun (vote : C.vote) ->
        { vote with signature = String.make 64 '\000' }) step.finalize.precommits } in
      Anchor.{ step with finalize }) (Anchor.steps anchor) in
  let damaged = Anchor.make ~steps ~finalize:(Anchor.finality anchor)
    ~validator_set:(Anchor.validator_set anchor) |> Anchor.encode in
  write authority damaged;
  reject "finality qc signature";
  write authority encoded;
  Manifest.write_json signature (Checkpoint.signature_json {
    exporter with signature = Base64.encode_exn (String.make 64 '\000') });
  let bad = Manifest.{ certificate with exporter_signatures = [
    { exporter with signature = Base64.encode_exn (String.make 64 '\000') }] } in
  let reason = match Manifest.verify_certificate ~validator_set:validators
    ~exporter_set:validators bad with
    | Error reason -> reason
    | Ok _ -> failwith "damaged exporter signature verified" in
  reject reason;
  Manifest.write_json signature (Checkpoint.signature_json exporter);
  accept "retry";
  Printf.printf "event = sync_cli bytes = %d status = pass\n%!" (String.length encoded)