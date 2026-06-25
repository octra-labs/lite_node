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


type source = {
  name : string;
  root : C_light_root.t;
  validator_set : C_light_validator_set.t;
  epochs : C_light_epoch.t list;
}

type ok = {
  root : C_light_root.t;
  signers : string list;
  sources : string list;
  epoch_count : int;
}

let bind r f =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let ( let* ) = bind

let same_scheduled a b =
  match a, b with
  | None, None -> true
  | Some x, Some y ->
    x.C_light_validator_set.activate_epoch = y.C_light_validator_set.activate_epoch
    && x.validators = y.validators
  | _ -> false

let same_validator_set a b =
  a.C_light_validator_set.chain_id = b.C_light_validator_set.chain_id
  && a.config_hash = b.config_hash
  && a.validator_set_hash = b.validator_set_hash
  && a.n = b.n
  && a.f = b.f
  && a.quorum = b.quorum
  && a.validators = b.validators
  && same_scheduled a.scheduled b.scheduled

let first = function
  | [] -> None
  | x :: _ -> Some x

let source_names sources =
  List.map (fun s -> s.name) sources

let verify_validator_sets (sources : source list) =
  match first sources with
  | None -> Error "no sources"
  | Some head ->
    if not (C_light_validator_set.verify head.validator_set) then
      Error ("invalid validator set: " ^ head.name)
    else
      let bad =
        sources
        |> List.find_opt (fun s ->
          not (C_light_validator_set.verify s.validator_set)
          || not (same_validator_set head.validator_set s.validator_set)) in
      match bad with
      | None -> Ok head.validator_set
      | Some s -> Error ("validator set mismatch: " ^ s.name)

let verify_epochs root (sources : source list) =
  match sources with
  | [] -> Error "no sources"
  | head :: rest ->
    let count = List.length head.epochs in
    let bad_count =
      rest
      |> List.find_opt (fun s -> List.length s.epochs <> count) in
    begin match bad_count with
    | Some s -> Error ("epoch chain length mismatch: " ^ s.name)
    | None ->
      let bad =
        sources
        |> List.find_opt (fun s ->
          not (C_light_client.verify_epoch_chain ~root s.epochs)) in
      match bad with
      | None -> Ok count
      | Some s -> Error ("epoch chain verification failed: " ^ s.name)
    end

let verify (sources : source list) =
  let* validator_set = verify_validator_sets sources in
  let roots = List.map (fun (s : source) -> s.root) sources in
  match C_light_client.verify_root_quorum ~validator_set_proof:validator_set roots with
  | None -> Error "signed root quorum failed"
  | Some quorum ->
    let* epoch_count = verify_epochs quorum.root sources in
    Ok {
      root = quorum.root;
      signers = quorum.signers;
      sources = source_names sources;
      epoch_count;
    }