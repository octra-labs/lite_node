(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Protocol = Octra_consensus.Resource_compute_protocol
module Certificate = Octra_consensus.Resource_compute_certificate

module Validators = Octra_consensus.C_types

type outcome = Complete | Stopped of int | Exhausted

type measured = {
  request_id : string;
  output_hash : string;
  output_bytes : int;
  trace_root : string;
  steps : int64;
  operations : int64;
  outcome : outcome;
}

let measure ~(request : Certificate.request) ~output ~trace_root ~steps ~operations ~outcome =
  let output_bytes = String.length output in
  if not (Certificate.request_shape request) then Error "compute bill request refused"
  else if output_bytes = 0 || output_bytes > request.max_output_bytes then
    Error "compute bill output size refused"
  else if String.length trace_root <> 32 || steps <= 0L || operations <= 0L then
    Error "compute bill work refused"
  else if Option.fold ~none:true ~some:(fun fuel -> steps > Int64.of_int fuel) request.fuel_limit then
    Error "compute bill work exceeds fuel"
  else if (match outcome with Complete | Exhausted -> false
      | Stopped code -> code <= 0 || code > Int32.to_int Int32.max_int) then
    Error "compute bill stop code refused"
  else if (match outcome with Complete -> false
      | Exhausted -> output <> Yojson.Safe.to_string (`Assoc ["compute_stop", `String "fuel"])
      | Stopped code -> output <> Yojson.Safe.to_string (`Assoc ["compute_stop", `Int code])) then
    Error "compute bill stop output differs"
  else Ok {request_id = Certificate.request_id request;
    output_hash = Protocol.output_hash output; output_bytes;
    trace_root; steps; operations; outcome}

type t = {
  header : Protocol.header;
  measured : measured;
  root : string;
  charged : Z.t;
  refund : Z.t;
  payments : (string * Z.t) list;
}

type fault = Rejected of string | Unavailable of string
type funding = Due of int64 | Paid of int64
type verifier = store:Store_irmin.t -> epoch:int64 ->
  funded:(Protocol.header -> (funding, fault) result Lwt.t) ->
  string -> (t list, fault) result Lwt.t

let max_wire = 262_144
let max_fuel = 5_000_000
let model_limit = 67_108_864L

let resource_limits ~fuel ~memory ~guest =
  match fuel, memory, guest with
  | Some fuel, Some memory, Some guest when fuel > 0 && fuel <= max_fuel
      && memory > 0 && Int64.of_int memory <= model_limit && guest > 0 && guest <= memory -> Ok ()
  | _ -> Error "compute payment resource limits"

let resources (request : Certificate.request) =
  resource_limits ~fuel:request.fuel_limit ~memory:request.memory_bytes ~guest:request.guest_bytes

let consensus_id = Printf.sprintf
  "signed_reserve_8192:fund_10000:return_1000:signed_pay_expiry:payer:nonce:settle_separate:pay_wire_%d:fuel_%d:model_%Ld:io_16:base_10000:wire_2:state_inputs_65:pin_16:entry_32:fund_lane:paid_refusal:session_replay:paid_epoch:paid_root:record_set:charged_receipt:graph_json"
  max_wire max_fuel model_limit

let encode_request ~fuel records =
  if fuel <= 0 || fuel > max_fuel || records = [] || List.length records > 64 then
    invalid_arg "compute payment limits";
  let raw = Octra_net.Oce1.encode (fun buffer ->
    Octra_net.Oce1.put_u32 buffer (Int32.of_int fuel);
    Octra_net.Oce1.put_u16 buffer (List.length records);
    List.iter (Octra_net.Oce1.put_string buffer) records) |> Base64.encode_exn in
  if String.length raw > max_wire then invalid_arg "compute payment size";
  raw

let decode_request raw =
  if String.length raw > max_wire then Error "compute payment size"
  else try
    let decoded = Base64.decode_exn raw in
    Octra_net.Oce1.decode (fun cursor ->
      let fuel = Int32.to_int (Octra_net.Oce1.get_u32 cursor) in
      let count = Octra_net.Oce1.get_u16 cursor in
      if fuel <= 0 || fuel > max_fuel || count <= 0 || count > 64 then
        Error "compute payment limits"
      else Ok (fuel, List.init count (fun _ ->
        Octra_net.Oce1.get_string_bounded ~max:max_wire cursor))) decoded
  with Invalid_argument _ | Failure _ -> Error "compute payment encoding"

let fee raw =
  match decode_request raw with
  | Error _ -> Z.of_int 10_000_000
  | Ok (fuel, _) -> Z.add (Z.div (Z.of_int64 model_limit) (Z.of_int 16))
      (Z.of_int (10_000 + fuel + 2 * String.length raw))

let verify ~epoch ~(header : Protocol.header) ~validators ~selected ~votes ~measured =
  let request = header.request in
  let weight_of = Validators.weight_of_addr validators in
  let verify_signature (vote : Certificate.vote) =
    match Validators.pubkey_of_addr validators vote.node_id with
    | None -> false
    | Some pubkey_raw -> Octra_consensus.C_hash.verify_ed25519 ~pubkey_raw
        ~msg:(Certificate.vote_sign_bytes vote) ~signature:vote.signature in
  let total = List.fold_left (fun sum node ->
    Z.add sum (Option.value ~default:Z.zero (weight_of node))) Z.zero selected in
  if request.validator_set_root <> Octra_consensus.C_config.validator_set_hash validators then
    Error "compute bill validator set differs"
  else if measured.request_id <> Certificate.request_id request then
    Error "compute bill request differs"
  else if not (Crypto.Address.verify_address_pubkey request.caller
      (Base64.encode_exn header.caller_public_key)
    && Octra_consensus.C_hash.verify_ed25519 ~pubkey_raw:header.caller_public_key
      ~msg:(Protocol.header_intent header) ~signature:header.caller_signature) then
    Error "compute bill caller signature refused"
  else if not (match Validators.pubkey_of_addr validators header.coordinator with
    | None -> false
    | Some pubkey_raw -> Octra_consensus.C_hash.verify_ed25519 ~pubkey_raw
        ~msg:(Protocol.header_sign header) ~signature:header.signature) then
    Error "compute bill coordinator signature refused"
  else match Certificate.decide ~chain_id:request.chain_id ~current_epoch:epoch
      ~max_ttl:Protocol.call_ttl ~selected ~signer_floor:(List.length selected * 2 / 3 + 1)
      ~weight_floor:(Z.succ (Z.div (Z.mul total (Z.of_int 2)) (Z.of_int 3)))
      ~weight_of ~verify_signature request votes with
  | Certificate.Rejected _ -> Error "compute bill certificate refused"
  | Certificate.Insufficient -> Error "compute bill certificate missing votes"
  | Certificate.Conflicting -> Error "compute bill certificate conflicts"
  | Certificate.Agreed certificate ->
    if certificate.output_hash <> measured.output_hash
        || certificate.output_bytes <> measured.output_bytes
        || certificate.trace_root <> measured.trace_root
        || certificate.steps <> measured.steps || certificate.operations <> measured.operations then
      Error "compute bill execution differs"
    else match request.unit_price, request.fee_cap, request.fuel_limit with
    | Some price, Some cap, Some fuel
        when price > 0L && cap > 0L && measured.steps <= Int64.of_int fuel ->
      let charged = Z.mul (Z.of_int64 price) (Z.of_int64 measured.steps) in
      let refund = Z.sub (Z.of_int64 cap) charged in
      if Z.sign refund < 0 then Error "compute bill exceeds cap"
      else
        let nodes = List.map (fun (signer : Certificate.signer) -> signer.node_id)
          certificate.signers |> List.sort_uniq String.compare in
        if List.exists (fun node -> not (Crypto.Address.is_valid_address node)) nodes then
          Error "compute bill payee refused"
        else
          let share, remainder = Z.ediv_rem charged (Z.of_int (List.length nodes)) in
          let payments = List.mapi (fun index node ->
            node, if Z.lt (Z.of_int index) remainder then Z.succ share else share) nodes in
          Ok {header; measured; root = certificate.root; charged; refund; payments}
    | _ -> Error "compute bill terms missing"