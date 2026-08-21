(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  candidates : Validator_admission.candidate list;
  slashes : slash_record list;
}

and slash_record = {
  id : string;
  offender : string;
  evidence_epoch : int64;
  applied_epoch : int64;
  amount : Z.t;
}

type bond_payload = {
  consensus_pubkey_b64 : string;
  proof_b64 : string;
}

type ready_payload = {
  consensus_pubkey_b64 : string;
  head_epoch : int64;
  state_root : string;
  chain_id : string option;
  binary_hash : string option;
  config_hash : string option;
  catchup_head_epoch : int64 option;
  shadow_epochs : int option;
}

let meta_key = "bft.validator_registry"
let escrow_address =
  "octra:validator_bond:escrow"
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_raw_string
  |> Base64.encode_exn
  |> Crypto.Address.address_from_pubkey
let empty = { candidates = []; slashes = [] }
let candidates registry = registry.candidates
let slashes registry = registry.slashes

let put_string buffer value =
  Buffer.add_string buffer (string_of_int (String.length value));
  Buffer.add_char buffer ':';
  Buffer.add_string buffer value

let bond_message ~chain_id ~address ~amount ~nonce ~pubkey =
  let buffer = Buffer.create 192 in
  put_string buffer "octra:validator_bond:v1";
  put_string buffer chain_id;
  put_string buffer address;
  put_string buffer (Z.to_string amount);
  put_string buffer (string_of_int nonce);
  put_string buffer pubkey;
  Buffer.contents buffer

let raw_pubkey value =
  try
    let raw = Base64.decode_exn value in
    if String.length raw = 32 then Ok raw
    else Error "validator consensus public key must contain 32 bytes"
  with _ ->
    Error "invalid validator consensus public key"

let verify_bond_proof ~chain_id ~address ~amount ~nonce
    (payload : bond_payload) =
  match raw_pubkey payload.consensus_pubkey_b64 with
  | Error _ as error -> error
  | Ok pubkey ->
    begin
      try
        let signature = Base64.decode_exn payload.proof_b64 in
        let message =
          bond_message
            ~chain_id
            ~address
            ~amount
            ~nonce
            ~pubkey
        in
        if Octra_ed25519.verify ~pub:pubkey ~msg:message signature then
          Ok pubkey
        else
          Error "invalid validator bond proof"
      with _ ->
        Error "invalid validator bond proof"
    end

let find address (registry : t) =
  List.find_opt
    (fun (candidate : Validator_admission.candidate) ->
      String.equal candidate.Validator_admission.address address)
    registry.candidates

let validate_unique (candidates : Validator_admission.candidate list) =
  let addresses = Hashtbl.create (List.length candidates) in
  let pubkeys = Hashtbl.create (List.length candidates) in
  let rec loop (remaining : Validator_admission.candidate list) =
    match remaining with
    | [] -> Ok ()
    | candidate :: rest ->
      if Hashtbl.mem addresses candidate.Validator_admission.address then
        Error "duplicate validator address"
      else if Hashtbl.mem pubkeys candidate.Validator_admission.pubkey then
        Error "duplicate validator consensus public key"
      else begin
        Hashtbl.add addresses candidate.Validator_admission.address ();
        Hashtbl.add pubkeys candidate.Validator_admission.pubkey ();
        loop rest
      end
  in
  loop candidates

let valid_hash value =
  try
    let raw = Base64.decode_exn value in
    String.length raw = 32
    && String.equal (Base64.encode_exn raw) value
  with _ ->
    false

let validate_slashes records =
  let ids = Hashtbl.create (List.length records) in
  let rec loop = function
    | [] -> Ok ()
    | record :: rest ->
      if not (valid_hash record.id) then
        Error "invalid validator evidence id"
      else if not (Crypto.Address.is_valid_address record.offender) then
        Error "invalid slashed validator address"
      else if Int64.compare record.evidence_epoch 0L < 0 then
        Error "validator evidence epoch is negative"
      else if Int64.compare record.applied_epoch record.evidence_epoch < 0 then
        Error "validator evidence application predates evidence"
      else if Z.sign record.amount <= 0 then
        Error "validator slash amount must be positive"
      else if Hashtbl.mem ids record.id then
        Error "duplicate validator evidence id"
      else begin
        Hashtbl.add ids record.id ();
        loop rest
      end
  in
  loop records

let canonical (candidates : Validator_admission.candidate list) =
  List.sort
    (fun (left : Validator_admission.candidate)
         (right : Validator_admission.candidate) ->
      String.compare
        left.Validator_admission.address
        right.Validator_admission.address)
    candidates

let canonical_slashes records =
  List.sort
    (fun left right -> String.compare left.id right.id)
    records

let validate (registry : t) =
  let rec validate_candidates
      (remaining : Validator_admission.candidate list) =
    match remaining with
    | [] -> Ok ()
    | candidate :: rest ->
      if not (Crypto.Address.is_valid_address candidate.Validator_admission.address) then
        Error "invalid validator address"
      else
        match Validator_admission.validate_candidate candidate with
        | Error _ as error -> error
        | Ok () -> validate_candidates rest
  in
  match validate_candidates registry.candidates with
  | Error _ as error -> error
  | Ok () ->
    begin
      match validate_unique registry.candidates with
      | Error _ as error -> error
      | Ok () -> validate_slashes registry.slashes
    end

let replace (candidate : Validator_admission.candidate) (registry : t) =
  {
    candidates =
      candidate
      :: List.filter
           (fun (current : Validator_admission.candidate) ->
             not
               (String.equal
                  current.Validator_admission.address
                  candidate.Validator_admission.address))
           registry.candidates
      |> canonical;
    slashes = registry.slashes;
  }

let apply_bond parameters ~chain_id ~epoch ~address ~sender_pubkey_b64
    ~amount ~nonce (payload : bond_payload) (registry : t) =
  if not (Crypto.Address.is_valid_address address) then
    Error "invalid validator address"
  else if Z.lt amount parameters.Validator_admission.min_bond then
    Error "validator bond is below minimum"
  else if Int64.compare epoch 0L < 0 then
    Error "validator bond epoch is negative"
  else
    match
      raw_pubkey sender_pubkey_b64,
      verify_bond_proof ~chain_id ~address ~amount ~nonce payload
    with
    | Error _, _ -> Error "invalid validator sender public key"
    | _, (Error _ as error) -> error
    | Ok sender_pubkey, Ok pubkey ->
      if not (String.equal sender_pubkey pubkey) then
        Error "validator consensus public key must match sender public key"
      else
      begin
        match
          List.find_opt
            (fun (candidate : Validator_admission.candidate) ->
              String.equal candidate.Validator_admission.pubkey pubkey
              && not
                   (String.equal
                      candidate.Validator_admission.address
                      address))
            registry.candidates
        with
        | Some _ -> Error "validator consensus public key is already bonded"
        | None ->
          let candidate =
            match find address registry with
            | None ->
              Validator_admission.{
                address;
                pubkey;
                bond = amount;
                bonded_epoch = epoch;
                ready_epoch = None;
                exit_epoch = None;
              }
            | Some current ->
              if current.Validator_admission.exit_epoch <> None then
                current
              else if not (String.equal current.pubkey pubkey) then
                current
              else
                { current with bond = Z.add current.bond amount }
          in
          begin
            match find address registry with
            | Some current when current.Validator_admission.exit_epoch <> None ->
              Error "exiting validator cannot add bond"
            | Some current when not (String.equal current.pubkey pubkey) ->
              Error "validator consensus key rotation requires withdrawal"
            | _ ->
              let next = replace candidate registry in
              begin
                match validate next with
                | Ok () -> Ok next
                | Error _ as error -> error
              end
          end
      end

let mark_ready ~epoch ~address ~consensus_pubkey_b64 (registry : t) =
  if Int64.compare epoch 0L < 0 then
    Error "validator ready epoch is negative"
  else
    match raw_pubkey consensus_pubkey_b64, find address registry with
    | Error _ as error, _ -> error
    | _, None -> Error "validator bond not found"
    | Ok pubkey, Some candidate ->
      if candidate.Validator_admission.exit_epoch <> None then
        Error "exiting validator cannot become ready"
      else if not (String.equal candidate.pubkey pubkey) then
        Error "validator ready consensus key mismatch"
      else
        let ready_epoch =
          match candidate.ready_epoch with
          | None -> Some epoch
          | Some current -> Some (Int64.min current epoch)
        in
        Ok (replace { candidate with ready_epoch } registry)

let request_exit ~epoch ~address (registry : t) =
  if Int64.compare epoch 0L < 0 then
    Error "validator exit epoch is negative"
  else
    match find address registry with
    | None -> Error "validator bond not found"
    | Some candidate when candidate.Validator_admission.exit_epoch <> None ->
      Error "validator exit already requested"
    | Some candidate ->
      Ok (replace { candidate with exit_epoch = Some epoch } registry)

let withdraw parameters ~current_epoch ~active_addresses ~address
    (registry : t) =
  match find address registry with
  | None -> Error "validator bond not found"
  | Some candidate ->
    if List.mem address active_addresses then
      Error "active validator cannot withdraw"
    else
      match
        Validator_admission.can_withdraw
          parameters
          ~current_epoch
          candidate
      with
      | Error _ as error -> error
      | Ok false -> Error "validator unbonding period is not complete"
      | Ok true ->
        Ok (
          {
            candidates =
              List.filter
                (fun (current : Validator_admission.candidate) ->
                  not
                    (String.equal
                       current.Validator_admission.address
                       address))
                registry.candidates;
            slashes = registry.slashes;
          },
          candidate.bond)

let evidence_fresh ~current_epoch ~evidence_epochs record =
  Int64.compare record.evidence_epoch current_epoch <= 0
  && Int64.compare
       (Int64.sub current_epoch record.evidence_epoch)
       evidence_epochs < 0

let apply_slash ~current_epoch ~evidence_epochs
    (evidence : Validator_evidence.verified) (registry : t) =
  let retained =
    List.filter
      (evidence_fresh ~current_epoch ~evidence_epochs)
      registry.slashes
  in
  if List.exists (fun record -> String.equal record.id evidence.id) retained then
    Error "validator evidence already applied"
  else
    match find evidence.offender registry with
    | None -> Error "validator bond not found"
    | Some candidate ->
      let record = {
        id = evidence.id;
        offender = evidence.offender;
        evidence_epoch = evidence.evidence_epoch;
        applied_epoch = current_epoch;
        amount = candidate.Validator_admission.bond;
      } in
      let next = {
        candidates =
          List.filter
            (fun (current : Validator_admission.candidate) ->
              not
                (String.equal
                   current.Validator_admission.address
                   evidence.offender))
            registry.candidates;
        slashes = canonical_slashes (record :: retained);
      } in
      begin
        match validate next with
        | Error _ as error -> error
        | Ok () -> Ok (next, record.amount)
      end

let snapshot parameters ~activate_epoch (registry : t) =
  Validator_admission.snapshot
    parameters
    ~activate_epoch
    registry.candidates

let candidate_to_yojson (candidate : Validator_admission.candidate) =
  `Assoc [
    "address", `String candidate.Validator_admission.address;
    "consensus_pubkey",
      `String (Base64.encode_exn candidate.Validator_admission.pubkey);
    "bond", `String (Z.to_string candidate.Validator_admission.bond);
    "bonded_epoch",
      `String (Int64.to_string candidate.Validator_admission.bonded_epoch);
    "ready_epoch",
      Option.fold
        ~none:`Null
        ~some:(fun value -> `String (Int64.to_string value))
        candidate.Validator_admission.ready_epoch;
    "exit_epoch",
      Option.fold
        ~none:`Null
        ~some:(fun value -> `String (Int64.to_string value))
        candidate.Validator_admission.exit_epoch;
  ]

let slash_to_yojson record =
  `Assoc [
    "id", `String record.id;
    "offender", `String record.offender;
    "evidence_epoch", `String (Int64.to_string record.evidence_epoch);
    "applied_epoch", `String (Int64.to_string record.applied_epoch);
    "amount", `String (Z.to_string record.amount);
  ]

let parse_int64 label = function
  | `String raw ->
    begin
      try Ok (Int64.of_string raw)
      with _ -> Error ("invalid " ^ label)
    end
  | `Int value -> Ok (Int64.of_int value)
  | _ -> Error (label ^ " must be string or int")

let optional_string fields name =
  match List.assoc_opt name fields with
  | Some (`String value) when value <> "" -> Some value
  | None
  | Some _ -> None

let lower_hash value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9'
         | 'a' .. 'f' -> true
         | _ -> false)
       value

let optional_hash fields name =
  match optional_string fields name with
  | Some value when lower_hash value -> Some value
  | None
  | Some _ -> None

let optional_int64 fields name =
  match List.assoc_opt name fields with
  | Some value ->
    begin
      match parse_int64 ("validator ready " ^ name) value with
      | Ok value when Int64.compare value 0L >= 0 -> Some value
      | Ok _
      | Error _ -> None
    end
  | None -> None

let optional_int fields name =
  match optional_int64 fields name with
  | Some value when Int64.compare value (Int64.of_int max_int) <= 0 ->
    Some (Int64.to_int value)
  | None
  | Some _ -> None

let parse_optional_int64 label = function
  | `Null -> Ok None
  | value -> Result.map Option.some (parse_int64 label value)

let parse_bond = function
  | `String raw ->
    begin
      try
        let value = Z.of_string raw in
        if Z.sign value <= 0 then Error "validator bond must be positive"
        else Ok value
      with _ ->
        Error "invalid validator bond"
    end
  | _ -> Error "validator bond must be string"

let candidate_of_yojson = function
  | `Assoc fields ->
    begin
      match
        List.assoc_opt "address" fields,
        List.assoc_opt "consensus_pubkey" fields,
        List.assoc_opt "bond" fields,
        List.assoc_opt "bonded_epoch" fields,
        List.assoc_opt "ready_epoch" fields,
        List.assoc_opt "exit_epoch" fields
      with
      | Some (`String address),
        Some (`String consensus_pubkey_b64),
        Some bond_json,
        Some bonded_epoch_json,
        Some ready_epoch_json,
        Some exit_epoch_json ->
        begin
          match
            raw_pubkey consensus_pubkey_b64,
            parse_bond bond_json,
            parse_int64 "validator bonded epoch" bonded_epoch_json,
            parse_optional_int64 "validator ready epoch" ready_epoch_json,
            parse_optional_int64 "validator exit epoch" exit_epoch_json
          with
          | Ok pubkey, Ok bond, Ok bonded_epoch, Ok ready_epoch, Ok exit_epoch ->
            Ok Validator_admission.{
              address;
              pubkey;
              bond;
              bonded_epoch;
              ready_epoch;
              exit_epoch;
            }
          | Error error, _, _, _, _
          | _, Error error, _, _, _
          | _, _, Error error, _, _
          | _, _, _, Error error, _
          | _, _, _, _, Error error -> Error error
        end
      | _ -> Error "invalid validator registry candidate"
    end
  | _ -> Error "validator registry candidate must be object"

let slash_of_yojson = function
  | `Assoc fields ->
    begin
      match
        List.assoc_opt "id" fields,
        List.assoc_opt "offender" fields,
        List.assoc_opt "evidence_epoch" fields,
        List.assoc_opt "applied_epoch" fields,
        List.assoc_opt "amount" fields
      with
      | Some (`String id),
        Some (`String offender),
        Some evidence_epoch_json,
        Some applied_epoch_json,
        Some amount_json ->
        begin
          match
            parse_int64 "validator evidence epoch" evidence_epoch_json,
            parse_int64 "validator evidence applied epoch" applied_epoch_json,
            parse_bond amount_json
          with
          | Ok evidence_epoch, Ok applied_epoch, Ok amount ->
            Ok {
              id;
              offender;
              evidence_epoch;
              applied_epoch;
              amount;
            }
          | Error error, _, _
          | _, Error error, _
          | _, _, Error error -> Error error
        end
      | _ -> Error "invalid validator slash record"
    end
  | _ -> Error "validator slash record must be object"

let to_yojson registry =
  `Assoc [
    "standard", `String Validator_policy.standard_name;
    "candidates",
      `List (List.map candidate_to_yojson (canonical registry.candidates));
    "slashes",
      `List (List.map slash_to_yojson (canonical_slashes registry.slashes));
  ]

let of_yojson = function
  | `Assoc fields ->
    begin
      match
        List.assoc_opt "standard" fields,
        List.assoc_opt "candidates" fields,
        List.assoc_opt "slashes" fields
      with
      | Some (`String standard), Some (`List values), slashes_json
          when String.equal standard Validator_policy.standard_name ->
        let parsed_candidates =
          List.fold_left
            (fun result value ->
              match result, candidate_of_yojson value with
              | (Error _ as error), _ -> error
              | _, (Error _ as error) -> error
              | Ok candidates, Ok candidate -> Ok (candidate :: candidates))
            (Ok [])
            values
        in
        let parsed_slashes =
          match slashes_json with
          | None -> Ok []
          | Some (`List records) ->
            List.fold_left
              (fun result value ->
                match result, slash_of_yojson value with
                | (Error _ as error), _ -> error
                | _, (Error _ as error) -> error
                | Ok slashes, Ok slash -> Ok (slash :: slashes))
              (Ok [])
              records
          | Some _ -> Error "validator slashes must be list"
        in
        begin
          match parsed_candidates, parsed_slashes with
          | (Error _ as error), _
          | _, (Error _ as error) -> error
          | Ok candidates, Ok slashes ->
            let registry = {
              candidates = canonical candidates;
              slashes = canonical_slashes slashes;
            } in
            begin
              match validate registry with
              | Ok () -> Ok registry
              | Error _ as error -> error
            end
        end
      | Some (`String _), _, _ -> Error "validator registry standard mismatch"
      | _ -> Error "invalid validator registry"
    end
  | _ -> Error "validator registry must be object"

let to_string registry =
  registry
  |> to_yojson
  |> Yojson.Safe.to_string

let of_string raw =
  try
    raw
    |> Yojson.Safe.from_string
    |> of_yojson
  with _ ->
    Error "invalid validator registry JSON"

let bond_payload_of_message = function
  | None -> Error "validator bond requires message payload"
  | Some raw ->
    begin
      try
        match Yojson.Safe.from_string raw with
        | `Assoc fields ->
          begin
            match
              List.assoc_opt "consensus_pubkey" fields,
              List.assoc_opt "proof" fields
            with
            | Some (`String consensus_pubkey_b64),
              Some (`String proof_b64) ->
              Ok { consensus_pubkey_b64; proof_b64 }
            | _ -> Error "validator bond requires consensus_pubkey and proof"
          end
        | _ -> Error "validator bond payload must be object"
      with _ ->
        Error "invalid validator bond payload"
    end

let ready_payload_of_message = function
  | None -> Error "validator ready requires message payload"
  | Some raw ->
    begin
      try
        match Yojson.Safe.from_string raw with
        | `Assoc fields ->
          begin
            match
              List.assoc_opt "consensus_pubkey" fields,
              List.assoc_opt "head_epoch" fields,
              List.assoc_opt "state_root" fields
            with
            | Some (`String consensus_pubkey_b64),
              Some head_epoch_json,
              Some (`String state_root) ->
              Result.map
                (fun head_epoch ->
                  {
                    consensus_pubkey_b64;
                    head_epoch;
                    state_root;
                    chain_id = optional_string fields "chain_id";
                    binary_hash = optional_hash fields "binary_hash";
                    config_hash = optional_hash fields "config_hash";
                    catchup_head_epoch =
                      optional_int64 fields "catchup_head_epoch";
                    shadow_epochs = optional_int fields "shadow_epochs";
                  })
                (parse_int64 "validator ready head epoch" head_epoch_json)
            | _ ->
              Error
                "validator ready requires consensus_pubkey, head_epoch and state_root"
          end
        | _ -> Error "validator ready payload must be object"
      with _ ->
        Error "invalid validator ready payload"
    end