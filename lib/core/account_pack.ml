(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type form = Hfhe | Text

type meta = {
  form : form;
  size : int;
  id : string;
  blob : string;
  parts : string list;
}

type data =
  | Old of Ledger_types.account
  | Parts of Ledger_types.account * string

type image = {
  data : string;
  meta : string option;
  parts : Blob_chunk.part list;
  id : string option;
}

let ( let* ) = Result.bind

let old account =
  Ledger_types.account_to_yojson account
  |> Yojson.Safe.to_string

let names fields =
  List.map fst fields

let exact expected fields =
  let got = names fields in
  List.length got = List.length expected
  && List.sort_uniq String.compare got = List.sort String.compare expected

let assoc name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("account field is missing: " ^ name)

let text name fields =
  let* value = assoc name fields in
  match value with
  | `String value -> Ok value
  | _ -> Error ("account field is not text: " ^ name)

let intv name fields =
  let* value = assoc name fields in
  match value with
  | `Int value -> Ok value
  | _ -> Error ("account field is not integer: " ^ name)

let opt_text name fields =
  let* value = assoc name fields in
  match value with
  | `Null -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error ("account field is not optional text: " ^ name)

let z name fields =
  let* value = text name fields in
  match Z.of_string value with
  | value -> Ok value
  | exception _ -> Error ("account field is not natural text: " ^ name)

let account fields ~decrypt =
  let* balance = z "balance" fields in
  let* nonce = intv "nonce" fields in
  let* public_key = opt_text "public_key" fields in
  let* encrypted_balance = opt_text "encrypted_balance" fields in
  let* decrypt_allowance =
    if decrypt then z "decrypt_allowance" fields else Ok Z.zero
  in
  Ok Ledger_types.{
    balance;
    nonce;
    public_key;
    encrypted_balance;
    decrypt_allowance;
  }

let old_fields = [
  "balance";
  "nonce";
  "public_key";
  "encrypted_balance";
  "decrypt_allowance";
]

let early_fields = [
  "balance";
  "nonce";
  "public_key";
  "encrypted_balance";
]

let part_fields = [
  "layout";
  "balance";
  "nonce";
  "public_key";
  "cipher_id";
  "decrypt_allowance";
]

let parse_parts fields =
  if not (exact part_fields fields) then Error "account part fields differ"
  else
    let* layout = text "layout" fields in
    if not (String.equal layout "parts") then Error "account layout differs"
    else
      let* balance = z "balance" fields in
      let* nonce = intv "nonce" fields in
      let* public_key = opt_text "public_key" fields in
      let* id = text "cipher_id" fields in
      let* decrypt_allowance = z "decrypt_allowance" fields in
      Ok
        (Parts
           (Ledger_types.{
              balance;
              nonce;
              public_key;
              encrypted_balance = None;
              decrypt_allowance;
            },
            id))

let data raw =
  match Yojson.Safe.from_string raw with
  | `Assoc fields ->
    if List.mem_assoc "layout" fields then parse_parts fields
    else if exact old_fields fields then Result.map (fun value -> Old value) (account fields ~decrypt:true)
    else if exact early_fields fields then Result.map (fun value -> Old value) (account fields ~decrypt:false)
    else Error "account fields differ"
  | _ -> Error "account value is not an object"
  | exception _ -> Error "account value is not json"

let hex value =
  String.length value = 64
  && String.for_all
       (function
         | '0' .. '9'
         | 'a' .. 'f' -> true
         | _ -> false)
       value

let form_key = function
  | Hfhe -> "hfhe"
  | Text -> "text"

let form = function
  | "hfhe" -> Ok Hfhe
  | "text" -> Ok Text
  | _ -> Error "account cipher form differs"

let meta_fields = ["form"; "size"; "id"; "blob"; "parts"]

let meta raw =
  match Yojson.Safe.from_string raw with
  | `Assoc fields when exact meta_fields fields ->
    let* form_raw = text "form" fields in
    let* form = form form_raw in
    let* size = intv "size" fields in
    let* id = text "id" fields in
    let* blob = text "blob" fields in
    let* parts_raw = assoc "parts" fields in
    let* parts =
      match parts_raw with
      | `List values ->
        List.fold_right
          (fun value acc ->
            let* rest = acc in
            match value with
            | `String id when hex id -> Ok (id :: rest)
            | _ -> Error "account part id differs")
          values
          (Ok [])
      | _ -> Error "account parts are not a list"
    in
    if size < 0 then Error "account cipher size is negative"
    else if not (hex id) then Error "account cipher id differs"
    else if not (hex blob) then Error "account blob id differs"
    else if not (Blob_chunk.count_ok ~size (List.length parts)) then
      Error "account part count differs"
    else Ok { form; size; id; blob; parts }
  | `Assoc _ -> Error "account cipher fields differ"
  | _ -> Error "account cipher value is not an object"
  | exception _ -> Error "account cipher value is not json"

let ids (meta : meta) = meta.parts
let key (meta : meta) = meta.id

let cipher_id form raw =
  let state = Digestif.SHA256.init () in
  let state = Digestif.SHA256.feed_string state "octra.account.cipher" in
  let state = Digestif.SHA256.feed_string state "\000" in
  let state = Digestif.SHA256.feed_string state (form_key form) in
  let state = Digestif.SHA256.feed_string state "\000" in
  Digestif.SHA256.feed_string state raw
  |> Digestif.SHA256.get
  |> Digestif.SHA256.to_hex

let source cipher =
  let prefix = Crypto.FheBalance.prefix in
  let plen = String.length prefix in
  if String.length cipher > plen && String.sub cipher 0 plen = prefix then
    let encoded = String.sub cipher plen (String.length cipher - plen) in
    match Base64.decode encoded with
    | Ok raw when String.equal (prefix ^ Base64.encode_exn raw) cipher ->
      Hfhe, raw
    | Ok _
    | Error _ -> Text, cipher
  else
    Text, cipher

let data_json account id =
  let opt = function None -> `Null | Some value -> `String value in
  `Assoc [
    "layout", `String "parts";
    "balance", `String (Z.to_string account.Ledger_types.balance);
    "nonce", `Int account.nonce;
    "public_key", opt account.public_key;
    "cipher_id", `String id;
    "decrypt_allowance", `String (Z.to_string account.decrypt_allowance);
  ]
  |> Yojson.Safe.to_string

let meta_json meta =
  `Assoc [
    "form", `String (form_key meta.form);
    "size", `Int meta.size;
    "id", `String meta.id;
    "blob", `String meta.blob;
    "parts", `List (List.map (fun id -> `String id) meta.parts);
  ]
  |> Yojson.Safe.to_string

let head account =
  match account.Ledger_types.encrypted_balance with
  | None -> None
  | Some cipher ->
    let form, raw = source cipher in
    let id = cipher_id form raw in
    Some (data_json account id, id)

let image account =
  match account.Ledger_types.encrypted_balance with
  | None ->
    {
      data = old account;
      meta = None;
      parts = [];
      id = None;
    }
  | Some cipher ->
    let form, raw = source cipher in
    let cut = Blob_chunk.cut raw in
    let id = cipher_id form raw in
    let meta = {
      form;
      size = cut.size;
      id;
      blob = cut.id;
      parts =
        List.map
          (fun (part : Blob_chunk.part) -> part.id)
          cut.parts;
    } in
    {
      data = data_json account id;
      meta = Some (meta_json meta);
      parts = cut.parts;
      id = Some id;
    }

let restore (meta : meta) get =
  let rec gather acc = function
    | [] -> Ok (List.rev acc)
    | id :: rest ->
      begin
        match get id with
        | None -> Error "account cipher part is missing"
        | Some raw -> gather ((id, raw) :: acc) rest
      end
  in
  let* parts = gather [] meta.parts in
  let* raw = Blob_chunk.join ~id:meta.blob ~size:meta.size parts in
  if not (String.equal (cipher_id meta.form raw) meta.id) then
    Error "account cipher id differs"
  else
    match meta.form with
    | Hfhe -> Ok (Crypto.FheBalance.prefix ^ Base64.encode_exn raw)
    | Text -> Ok raw

let read data ~meta:meta_raw ~get =
  match data with
  | Old account -> Ok account
  | Parts (account, id) ->
    begin
      match meta_raw with
      | None -> Error "account cipher metadata is missing"
      | Some raw ->
        let* meta = meta raw in
        if not (String.equal meta.id id) then Error "account cipher reference differs"
        else
          let* cipher = restore meta get in
          Ok { account with Ledger_types.encrypted_balance = Some cipher }
    end

let balance raw =
  let* data = data raw in
  match data with
  | Old account
  | Parts (account, _) -> Ok account.Ledger_types.balance