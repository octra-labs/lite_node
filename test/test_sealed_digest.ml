(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Sealed = Octra_core.Circle_sealed
module Json = Yojson.Safe.Util

let field value key = Json.member key value |> Json.to_string

let check name valid = if not valid then failwith name

let raw hex =
  check "hex size" (String.length hex mod 2 = 0);
  String.init (String.length hex / 2) (fun index ->
    Char.chr (int_of_string ("0x" ^ String.sub hex (index * 2) 2)))

let hex value =
  String.to_seq value |> Seq.map (fun value -> Printf.sprintf "%02x" (Char.code value))
  |> List.of_seq |> String.concat ""

let rejected action =
  match action () with
  | _ -> false
  | exception Invalid_argument message -> message = "invalid sealed text"

let check_vector value =
  let name = field value "name" in
  let circle_id = field value "circle_id" in
  let key_id = field value "key_id" in
  let passphrase = field value "passphrase" in
  let plaintext = field value "plaintext_hex" |> raw in
  let salt = Sealed.digest_salt ~circle_id ~key_id in
  check (name ^ " salt") (hex salt = field value "salt_hex");
  check (name ^ " prior salt") (salt = Yojson.Safe.to_string (`List [
    `String "octra:circle:sealed_digest"; `String circle_id; `String key_id]));
  check (name ^ " digest") (Sealed.digest ~circle_id ~key_id ~passphrase plaintext = field value "digest_hex");
  check (name ^ " default") (Sealed.write_digest ~circle_id ~key_id ~passphrase plaintext = field value "plaintext_hash");
  check (name ^ " keyed") (Sealed.write_digest ~mode:Sealed.Keyed ~circle_id ~key_id ~passphrase plaintext = field value "digest_hex");
  List.iter (fun key ->
    check (name ^ " read " ^ key)
      (Sealed.matches_digest ~circle_id ~key_id ~passphrase ~expected:(field value key) plaintext);
    check (name ^ " changed " ^ key)
      (not (Sealed.matches_digest ~circle_id ~key_id ~passphrase ~expected:(field value key) (plaintext ^ "!"))))
    ["plaintext_hash"; "digest_hex"; "browser_digest_hex"];
  List.iter (fun key ->
    let expected = field value key in
    check (name ^ " passphrase") (not (Sealed.matches_digest ~circle_id ~key_id ~passphrase:"wrong" ~expected plaintext));
    check (name ^ " circle") (not (Sealed.matches_digest ~circle_id:(circle_id ^ "!") ~key_id ~passphrase ~expected plaintext));
    check (name ^ " key") (not (Sealed.matches_digest ~circle_id ~key_id:(key_id ^ "!") ~passphrase ~expected plaintext)))
    ["digest_hex"; "browser_digest_hex"];
  List.iter (fun expected ->
    check (name ^ " invalid digest")
      (not (Sealed.matches_digest ~circle_id ~key_id ~passphrase ~expected plaintext)))
    [""; String.make 64 '0'; String.uppercase_ascii (field value "digest_hex"); field value "digest_hex" ^ "0"];
  let envelope = field value "ciphertext_b64" |> Base64.decode_exn in
  let nonce = String.sub envelope 5 12 in
  let ciphertext = String.sub envelope 17 (String.length envelope - 17) in
  let key = Sealed.read_key ~circle_id ~key_id ~passphrase |> Mirage_crypto.AES.GCM.of_secret in
  let frame = "\000\000\000" ^ String.make 1 (Char.chr (String.length plaintext)) ^ plaintext in
  check (name ^ " envelope") (String.sub envelope 0 5 = "OCRS1");
  check (name ^ " decrypt") (Mirage_crypto.AES.GCM.authenticate_decrypt ~key ~nonce ciphertext = Some frame);
  check (name ^ " encrypt") (Mirage_crypto.AES.GCM.authenticate_encrypt ~key ~nonce frame = ciphertext);
  let changed = Bytes.of_string ciphertext in
  Bytes.set changed 0 (Char.chr (Char.code (Bytes.get changed 0) lxor 1));
  check (name ^ " cipher") (Mirage_crypto.AES.GCM.authenticate_decrypt ~key ~nonce (Bytes.to_string changed) = None);
  let wrong_key = Sealed.read_key ~circle_id ~key_id ~passphrase:"wrong" |> Mirage_crypto.AES.GCM.of_secret in
  check (name ^ " authentication") (Mirage_crypto.AES.GCM.authenticate_decrypt ~key:wrong_key ~nonce ciphertext = None)

let check_invalid value =
  let check_text value =
    check "circle text" (rejected (fun () -> Sealed.digest_salt ~circle_id:value ~key_id:"key"));
    check "key text" (rejected (fun () -> Sealed.digest_salt ~circle_id:"circle" ~key_id:value));
    check "passphrase text" (rejected (fun () -> Sealed.read_key ~circle_id:"circle" ~key_id:"key" ~passphrase:value));
    check "writer text" (rejected (fun () -> Sealed.write_digest ~circle_id:value ~key_id:"key" ~passphrase:"secret" "document"))
  in
  Json.member "invalid" value |> Json.to_list |> List.iter (fun value -> field value "utf8_hex" |> raw |> check_text);
  Json.member "invalid_utf8" value |> Json.to_list |> List.iter (fun value -> Json.to_string value |> raw |> check_text)

let () =
  let values = Yojson.Safe.from_file Sys.argv.(1) in
  let valid = Json.member "valid" values |> Json.to_list in
  check "vectors required" (List.length valid >= 7);
  List.iter check_vector valid;
  check_invalid values;
  Printf.printf "event = sealed_digest vectors = %d\n" (List.length valid)