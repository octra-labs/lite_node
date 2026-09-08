(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Account = Octra_core.Account_pack
module Blob = Octra_core.Blob_chunk
module Types = Octra_core.Ledger_types

let fail label =
  failwith ("test_account_pack: " ^ label)

let expect label condition =
  if not condition then fail label

let expect_ok label = function
  | Ok value -> value
  | Error error -> fail (label ^ ": " ^ error)

let equal_account left right =
  Z.equal left.Types.balance right.Types.balance
  && left.nonce = right.nonce
  && left.public_key = right.public_key
  && left.encrypted_balance = right.encrypted_balance
  && Z.equal left.decrypt_allowance right.decrypt_allowance

let bytes count =
  String.init count (fun index -> Char.chr ((index * 37 + 11) land 0xff))

let part_pairs parts =
  List.map (fun (part : Blob.part) -> part.id, part.raw) parts

let get_part parts id =
  List.assoc_opt id (part_pairs parts)

let test_blob_roundtrip () =
  let empty = Blob.cut "" in
  expect "empty blob has no parts" (empty.parts = []);
  expect "empty blob joins"
    (Blob.join ~id:empty.id ~size:empty.size [] = Ok "");
  let raw = bytes 220_000 in
  let cut = Blob.cut raw in
  expect "large blob has several parts" (List.length cut.parts > 3);
  expect "large blob parts are size-limited"
    (List.for_all
       (fun (part : Blob.part) ->
         String.length part.raw > 0 && String.length part.raw <= 65_536)
       cut.parts);
  let joined =
    Blob.join ~id:cut.id ~size:cut.size (part_pairs cut.parts)
    |> expect_ok "large blob join"
  in
  expect "large blob roundtrip" (String.equal joined raw);
  let first, rest =
    match cut.parts with
    | first :: rest -> first, rest
    | [] -> fail "large blob unexpectedly empty"
  in
  let tampered =
    (first.id, first.raw ^ "x") :: part_pairs rest
  in
  expect "tampered blob refused"
    (Result.is_error (Blob.join ~id:cut.id ~size:(cut.size + 1) tampered));
  expect "reordered blob refused"
    (Result.is_error
       (Blob.join
          ~id:cut.id
          ~size:cut.size
          (List.rev (part_pairs cut.parts))))

let account encrypted_balance = Types.{
  balance = Z.of_int 123_456;
  nonce = 17;
  public_key = Some "public-key";
  encrypted_balance;
  decrypt_allowance = Z.of_int 91;
}

let read_image image =
  let data = Account.data image.Account.data |> expect_ok "account data" in
  Account.read
    data
    ~meta:image.meta
    ~get:(get_part image.parts)

let test_old_account () =
  let source = account None in
  let raw = Account.old source in
  let parsed =
    match Account.data raw |> expect_ok "old account parse" with
    | Account.Old value -> value
    | Account.Parts _ -> fail "old account parsed as parts"
  in
  expect "old account roundtrip" (equal_account source parsed);
  expect "old account balance"
    (Account.balance raw = Ok source.balance)

let test_text_account () =
  let source = account (Some (bytes 180_000)) in
  let image = Account.image source in
  expect "text account has metadata" (Option.is_some image.meta);
  expect "text account has parts" (List.length image.parts > 2);
  let restored = read_image image |> expect_ok "text account restore" in
  expect "text account roundtrip" (equal_account source restored);
  let data = Account.data image.data |> expect_ok "text account data" in
  expect "missing text part refused"
    (Result.is_error (Account.read data ~meta:image.meta ~get:(fun _ -> None)));
  let wrong_meta =
    match image.meta with
    | None -> fail "text metadata absent"
    | Some raw -> String.map (fun value -> if value = 'a' then 'b' else value) raw
  in
  expect "changed text metadata refused"
    (Result.is_error
       (Account.read data ~meta:(Some wrong_meta) ~get:(get_part image.parts)))

let test_hfhe_account () =
  let raw = bytes 96_000 in
  let cipher =
    Octra_core.Crypto.FheBalance.prefix ^ Base64.encode_exn raw
  in
  let source = account (Some cipher) in
  let image = Account.image source in
  let restored = read_image image |> expect_ok "hfhe account restore" in
  expect "hfhe account roundtrip" (equal_account source restored);
  let head_data, head_id =
    match Account.head source with
    | Some value -> value
    | None -> fail "hfhe account head absent"
  in
  expect "hfhe head matches image" (image.id = Some head_id);
  expect "hfhe head parses"
    (match Account.data head_data with
     | Ok (Account.Parts (_, id)) -> String.equal id head_id
     | _ -> false)

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let ensure_dir path =
  if Sys.file_exists path then
    if not (Sys.is_directory path) then fail "work path is not a directory"
    else ()
  else
    Unix.mkdir path 0o700

let work_dir () =
  let root = Filename.concat (Sys.getcwd ()) "runtime_data" in
  let scope = Filename.concat root "account-pack-tests" in
  ensure_dir root;
  ensure_dir scope;
  let name =
    Printf.sprintf "run-%d-%.0f"
      (Unix.getpid ())
      (Unix.gettimeofday () *. 1_000_000.)
  in
  let path = Filename.concat scope name in
  Unix.mkdir path 0o700;
  path

let test_store_layout () =
  let root = work_dir () in
  let path = Filename.concat root "irmin_store" in
  Fun.protect
    ~finally:(fun () -> remove_tree root)
    (fun () ->
      let store =
        Lwt_main.run (Octra_core.Store_irmin.open_store ~fresh:true path)
      in
      Fun.protect
        ~finally:(fun () -> Lwt_main.run (Octra_core.Store_irmin.close store))
        (fun () ->
          let address = "oct-account-pack-test" in
          let source = account (Some (bytes 140_000)) in
          Lwt_main.run
            (Octra_core.Store_irmin.begin_epoch_batch
               ~mode:Octra_core.Rule_graph.Active
               store);
          Lwt_main.run (Octra_core.Store_irmin.set_account store address source);
          Lwt_main.run
            (Octra_core.Store_irmin.commit_epoch_batch store "account-pack-test");
          let raw =
            Lwt_main.run
              (Octra_core.Store_irmin.read
                 store
                 ["accounts"; address; "data"])
          in
          expect "active store uses parts"
            (match Option.bind raw (fun value -> Result.to_option (Account.data value)) with
             | Some (Account.Parts _) -> true
             | _ -> false);
          let restored =
            Lwt_main.run (Octra_core.Store_irmin.get_account store address)
          in
          expect "active store account roundtrip"
            (match restored with
             | Some value -> equal_account source value
             | None -> false);
          let proof =
            Lwt_main.run
              (Octra_core.Store_irmin.account_merkle_proof store address)
            |> expect_ok "active store proof"
          in
          expect "active store proof kind"
            (String.equal proof.proof_kind "irmin_account_tree");
          expect "active store proof path"
            (proof.path = ["accounts"; address]);
          let verified =
            Octra_core.Store_irmin.verify_account_merkle_proof
              ~ledger_state_root:proof.ledger_state_root
              ~addr:address
              ~proof:proof.proof
            |> expect_ok "active store proof verify"
          in
          expect "active store proof value" (Option.is_some verified)))

let () =
  test_blob_roundtrip ();
  test_old_account ();
  test_text_account ();
  test_hfhe_account ();
  test_store_layout ();
  Printf.printf "account_pack tests passed\n%!"