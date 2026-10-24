(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus

let check message condition = if not condition then failwith message

let test_clock () =
  let open C_reply_window in
  let window = create ~deadline:100L ["a"; "b"] in
  check "early peer accepted" (take ~now:0L ~peer:"c" window = Unexpected_peer);
  check "expired reply accepted" (take ~now:100L ~peer:"a" window = Expired);
  let next = match take ~now:99L ~peer:"a" window with
    | Read value -> value | _ -> failwith "expected peer rejected" in
  check "repeat decoded" (take ~now:99L ~peer:"a" next = Repeated);
  check "window renewed" (next.deadline = 100L);
  let extended = extend ["a"; "c"] next in
  check "wave reopened consumed peer" (take ~now:99L ~peer:"a" extended = Repeated);
  check "wave renewed deadline" (extended.deadline = 100L);
  check "new wave peer rejected" (match take ~now:99L ~peer:"c" extended with
    | Read _ -> true | _ -> false);
  check "other peer consumed" (match take ~now:99L ~peer:"b" extended with
    | Read _ -> true | _ -> false)

let old_set entries =
  let rec validate addresses pubkeys = function
    | [] when addresses = [] -> Error "validator set is empty"
    | [] -> Ok ()
    | (v, w) :: rest ->
      if v.C_types.address = "" then Error "validator address is empty"
      else if String.length v.pubkey <> 32 then Error "validator public key must contain 32 bytes"
      else if Z.sign w <= 0 then Error "validator weight must be positive"
      else if List.mem v.address addresses then Error "duplicate validator address"
      else if List.mem v.pubkey pubkeys then Error "duplicate validator public key"
      else validate (v.address :: addresses) (v.pubkey :: pubkeys) rest in
  match validate [] [] entries with
  | Error _ as error -> error
  | Ok () -> Ok (C_types.build_validator_set ~weighted:true entries)

let old_encode set =
  let module O = Octra_net.Oce1 in
  O.encode (fun buf ->
    O.put_bool buf set.C_types.weighted;
    O.put_list (fun buf (v : C_types.validator_info) ->
      O.put_string buf v.address;
      O.put_bytes buf v.pubkey;
      O.put_string buf (Z.to_string (List.assoc v.address set.weights))) buf set.validators;
    Option.iter (fun (height, entries) ->
      O.put_string buf "octra:relief_authors";
      O.put_u64 buf height;
      O.put_list (fun buf (v, w) ->
        O.put_string buf v.C_types.address;
        O.put_bytes buf v.pubkey;
        O.put_string buf (Z.to_string w)) buf entries) set.authors)

let member i = C_types.{address = Printf.sprintf "member-%04d" i;
  pubkey = Digestif.SHA256.(digest_string (string_of_int i) |> to_raw_string)}

let test_set_bytes () =
  let entries = List.init 4096 (fun i -> member i, Z.of_int (i + 1)) in
  let expect entries =
    check "set validation changed" (old_set entries = C_types.make_weighted_validator_set entries) in
  List.iter expect [ []; entries; List.rev entries;
    (member 0, Z.one) :: entries;
    (C_types.{(member 5000) with pubkey = (member 0).pubkey}, Z.one) :: entries;
    (C_types.{(member 0) with address = ""}, Z.zero) :: entries;
    (member 5000, Z.zero) :: entries;
    (C_types.{(member 5000) with pubkey = ""}, Z.one) :: entries ];
  let set = match C_types.make_weighted_validator_set entries with
    | Ok value -> value | Error reason -> failwith reason in
  let sets = [set; C_types.make_validator_set set.validators;
    {set with authors = Some (1_738_000L, [member 5000, Z.of_int 9])}] in
  List.iter (fun set ->
    let wire = old_encode set in
    check "validator bytes changed" (C_codec.encode_validator_set set = wire);
    check "old set no longer decodes" (C_codec.decode_validator_set wire = set);
    check "weight order changed" (C_types.weights_of_set set =
      List.map (fun v -> v, List.assoc v.C_types.address set.weights) set.validators)) sets;
  let wire = old_encode set in
  let start = Sys.time () in
  for _ = 1 to 32 do ignore (C_codec.decode_validator_set wire) done;
  Printf.printf "event = set_decode iterations = 32 members = 4096 cpu_s = %.6f\n"
    (Sys.time () -. start)

let test_header () =
  let module O = Octra_net.Oce1 in
  let wire = O.encode (fun b -> O.put_string b "chain"; O.put_string b "request";
    O.put_raw b (String.make (8 * 1024 * 1024 - 20) '\255')) in
  check "header read body" (C_codec.catchup_reply_header wire = ("chain", "request"));
  let invalid value = try ignore (C_codec.catchup_reply_header value); false with _ -> true in
  check "truncated header accepted" (invalid "\000\000\000");
  check "long header accepted" (invalid (O.encode (fun b -> O.put_string b (String.make 129 'x'))))

let () =
  test_clock ();
  test_header ();
  test_set_bytes ();
  print_endline "status = pass suite = reply_window"