(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module E = Octra_core.Epoch_exec
module FB = Octra_core.Crypto.FheBalance
module C = Octra_consensus.C_types
module CE = Octra_consensus.C_engine
module CH = Octra_consensus.C_hash
module J = Octra_node_runtime.Consensus_finality_journal
module JR = Octra_node_runtime.Consensus_finality_journal_recovery
module PC = Octra_node_runtime.Preverify_cache
module P = Pvac_ffi
module PL = Octra_core.Private_ledger
module R = Octra_core.Preverify_receipt
module T = Octra_core.Transaction
module VP = Octra_core.Pvac_verify_protocol
module VW = Octra_core.Pvac_verify_worker
module W = Octra_core.Preverify_worker

external deserialize_cipher_legacy : bytes -> P.cipher
  = "caml_pvac_deserialize_cipher"

let () =
  Mirage_crypto_rng_unix.use_default ()

let fail reason =
  failwith ("test_private_transition_receipt: " ^ reason)

let expect value reason =
  if not value then fail reason

let bytes value =
  Bytes.make 32 value

let cipher_depth_case () =
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\011') in
  let left = P.enc_value_seeded pk sk 2L (bytes '\012') in
  let right = P.enc_value_seeded pk sk 3L (bytes '\013') in
  expect (P.cipher_mul_depth left = 0) "cipher input depth";
  let sum = P.ct_add pk left right in
  expect (P.cipher_mul_depth sum = 0) "cipher addition depth";
  let product = P.ct_mul_seeded pk left right (bytes '\014') in
  expect (P.cipher_mul_depth product = 1) "cipher product depth";
  let nested = P.ct_mul_seeded pk product sum (bytes '\015') in
  expect (P.cipher_mul_depth nested = 2) "cipher nested depth";
  let square = P.ct_square_seeded pk left (bytes '\016') in
  expect (P.cipher_mul_depth square = 1) "cipher square depth"

let pubkey_error_case () =
  let malformed = Bytes.of_string "PVAC\005\001" in
  begin
    match P.deserialize_pubkey_result malformed with
    | Error _ -> ()
    | Ok _ -> fail "malformed pubkey accepted"
  end;
  begin
    try
      P.deserialize_pubkey malformed |> ignore;
      fail "malformed pubkey legacy accepted"
    with
    | Failure _ -> ()
  end;
  let pubkey, _ = P.keygen_from_seed (P.default_params ()) (bytes '\020') in
  let image = P.serialize_pubkey pubkey in
  match P.deserialize_pubkey_result image with
  | Error reason -> fail reason
  | Ok decoded ->
    expect (P.serialize_pubkey decoded = image) "pubkey recovery after error"

let image_u64 image offset =
  let rec fold index value =
    if index = 8 then Int64.to_int value
    else
      let octet = Char.code (Bytes.get image (offset + index)) in
      fold
        (index + 1)
        Int64.(logor value (shift_left (of_int octet) (8 * index)))
  in
  fold 0 0L

let image_set_u64 image offset value =
  let value = Int64.of_int value in
  for index = 0 to 7 do
    let octet =
      Int64.(to_int (logand (shift_right_logical value (8 * index)) 0xffL))
    in
    Bytes.set image (offset + index) (Char.chr octet)
  done

let wire_cipher version slots size =
  let image = Bytes.make size '\000' in
  Bytes.blit_string "PVAC" 0 image 0 4;
  Bytes.set image 4 (Char.chr version);
  Bytes.set image 5 (Char.chr 0);
  image_set_u64 image 6 slots;
  if size >= 22 then image_set_u64 image 14 1;
  image

let wire_range version slots =
  let image = Bytes.make 526 '\000' in
  Bytes.blit_string "PVAC" 0 image 0 4;
  Bytes.set image 4 (Char.chr version);
  Bytes.set image 5 (Char.chr 4);
  image_set_u64 image 6 64;
  image_set_u64 image 14 slots;
  image

let wire_public_cipher slots layers =
  let image = Bytes.make (38 + (25 * layers)) '\000' in
  Bytes.blit_string "PVAC" 0 image 0 4;
  Bytes.set image 4 (Char.chr 4);
  Bytes.set image 5 (Char.chr 0);
  image_set_u64 image 6 slots;
  image_set_u64 image 14 layers;
  image

let expect_cipher_error name image =
  let expected = "pvac_ser: cipher slots exceed maximum" in
  begin
    match P.deserialize_cipher_result image with
    | Error reason -> expect (reason = expected) (name ^ " result reason")
    | Ok _ -> fail (name ^ " result accepted")
  end;
  begin
    try
      deserialize_cipher_legacy image |> ignore;
      fail (name ^ " legacy accepted")
    with
    | Failure reason -> expect (reason = expected) (name ^ " legacy reason")
  end;
  let encoded = "hfhe_v1|" ^ Base64.encode_exn (Bytes.to_string image) in
  expect
    (Result.is_error (FB.decode_cipher ~strict:true encoded))
    (name ^ " balance decode")

let cipher_wire_case () =
  expect_cipher_error "v1 slot limit" (wire_cipher 1 max_int 63);
  expect_cipher_error "v2 slot limit" (wire_cipher 2 max_int 63);
  expect_cipher_error "v3 slot limit" (wire_cipher 3 max_int 63);
  expect_cipher_error "v4 slot limit" (wire_cipher 4 max_int 63);
  expect_cipher_error "v4 public slot limit" (wire_cipher 4 9 63);
  expect_cipher_error "v5 slot limit" (wire_cipher 5 9 63);
  let prior_public = wire_cipher 4 9 63 in
  begin
    match P.deserialize_cipher_prior_result prior_public with
    | Error reason -> fail reason
    | Ok decoded ->
      expect ((P.cipher_shape decoded).slots = 9) "prior public slot shape"
  end;
  begin
    match P.deserialize_cipher_cap_result prior_public with
    | Error reason -> fail reason
    | Ok decoded ->
      expect ((P.cipher_shape decoded).slots = 9) "capped public slot shape"
  end;
  let too_many_cells = wire_public_cipher 8 513 in
  begin
    match P.deserialize_cipher_prior_result too_many_cells with
    | Error reason -> fail reason
    | Ok decoded ->
      let shape = P.cipher_shape decoded in
      expect (shape.slots = 8 && shape.layers = 513) "prior public cell shape"
  end;
  begin
    match P.deserialize_cipher_cap_result too_many_cells with
    | Error reason ->
      expect
        (reason = "pvac_ser: public cipher size exceeds maximum")
        "capped public cell reason"
    | Ok _ -> fail "capped public cell limit accepted"
  end;
  begin
    match P.deserialize_cipher_result too_many_cells with
    | Error reason ->
      expect
        (reason = "pvac_ser: public cipher size exceeds maximum")
        "active public cell reason"
    | Ok _ -> fail "active public cell limit accepted"
  end;
  begin
    match P.deserialize_cipher_result (wire_cipher 4 8 30) with
    | Error _ -> ()
    | Ok _ -> fail "truncated v4 cipher accepted"
  end;
  begin
    try
      P.deserialize_range_proof (wire_range 4 9) |> ignore;
      fail "nested range cipher accepted"
    with
    | Failure reason ->
      expect
        (reason = "pvac_ser: cipher slots exceed maximum")
        "nested range cipher reason"
  end;
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\017') in
  let values = [|1L; 2L; 3L; 4L; 5L; 6L; 7L; 8L|] in
  let cipher = P.enc_values_seeded pk sk values (bytes '\018') in
  let image = P.serialize_cipher cipher in
  let decoded = P.deserialize_cipher image in
  expect ((P.cipher_shape decoded).slots = 8) "eight slot wire shape";
  expect (P.serialize_cipher decoded = image) "eight slot wire bytes";
  expect (P.dec_values pk sk decoded = values) "eight slot wire values";
  let wide_values = Array.init 9 (fun index -> Int64.of_int (index + 1)) in
  let wide = P.enc_values_seeded pk sk wide_values (bytes '\019') in
  let wide_image = P.serialize_cipher wide in
  let wide_decoded = P.deserialize_cipher wide_image in
  expect ((P.cipher_shape wide_decoded).slots = 9) "v3 wide wire shape";
  expect (P.serialize_cipher wide_decoded = wide_image) "v3 wide wire bytes";
  expect (P.dec_values pk sk wide_decoded = wide_values) "v3 wide wire values"

let image_skip_layers image count offset =
  let rec fold index offset =
    if index = count then offset
    else
      let rule = Char.code (Bytes.get image offset) in
      let offset = offset + 1 + if rule = 0 then 24 else 8 in
      let offset = offset + 32 in
      let r_pc = image_u64 image offset in
      let offset = offset + 8 + (32 * r_pc) in
      let pc = image_u64 image offset in
      fold (index + 1) (offset + 8 + (32 * pc))
  in
  fold 0 offset

let image_edge_end image offset =
  let weights = image_u64 image (offset + 7) in
  let bits = offset + 15 + (16 * weights) in
  let words = image_u64 image (bits + 8) in
  bits + 16 + (8 * words)

let amount_value_image cipher amount =
  let image = P.serialize_cipher cipher in
  expect (Bytes.length image >= 22) "cipher image size";
  expect (Char.code (Bytes.get image 4) = 3) "cipher image version";
  expect (Char.code (Bytes.get image 5) = 0) "cipher image tag";
  let layers = image_u64 image 14 in
  let c0_count = image_skip_layers image layers 22 in
  expect (image_u64 image c0_count = 1) "cipher image slot";
  image_set_u64 image (c0_count + 8) (Int64.to_int amount);
  image_set_u64 image (c0_count + 16) 0;
  let edge_count = c0_count + 24 in
  expect (image_u64 image edge_count > 0) "cipher image edge";
  image, edge_count

let amount_image cipher amount cancel =
  let image, edge_count = amount_value_image cipher amount in
  if not cancel then begin
    let result = Bytes.sub image 0 (edge_count + 8) in
    image_set_u64 result edge_count 0;
    result
  end else begin
    let edge_start = edge_count + 8 in
    let edge_size = image_edge_end image edge_start - edge_start in
    let result = Bytes.create (edge_start + (2 * edge_size)) in
    Bytes.blit image 0 result 0 edge_start;
    image_set_u64 result edge_count 2;
    Bytes.blit image edge_start result edge_start edge_size;
    Bytes.blit image edge_start result (edge_start + edge_size) edge_size;
    Bytes.set result (edge_start + 6) (Char.chr 0);
    Bytes.set result (edge_start + edge_size + 6) (Char.chr 1);
    result
  end

let amount_cipher cipher amount =
  let image, _ = amount_value_image cipher amount in
  P.deserialize_cipher image

let empty_amount_proof =
  String.concat
    ""
    [
      "Kk8ITJf0iUeUnCLDwmGimuhD7Q9QYbcnxaCb/sfL/Tpy28U0o1Ejxe8ooTOQA+BEt/hzy7UdjpJtT/dqJk7IFFo6arH1t2Yk";
      "kFhzCnbg9vhkbkhFBOa8sfpYcCIckR8E6s3v5KTo3V9LUdfBMDHSVxLQB9scBaLhan82sQ3BCzEiLhIo+o70D7GgyvbEP3KG";
      "v7J9skxpQC8Cc1/d/EoPY8qw0kJ2xftN2Qv6jKONdUq0/z323aN1ohmE9TaEVuB4dk20uTzcThPCRnvkMyFkNTzUm7HgjuUK";
      "ZDZbc/Xj5nKU/vuVI6DMuD6iDcNKg6J1HBctIY8V7AZG401V9EPrJK2rTYbfTvlz9IT4Q0Ye4UQk5e/9+IChOEL1kx1dy80J";
      "Y8EDdwYWbakjxDhmMV90UusUOgLoKZ6oOusgRqQ5PA7OCXxIz545Ma+zEdKnCdQ+VLnW14gfHUs5DRrbXJOmAhIAAAAAAAAA";
      "2kGq3zyXfOfoaVExyqrUbIdlLtulOK2d1laQ8PFfGGsoxtKBe20sM0A8Gohpigkbde9J8pZmUPSkMBv0rOL9ZxbK0ufhfrmE";
      "os6NVY419EkWu28JIqltWP2sDmCAmOZyiB7k4i4NbhjiY8YdYEQs1HVht5HmnF/ViA6Tju0Xn3JaWfRR4rCdOHTS27/TwHPi";
      "LshF/2MfD2ddB/V3wbnSWABxF1uxA0yyaTGOGyNL6aTciWqN26uGZKw3VWIaRLkUmmP7GfCP/pCaGvCsMgt8ksfet9jjr2cS";
      "CTjv39n7wSM4cqbmub7wgYvScKMpuNUBIzsxofUcvvhs4MbtN/GLbr7PB0Ad/2DG3B4IP75P3reghhVTSY2nWXVeW1w36nZG";
      "PLLA8QmFvDyQu7+ojCgB5c7RjlgftV5sysYYB1tcsWL6EDL0cEEXHqCScHzF06bvvVTc3ft89nB9agJY7Be4HuoomFQ+K6fn";
      "28K8xiFwtkyctq2ZcjVkKbjos6pFE+kkLoz3bC4pJJfiRGNoWHmAdB7sbYUrwQKeDhoX3974D0hcQnp5rSiw33ahgiTuw5h+";
      "3yWYWGV6yMP0UYnmU38vJ06l0oC2mTY9ry1MOFCCc7gdiCoyb+i5wCvNJM88OzUV1CvnQ80B05K8eyvoCdqySqc13kPvU+78";
      "EAwNxngAmUQqVfpOjH17fWaIXAM5r1R1D4B5xNASKCgwYiBYGqjqXxz5/J/hv9vS164y6X7KUe5lgoNEatQMdMxi0rz8y/EO";
      "1hROTd+oCDXlVf9Kl41vSUbK8q2s1NjL0zqAoxffUjdaF0YtpKFk1RUqLvAOiQYUD86RdKpN6cDmSXR61P5JHnyzjtaENEak";
      "Jp3BADng8F8PWS8th0qcB4n/NwfEWL9c9P/vvmE9MbwQTBAKkFT+VE5eQNiWs02K9cCNZWqmYRcyoqxrK4Oziui8vbvtGEhR";
      "fxbny264sKda/P/ZwGwcYVY4MdIZTA9ijWjHMSPkAUYTSkgiaiC0n9i223BqjFM3tDK8FBXBonhLU23a/mIdCwjrlhUy6frk";
      "XV4ZPp0Rkhy8ke0Bsio3Nnz0kw02NklC59kXMlQVa0MyCY0tGSx3fZxwTmLMfiOC6dvDbIBzOVpQhKMb5Pq2Rj+XM9DE7mBW";
      "2qqyI2ooe1WCrzihllpI7vcviaxFUKgLt5XUHzQT6DbqoEnYrDEtr4NtcZmWlS78gx+gb9DoUhGrFvEpmGmNTcZyjY6e/Wfb";
      "hq8qDXBzUXX1WZmpXD8CymsPbwNJhHdpHuqhnkhICPwCwVGpTmoMWOcnsEPQ5KbghosaburV+AQWLolbAsU1IUmCK+YfVPdw";
      "QkG98wmHhxhuR1IENdPDRFaMn90so+FdZPqmgsb5VFPX77KYAHczVt1KYu2wmfF1yqwSQ6Ep343XwY+WdOxRT8av+VCf9rZD";
      "ncRVtaXJKHVqju92C9TerMyN8eiddSkBL5/tjf5VIhTm1phoAeKvdvT9O9UBj69b2l1t/N+kH4R7fGWa0WY5Lp5AA8fGcQwx";
      "xSxVOL/fGtR9UgGobV3LhbsZjkKieatFVLnWZTwkWQ6p+rZQs9dLMG3kgb858GoCN9qjzSGG22mF+D02QQXhDQYAAAAAAAAA";
      "3Pk6KgU4No2IN0wkpWBl47DD6Us3jXjobKTsXri7cyg4bFtnom2ph7iYwNP1DAgG8I+sBk1VGPVrkjQC6aG/Z87CS+QpuSOb";
      "riqizeY+ZY4ZDMHFr3t5gIhIC7+sBm5Bdsf2APWIG4PGBlGyzb4fhip9Br7lRWw+nlKd45F/WAAOOKkKUGDJYrOMWIwpctWp";
      "yi4uqNKVFikUg2s8WIafDUISk5fA4Nd8XlrqvR20x4cDO2f5uWnhmzs9x/rnA0R5AQ==";
    ]

let cancelled_amount_proof =
  String.concat
    ""
    [
      "kFMp2u7wB1voTmAekG2BxEuuCwy+cMAIqU0ZzJ0Q33jqYWoVr1uHsRwUSo01ompKf2xOiEIdAvX8XhwgtSCxW9yNR/pPoyDu";
      "Tq2J+jzNO7Zw0DcNfdqLNvzwaFFDQfcHtjBTguqbMQmEY4+ehqCbwjsY7uZGxNSzuLQoxwVCVW+qS1m7YL1A4rdKbi1hNLQJ";
      "Ul/JcMg5Tm3Yf4B1wXAwfuCK+udbMxc6Hs1LKR3zjXw7S5DOM6u9/U4JJc4DXgI0WkrpCF4K09bmnT7vyaOBRUn1Txk5+MaP";
      "ddTq2a78YC0QEzBBnLi5c7df/vU3TISlanMUABYz+Ye6yB4laro9Qn7OugJiTvNj4Gfz0eFZUpqMNEITq0VWWvomdqZY1boC";
      "MWOK+WzZW4R/ujQy3V673SaUhFh4+lSw7G6MnfrpIAHVQGwkPIm+9YSivBZU7n7bzhTkWF3RiOYieOrjmpdYBhIAAAAAAAAA";
      "/LCDH0NvLry6h+w8XA85tObfwKM3Ab0rYD+fWRqc6xOEw7xmU/aL5vkMBQ7mP13zRHEryeTXPt6yh2kbgGRlNEQRa6gQDFd2";
      "9JTjMpBd2PKJeBXYG3pC9rl20reN+NkkyGtrup6ObzmuPFjyC3o2w2Ip1meJ9IhPziwm8Gt0uEggza1WQooDTsg1vOMQ+ij4";
      "AHSWTM/VVusqjbrBf4ecX956/RPX7vwHIpz4vpzImpHBBMwvKE2ewSZCffx2D15JDtXg1Efzxi7X09c9lY2Sl2nBo2kMGh1d";
      "D87/xNm8dB0CY/gvXTxdndsXKobnbNCWzzfqCRCt9ukV/PFP3npJKoy4sDd0NHxrBe/LSJjhHoAlE1yweFnrwCKhEsu71zIV";
      "HFRvcFijcFV+zVxe5VHgRCns1fwcXlYZZ3DS0xE1RF7u/9bBRtFAPUu1e7kfAZY0Hi9ofpUgdZeBcCmVq9LRF1TmDEnrTc0W";
      "RuxdRYv4IlRuMfh1pGmJAIeyRZRXVCN4SmsW6WjM74PvfhDzQMlg9BF1EPdY4pW6nNbVoEnQlDfiyrRMYVwzbQGHMQD678X2";
      "u03QXX2xW28cLdYezbzpSK75LjqIWLJny5wR5BIadYsnZfUR4PHGjlYN9VLS0B8EpqJxpS0+iuv+vk8g/vb50PD3/CFLUZr+";
      "hxAHdHoHBkLIWUauvBflAwcagubLH/a3G/AiJXVFt0ymjwQJZB4VT8zGqHXocbW6FnzEP9rL8D+R9UnXEanb070P3mpmmfJC";
      "JFLlFT40pzMeGPstfz2lqq/GvTn1Az1SvpfNkiuW4AyIIPsl6bfuFmhW6FMhmXE1yuLPYn1PdjzMYYsM5/0xKsruSaVO3KJD";
      "L9f//ZoGVAGeWC/EpnBtoXirdGEbQAU9jLJr8H06Xgsp/efQd9D0WtbqFM/R7W8NYMhB0T7IgWVYgPdN61qi4CxWJL2R0gug";
      "gNX5oFu1TRJLzD+O/0TqRVw9j9lFb8cgEXN8B9hCmSimqnVLPj8+6zyeLNyhvvp0wOhM29eipLTxrsHLB0Qs5OhoE5KRXayo";
      "JCp3/NK3HUsA5c5R6jZARPve2HNgf0CnI3wQttxsGURoJtQGtBNvNMKhHkm0CNcbnNApVhFv793CsdZKwwchvWcB9lSC6104";
      "7EanC48BPo0SvwIRpzfL9nwxcPCmopxeEB8jHAQgIQromyUOFeAWrPPd0JNkTHJiLcCt5JCEsOdJyt+y4tYEecCxchFPdSdc";
      "2PUceUEcGAWaxL9H4JU88edQV/vPXD9rDECKDEn+CiSVMybajYqmj6cT4d5OOdHv91zsWEu6yk1SbSuVg+Z7JG8ihUAJ4ajj";
      "pR67DhUfS3P9aKD6ilUKdoTG4q1sBESFX/FOpmLtqDOQP+oDBZ/gpZLdEUWivTR2vDhIA8TDhPN2wk8LpjVjguZfxY3b0iRg";
      "mERfapRw7ERmAW5xi+19J3znACCt73lTxzdXGB62pEitR2EBG9lbe5pWeisSN9hPvFOIeNq1eXzG1aMMfkAtCh73L6m9FHRN";
      "pyco6cr7+7WCkik8Q7S8xpHu6XeYFrW6VeXbm/vaMQcY9ZSfydVHkqG0Eocvh9ExcPZJCMWWjxdducpHh9e6CwYAAAAAAAAA";
      "3Pk6KgU4No2IN0wkpWBl47DD6Us3jXjobKTsXri7cyg4bFtnom2ph7iYwNP1DAgG8I+sBk1VGPVrkjQC6aG/Z87CS+QpuSOb";
      "riqizeY+ZY4ZDMHFr3t5gIhIC7+sBm5Bdsf2APWIG4PGBlGyzb4fhip9Br7lRWw+nlKd45F/WAAOOKkKUGDJYrOMWIwpctWp";
      "yi4uqNKVFikUg2s8WIafDUISk5fA4Nd8XlrqvR20x4cDO2f5uWnhmzs9x/rnA0R5AQ==";
    ]

let duplicate_layer_amount_proof =
  String.concat
    ""
    [
      "3svoMgoUtceYMSLix6gmlgOG5vwpNN0dqcqm+zO/TGtkuhmGCkR4pEptQmszkaP3WDX0aA1WTgoPox/BSKe5aZbdlUMXbK4UcoTP7q/X";
      "TOv+psEfyunPoMGteyLq4t5xML6nMbAbG+1+OGJCuK61TGK1qT+xL4gZwokvA7mGljHOlql5jbR55ZmB2tvXmou/pgYIV6vwqmGbYYnb";
      "FpZ8WNwaAmfk/N9heRGWL9zCChECwLQd7MGOi1d1Ey8vE+4A7pFcBcDaq17uORKDZoGUS7j/iV6Yf+6AX7On5lkJg1WaybV810lVllVj";
      "99MWgGfK0nh97Vy83G79fqCdXzoRIZYDYoWfmNODJmelmv4gF4sEOVjmkSNqzQ/iUYNmtu0FhdgVhNnYBxhq/dObDhAzSFeGeElPN9GI";
      "PJHtmvyIkQy6juq+G8btNYGs07aFqqdh2H2zR32Y1fvKIeuoRMPxCRMAAAAAAAAAYLcU5KKz/XD6pF4aApOoRng74662oqqGtI5Xrn5A";
      "sUB+czBWkZdEAcVlrh2bPnVbX8z2Rr7CqJPB3pvQ/2v9TRgQhT62kW6X+9bP8Q25+3mmbb2DGR+PVeWvUJlSe9xxQFgkz/Wp+/ll+z2H";
      "+swRQPiyUjxeq3YzGETBBCsC1jUehilhEeuphfgLkNpAcZtyUGdiESH7gMuWoLwHcpmpCwh0deMysy0DhtAfGGZzOQXo90DGOOY8i0fT";
      "opOaTRogPtqJUNGHrEH4vWLEggSKYTCsTab0CdQtMRq9+qipnVNSyijKRuahfq930rbb2X7VRM1CRWHuTzCI0iLONTd2WpDIE4XA3ofn";
      "m/SBA8DbyLWDYuGR67ZyateCpox+upxwItZYrQ/m4LrZrw7SZzbaR/4Jjn3rNX95BcOlE7KV80701Od6Y5L5KTErgsxe/kRuafn2lAgu";
      "JPTgyNvEr5xiS9hXeFXqXdeck5wXIZIcy4vvL1kPur91DQFjO4i+SGF4LCyEFUbBuDmI+O3kRwhX3cW+jDj/eaSU4RDJ8jgpz34Y9CVc";
      "8G04Nox/RjxcCcAzpjm49nfpWuESQ86AIgA7ZwDsjJMYjgkhNCAGbZL28vPixKuEe0G3kuG8i9f5keUuAPCCbovU55N2ttNc1GCGMa3a";
      "UEO2YBeApyowGIp110hKeB3CTVo2tD+6CI9RtHysG6worCJK6huyGUPPILTefrjboKLCoCHy6RRQLyNGSss3i/hpotq2WBtuaqLChEEG";
      "GrdvsfYZsXrDuI+DAOvWgWudW1bOLcXwRaAEPufnZQZwCsaAa+8AGCrF0d/w29jsR+XOVqfkC8hK4Ecqo7vFPliktVtYnX+opqAONBre";
      "b9wRYMQv0iXEoqQV/i2Fge4HbMZlEqSIzxUKKTsjq56D2xXKmC32Vwv05zO5D9hfKG2iZjL7i/MGP/5VGMYzbcELMPZo2GGBy70vrdhm";
      "maslegQAETuU6OVR7O/725YdYcoN4du0nynddKr/tl9CRDspQJNgn1aKYZgYN4lzOTzsJVUBT+tfLCCwjUlkAqTXFGcAOXSZEzdi8Qn+";
      "s3SZMrkh5mjo8DO78CJg76FpXa63IS6mi5M5ug8oQbDlg6h7tuV2ACz83Hne2fx8SN7+eGYHrJcx8nefafMd4TUmYkitSXWH2i92Hn2x";
      "uyhc7z6rVl8mJfiitIfoktTnalHhT8ML8om/cH00ah+hPv5ZFsgXdpjXg/Qxd0YGCTpMh0HwtnXHjUZzkI8U0E7o41aOpitWfG7x1Dho";
      "sRJOUhRLBDDh0QVxoPIMjqDG9ZmJ0vsJ+DKwyXcHXzhc4GmLEaRLtaUkQmsz65sfwOtvR6N5ojspQm5r3jQQPIlmPLdhGtVZjAsF95Ad";
      "7DhCRHLgi5CFl09ojH3+KxaTwg+iPgn4dN2bE6ZSttU5lIP2uoFNu3n99QrWVTfLSfmCEqDiSfCZLZBH/PaxUUnHfe38ytyYXNmrOTzB";
      "JUSDwLz12Jp2wbXm411FvGCuXo3hXOETMG6W4TBB/q0G6p5qT8Kl4W6qEhGOZIql2Z8/U9iDTzLMJ3vLQ1O6qSlckHfa/wEJDnIKQy1p";
      "YQKkWC6gh0f3emIG/8aodv8KGedBbTIDcY5gMRW7TGfcl4GnLAR1IBMta80IhksMRRRECUnOc2cj853b0imhuD0i74d0HsjOB/kHcRBI";
      "JA8KAAAAAAAAANz5OioFODaNiDdMJKVgZeOww+lLN4146Gyk7F64u3MoOGxbZ6JtqYe4mMDT9QwIBvCPrAZNVRj1a5I0Aumhv2fOwkvk";
      "Kbkjm64qos3mPmWOGQzBxa97eYCISAu/rAZuQXbH9gD1iBuDxgZRss2+H4YqfQa+5UVsPp5SneORf1gADjipClBgyWKzjFiMKXLVqcou";
      "LqjSlRYpFINrPFiGnw04bFtnom2ph7iYwNP1DAgG8I+sBk1VGPVrkjQC6aG/Z87CS+QpuSObriqizeY+ZY4ZDMHFr3t5gIhIC7+sBm5B";
      "dsf2APWIG4PGBlGyzb4fhip9Br7lRWw+nlKd45F/WAAOOKkKUGDJYrOMWIwpctWpyi4uqNKVFikUg2s8WIafDUISk5fA4Nd8XlrqvR20";
      "x4cDO2f5uWnhmzs9x/rnA0R5AQ==";
    ]

let amount_proof image =
  image
  |> Base64.decode_exn
  |> Bytes.of_string
  |> P.deserialize_zero_proof

let protocol_mode_case () =
  let pubkey = "key" in
  let prior =
    VP.Claim {
      pubkey;
      cipher = "cipher";
      proof = "proof";
      commitment = "commitment";
      strict = false;
    }
  in
  let expected =
    Yojson.Safe.to_string
      (`Assoc [
        "schema", `String "octra_pvac_verify";
        "op", `String "claim";
        "pubkey", `String (Base64.encode_exn pubkey);
        "cipher", `String "cipher";
        "proof", `String "proof";
        "commitment", `String "commitment";
      ])
  in
  expect
    (String.equal (VP.request_bytes prior) expected)
    "prior request bytes";
  begin
    match VP.request_of_string expected with
    | Ok (VP.Claim value) -> expect (not value.strict) "prior request mode"
    | Ok _ -> fail "prior request operation"
    | Error reason -> fail reason
  end;
  let active =
    VP.Claim {
      pubkey;
      cipher = "cipher";
      proof = "proof";
      commitment = "commitment";
      strict = true;
    }
  in
  expect
    (not (String.equal (VP.request_hash prior) (VP.request_hash active)))
    "proof mode request hash";
  let prior_range =
    VP.Range {
      pubkey;
      cipher = "cipher";
      proof = "proof";
      strict = false;
    }
  in
  let expected_range =
    Yojson.Safe.to_string
      (`Assoc [
        "schema", `String "octra_pvac_verify";
        "op", `String "range";
        "pubkey", `String (Base64.encode_exn pubkey);
        "cipher", `String "cipher";
        "proof", `String "proof";
      ])
  in
  expect
    (String.equal (VP.request_bytes prior_range) expected_range)
    "prior range request bytes";
  begin
    match VP.request_of_string expected_range with
    | Ok (VP.Range value) -> expect (not value.strict) "prior range request mode"
    | Ok _ -> fail "prior range request operation"
    | Error reason -> fail reason
  end;
  let active_range =
    VP.Range {
      pubkey;
      cipher = "cipher";
      proof = "proof";
      strict = true;
    }
  in
  expect
    (not
       (String.equal
          (VP.request_hash prior_range)
          (VP.request_hash active_range)))
    "range proof mode request hash"

let cache_mode_case () =
  let hash = String.make 64 'c' in
  let sender_enc_snapshot = "cipher" in
  let result : PC.result = {
    delta_ok = true;
    balance_ok = true;
    strict = false;
    sender_enc_snapshot;
  } in
  PC.remove hash;
  PC.insert_with_cap hash (Lwt.return (PC.Checked result));
  expect
    (Option.is_some
       (PC.ready_result hash ~strict:false ~sender_enc_snapshot))
    "prior cache result";
  expect
    (Option.is_none
       (PC.ready_result hash ~strict:true ~sender_enc_snapshot))
    "active cache isolation";
  expect
    (Option.is_none
       (PC.ready_result hash ~strict:false ~sender_enc_snapshot:"other"))
    "cipher cache isolation";
  PC.remove hash

let amount_link_case () =
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\021') in
  let amount = 29L in
  let blind = bytes '\022' in
  let source = P.enc_value_seeded pk sk amount (bytes '\023') in
  let commitment = P.pedersen_commit_amount amount blind in
  let check name cancel proof_image =
    let cipher = P.deserialize_cipher (amount_image source amount cancel) in
    let proof = amount_proof proof_image in
    expect (not (P.verify_zero_bound pk cipher proof commitment)) name
  in
  check "empty edge amount link" false empty_amount_proof;
  check "cancelled edge amount link" true cancelled_amount_proof;
  let duplicate = P.ct_sub pk source source |> fun cipher -> amount_cipher cipher amount in
  let duplicate_proof = amount_proof duplicate_layer_amount_proof in
  expect
    (not (P.verify_zero_bound pk duplicate duplicate_proof commitment))
    "duplicate layer amount link";
  let cipher = P.deserialize_cipher (amount_image source amount false) in
  let proof = amount_proof empty_amount_proof in
  expect
    (P.verify_zero_amount_prior pk cipher proof commitment)
    "prior empty edge amount link";
  let request =
    VP.Circle_cell {
      pubkey = P.serialize_pubkey pk |> Bytes.to_string;
      cipher = FB.encode_cipher cipher;
      ciphertext_commitment =
        P.commit_ct pk cipher
        |> Bytes.to_string
        |> Base64.encode_exn;
      proof_kind = VP.Circle_bound_zero;
      proof = FB.encode_zero_proof proof;
      amount_commitment =
        commitment
        |> Bytes.to_string
        |> Base64.encode_exn;
      strict = true;
    }
  in
  match Lwt_main.run (VW.classified_result request) with
  | Error (VW.Proof_rejected _) -> ()
  | Error failure -> fail (VW.verification_failure_message failure)
  | Ok () -> fail "unlinked circle amount accepted"

let zero_proof_cache = ref None

let zero_proof pk sk cipher amount blind =
  match !zero_proof_cache with
  | Some proof -> proof
  | None ->
    let proof =
      P.make_zero_proof_bound pk sk cipher amount blind
      |> FB.encode_zero_proof
    in
    zero_proof_cache := Some proof;
    proof

let rec remove_tree path =
  if Sys.file_exists path then
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name ->
        remove_tree (Filename.concat path name));
      Unix.rmdir path
    | _ ->
      Unix.unlink path

let clear_case path =
  remove_tree path;
  remove_tree (path ^ "_pvac")

let addr =
  "oct" ^ String.make 44 '1'

let tx encrypted_data =
  T.{
    from = addr;
    to_ = addr;
    amount = Z.of_int 10;
    nonce = 1;
    ou = Z.one;
    timestamp = 1.;
    signature = "sig";
    public_key = Some "pub";
    message = None;
    op_type = EncryptOp;
    encrypted_data = Some encrypted_data;
  }

let pending_case () =
  let first = { (tx "first") with T.op_type = DecryptOp } in
  let second = {
    first with
    T.nonce = 2;
    timestamp = 2.;
    signature = "sig2";
    op_type = StealthOp;
    encrypted_data = Some "second";
  } in
  let check txs =
    match W.isolate_private_transitions txs with
    | [ready], [skip] ->
      expect (T.hash ready = T.hash first) "pending first";
      expect (T.hash skip.W.tx = T.hash second) "pending second";
      expect
        (skip.reason = "private_transition_dependency_deferred")
        "pending reason";
      expect (skip.kind = W.Deferred) "pending kind"
    | _ -> fail "pending shape"
  in
  check [first; second];
  check [second; first];
  let other = {
    second with
    T.from = "oct" ^ String.make 44 '2';
    to_ = "oct" ^ String.make 44 '2';
    nonce = 1;
  } in
  match W.isolate_private_transitions [first; other] with
  | ready, [] -> expect (List.length ready = 2) "pending independent"
  | _ -> fail "pending independent shape"

let payload pk sk proof_ok =
  let amount = 10L in
  let blind = bytes '\003' in
  let cipher = P.enc_value_seeded pk sk amount (bytes '\002') in
  let zero_proof =
    if proof_ok then
      zero_proof pk sk cipher amount blind
    else
      "invalid"
  in
  Yojson.Safe.to_string
    (`Assoc [
      "cipher", `String (FB.encode_cipher cipher);
      "amount_commitment",
      `String
        (P.pedersen_commit_amount amount blind
         |> Bytes.to_string
         |> Base64.encode_exn);
      "zero_proof", `String zero_proof;
      "blinding", `String (Base64.encode_exn (Bytes.to_string blind));
    ])

let legacy_replay ~epoch:_ ~address:_ ~cipher:_ =
  {
    Octra_core.Pvac_legacy_public_replay.audit_class =
      Octra_core.Pvac_legacy_public_replay.Poisoned;
    can_public_migrate = false;
    public_net = None;
    commitment_net = None;
    blockers = ["unused"];
    effects = [];
    reason = "unused";
  }

let setup name =
  let path =
    Filename.concat
      "runtime_data/private_transition_receipt"
      (Printf.sprintf
         "private_transition_receipt_%s_%d"
         name
         (Unix.getpid ()))
  in
  clear_case path;
  let store =
    Lwt_main.run (Octra_core.Store_irmin.open_store path)
  in
  let ledger = Octra_core.Ledger.create store in
  let pk, sk =
    P.keygen_from_seed (P.default_params ()) (bytes '\001')
  in
  begin
    match Octra_core.Ledger.add_account ledger addr (Z.of_int 1_000_000) with
    | Ok () -> ()
    | Error e -> fail e
  end;
  Lwt_main.run
    (Octra_core.Ledger.set_pvac_pubkey
       ledger
       addr
       (P.serialize_pubkey pk |> Bytes.to_string));
  Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt ledger);
  path, store, ledger, pk, sk

let cache_key_mode_case () =
  let path, store, ledger, _, _ = setup "cache_key_mode" in
  let transaction = { (tx "switch") with T.op_type = T.KeySwitch } in
  let key cap =
    Lwt_main.run (PL.key_switch_cache_key PL.Unique_fields cap ledger transaction)
  in
  expect (String.equal (key false) (key false)) "prior cache key stable";
  expect (String.equal (key true) (key true)) "active cache key stable";
  expect (not (String.equal (key false) (key true))) "cache key mode isolation";
  Lwt_main.run (Octra_core.Store_irmin.close store);
  clear_case path

let migration_case () =
  let module M = Octra_core.Pvac_migration in
  let pk, sk = P.keygen_from_seed (P.default_params ()) (bytes '\001') in
  let pubkey = Some (P.serialize_pubkey pk |> Bytes.to_string) in
  let cipher = P.enc_value_seeded pk sk 7L (bytes '\002') |> FB.encode_cipher in
  List.iter
    (fun cap ->
      expect (M.classify_cipher ~cap "0" = M.Empty) "empty migration cipher";
      expect (M.classify_cipher ~cap cipher = M.V3) "current migration cipher";
      let status = M.status_of_state ~cap ~cipher ~pubkey in
      expect (status.cipher_class = M.V3) "migration status cipher";
      expect (status.key_class = M.Current) "migration status key";
      expect (M.can_bound_migrate status) "migration status route")
    [false; true];
  expect
    (M.classify_cipher cipher = M.classify_cipher ~cap:true cipher)
    "migration rpc policy"

let circle_policy_case () =
  let module H = Octra_core.Circle_wasm_hfhe_backend in
  expect (H.require_cap [] = Error "missing cap") "circle mode missing";
  expect
    (H.require_cap ["cap", `String "false"] = Error "invalid cap")
    "circle mode type";
  List.iter
    (fun cap ->
      expect (H.require_cap ["cap", `Bool cap] = Ok cap) "circle explicit mode")
    [false; true]

let receipt ledger transaction transition_hash =
  let root = Lwt_main.run (Octra_core.Ledger.hash ledger) in
  let pre_state_hash = W.state_hash root in
  let state =
    match
      Lwt_main.run
        (W.source_binding ledger pre_state_hash transaction)
    with
    | Ok state ->
      {
        state with
        R.transition_hash = Some transition_hash;
      }
    | Error e -> fail e
  in
  match
    R.for_tx_bound
      ~input_hash:(W.bound_input_hash transaction state)
      ~output_hash:(W.bound_output_hash transaction state "ok")
      ~state
      ~ok:true
      ~reason:""
      transaction
  with
  | Ok value -> value
  | Error e -> fail e

let env =
  E.{
    chain_id = "octra-transition-receipt";
    epoch_id = 1;
    proposer_addr = addr;
    validator_addrs = [addr];
    validator_pubkeys = [];
    prev_state_root = String.make 64 '0';
    epoch_ts = 1.;
    ready_state_root_at = None;
    ready_max_lag = 0;
  }

let process proof_mode store ledger transaction receipt =
  let gate =
    Octra_core.Preverify_commit.create [receipt]
  in
  begin
    match
      Lwt_main.run
        (Octra_core.Preverify_commit.check_bound
           ledger
           gate
           [transaction])
    with
    | Ok () -> ()
    | Error e -> fail e
  end;
  let transition =
    Octra_core.Private_transition.create
      ~preverify:(Some gate)
      ~ledger
      ~epoch_id:env.epoch_id
      ~owner_migration_mode:Octra_core.Rule_graph.Active
      ~proof_mode
      ~field_policy:Octra_core.Private_ledger.Unique_fields
      ~result_policy:Octra_core.Private_result_policy.Recoverable
      ~legacy_replay
      ~limits:Octra_core.Private_transition.{
        max_fhe = 1;
        max_stealth = 1;
      }
  in
  Lwt_main.run
    (Octra_core.Private_transition.process
       transition
       ~backend:(E.make_live_backend store ledger)
       ~env
       transaction)

let valid_case () =
  let path, store, ledger, pk, sk = setup "valid" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk true) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let prior_artifact =
        match
          Lwt_main.run
            (PL.preverify_private_artifact
               ~field_policy:PL.Unique_fields
               ~strict:false
               ledger
               transaction)
        with
        | Ok artifact -> artifact
        | Error e -> fail e.PL.reason
      in
      begin
        match
          Lwt_main.run
            (PL.bind_private_artifact
               ~field_policy:PL.Unique_fields
               ~strict:true
               ledger
               transaction
               prior_artifact)
        with
        | PL.Private_source_changed -> ()
        | PL.Private_bound _ -> fail "prior artifact crossed proof activation"
        | PL.Private_artifact_invalid rejection ->
          fail rejection.PL.private_preverify_reason
      end;
      let transition_hash =
        PL.hash_prepared (PL.Prepared_encrypt plan)
      in
      let receipt = receipt ledger transaction transition_hash in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            store
            ledger
            transaction
            receipt
        with
        | Ok fee -> expect (Z.equal fee Z.one) "certified fee"
        | Error (_, reason) -> fail reason
      end;
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 1) "certified nonce";
        expect
          (account.encrypted_balance = Some plan.PL.next_cipher)
          "certified cipher"
      | None -> fail "certified account missing")

let mismatch_case () =
  let path, store, ledger, pk, sk = setup "mismatch" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk true) in
      let receipt =
        receipt ledger transaction (String.make 64 'f')
      in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            store
            ledger
            transaction
            receipt
        with
        | Error ("preverify_transition_mismatch", _) -> ()
        | Error (_, reason) -> fail ("unexpected mismatch: " ^ reason)
        | Ok _ -> fail "mismatched transition accepted"
      end;
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 0) "mismatch nonce";
        expect
          (account.encrypted_balance = None)
          "mismatch cipher"
      | None -> fail "mismatch account missing")

let forged_case () =
  let path, store, ledger, pk, sk = setup "forged" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk false) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let transition_hash =
        PL.hash_prepared (PL.Prepared_encrypt plan)
      in
      let receipt = receipt ledger transaction transition_hash in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            store
            ledger
            transaction
            receipt
        with
        | Error ("bad_zero_proof", _) -> ()
        | Error (tag, _) -> fail ("unexpected forged result: " ^ tag)
        | Ok _ -> fail "forged proof accepted"
      end;
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 0) "forged nonce";
        expect (account.encrypted_balance = None) "forged cipher"
      | None -> fail "forged account missing")

let prior_receipt_case () =
  let path, store, ledger, pk, sk = setup "prior_receipt" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk false) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let receipt =
        receipt
          ledger
          transaction
          (PL.hash_prepared (PL.Prepared_encrypt plan))
      in
      match
        process
          Octra_core.Rule_graph.Prior
          store
          ledger
          transaction
          receipt
      with
      | Ok fee -> expect (Z.equal fee Z.one) "prior receipt fee"
      | Error (tag, _) -> fail ("prior receipt rejected: " ^ tag))

let worker_retry_case () =
  let path, store, ledger, pk, sk = setup "worker_retry" in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close store);
      clear_case path)
    (fun () ->
      let transaction = tx (payload pk sk true) in
      let plan =
        match
          Lwt_main.run
            (PL.prepare_encrypt_plan
               ~field_policy:PL.Unique_fields
               ledger
               transaction)
        with
        | Ok plan -> plan
        | Error e -> fail e.PL.reason
      in
      let transition_hash =
        PL.hash_prepared (PL.Prepared_encrypt plan)
      in
      let receipt = receipt ledger transaction transition_hash in
      let worker =
        Octra_core.Pvac_verify_worker.worker_path ()
        |> Option.get
      in
      let retried =
        Fun.protect
          ~finally:(fun () ->
            Unix.putenv "OCTRA_PVAC_VERIFY_WORKER" worker)
          (fun () ->
            Unix.putenv
              "OCTRA_PVAC_VERIFY_WORKER"
              "runtime_data/private_transition_receipt/absent_worker";
            try
              ignore
                (process
                   Octra_core.Rule_graph.Active
                   store
                   ledger
                   transaction
                   receipt);
              false
            with
            | PL.Worker_retry _ -> true)
      in
      expect retried "worker failure became a transaction rejection";
      match Octra_core.Ledger.find_opt ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 0) "retry nonce";
        expect (account.encrypted_balance = None) "retry cipher"
      | None -> fail "retry account missing")

let circle_reject_case () =
  let request =
    VP.Circle_cell {
      pubkey = "invalid";
      cipher = "invalid";
      ciphertext_commitment = "invalid";
      proof_kind = VP.Circle_bound_zero;
      proof = "invalid";
      amount_commitment = "invalid";
      strict = true;
    }
  in
  match Lwt_main.run (VW.classified_result request) with
  | Error (VW.Proof_rejected _) -> ()
  | Error failure -> fail (VW.verification_failure_message failure)
  | Ok () -> fail "forged circle proof accepted"

let validator_keys =
  ["octA"; "octB"; "octC"; "octD"]
  |> List.map (fun address ->
    let private_key, public_key = Mirage_crypto_ec.Ed25519.generate () in
    C.{
      address;
      pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public_key;
    },
    private_key)

let validator_set =
  validator_keys
  |> List.map fst
  |> C.make_validator_set

let sign address value =
  let _, private_key =
    List.find
      (fun (validator, _) ->
        String.equal validator.C.address address)
      validator_keys
  in
  Mirage_crypto_ec.Ed25519.sign ~key:private_key value

let finalized transaction receipt =
  let receipt_json = R.canonical receipt in
  let tx_hashes = [T.hash transaction] in
  let header =
    C.{
      proto_version = proto_version_current;
      chain_id = "octra-transition-recovery";
      epoch_id = 1L;
      prev_state_root = String.make 32 '\x11';
      tx_list_hash = CE.tx_list_hash_for_header tx_hashes;
      receipt_root = CH.receipt_root [receipt_json];
      proposed_state_root = String.make 32 '\x22';
      parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = "octA";
      txid_hi = 1L;
      ts = 1.;
    }
  in
  let proposal_id = CH.proposal_id header in
  let vote address =
    let unsigned =
      C.{
        chain_id = header.chain_id;
        epoch_id = header.epoch_id;
        round = 0;
        vote_type = Precommit;
        proposal_id;
        validator = address;
        signature = String.make 64 '\x00';
      }
    in
    {
      unsigned with
      signature = sign address (CH.vote_sign_bytes unsigned);
    }
  in
  C.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    commit_round = 0;
    header;
    proposal_id;
    precommits = List.map vote ["octA"; "octB"; "octC"];
    parent_commit = None;
  },
  J.{
    tx_hashes;
    txs = [transaction];
    receipts_json = [receipt_json];
  }

let recovered_bundle result =
  let stored = ref None in
  let recovery =
    JR.{
      read_journal = (fun () -> result);
      read_pending_epoch = (fun () -> Ok None);
      drop_invalid_unapplied = (fun ~head_epoch:_ -> Ok 0);
      head_epoch = (fun () -> 0);
      root_at_epoch = (fun _ -> None);
      current_root = (fun () -> Some (String.make 32 '\x11'));
      write_finality = (fun _ -> ());
      store_finalized = (fun ~epoch:_ ~validator_set:_ _ -> ());
      store_proposer = (fun _ -> ());
      store_expected_root = (fun ~epoch:_ ~root:_ -> ());
      store_bundle =
        (fun ~proposal_id:_ ~tx_hashes ~txs ~receipts_json ->
          stored := Some J.{ tx_hashes; txs; receipts_json });
      set_proposal = (fun _ _ -> ());
      reset_proposal_state = (fun () -> ());
      set_consensus_finalized = (fun _ -> ());
      clear_state_attested = (fun () -> ());
      commit_journal = (fun ~epoch:_ ~state_root:_ -> ());
      mark_quarantine = (fun reason -> fail reason);
      require_sync = (fun _ -> ());
    }
  in
  expect (JR.run recovery = JR.Armed) "journal recovery armed";
  match !stored with
  | Some bundle -> bundle
  | None -> fail "journal recovery bundle missing"

let recovery_case () =
  let path, store, ledger, pk, sk = setup "recovery" in
  let transaction = tx (payload pk sk true) in
  let plan =
    match
      Lwt_main.run
        (PL.prepare_encrypt_plan
           ~field_policy:PL.Unique_fields
           ledger
           transaction)
    with
    | Ok plan -> plan
    | Error e -> fail e.PL.reason
  in
  let transition_hash =
    PL.hash_prepared (PL.Prepared_encrypt plan)
  in
  let receipt = receipt ledger transaction transition_hash in
  let finalize, bundle = finalized transaction receipt in
  J.persist_certificate path ~validator_set finalize;
  J.persist_bundle path finalize bundle;
  Lwt_main.run (Octra_core.Store_irmin.close store);
  let reopened =
    Lwt_main.run (Octra_core.Store_irmin.open_store path)
  in
  Fun.protect
    ~finally:(fun () ->
      Lwt_main.run (Octra_core.Store_irmin.close reopened);
      clear_case path)
    (fun () ->
      let recovered =
        J.read_validated
          ~chain_id:"octra-transition-recovery"
          ~validator_set
          path
        |> recovered_bundle
      in
      let recovered_receipt =
        match
          Octra_core.Preverify_commit.receipts_of_strings
            recovered.J.receipts_json
        with
        | Ok [value] -> value
        | Ok _ -> fail "journal receipt count"
        | Error e -> fail e
      in
      let recovered_tx =
        match recovered.J.txs with
        | [value] -> value
        | _ -> fail "journal transaction count"
      in
      let recovered_ledger = Octra_core.Ledger.create reopened in
      begin
        match
          process
            Octra_core.Rule_graph.Active
            reopened
            recovered_ledger
            recovered_tx
            recovered_receipt
        with
        | Ok fee -> expect (Z.equal fee Z.one) "recovered fee"
        | Error (_, reason) -> fail reason
      end;
      Lwt_main.run
        (Octra_core.Ledger.flush_dirty_lwt recovered_ledger);
      let post_root =
        Lwt_main.run (Octra_core.Ledger.hash recovered_ledger)
      in
      let gate =
        Octra_core.Preverify_commit.create [recovered_receipt]
      in
      begin
        match
          Lwt_main.run
            (Octra_core.Preverify_commit.check_bound
               recovered_ledger
               gate
               [recovered_tx])
        with
        | Error _ -> ()
        | Ok () -> fail "duplicate receipt remained applicable"
      end;
      expect
        (Lwt_main.run (Octra_core.Ledger.hash recovered_ledger) = post_root)
        "duplicate recovery changed root";
      match Octra_core.Ledger.find_opt recovered_ledger addr with
      | Some account ->
        expect (account.Octra_core.Ledger_types.nonce = 1) "recovered nonce";
        expect
          (account.encrypted_balance = Some plan.PL.next_cipher)
          "recovered cipher"
      | None -> fail "recovered account missing")

let () =
  if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o755;
  if not (Sys.file_exists "runtime_data/private_transition_receipt") then
    Unix.mkdir "runtime_data/private_transition_receipt" 0o755;
  Unix.putenv "OCTRA_BFT_CRYPTO_PROFILE" "private_v1";
  match Array.to_list Sys.argv with
  | [_; "cache_key"] ->
    cache_key_mode_case ();
    print_endline "status = pass test = private_transition_receipt case = cache_key"
  | [_; "migration"] ->
    migration_case ();
    print_endline "status = pass test = private_transition_receipt case = migration"
  | [_; "circle_policy"] ->
    circle_policy_case ();
    print_endline "status = pass test = private_transition_receipt case = circle_policy"
  | [_; "valid"] ->
    valid_case ();
    print_endline "status = pass test = private_transition_receipt case = valid"
  | [_] ->
    migration_case ();
    circle_policy_case ();
    cipher_depth_case ();
    pubkey_error_case ();
    cipher_wire_case ();
    protocol_mode_case ();
    cache_mode_case ();
    cache_key_mode_case ();
    amount_link_case ();
    pending_case ();
    prior_receipt_case ();
    forged_case ();
    valid_case ();
    mismatch_case ();
    worker_retry_case ();
    circle_reject_case ();
    recovery_case ();
    print_endline "status = pass test = private_transition_receipt"
  | _ -> fail "expected cache_key, migration, circle_policy or valid"