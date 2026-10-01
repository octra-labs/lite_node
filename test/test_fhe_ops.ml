(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_ffi

let expect name value = if not value then failwith name
let seed value = Bytes.make 32 value

let check_ops math slots =
  let pk, sk = P.keygen_from_seed (P.default_params ()) (seed '\011') in
  let pk = P.deserialize_pubkey (P.serialize_pubkey pk) in
  let sk = P.deserialize_seckey (P.serialize_seckey sk) in
  let left = Array.init slots (fun index -> Int64.of_int (2 + index)) in
  let right = Array.init slots (fun index -> Int64.of_int (1 + index)) in
  let a = P.enc_values_seeded pk sk left (seed '\012') in
  let b = P.enc_values_seeded pk sk right (seed '\013') in
  let image_a = P.serialize_cipher a in
  let image_b = P.serialize_cipher b in
  let check name expected cipher =
    expect (name ^ " value") (P.dec_values pk sk cipher = expected);
    let image = P.serialize_cipher cipher in
    let decoded = P.deserialize_cipher image in
    expect (name ^ " bytes") (P.serialize_cipher decoded = image);
    expect (name ^ " reopened") (P.dec_values pk sk decoded = expected);
    let public = P.serialize_cipher_public cipher |> P.deserialize_cipher in
    expect (name ^ " public") (P.dec_values pk sk public = expected)
  in
  check "input" left a;
  check "sum" (Array.map2 Int64.add left right) (P.ct_add pk a b);
  check "difference" (Array.map2 Int64.sub left right) (P.ct_sub pk a b);
  check "product" (Array.map2 Int64.mul left right)
    (P.ct_mul_seeded ~math pk a b (seed '\014'));
  check "square" (Array.map (fun value -> Int64.mul value value) left)
    (P.ct_square_seeded ~math pk a (seed '\015'));
  check "scale" (Array.map (Int64.mul 3L) left) (P.ct_scale ~math pk a 3L);
  check "add_constant" (Array.map (Int64.add 5L) left)
    (P.ct_add_const ~math pk a 5L 0L);
  check "sub_constant" (Array.map (fun value -> Int64.sub value 1L) left)
    (P.ct_sub_const ~math pk a 1L);
  check "division" left (P.ct_div_const pk (P.ct_add pk a a) 2L 0L);
  check "cancellation" (Array.make slots 0L) (P.ct_sub pk a a);
  if math then
    check "negative_scale" (Array.make slots 0L)
      (P.ct_add pk a (P.ct_scale ~math pk a (-1L)));
  expect "input_a mutated" (P.serialize_cipher a = image_a);
  expect "input_b mutated" (P.serialize_cipher b = image_b);
  expect "seeded product differs"
    (P.serialize_cipher (P.ct_mul_seeded ~math pk a b (seed '\014'))
     = P.serialize_cipher (P.ct_mul_seeded ~math pk a b (seed '\014')));
  Printf.printf "event = test name = fhe_ops math = %b slots = %d status = passed\n%!"
    math slots

let () =
  List.iter (fun math -> List.iter (check_ops math) [1; 2; 8]) [false; true]