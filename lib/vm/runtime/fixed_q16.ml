(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Z.t

let frac_bits = 16
let scale = Z.of_int (1 lsl frac_bits)
let min_exp = Z.neg (Z.mul (Z.of_int 16) scale)
let max_exp = Z.mul (Z.of_int 8) scale
let max_raw = Z.sub (Z.shift_left Z.one 63) Z.one
let pi = Z.of_int 205887
let half_pi = Z.of_int 102944
let two_pi = Z.of_int 411775
let ln_two = Z.of_int 45426

let trunc_mul (a : t) (b : t) =
  Z.shift_right (Z.mul a b) frac_bits

let in_range value = Z.compare (Z.abs value) max_raw <= 0

let round16 (value : t) =
  let half = Z.shift_left Z.one (frac_bits - 1) in
  if Z.sign value >= 0 then
    Z.shift_right (Z.add value half) frac_bits
  else
    Z.neg (Z.shift_right (Z.add (Z.neg value) half) frac_bits)

let make_round bits =
  if bits <= 0 || bits > 62 then None
  else
    let half = Z.shift_left Z.one (bits - 1) in
    Some (fun value ->
      if Z.sign value >= 0 then
        Z.shift_right (Z.add value half) bits
      else
        Z.neg (Z.shift_right (Z.add (Z.neg value) half) bits))

let round_div numerator denominator =
  let half = Z.shift_right denominator 1 in
  if Z.sign numerator >= 0 then
    Z.div (Z.add numerator half) denominator
  else
    Z.neg (Z.div (Z.add (Z.neg numerator) half) denominator)

let exp value =
  let value =
    if Z.compare value min_exp < 0 then min_exp
    else if Z.compare value max_exp > 0 then max_exp
    else value
  in
  let exponent = round_div value ln_two in
  let reduced = Z.sub value (Z.mul exponent ln_two) in
  let rec series index term total =
    if index = 13 then total
    else
      let next = Z.div (trunc_mul term reduced) (Z.of_int index) in
      series (index + 1) next (Z.add total next)
  in
  let base = series 1 scale scale in
  if Z.sign exponent >= 0 then
    let shifted = Z.shift_left base (Z.to_int exponent) in
    if Z.compare shifted max_raw > 0 then max_raw else shifted
  else
    let shift = Z.to_int (Z.neg exponent) in
    Z.shift_right (Z.add base (Z.shift_left Z.one (shift - 1))) shift

let scale_floor value total =
  if Z.sign value < 0 || Z.sign total <= 0 then None
  else Some (Z.div (Z.mul value scale) total)

let inv_sqrt value =
  if Z.sign value <= 0 then None
  else
    let root = Z.sqrt (Z.mul value scale) in
    if Z.sign root <= 0 then None
    else Some (Z.div (Z.mul scale scale) root)

let ln value =
  if Z.sign value <= 0 || not (in_range value) then None
  else
    let rec normalize current exponent =
      if Z.compare current (Z.mul scale (Z.of_int 2)) >= 0 then
        normalize (Z.shift_right current 1) (exponent + 1)
      else if Z.compare current scale < 0 then
        normalize (Z.shift_left current 1) (exponent - 1)
      else
        current, exponent
    in
    let normalized, exponent = normalize value 0 in
    let z = Z.div (Z.mul (Z.sub normalized scale) scale)
        (Z.add normalized scale) in
    let z_square = trunc_mul z z in
    let rec series power index acc =
      if index = 16 then acc
      else
        let term = Z.div power (Z.of_int (2 * index + 1)) in
        series (trunc_mul power z_square) (index + 1) (Z.add acc term)
    in
    Some (Z.add (Z.mul (Z.of_int 2) (series z 0 Z.zero))
      (Z.mul (Z.of_int exponent) ln_two))

let inverse_pow base exponent =
  match ln base with
  | None -> None
  | Some base_log -> Some (exp (Z.neg (trunc_mul exponent base_log)))

let sin0 value =
  let square = trunc_mul value value in
  let cube = trunc_mul square value in
  let fifth = trunc_mul cube square in
  let seventh = trunc_mul fifth square in
  Z.add (Z.sub value (Z.div cube (Z.of_int 6)))
    (Z.sub (Z.div fifth (Z.of_int 120)) (Z.div seventh (Z.of_int 5040)))

let cos0 value =
  let square = trunc_mul value value in
  let fourth = trunc_mul square square in
  let sixth = trunc_mul fourth square in
  Z.add (Z.sub scale (Z.div square (Z.of_int 2)))
    (Z.sub (Z.div fourth (Z.of_int 24)) (Z.div sixth (Z.of_int 720)))

let sin_cos angle =
  let remainder = Z.rem angle two_pi in
  let angle = if Z.sign remainder < 0 then Z.add remainder two_pi else remainder in
  let pi_plus_half = Z.add pi half_pi in
  if Z.compare angle half_pi <= 0 then
    sin0 angle, cos0 angle
  else if Z.compare angle pi <= 0 then
    let reduced = Z.sub pi angle in
    sin0 reduced, Z.neg (cos0 reduced)
  else if Z.compare angle pi_plus_half <= 0 then
    let reduced = Z.sub angle pi in
    Z.neg (sin0 reduced), Z.neg (cos0 reduced)
  else
    let reduced = Z.sub two_pi angle in
    Z.neg (sin0 reduced), cos0 reduced

let silu value =
  if not (in_range value) then None
  else
    let denominator = Z.add scale (exp (Z.neg value)) in
    if Z.sign denominator <= 0 then None
    else Some (trunc_mul value (Z.div (Z.mul scale scale) denominator))

let rope values base position =
  let n = Array.length values in
  if n = 0 || n mod 2 <> 0 || Z.sign base <= 0 ||
     not (in_range base) || not (Array.for_all in_range values) then None
  else
    let half = n / 2 in
    let rec pairs index acc =
      if index = half then Some (Array.of_list (List.rev acc))
      else
        let exponent = Z.div (Z.mul (Z.of_int (2 * index)) scale) (Z.of_int n) in
        match inverse_pow base exponent with
        | None -> None
        | Some frequency ->
          let angle = Z.mul (Z.of_int position) frequency in
          let sine, cosine = sin_cos angle in
          let real = Z.sub (trunc_mul values.(index) cosine)
              (trunc_mul values.(index + half) sine) in
          let imaginary = Z.add (trunc_mul values.(index) sine)
              (trunc_mul values.(index + half) cosine) in
          if in_range real && in_range imaginary then
            pairs (index + 1) ((real, imaginary) :: acc)
          else None
    in
    match pairs 0 [] with
    | None -> None
    | Some rotated ->
      Some (Array.init n (fun index ->
        if index < half then fst rotated.(index)
        else snd rotated.(index - half)))

let dot left left_offset right right_offset length =
  let products = Array.init length (fun index ->
    trunc_mul left.(left_offset + index) right.(right_offset + index)) in
  Array.fold_left Z.add Z.zero products

let map2_checked f left right =
  if Array.length left <> Array.length right ||
     not (Array.for_all in_range left) ||
     not (Array.for_all in_range right) then None
  else
    let result = Array.map2 f left right in
    if Array.for_all in_range result then Some result else None

let elementwise_mul left right =
  map2_checked trunc_mul left right

let residual_add left right =
  map2_checked Z.add left right

let attention q k v total_tokens query_heads key_heads head_dim =
  if total_tokens <= 0 || query_heads <= 0 || key_heads <= 0 || head_dim <= 0 ||
     query_heads mod key_heads <> 0 ||
     query_heads > max_int / head_dim ||
     key_heads > max_int / head_dim ||
     total_tokens > max_int / (key_heads * head_dim) then None
  else
    let query_size = query_heads * head_dim in
    let key_size = total_tokens * key_heads * head_dim in
    if Array.length q <> query_size || Array.length k <> key_size ||
       Array.length v <> key_size || not (Array.for_all in_range q) ||
       not (Array.for_all in_range k) || not (Array.for_all in_range v) then None
    else
      match inv_sqrt (Z.mul (Z.of_int head_dim) scale) with
      | None -> None
      | Some inv_sqrt_dim ->
        let group = query_heads / key_heads in
        let head query_head =
          let query_offset = query_head * head_dim in
          let key_head = query_head / group in
          let scores = Array.init total_tokens (fun token ->
            let key_offset = (token * key_heads + key_head) * head_dim in
            trunc_mul (dot q query_offset k key_offset head_dim) inv_sqrt_dim) in
          let maximum = Array.fold_left Z.max scores.(0) scores in
          let weights = Array.map (fun score -> exp (Z.sub score maximum)) scores in
          let total = Array.fold_left Z.add Z.zero weights in
          if Z.sign total <= 0 then None
          else
            let probabilities = Array.map (fun weight -> scale_floor weight total) weights in
            if not (Array.for_all Option.is_some probabilities) then None
            else
              let probabilities = Array.map Option.get probabilities in
              Some (Array.init head_dim (fun dimension ->
                let terms = Array.init total_tokens (fun token ->
                  let value_offset = (token * key_heads + key_head) * head_dim + dimension in
                  trunc_mul probabilities.(token) v.(value_offset)) in
                Array.fold_left Z.add Z.zero terms))
        in
        let heads = Array.init query_heads head in
        if not (Array.for_all Option.is_some heads) then None
        else
          let heads = Array.map Option.get heads in
          let result = Array.concat (Array.to_list heads) in
          if Array.for_all in_range result then Some result else None

let mean values =
  let n = Array.length values in
  if n = 0 then None
  else Some (Z.div (Array.fold_left Z.add Z.zero values) (Z.of_int n))

let layer values gamma beta =
  let n = Array.length values in
  if n = 0 || Array.length gamma <> n || Array.length beta <> n ||
     not (Array.for_all in_range values) ||
     not (Array.for_all in_range gamma) ||
     not (Array.for_all in_range beta) then None
  else
    match mean values with
    | None -> None
    | Some avg ->
      let sum_sq =
        Array.fold_left
          (fun acc value ->
             let delta = Z.sub value avg in
             Z.add acc (Z.mul delta delta))
          Z.zero values
      in
      let variance = Z.div sum_sq (Z.mul (Z.of_int n) scale) in
      (match inv_sqrt (Z.add variance Z.one) with
       | None -> None
       | Some inv ->
         Some (Array.mapi (fun i value ->
           let normalized = trunc_mul (Z.sub value avg) inv in
           Z.add (trunc_mul gamma.(i) normalized) beta.(i)) values))

let rms values gamma =
  let n = Array.length values in
  if n = 0 || Array.length gamma <> n ||
     not (Array.for_all in_range values) ||
     not (Array.for_all in_range gamma) then None
  else
    let sum_sq =
      Array.fold_left (fun acc value -> Z.add acc (Z.mul value value))
        Z.zero values
    in
    let mean_sq = Z.div sum_sq (Z.mul (Z.of_int n) scale) in
    match inv_sqrt (Z.add mean_sq Z.one) with
    | None -> None
    | Some inv ->
      Some (Array.mapi (fun i value ->
        trunc_mul gamma.(i) (trunc_mul value inv)) values)