(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type usage = {regular : int; duty : int}

type limits = {
  max_regular : int;
  max_sender : int;
  max_duty : int;
  max_duty_sender : int;
}

let zero = {regular = 0; duty = 0}

let limits ~regular ~sender ~duty ~duty_sender =
  if regular <= 0 || sender <= 0 || duty <= 0 || duty_sender <= 0 then
    Error "staging byte limits must be positive"
  else if sender > regular || duty_sender > duty then
    Error "staging sender byte limit exceeds lane"
  else if regular > max_int - duty then
    Error "staging byte limits exceed integer range"
  else Ok {max_regular = regular; max_sender = sender;
    max_duty = duty; max_duty_sender = duty_sender}

let of_env get =
  let ( let* ) = Result.bind in
  let read name default =
    match get name with
    | None -> Ok default
    | Some raw ->
      if raw = "" || String.exists (fun c -> c < '0' || c > '9') raw then
        Error ("staging byte limit invalid: " ^ name)
      else match int_of_string_opt raw with
      | Some value when value > 0 -> Ok value
      | _ -> Error ("staging byte limit invalid: " ^ name) in
  let* regular = read "OCTRA_STAGING_MAX_BYTES" (1_024 * 1_024 * 1_024) in
  let* sender = read "OCTRA_STAGING_SENDER_BYTES" (16 * 1_024 * 1_024) in
  let* duty = read "OCTRA_STAGING_DUTY_BYTES" (128 * 1_024 * 1_024) in
  let* duty_sender = read "OCTRA_STAGING_DUTY_SENDER_BYTES" (64 * 1_024) in
  limits ~regular ~sender ~duty ~duty_sender

let measure tx =
  let {Transaction.from; to_; amount; nonce; ou; timestamp = _;
    signature; public_key; message; op_type = _; encrypted_data} = tx in
  let word = Sys.word_size / 8 in
  let string_size length = Z.mul (Z.of_int word) (Z.add (Z.of_int 2)
    (Z.div length (Z.of_int word))) in
  let text value = string_size (Z.of_int (String.length value)) in
  let optional = function
    | None -> Z.zero
    | Some value -> Z.add (Z.of_int (2 * word)) (text value) in
  let integer value =
    if Z.fits_int value then Z.zero
    else Z.mul (Z.of_int word) (Z.add (Z.of_int 4)
      (Z.cdiv (Z.of_int (Z.numbits value)) (Z.of_int Sys.word_size))) in
  let key_length = Z.add (Z.of_int (String.length from))
    (Z.of_int (String.length (string_of_int nonce))) in
  let size = List.fold_left Z.add (Z.of_int (64 * word)) [
    text from; text to_; integer amount; integer ou; text signature;
    optional public_key; optional message; optional encrypted_data;
    string_size key_length; string_size (Z.of_int 64);
  ] in
  if Z.fits_int size then Ok (Z.to_int size)
  else Error "transaction staging bytes exceed integer range"

let of_tx tx bytes =
  if bytes <= 0 then invalid_arg "staging entry bytes must be positive";
  match tx.Transaction.op_type with
  | Transaction.ValidatorReady -> {regular = 0; duty = bytes}
  | _ -> {regular = bytes; duty = 0}

let valid usage = usage.regular >= 0 && usage.duty >= 0

let add left right =
  if not (valid left && valid right)
     || left.regular > max_int - right.regular
     || left.duty > max_int - right.duty then
    invalid_arg "staging byte addition exceeds integer range";
  {regular = left.regular + right.regular; duty = left.duty + right.duty}

let remove current removed =
  if not (valid current && valid removed)
     || removed.regular > current.regular || removed.duty > current.duty then
    invalid_arg "staging byte removal exceeds retained bytes";
  {regular = current.regular - removed.regular; duty = current.duty - removed.duty}

let plan limits ~total ~sender ~removed ~added =
  if not (valid total && valid sender && valid removed && valid added)
     || sender.regular > total.regular || sender.duty > total.duty
     || removed.regular > sender.regular || removed.duty > sender.duty then
    Error "staging byte accounting is inconsistent"
  else
    let total = remove total removed and sender = remove sender removed in
    let exceeds used incoming maximum = used > maximum || incoming > maximum - used in
    if exceeds total.regular added.regular limits.max_regular then
      Error "staging byte limit exceeded"
    else if exceeds total.duty added.duty limits.max_duty then
      Error "validator duty staging byte limit exceeded"
    else if exceeds sender.regular added.regular limits.max_sender then
      Error "sender staging byte limit exceeded"
    else if exceeds sender.duty added.duty limits.max_duty_sender then
      Error "sender validator duty byte limit exceeded"
    else Ok ()