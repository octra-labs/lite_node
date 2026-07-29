(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let put_u8 buf v =
  Buffer.add_char buf (Char.chr (v land 0xff))

let put_u16 buf v =
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (v land 0xff))

let put_u32 buf v =
  let v = Int32.to_int v in
  Buffer.add_char buf (Char.chr ((v lsr 24) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 16) land 0xff));
  Buffer.add_char buf (Char.chr ((v lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (v land 0xff))

let put_u32_int buf v =
  put_u32 buf (Int32.of_int v)

let put_u64 buf v =
  for i = 7 downto 0 do
    Buffer.add_char buf (Char.chr (Int64.to_int (Int64.shift_right_logical v (i * 8)) land 0xff))
  done

let put_bool buf v =
  put_u8 buf (if v then 1 else 0)

let put_raw buf s =
  Buffer.add_string buf s

let put_bytes buf s =
  put_u32_int buf (String.length s);
  Buffer.add_string buf s

let put_string = put_bytes

let hex_nibble = function
  | '0'..'9' as value -> Ok (Char.code value - Char.code '0')
  | 'a'..'f' as value -> Ok (Char.code value - Char.code 'a' + 10)
  | _ -> Error "hash32 contains non-hex characters"

let hash32_of_hex value =
  if String.length value <> 64 then Error "hash32 hex length must be 64"
  else
    let output = Bytes.create 32 in
    let rec loop index =
      if index = 32 then Ok (Bytes.unsafe_to_string output)
      else
        match hex_nibble value.[index * 2], hex_nibble value.[(index * 2) + 1] with
        | Ok high, Ok low ->
            Bytes.set output index (Char.chr ((high lsl 4) lor low));
            loop (index + 1)
        | Error reason, _ | _, Error reason -> Error reason
    in
    loop 0

let hash32_bytes value =
  if String.length value = 32 then Ok value else hash32_of_hex value

let put_hash32_checked buf value =
  match hash32_bytes value with
  | Error _ as error -> error
  | Ok raw ->
      Buffer.add_string buf raw;
      Ok ()

let put_hash32 buf value =
  match put_hash32_checked buf value with
  | Ok () -> ()
  | Error reason -> invalid_arg reason

let put_sig64_checked buf value =
  if String.length value <> 64 then Error "sig64 length must be 64"
  else begin
    Buffer.add_string buf value;
    Ok ()
  end

let put_sig64 buf value =
  match put_sig64_checked buf value with
  | Ok () -> ()
  | Error reason -> invalid_arg reason

let put_addr = put_string

let put_option put_inner buf = function
  | None -> put_u8 buf 0
  | Some v -> put_u8 buf 1; put_inner buf v

let put_list put_inner buf lst =
  put_u32_int buf (List.length lst);
  List.iter (put_inner buf) lst

type cursor = { data : string; mutable pos : int }

let make_cursor data = { data; pos = 0 }

let remaining c = String.length c.data - c.pos

let check_len c n =
  if remaining c < n then failwith (Printf.sprintf "OCE1: need %d bytes, have %d" n (remaining c))

let get_u8 c =
  check_len c 1;
  let v = Char.code c.data.[c.pos] in
  c.pos <- c.pos + 1;
  v

let get_u16 c =
  check_len c 2;
  let b0 = Char.code c.data.[c.pos] in
  let b1 = Char.code c.data.[c.pos + 1] in
  c.pos <- c.pos + 2;
  (b0 lsl 8) lor b1

let get_u32 c =
  check_len c 4;
  let b0 = Char.code c.data.[c.pos] in
  let b1 = Char.code c.data.[c.pos + 1] in
  let b2 = Char.code c.data.[c.pos + 2] in
  let b3 = Char.code c.data.[c.pos + 3] in
  c.pos <- c.pos + 4;
  Int32.of_int ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)

let get_u32_int c = Int32.to_int (get_u32 c)

let get_u64 c =
  check_len c 8;
  let v = ref 0L in
  for i = 0 to 7 do
    v := Int64.logor (Int64.shift_left !v 8) (Int64.of_int (Char.code c.data.[c.pos + i]))
  done;
  c.pos <- c.pos + 8;
  !v

let get_bool c =
  match get_u8 c with
  | 0 -> false
  | 1 -> true
  | _ -> failwith "OCE1: bad bool tag"

let get_raw c n =
  check_len c n;
  let s = String.sub c.data c.pos n in
  c.pos <- c.pos + n;
  s

let get_bytes_bounded ~max c =
  let len = get_u32_int c in
  if len < 0 || len > max then failwith "OCE1: bytes len out of range";
  check_len c len;
  let s = String.sub c.data c.pos len in
  c.pos <- c.pos + len;
  s

let get_bytes c = get_bytes_bounded ~max:10_000_000 c

let get_string = get_bytes

let get_string_bounded = get_bytes_bounded

let get_hash32 c =
  check_len c 32;
  let s = String.sub c.data c.pos 32 in
  c.pos <- c.pos + 32;
  s

let get_sig64 c =
  check_len c 64;
  let s = String.sub c.data c.pos 64 in
  c.pos <- c.pos + 64;
  s

let get_addr = get_string

let get_option get_inner c =
  match get_u8 c with
  | 0 -> None
  | 1 -> Some (get_inner c)
  | _ -> failwith "OCE1: bad option tag"

let get_list_bounded ~max get_inner c =
  let count = get_u32_int c in
  if count < 0 || count > max then failwith "OCE1: list count out of range";
  List.init count (fun _ -> get_inner c)

let get_list get_inner c = get_list_bounded ~max:1_000_000 get_inner c

let encode f =
  let buf = Buffer.create 256 in
  f buf;
  Buffer.contents buf

let decode f data =
  let c = make_cursor data in
  let v = f c in
  if remaining c > 0 then
    failwith (Printf.sprintf "OCE1: %d trailing bytes" (remaining c));
  v