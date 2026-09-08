(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_cert.bits

type error =
  | Source of C_parse.error
  | Cert of C_cert.error
  | Encode

type item = {
  core : bits;
  res : C_limit.t;
}

let ( let* ) value next =
  match value with
  | Some value -> next value
  | None -> None

let bit_code value =
  Some (C_bin.Tag ((if value then Z.one else Z.zero), C_bin.Nil))

let bit_get = function
  | C_bin.Tag (tag, C_bin.Nil) when Z.equal tag Z.zero -> Some false
  | C_bin.Tag (tag, C_bin.Nil) when Z.equal tag Z.one -> Some true
  | _ -> None

let item_code value =
  let* core = C_bin.list_code bit_code value.core in
  let* res = C_bin.res_code value.res in
  Some (C_bin.Tag (Z.zero,
    C_bin.Cons (core, C_bin.Cons (res, C_bin.Nil))))

let item_get = function
  | C_bin.Tag (tag, C_bin.Cons (core, C_bin.Cons (res, C_bin.Nil)))
      when Z.equal tag Z.zero ->
      let* core = C_bin.list_get bit_get core in
      let* res = C_bin.res_get res in
      Some { core; res }
  | _ -> None

let enc value =
  let* code = item_code value in
  C_bin.enc_code code

let decode input =
  let* code = C_bin.dec_code input in
  let* value = item_get code in
  let* exact = enc value in
  if List.equal Bool.equal exact input then Some value else None

let make rules ~epoch src =
  match C_parse.parse src with
  | Error error -> Error (Source error)
  | Ok parsed ->
      begin
        match C_parse.compile parsed with
        | Error error -> Error (Source error)
        | Ok (prog, info) ->
            begin
              match C_cert.issue rules ~epoch prog with
              | Ok core ->
                  begin
                    match enc { core; res = info.res } with
                    | Some bits -> Ok bits
                    | None -> Error Encode
                  end
              | Error error -> Error (Cert error)
            end
      end

let accept rules ~epoch src input =
  match make rules ~epoch src with
  | Ok exact -> List.equal Bool.equal exact input
  | Error _ -> false

let text = function
  | Source error -> C_parse.text error
  | Cert error -> C_cert.text error
  | Encode -> "source artifact encoding failed"