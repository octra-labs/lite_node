(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

let ( let* ) value next =
  match value with
  | Some value -> next value
  | None -> None

let raw_code value =
  let rec loop index out =
    if index < 0 then out
    else
      loop (index - 1)
        (C_bin.Cons (C_bin.Num (Z.of_int (Char.code value.[index])), out))
  in
  loop (String.length value - 1) C_bin.Nil

let raw_get input =
  let out = Buffer.create 32 in
  let rec loop = function
    | C_bin.Nil -> Some (Buffer.contents out)
    | C_bin.Cons (C_bin.Num value, rest) ->
        let* value = C_nat.byte value in
        Buffer.add_char out (Char.chr value);
        loop rest
    | _ -> None
  in
  loop input

let scope_code value =
  C_bin.Tag (Z.zero,
    C_bin.Cons (raw_code value.C_sess.chain,
      C_bin.Cons (raw_code value.C_sess.prog,
        C_bin.Cons (raw_code value.C_sess.root, C_bin.Nil))))

let scope_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (chain, C_bin.Cons (prog, C_bin.Cons (root, C_bin.Nil))))
      when Z.equal tag Z.zero ->
      let* chain = raw_get chain in
      let* prog = raw_get prog in
      let* root = raw_get root in
      begin
        match C_sess.scope ~chain ~prog ~root with
        | Ok value -> Some value
        | Error _ -> None
      end
  | _ -> None

let tok_code value =
  C_bin.Tag (Z.one,
    C_bin.Cons (scope_code value.C_sess.scope,
      C_bin.Cons (C_bin.Num (C_nat.to_z value.C_sess.kind),
        C_bin.Cons (C_bin.Num (C_nat.to_z value.C_sess.id),
          C_bin.Cons (C_bin.Num (C_nat.to_z value.C_sess.rev), C_bin.Nil)))))

let tok_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (domain,
        C_bin.Cons (C_bin.Num kind,
          C_bin.Cons (C_bin.Num id, C_bin.Cons (C_bin.Num rev, C_bin.Nil)))))
      when Z.equal tag Z.one ->
      let* domain = scope_get domain in
      begin
        match C_sess.token domain ~kind ~id ~rev with
        | Ok value -> Some value
        | Error _ -> None
      end
  | _ -> None

let cell_code value =
  C_bin.Tag (Z.of_int 2,
    C_bin.Cons (C_bin.Num (C_nat.to_z value.C_sess.kind),
      C_bin.Cons (C_bin.Num (C_nat.to_z value.C_sess.id),
        C_bin.Cons (C_bin.Num (C_nat.to_z value.C_sess.rev),
          C_bin.Cons
            (C_bin.Num (if value.C_sess.live then Z.one else Z.zero),
             C_bin.Nil)))))

let cell_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (C_bin.Num kind,
        C_bin.Cons (C_bin.Num id,
          C_bin.Cons (C_bin.Num rev, C_bin.Cons (C_bin.Num live, C_bin.Nil)))))
      when Z.equal tag (Z.of_int 2) ->
      let* live =
        if Z.equal live Z.zero then Some false
        else if Z.equal live Z.one then Some true
        else None
      in
      begin
        match C_sess.cell ~kind ~id ~rev ~live with
        | Ok value -> Some value
        | Error _ -> None
      end
  | _ -> None

let rec cells_code = function
  | [] -> C_bin.Nil
  | cell :: rest -> C_bin.Cons (cell_code cell, cells_code rest)

let rec cells_get = function
  | C_bin.Nil -> Some []
  | C_bin.Cons (cell, rest) ->
      let* cell = cell_get cell in
      let* rest = cells_get rest in
      Some (cell :: rest)
  | _ -> None

let state_code state =
  let domain, cells = C_sess.view state in
  C_bin.Tag (Z.of_int 3,
    C_bin.Cons (scope_code domain, C_bin.Cons (cells_code cells, C_bin.Nil)))

let state_get = function
  | C_bin.Tag (tag, C_bin.Cons (domain, C_bin.Cons (cells, C_bin.Nil)))
      when Z.equal tag (Z.of_int 3) ->
      let* domain = scope_get domain in
      let* cells = cells_get cells in
      begin
        match C_sess.of_cells domain cells with
        | Ok value -> Some value
        | Error _ -> None
      end
  | _ -> None

let enc put value =
  let* code = put value in
  C_bin.enc_code code

let dec put get input =
  let* code = C_bin.dec_code input in
  let* value = get code in
  let* exact = enc put value in
  if List.equal Bool.equal exact input then Some value else None

let enc_scope = enc (fun value -> Some (scope_code value))
let dec_scope = dec (fun value -> Some (scope_code value)) scope_get
let enc_tok = enc (fun value -> Some (tok_code value))
let dec_tok = dec (fun value -> Some (tok_code value)) tok_get
let enc_state = enc (fun value -> Some (state_code value))
let dec_state = dec (fun value -> Some (state_code value)) state_get