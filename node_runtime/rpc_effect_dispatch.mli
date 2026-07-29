(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'handler route = string * 'handler

type 'handler handlers = {
  submit : 'handler;
  submit_batch : 'handler;
  staging_remove : 'handler;
  register_public_key : 'handler;
  register_pvac_pubkey : 'handler;
  private_transfer : 'handler;
}

type 'handler groups = {
  submission : 'handler route list;
  staging : 'handler route list;
  mutation : 'handler route list;
}

val dispatch :
  'handler handlers ->
  'handler groups