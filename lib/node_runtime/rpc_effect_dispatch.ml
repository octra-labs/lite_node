(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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

let route name handler =
  [name, handler]

let dispatch h =
  {
    submission =
      route "octra_submit" h.submit
      @ route "octra_submitBatch" h.submit_batch;
    staging =
      route "staging_remove" h.staging_remove;
    mutation =
      route "octra_registerPublicKey" h.register_public_key
      @ route "octra_registerPvacPubkey" h.register_pvac_pubkey
      @ route "octra_privateTransfer" h.private_transfer;
  }