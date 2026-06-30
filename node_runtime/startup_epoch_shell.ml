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


type source =
  | Meta of int
  | Files of int
  | Genesis

type deps = {
  last_epoch_meta : unit -> string option;
  saved_epochs : unit -> int list;
  set_current_epoch : int -> unit;
}

let source ~last_epoch_meta ~saved_epochs =
  match last_epoch_meta with
  | Some s -> Meta (int_of_string s)
  | None ->
    match saved_epochs with
    | last :: _ -> Files last
    | [] -> Genesis

let current_epoch = function
  | Meta last -> last + 1
  | Files last -> last + 1
  | Genesis -> 0

let log_source = function
  | Meta last ->
    Octra_log.info "init" "last epoch source = db epoch = %d" last
  | Files last ->
    Octra_log.info "init" "last epoch source = files epoch = %d" last
  | Genesis -> ()

let run deps =
  let source =
    source
      ~last_epoch_meta:(deps.last_epoch_meta ())
      ~saved_epochs:(deps.saved_epochs ())
  in
  log_source source;
  let epoch = current_epoch source in
  Octra_log.info "init" "starting epoch = %d" epoch;
  deps.set_current_epoch epoch;
  epoch