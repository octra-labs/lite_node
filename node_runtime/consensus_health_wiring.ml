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


type deps = {
  sleep : float -> unit Lwt.t;
  catchup_active : unit -> bool;
  set_catchup_active : bool -> unit;
  quarantine_active : unit -> bool;
  replay_stashed : source:string -> unit Lwt.t;
  probe_health : unit -> unit Lwt.t;
  reset_liveness : source:string -> unit Lwt.t;
}

let startup_probe ?(delay = 5.0) deps =
  let open Lwt.Syntax in
  deps.set_catchup_active true;
  let* () = deps.sleep delay in
  deps.set_catchup_active false;
  let* () = deps.probe_health () in
  deps.reset_liveness ~source:"startup_probe"

let poll_once deps =
  let open Lwt.Syntax in
  if deps.catchup_active () then
    Lwt.return_unit
  else
    let* () =
      if deps.quarantine_active () then
        deps.replay_stashed ~source:"quarantine_poll"
      else
        Lwt.return_unit
    in
    let* () = deps.probe_health () in
    deps.reset_liveness ~source:"health_poll"

let rec poll_loop deps ~interval =
  let open Lwt.Syntax in
  let* () = deps.sleep interval in
  let* () = poll_once deps in
  poll_loop deps ~interval