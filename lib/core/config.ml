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


let read_nodes path =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line ->
      let s = String.trim line in
      loop (if s <> "" && s.[0] <> '#' then s :: acc else acc)
    | exception End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []