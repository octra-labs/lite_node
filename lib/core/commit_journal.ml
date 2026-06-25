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


type record =
  | Prepare of {
      commit_id : string;
      prev_generation : int;
      epoch_id : int;
      planned_txid_hi : int64;
      planned_state_root : string;
      ts : float;
    }
  | Commit of {
      commit_id : string;
      generation : int;
      ts : float;
    }
  | Abort of {
      commit_id : string;
      reason : string;
      ts : float;
    }

let record_to_json = function
  | Prepare p ->
    `Assoc [
      "type", `String "PREPARE";
      "commit_id", `String p.commit_id;
      "prev_generation", `Int p.prev_generation;
      "epoch_id", `Int p.epoch_id;
      "planned_txid_hi", `String (Int64.to_string p.planned_txid_hi);
      "planned_state_root", `String p.planned_state_root;
      "ts", `Float p.ts;
    ]
  | Commit c ->
    `Assoc [
      "type", `String "COMMIT";
      "commit_id", `String c.commit_id;
      "generation", `Int c.generation;
      "ts", `Float c.ts;
    ]
  | Abort a ->
    `Assoc [
      "type", `String "ABORT";
      "commit_id", `String a.commit_id;
      "reason", `String a.reason;
      "ts", `Float a.ts;
    ]

let record_of_json j =
  let open Yojson.Safe.Util in
  match j |> member "type" |> to_string with
  | "PREPARE" -> Some (Prepare {
      commit_id = j |> member "commit_id" |> to_string;
      prev_generation = j |> member "prev_generation" |> to_int;
      epoch_id = j |> member "epoch_id" |> to_int;
      planned_txid_hi = Int64.of_string (j |> member "planned_txid_hi" |> to_string);
      planned_state_root = j |> member "planned_state_root" |> to_string;
      ts = j |> member "ts" |> to_number;
    })
  | "COMMIT" -> Some (Commit {
      commit_id = j |> member "commit_id" |> to_string;
      generation = j |> member "generation" |> to_int;
      ts = j |> member "ts" |> to_number;
    })
  | "ABORT" -> Some (Abort {
      commit_id = j |> member "commit_id" |> to_string;
      reason = j |> member "reason" |> to_string;
      ts = j |> member "ts" |> to_number;
    })
  | _ -> None

let path data_dir = Filename.concat data_dir "commit_journal.log"

let mk_commit_id epoch_id =
  Printf.sprintf "ep%d-%d-%d" epoch_id
    (int_of_float (Unix.gettimeofday () *. 1000.))
    (Random.bits ())

let append data_dir rec_ =
  let p = path data_dir in
  let oc = open_out_gen [Open_append; Open_creat; Open_binary] 0o644 p in
  let line = Yojson.Safe.to_string (record_to_json rec_) ^ "\n" in
  output_string oc line;
  flush oc;
  (try Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ());
  close_out oc

let read_all data_dir =
  let p = path data_dir in
  if not (Sys.file_exists p) then []
  else
    let ic = open_in p in
    let acc = ref [] in
    (try
       while true do
         let line = input_line ic in
         if String.length line > 0 then
           match record_of_json (Yojson.Safe.from_string line) with
           | Some r -> acc := r :: !acc
           | None -> ()
       done
     with End_of_file -> ());
    close_in ic;
    List.rev !acc

let last_commit_id_for_epoch journal epoch_id =
  let last = ref None in
  List.iter (function
    | Prepare p when p.epoch_id = epoch_id -> last := Some p.commit_id
    | Commit c when (try Scanf.sscanf c.commit_id "ep%d-%_d-%_d"
                            (fun e -> e = epoch_id) with _ -> false) ->
      last := Some c.commit_id
    | _ -> ()
  ) journal;
  !last

let pending_prepares ?head_commit_id journal =
  let committed = Hashtbl.create 16 in
  List.iter (function
    | Commit c -> Hashtbl.replace committed c.commit_id ()
    | _ -> ()
  ) journal;
  (match head_commit_id with
   | Some cid -> Hashtbl.replace committed cid ()
   | None -> ());
  List.filter_map (function
    | Prepare p when not (Hashtbl.mem committed p.commit_id) ->
      Some (p.commit_id, p.epoch_id)
    | _ -> None
  ) journal