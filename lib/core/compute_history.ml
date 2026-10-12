(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let work_limit = 64 * 1024 * 1024
let max_reply_bytes = 263_168
let max_context_bytes = 2 * 1024 * 1024
let max_notices = 256
let caller_notices = 32
let notice_rounds = 64
let max_notice_bytes = 1024
let max_cancel_acks = 256

let map_limit page =
  if page < 512 || page > 65536 || page land (page - 1) <> 0 then
    invalid_arg "compute page size";
  let frame = 1 + 8 + 128 + 32 + 128 + 16 + 64 in
  let pages = 2 + (max_notice_bytes + frame + page - 1) / page in
  let slots = max_notices * (max_cancel_acks + 3) in
  let bytes = Int64.mul (Int64.of_int (pages * slots + 8)) (Int64.of_int page) in
  let bytes = Int64.mul 2L (Int64.add bytes (Int64.of_int work_limit)) in
  if bytes > Int64.of_int max_int then invalid_arg "compute map address space";
  Int64.to_int bytes

type t = {
  env : Lmdb.Env.t;
  resources : Store_scope.t;
  rows : (string, string, [ `Uni ]) Lmdb.Map.t;
  callers : (string, string, [ `Uni ]) Lmdb.Map.t;
  jobs : (string, string, [ `Uni ]) Lmdb.Map.t;
  identity : (string, string, [ `Uni ]) Lmdb.Map.t;
  max_sessions : int;
  caller_limit : int;
  mutable closed : bool;
}

let encode value =
  let bytes = Bytes.create 8 in
  Bytes.set_int64_be bytes 0 value;
  Bytes.to_string bytes

let decode bytes =
  if String.length bytes <> 8 then invalid_arg "compute history integer length";
  let value = Bytes.get_int64_be (Bytes.of_string bytes) 0 in
  if value < 0L then invalid_arg "compute history negative integer";
  value

let private_dir path =
  (try Unix.mkdir path 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let stat = Unix.lstat path in
  if stat.Unix.st_kind <> Unix.S_DIR || stat.st_uid <> Unix.geteuid ()
     || stat.st_perm land 0o077 <> 0 then
    invalid_arg "compute history directory permissions"

let open_db ?(max_sessions = 262_144) ?(caller_limit = 4096)
    ~chain_id ~node_id data_dir =
  if max_sessions < 1 || max_sessions > 262_144
     || caller_limit < 1 || caller_limit > 4096
     || chain_id = "" || String.length chain_id > 256
     || node_id = "" || String.length node_id > 128 then
    invalid_arg "compute history configuration";
  let dir = Filename.concat data_dir "compute_history" in
  private_dir dir;
  let owner = Store_lock.acquire dir in
  let resources = Store_scope.create ~release:(fun () -> Store_lock.release owner) in
  Store_scope.guard resources (fun () ->
    let env = Store_scope.acquire resources
      (fun () -> Lmdb.Env.create Lmdb.Rw ~max_maps:4
        ~map_size:work_limit ~flags:Lmdb.Env.Flags.no_tls dir)
      Lmdb.Env.close in
    let limit = map_limit (Lmdb.Env.stat env).psize in
    Lmdb.Env.set_map_size env limit;
    if (Lmdb.Env.info env).map_size <> limit then
      invalid_arg "compute history map exceeds limit";
    let map name = Store_scope.acquire resources
      (fun () -> Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
        ~value:Lmdb.Conv.string ~name env) Lmdb_handle.close in
    let rows = map "sessions" in
    let callers = map "callers" in
    let jobs = map "jobs" in
    let identity = map "identity" in
    let find txn key = try Some (Lmdb.Map.get identity ~txn key) with Not_found -> None in
    begin match Lmdb.Txn.go Lmdb.Rw env (fun txn ->
      match find txn "chain", find txn "node" with
      | None, None when (Lmdb.Map.stat rows ~txn).entries = 0
          && (Lmdb.Map.stat callers ~txn).entries = 0
          && (Lmdb.Map.stat jobs ~txn).entries = 0 ->
        Lmdb.Map.set identity ~txn "chain" chain_id;
        Lmdb.Map.set identity ~txn "node" node_id
      | Some chain, Some node when chain = chain_id && node = node_id -> ()
      | _ -> invalid_arg "compute history identity mismatch") with
    | Some () -> ()
    | None -> failwith "compute history identity write aborted"
    end;
    List.iter (fun path ->
      let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
      Fun.protect ~finally:(fun () -> Unix.close fd)
        (fun () -> Unix.fsync fd)) [dir; data_dir];
    {env; resources; rows; callers; jobs; identity; max_sessions; caller_limit; closed = false})

let live_bytes t txn =
  List.fold_left (fun bytes map ->
    let stat = Lmdb.Map.stat map ~txn in
    let pages = stat.branch_pages + stat.leaf_pages + stat.overflow_pages in
    Int64.add bytes (Int64.mul (Int64.of_int pages) (Int64.of_int stat.psize)))
    0L [t.rows; t.callers; t.jobs; t.identity]

let check_work t txn =
  if live_bytes t txn > Int64.of_int work_limit then
    invalid_arg "compute history work space full"

type saved = { reply : string; context : string option }

type scope = { sequence : int64; request : string; body : string }

let scope_key caller session =
  "\008" ^ encode (Int64.of_int (String.length caller)) ^ caller ^ session

let scope_prefix caller session =
  "\009" ^ encode (Int64.of_int (String.length caller)) ^ caller ^ session

let scope_bytes (scope : scope) =
  if scope.sequence < 0L || String.length scope.request <> 32
     || scope.body = "" || String.length scope.body > max_context_bytes then
    invalid_arg "compute scope shape";
  encode scope.sequence ^ scope.request ^ scope.body

let read_scope bytes =
  if String.length bytes <= 40 || String.length bytes > 40 + max_context_bytes then
    invalid_arg "compute scope length";
  {sequence = decode (String.sub bytes 0 8); request = String.sub bytes 8 32;
   body = String.sub bytes 40 (String.length bytes - 40)}

let archive_scope t txn ~caller ~session bytes =
  let value = read_scope bytes in
  let key = scope_prefix caller session ^ encode value.sequence in
  match Lmdb.Map.get t.jobs ~txn key with
  | previous when previous <> bytes -> invalid_arg "compute scope archive changed"
  | _ -> ()
  | exception Not_found -> Lmdb.Map.set t.jobs ~txn key bytes

let put_scope t txn ~caller ~session value =
  let key = scope_key caller session in
  let bytes = scope_bytes value in
  begin match Lmdb.Map.get t.jobs ~txn key with
  | previous ->
    let scope = read_scope previous in
    if scope.sequence > value.sequence || scope.sequence = value.sequence && previous <> bytes then
      invalid_arg "compute scope changed";
    archive_scope t txn ~caller ~session previous
  | exception Not_found -> ()
  end;
  archive_scope t txn ~caller ~session bytes;
  Lmdb.Map.set t.jobs ~txn key bytes

type row = Ready | Reserved of int64 * string * int option
  | Finished of int64 * string * saved | Cancelled

let zero_suffix value start =
  let rec loop index =
    index = String.length value || value.[index] = '\000' && loop (index + 1) in
  loop start

let padded size body =
  if String.length body > size then invalid_arg "compute reply exceeds reserved space";
  let bytes = Bytes.make size '\000' in
  Bytes.blit_string body 0 bytes 0 (String.length body);
  Bytes.unsafe_to_string bytes

let read_row = function
  | "" -> Ready
  | "\001" -> Cancelled
  | row when String.length row = 40 ->
    Reserved (decode (String.sub row 0 8), String.sub row 8 32, None)
  | row when String.length row > 41 && String.length row <= 41 + max_reply_bytes
      && row.[0] = '\002' ->
    Finished (decode (String.sub row 1 8), String.sub row 9 32,
      {reply = String.sub row 41 (String.length row - 41); context = None})
  | row when String.length row > 49 && row.[0] = '\003' ->
    let size = decode (String.sub row 41 8) in
    if size < 1L || size > Int64.of_int max_reply_bytes then
      invalid_arg "compute history reply length";
    let size = Int64.to_int size in
    let rest = String.length row - 49 - size in
    if rest < 1 || rest > max_context_bytes then
      invalid_arg "compute history context length";
    Finished (decode (String.sub row 1 8), String.sub row 9 32,
      {reply = String.sub row 49 size;
       context = Some (String.sub row (49 + size) rest)})
  | row when String.length row > 57
      && String.length row <= 57 + max_reply_bytes + max_context_bytes
      && row.[0] = '\004' && zero_suffix row 41 ->
    Reserved (decode (String.sub row 1 8), String.sub row 9 32, Some (String.length row))
  | row when String.length row > 57
      && String.length row <= 57 + max_reply_bytes + max_context_bytes
      && row.[0] = '\005' ->
    let size = decode (String.sub row 41 8) in
    let context = decode (String.sub row 49 8) in
    if size < 1L || size > Int64.of_int max_reply_bytes
       || context > Int64.of_int max_context_bytes
       || Int64.add size context > Int64.of_int (String.length row - 57) then
      invalid_arg "compute reserved reply length";
    let size = Int64.to_int size and context = Int64.to_int context in
    if not (zero_suffix row (57 + size + context)) then
      invalid_arg "compute reserved reply padding";
    Finished (decode (String.sub row 1 8), String.sub row 9 32,
      {reply = String.sub row 57 size;
       context = if context = 0 then None else Some (String.sub row (57 + size) context)})
  | _ -> invalid_arg "compute history row length"

let write_row = function
  | Ready -> ""
  | Cancelled -> "\001"
  | Reserved (sequence, request, None) -> encode sequence ^ request
  | Reserved (sequence, request, Some size) ->
    padded size ("\004" ^ encode sequence ^ request)
  | Finished (sequence, request, {reply; context = None}) ->
    "\002" ^ encode sequence ^ request ^ reply
  | Finished (sequence, request, {reply; context = Some context}) ->
    "\003" ^ encode sequence ^ request ^ encode (Int64.of_int (String.length reply))
    ^ reply ^ context

type write = Work | Control | Complete

let held_key caller session = "\011" ^ encode (Int64.of_int (String.length caller)) ^ caller ^ session
let until_key caller session = "\012" ^ encode (Int64.of_int (String.length caller)) ^ caller ^ session
let expiry_key caller session = "\013" ^ encode (Int64.of_int (String.length caller)) ^ caller ^ session

let edit ?(mode = Work) ?(commit = fun _ -> ()) t ~caller ~session change =
  try
    if t.closed then invalid_arg "compute history closed";
    if caller = "" || String.length caller > 128 || String.length session <> 32 then
      invalid_arg "compute history request shape";
    let key = encode (Int64.of_int (String.length caller)) ^ caller ^ session in
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let previous = try Some (read_row (Lmdb.Map.get t.rows ~txn key))
        with Not_found -> None in
      let updated, accepted = change previous in
      Option.iter (fun row ->
        let held = match previous, row with
          | Some (Reserved (_, _, Some size)), Finished _ when mode = Complete -> Some size
          | _ -> None in
        let pages = Option.map (fun _ -> live_bytes t txn) held in
        if previous = None then begin
          let count = try Lmdb.Map.get t.callers ~txn caller |> decode
            with Not_found -> 0L in
          if count >= Int64.of_int t.caller_limit
             || (Lmdb.Map.stat t.rows ~txn).entries >= t.max_sessions then
            invalid_arg "compute history session capacity";
          Lmdb.Map.set t.callers ~txn caller (encode (Int64.succ count))
        end;
        let bytes = match held, row with
          | Some size, Finished (sequence, request, {reply; context}) ->
            let context = Option.value ~default:"" context in
            let body = "\005" ^ encode sequence ^ request
              ^ encode (Int64.of_int (String.length reply))
              ^ encode (Int64.of_int (String.length context)) ^ reply ^ context in
            if String.length body > size then
              invalid_arg "compute reply exceeds reserved space";
            padded (max (57 + (Lmdb.Env.stat t.env).psize) (String.length body)) body
          | _ -> write_row row in
        begin match held with
        | Some size when String.length bytes < size -> Lmdb.Map.remove t.rows ~txn key
        | _ -> ()
        end;
        Lmdb.Map.set t.rows ~txn key bytes;
        commit txn;
        match pages with
        | Some before when live_bytes t txn > before ->
          invalid_arg "compute reserved reply grew storage"
        | Some _ -> ()
        | None when not (mode = Control && Option.is_some previous) -> check_work t txn
        | None -> ()) updated;
      accepted) with
    | Some accepted -> Ok accepted
    | None -> Error "compute history write aborted"
  with exn -> Error (Printexc.to_string exn)

let ensure t ~caller ~session =
  edit t ~caller ~session (function
    | Some Cancelled -> None, false
    | Some _ -> None, true
    | None -> Some Ready, true)

let cancel_edit ?expires ?(commit = fun _ -> ()) t ~caller ~session =
  let permanent = ref false in
  let closed = ref false in
  let commit txn =
    let get key = try Some (Lmdb.Map.get t.jobs ~txn key) with Not_found -> None in
    let key = until_key caller session in
    begin match expires with
    | None -> (try Lmdb.Map.remove t.jobs ~txn key with Not_found -> ())
    | Some until ->
      if until < 0L then invalid_arg "compute cancel expiry";
      let held = match get (held_key caller session) with
        | Some bytes -> decode bytes <> 0L | None -> false in
      if not !permanent && not (!closed && get key = None) && not held then begin
        begin match get (expiry_key caller session) with
        | Some bytes when decode bytes <> until -> invalid_arg "compute cancel deadline differs"
        | _ -> ()
        end;
        match get key with
        | Some bytes when decode bytes <> until -> invalid_arg "compute cancel deadline changed"
        | Some _ -> ()
        | None -> Lmdb.Map.set t.jobs ~txn key (encode until)
      end
    end;
    commit txn in
  edit ~mode:Control ~commit t ~caller ~session (fun previous ->
    closed := previous = Some Cancelled;
    permanent := (match previous with
      | Some (Reserved _ | Finished _) -> true
      | _ -> false);
    Some Cancelled, true)

let cancel ?expires t ~caller ~session = cancel_edit ?expires t ~caller ~session

let cancelled t ~caller ~session =
  edit t ~caller ~session (function
    | Some Cancelled -> None, true
    | _ -> None, false)

let cancel_deadline t ~caller ~session =
  try
    if t.closed then invalid_arg "compute history closed";
    match Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
      let get key = try Some (Lmdb.Map.get t.jobs ~txn key) with Not_found -> None in
      match get (held_key caller session) with
      | Some raw when decode raw > 0L -> None
      | _ -> Option.map decode (get (expiry_key caller session))) with
    | Some until -> Ok until
    | None -> Error "compute cancel deadline read aborted"
  with error -> Error (Printexc.to_string error)

let send_limit = 160
let forward_limit = send_limit / 2
let transit_limit = 32

let take_output t txn ~key ~limit ~now =
  if t.closed || now < 0L || now > Int64.sub Int64.max_int 60_000L then
    invalid_arg "compute relay time";
  let next, count = match Lmdb.Map.get t.identity ~txn key with
    | raw when String.length raw = 16 ->
      decode (String.sub raw 0 8), decode (String.sub raw 8 8)
    | _ -> invalid_arg "compute relay counter"
    | exception Not_found -> 0L, 0L in
  let next, count = if now >= next then Int64.add now 60_000L, 0L else next, count in
  if count >= Int64.of_int limit then false
  else begin
    Lmdb.Map.set t.identity ~txn key (encode next ^ encode (Int64.succ count));
    true
  end

let claim_output t ~key ~limit ~now =
  try
    if t.closed then invalid_arg "compute history closed";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn -> take_output t txn ~key ~limit ~now) with
    | Some allowed -> Ok allowed
    | None -> Error "compute relay claim aborted"
  with error -> Error (Printexc.to_string error)

let claim_relay t ~now = claim_output t ~key:"cancel_relay" ~limit:caller_notices ~now
let claim_send ?(local = true) t ~now =
  try
    if t.closed then invalid_arg "compute history closed";
    if now < 0L || now > Int64.sub Int64.max_int 60_000L then
      invalid_arg "compute send time";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let read key = match Lmdb.Map.get t.identity ~txn key with
        | raw when String.length raw = 16 ->
          decode (String.sub raw 0 8), decode (String.sub raw 8 8)
        | _ -> invalid_arg "compute send counter"
        | exception Not_found -> 0L, 0L in
      let next, count = read "compute_send" in
      let next, count = if now >= next then Int64.add now 60_000L, 0L else next, count in
      let until, forwarded = read "compute_forward" in
      let forwarded = if until = next then forwarded else 0L in
      if count >= Int64.of_int send_limit
          || not local && forwarded >= Int64.of_int forward_limit then false
      else begin
        Lmdb.Map.set t.identity ~txn "compute_send" (encode next ^ encode (Int64.succ count));
        Lmdb.Map.set t.identity ~txn "compute_forward"
          (encode next ^ encode (if local then forwarded else Int64.succ forwarded));
        true
      end) with
    | Some allowed -> Ok allowed
    | None -> Error "compute send claim aborted"
  with error -> Error (Printexc.to_string error)
let claim_transit t ~now = claim_output t ~key:"cancel_transit" ~limit:transit_limit ~now

type notice = {
  caller : string;
  session : string;
  expires : int64;
  round : int;
  body : string;
}

let notice_key caller session =
  "\005" ^ encode (Int64.of_int (String.length caller)) ^ caller ^ session

let notice_bytes value =
  encode value.expires ^ encode (Int64.of_int value.round) ^ value.body

let read_notice key value =
  if String.length key < 42 || key.[0] <> '\005'
     || String.length value < 17 || String.length value > 16 + max_notice_bytes then
    invalid_arg "compute cancel row shape";
  let size = decode (String.sub key 1 8) in
  if size < 1L || size > 128L || Int64.to_int size <> String.length key - 41 then
    invalid_arg "compute cancel caller shape";
  let size = Int64.to_int size in
  let round = decode (String.sub value 8 8) in
  if round > Int64.of_int notice_rounds then invalid_arg "compute cancel round";
  {caller = String.sub key 9 size; session = String.sub key (9 + size) 32;
   expires = decode (String.sub value 0 8); round = Int64.to_int round;
   body = String.sub value 16 (String.length value - 16)}

let notice_rows t txn =
  Lmdb.Cursor.go Lmdb.Ro t.jobs ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
    let rec loop count acc = function
      | Some (key, value) when String.length key > 0 && key.[0] = '\005' ->
        if count >= max_notices then invalid_arg "compute cancel capacity differs";
        let value = read_notice key value in
        begin match read_row (Lmdb.Map.get t.rows ~txn
          (String.sub key 1 (String.length key - 1))) with
        | Cancelled -> ()
        | _ -> invalid_arg "compute cancel session differs"
        end;
        loop (count + 1) ((key, value) :: acc)
          (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
      | _ -> List.rev acc in
    loop 0 [] (try Some (Lmdb.Cursor.seek_range cursor "\005") with Not_found -> None))

let ack_prefix caller session =
  "\007" ^ encode (Int64.of_int (String.length caller)) ^ caller ^ session

let ack_rows t txn caller session =
  let prefix = ack_prefix caller session in
  Lmdb.Cursor.go Lmdb.Ro t.jobs ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
    let rec loop count rows = function
      | Some (key, body) when String.starts_with ~prefix key ->
        let size = String.length key - String.length prefix in
        if count >= max_cancel_acks || size < 1 || size > 128
           || body = "" || String.length body > max_notice_bytes then
          invalid_arg "compute cancel ack row shape";
        loop (count + 1) ((key, body) :: rows)
          (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
      | _ -> List.rev rows in
    loop 0 [] (try Some (Lmdb.Cursor.seek_range cursor prefix) with Not_found -> None))

let live_notices t txn epoch =
  notice_rows t txn |> List.filter (fun (key, value) ->
    if value.expires >= epoch then true
    else begin
      List.iter (fun (key, _) -> Lmdb.Map.remove t.jobs ~txn key)
        (ack_rows t txn value.caller value.session);
      Lmdb.Map.remove t.jobs ~txn key;
      false
    end)

let queue_cancel ?until t ~epoch ~expires ~caller ~session ~body =
  if epoch < 0L || expires < epoch || body = "" || String.length body > max_notice_bytes
     || (match until with None -> false | Some value -> value <> expires) then
    Error "compute cancel message shape"
  else
    let commit txn =
      let rows = live_notices t txn epoch in
      let key = notice_key caller session in
      match List.assoc_opt key rows with
      | Some value when value.body = body -> ()
      | Some _ -> invalid_arg "compute cancel message differs"
      | None ->
        if List.length rows >= max_notices
           || List.length (List.filter (fun (_, row) -> row.caller = caller) rows)
                >= caller_notices then invalid_arg "compute cancel queue full";
        Lmdb.Map.set t.jobs ~txn key (notice_bytes {caller; session; expires; round = 0; body}) in
    cancel_edit ?expires:until ~commit t ~caller ~session

let pending_notices t ~epoch =
  try
    if t.closed || epoch < 0L then invalid_arg "compute cancel read refused";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn -> live_notices t txn epoch |> List.map snd) with
    | Some rows -> Ok rows
    | None -> Error "compute cancel read aborted"
  with error -> Error (Printexc.to_string error)

let claim_notice ?target ?now ?(rounds = true) t ~epoch =
  try
    if t.closed || epoch < 0L then invalid_arg "compute cancel claim refused";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let rows = live_notices t txn epoch in
      let allowed = match now with
        | None -> true
        | Some time ->
          if time < 0L || time > Int64.sub Int64.max_int 3000L then
            invalid_arg "compute cancel time";
          let next = try Lmdb.Map.get t.identity ~txn "cancel_next" |> decode with Not_found -> 0L in
          time >= next in
      let next = List.fold_left (fun best ((_, row) as entry) ->
        if not allowed || row.round >= (if rounds then notice_rounds else 1)
           || (match target with None -> false
               | Some (caller, session) -> row.caller <> caller || row.session <> session)
        then best else
        match best with
        | Some (_, previous) when previous.round <= row.round -> best
        | _ -> Some entry) None rows in
      match next with
      | None -> None
      | Some (key, row) ->
        let updated = {row with round = row.round + 1} in
        Lmdb.Map.set t.jobs ~txn key (notice_bytes updated);
        Option.iter (fun time -> Lmdb.Map.set t.identity ~txn "cancel_next"
          (encode (Int64.add time 3000L))) now;
        Some updated) with
    | Some value -> Ok value
    | None -> Error "compute cancel claim aborted"
  with error -> Error (Printexc.to_string error)

let confirm_cancel t ~epoch ~expires ~round ~caller ~session ~cancel ~node ~body =
  try
    if t.closed || epoch < 0L || caller = "" || String.length caller > 128
       || String.length session <> 32 || node = "" || String.length node > 128
       || body = "" || String.length body > max_notice_bytes then
      invalid_arg "compute cancel ack shape";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let rows = live_notices t txn epoch in
      match List.assoc_opt (notice_key caller session) rows with
      | None -> false
      | Some notice when notice.body <> cancel || notice.expires <> expires
          || round < 2 || round > notice.round -> false
      | Some _ ->
        let rows = ack_rows t txn caller session in
        let key = ack_prefix caller session ^ node in
        if List.mem_assoc key rows then false
        else if List.length rows >= max_cancel_acks then
          invalid_arg "compute cancel ack capacity"
        else (Lmdb.Map.set t.jobs ~txn key body; true)) with
    | Some accepted -> Ok accepted
    | None -> Error "compute cancel ack write aborted"
  with error -> Error (Printexc.to_string error)

let cancel_acks t ~epoch ~caller ~session =
  try
    if t.closed || epoch < 0L || caller = "" || String.length caller > 128
       || String.length session <> 32 then invalid_arg "compute cancel ack read shape";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let rows = live_notices t txn epoch in
      match List.assoc_opt (notice_key caller session) rows with
      | None -> None
      | Some notice -> Some (notice, List.map snd (ack_rows t txn caller session))) with
    | Some value -> Ok value
    | None -> Error "compute cancel ack read aborted"
  with error -> Error (Printexc.to_string error)

let relay_rows t txn =
  Lmdb.Cursor.go Lmdb.Ro t.jobs ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
    let rec loop count rows = function
      | Some (key, value) when String.length key > 0 && key.[0] = '\006' ->
        if count >= max_notices then invalid_arg "compute cancel relay capacity differs";
        if String.length key < 42 || String.length value <> 16 then
          invalid_arg "compute cancel relay shape";
        let size = decode (String.sub key 1 8) in
        if size < 1L || size > 128L || Int64.to_int size <> String.length key - 41 then
          invalid_arg "compute cancel relay origin";
        let origin = String.sub key 9 (Int64.to_int size) in
        let expires = decode (String.sub value 0 8) in
        let round = decode (String.sub value 8 8) in
        if round < 1L || round > Int64.of_int notice_rounds then
          invalid_arg "compute cancel relay round";
        loop (count + 1) ((key, (origin, expires, round)) :: rows)
          (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
      | _ -> List.rev rows in
    loop 0 [] (try Some (Lmdb.Cursor.seek_range cursor "\006") with Not_found -> None))

let relay_cancel ?until ?(close = true) ?now ?(transit = false)
    t ~epoch ~expires ~round ~origin ~id ~caller ~session =
  if t.closed || epoch < 0L || expires < epoch || Int64.sub expires epoch > 64L
     || round < 1 || round > notice_rounds || origin = ""
     || String.length origin > 128 || String.length id <> 32
     || (match until with None -> false | Some value -> value <> expires) then
    Error "compute cancel relay request"
  else
    let commit txn =
      let rows = relay_rows t txn |> List.filter (fun (key, (_, until, _)) ->
        if until >= epoch then true else (Lmdb.Map.remove t.jobs ~txn key; false)) in
      let key = "\006" ^ encode (Int64.of_int (String.length origin)) ^ origin ^ id in
      begin match List.assoc_opt key rows with
      | Some (_, until, _) when until <> expires ->
        invalid_arg "compute cancel relay expiry differs"
      | Some (_, _, previous) when previous >= Int64.of_int round ->
        invalid_arg "compute cancel relay repeated"
      | Some _ -> ()
      | None ->
        if List.length rows >= max_notices
           || List.length (List.filter (fun (_, (owner, _, _)) -> owner = origin) rows)
                >= caller_notices then invalid_arg "compute cancel relay full"
      end;
      Option.iter (fun now ->
        let key, limit = if transit then "cancel_transit", transit_limit
          else "cancel_relay", caller_notices in
        if not (take_output t txn ~key ~limit ~now) then
          invalid_arg "compute cancel relay quota") now;
      Lmdb.Map.set t.jobs ~txn key (encode expires ^ encode (Int64.of_int round)) in
    if close then cancel_edit ?expires:until ~commit t ~caller ~session
    else try
      if t.closed then invalid_arg "compute history closed";
      match Lmdb.Txn.go Lmdb.Rw t.env (fun txn -> commit txn; check_work t txn; true) with
      | Some value -> Ok value
      | None -> Error "compute relay write aborted"
    with error -> Error (Printexc.to_string error)

let reserve ?scope ?(room = max_reply_bytes + max_context_bytes)
    t ~caller ~session ~sequence ~request =
  if sequence < 0L || String.length request <> 32
     || room < 1 || room > max_reply_bytes + max_context_bytes
     || (match scope with None -> false | Some body -> body = "" || String.length body > max_context_bytes) then
    Error "compute history request shape"
  else
    let commit txn = match scope with
      | Some body -> put_scope t txn ~caller ~session {sequence; request; body}
      | None ->
        let key = scope_key caller session in
        (try
           archive_scope t txn ~caller ~session (Lmdb.Map.get t.jobs ~txn key);
           Lmdb.Map.remove t.jobs ~txn key
         with Not_found -> ()) in
    edit ~commit t ~caller ~session (function
    | Some Cancelled -> None, false
    | Some (Reserved (saved, _, _) | Finished (saved, _, _)) when sequence <= saved -> None, false
    | _ -> Some (Reserved (sequence, request,
        Some (57 + max room (Lmdb.Env.stat t.env).psize))), true)

let scope t ~caller ~session =
  try
    if t.closed || caller = "" || String.length caller > 128 || String.length session <> 32 then
      invalid_arg "compute scope read shape";
    match Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
      try Some (read_scope (Lmdb.Map.get t.jobs ~txn (scope_key caller session)))
      with Not_found -> None) with
    | Some value -> Ok value
    | None -> Error "compute scope read aborted"
  with error -> Error (Printexc.to_string error)

let find_scope t ~caller ~session accept =
  try
    if t.closed || caller = "" || String.length caller > 128 || String.length session <> 32 then
      invalid_arg "compute scope read shape";
    match Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
      let latest = try Some (read_scope (Lmdb.Map.get t.jobs ~txn (scope_key caller session)))
        with Not_found -> None in
      match latest with
      | Some value when accept value -> latest
      | _ ->
        let prefix = scope_prefix caller session in
        Lmdb.Cursor.go Lmdb.Ro t.jobs ~txn (fun cursor ->
          let rec find = function
            | Some (key, bytes) when String.starts_with ~prefix key ->
              if String.length key <> String.length prefix + 8 then
                invalid_arg "compute scope archive key";
              let value = read_scope bytes in
              if String.sub key (String.length prefix) 8 <> encode value.sequence then
                invalid_arg "compute scope archive sequence";
              if accept value then Some value
              else find (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
            | _ -> None in
          find (try Some (Lmdb.Cursor.seek_range cursor prefix) with Not_found -> None))) with
    | Some value -> Ok value
    | None -> Error "compute scope read aborted"
  with error -> Error (Printexc.to_string error)

let finish ?context t ~caller ~session ~sequence ~request ~reply =
  if sequence < 0L || String.length request <> 32
     || reply = "" || String.length reply > max_reply_bytes
     || (match context with None -> false
         | Some value -> value = "" || String.length value > max_context_bytes) then
    Error "compute history reply shape"
  else edit ~mode:Complete t ~caller ~session (function
    | Some (Reserved (saved, id, _)) when saved = sequence && id = request ->
      Some (Finished (sequence, request, {reply; context})), true
    | Some (Finished (saved, id, prior)) ->
      None, saved = sequence && id = request && prior = {reply; context}
    | _ -> None, false)

let saved t ~caller ~session ~sequence ~request =
  if sequence < 0L || String.length request <> 32 then
    Error "compute history request shape"
  else edit t ~caller ~session (function
    | Some (Finished (saved, id, reply)) when saved = sequence && id = request ->
      None, Some reply
    | _ -> None, None)

let reply t ~caller ~session ~sequence ~request =
  Result.map (Option.map (fun value -> value.reply))
    (saved t ~caller ~session ~sequence ~request)

let max_job_bytes = 2 * 1024 * 1024

let job_key caller id =
  if caller = "" || String.length caller > 128 || String.length id <> 32 then
    invalid_arg "compute job identity shape";
  encode (Int64.of_int (String.length caller)) ^ caller ^ id

type work = Planned of string | Funding of string | Issued of string

type job_state = Pending of work | Stored of string
type job_row = { session : string; value : job_state; room : int option }

let work_bytes = function
  | Planned body -> "\000" ^ body
  | Funding body -> "\004" ^ body
  | Issued body -> "\001" ^ body

let read_work value =
  if String.length value < 34 || String.length value > 41 + max_job_bytes then
    invalid_arg "compute work row shape";
  let session = String.sub value 0 32 in
  match value.[32] with
    | ('\000' | '\001' | '\004') as tag when String.length value <= 33 + max_job_bytes ->
      let body = String.sub value 33 (String.length value - 33) in
      let work = match tag with
        | '\000' -> Planned body | '\004' -> Funding body | _ -> Issued body in
      {session; value = Pending work; room = None}
    | ('\002' | '\003' | '\005') as tag when String.length value > 41 ->
      let size = decode (String.sub value 33 8) in
      if size < 1L || size > Int64.of_int max_job_bytes
         || size > Int64.of_int (String.length value - 41) then
        invalid_arg "compute work payload size";
      let size = Int64.to_int size in
      if not (zero_suffix value (41 + size)) then
        invalid_arg "compute work padding";
      let body = String.sub value 41 size in
      {session; value = (match tag with
         | '\002' -> Pending (Issued body) | '\005' -> Pending (Funding body)
         | _ -> Stored body);
       room = Some (String.length value)}
    | _ -> invalid_arg "compute work stage"

let job_bytes row =
  match row.room, row.value with
  | None, Pending work -> row.session ^ work_bytes work
  | Some size, (Pending (Issued body | Funding body) | Stored body) ->
    let tag = match row.value with
      | Stored _ -> '\003' | Pending (Funding _) -> '\005' | _ -> '\002' in
    let bytes = row.session ^ String.make 1 tag ^ encode (Int64.of_int (String.length body)) ^ body in
    if String.length bytes > size then invalid_arg "compute job exceeds reserved space";
    padded size bytes
  | _ -> invalid_arg "compute work allocation stage"

let check_job t txn = function
  | None -> check_work t txn
  | Some before when live_bytes t txn > before ->
    invalid_arg "compute reserved job grew storage"
  | Some _ -> ()

let split_job key =
  if String.length key < 42 then invalid_arg "compute job key size";
  let size = decode (String.sub key 1 8) in
  if size < 1L || size > 128L || Int64.to_int size <> String.length key - 41 then
    invalid_arg "compute job key caller";
  let size = Int64.to_int size in
  String.sub key 9 size, String.sub key (9 + size) 32

let job_refs caller session = "\010" ^ job_key caller session

let add_ref t txn caller session =
  let key = job_refs caller session in
  let count = try Lmdb.Map.get t.jobs ~txn key |> decode with Not_found -> 0L in
  if count >= Int64.of_int t.max_sessions then invalid_arg "compute job reference capacity";
  Lmdb.Map.set t.jobs ~txn key (encode (Int64.succ count))

let index_refs t txn =
  match Lmdb.Map.get t.identity ~txn "job_refs" with
  | "ready" -> ()
  | _ -> invalid_arg "compute job reference index"
  | exception Not_found ->
    List.iter (fun tag ->
      Lmdb.Cursor.go Lmdb.Ro t.jobs ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
        let rec loop count = function
          | Some (key, value) when String.starts_with ~prefix:tag key ->
            if count >= t.max_sessions then invalid_arg "compute job index capacity";
            let caller, _ = split_job key in
            let session = if tag = "\003" then (read_work value).session
              else if String.length value > 32 then String.sub value 0 32
              else invalid_arg "compute saved job size" in
            add_ref t txn caller session;
            loop (count + 1) (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
          | _ -> () in
        loop 0 (try Some (Lmdb.Cursor.seek_range cursor tag) with Not_found -> None)))
      ["\002"; "\003"];
    Lmdb.Map.set t.identity ~txn "job_refs" "ready"

let count_job t txn caller session =
  index_refs t txn;
  let count key = try Lmdb.Map.get t.jobs ~txn key |> decode with Not_found -> 0L in
  let total = count "\000" in
  let caller_key = "\001" ^ caller in
  let own = count caller_key in
  if total >= Int64.of_int t.max_sessions || own >= Int64.of_int t.caller_limit then
    invalid_arg "compute job capacity";
  Lmdb.Map.set t.jobs ~txn "\000" (encode (Int64.succ total));
  Lmdb.Map.set t.jobs ~txn caller_key (encode (Int64.succ own));
  add_ref t txn caller session

let known_job t ~caller ~session =
  try
    if t.closed then invalid_arg "compute history closed";
    let key = job_key caller session in
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      index_refs t txn;
      let get map key = try Some (Lmdb.Map.get map ~txn key) with Not_found -> None in
      match get t.jobs (job_refs caller session) with
      | Some count when decode count > 0L -> true
      | _ ->
        match get t.rows key with
        | Some row -> (match read_row row with Reserved _ | Finished _ -> true
            | _ -> Option.is_some (get t.jobs (notice_key caller session))
                || Option.is_some (get t.jobs (scope_key caller session)))
        | None -> false) with
    | Some known -> Ok known
    | None -> Error "compute job lookup aborted"
  with error -> Error (Printexc.to_string error)

let plan_job ?expires t ~caller ~session ~id ~body =
  try
    if t.closed then invalid_arg "compute history closed";
    if String.length session <> 32 || body = "" || String.length body > max_job_bytes then
      invalid_arg "compute job body shape";
    let key = job_key caller id in
    let value = session ^ work_bytes (Planned body) in
    let session_key = encode (Int64.of_int (String.length caller)) ^ caller ^ session in
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let get map key = try Some (Lmdb.Map.get map ~txn key) with Not_found -> None in
      match get t.rows session_key with
      | Some "\001" -> false
      | row ->
        Option.iter (fun row -> ignore (read_row row)) row;
        if Option.is_some (get t.jobs ("\002" ^ key)) then false
        else match get t.jobs ("\003" ^ key) with
        | Some previous ->
          let saved = read_work previous in
          saved.session = session && saved.value = Pending (Planned body)
        | None ->
          if row = None then begin
            let count = match get t.callers caller with None -> 0L | Some raw -> decode raw in
            if count >= Int64.of_int t.caller_limit
                || (Lmdb.Map.stat t.rows ~txn).entries >= t.max_sessions then
              invalid_arg "compute history session capacity";
            Lmdb.Map.set t.callers ~txn caller (encode (Int64.succ count));
            Lmdb.Map.set t.rows ~txn session_key ""
          end;
          count_job t txn caller session;
          Option.iter (fun until ->
            if until < 0L then invalid_arg "compute job expiry";
            let expiry = expiry_key caller session in
            let previous = match get t.jobs expiry with None -> 0L | Some raw -> decode raw in
            Lmdb.Map.set t.jobs ~txn expiry (encode (max previous until));
            let held = held_key caller session in
            if get t.jobs held = None then Lmdb.Map.set t.jobs ~txn held (encode 0L)) expires;
          Lmdb.Map.set t.jobs ~txn ("\003" ^ key) value;
          check_work t txn;
          true) with
    | Some accepted -> Ok accepted
    | None -> Error "compute work write aborted"
  with error -> Error (Printexc.to_string error)

let work t ~caller ~id =
  try
    if t.closed then invalid_arg "compute history closed";
    let key = "\003" ^ job_key caller id in
    match Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
      try match (read_work (Lmdb.Map.get t.jobs ~txn key)).value with
        | Pending value -> Some value
        | Stored _ -> None
      with Not_found -> None) with
    | Some value -> Ok value
    | None -> Error "compute work read aborted"
  with error -> Error (Printexc.to_string error)

let prune_jobs t ~expired =
  try
    if t.closed then invalid_arg "compute history closed";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      index_refs t txn;
      let start = try Lmdb.Map.get t.identity ~txn "job_cursor" with Not_found -> "\003" in
      let rows, next = Lmdb.Cursor.go Lmdb.Ro t.jobs
        ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
          let rec read left acc = function
            | Some (key, value) when String.starts_with ~prefix:"\003" key ->
              if left = 0 then List.rev acc, key
              else read (left - 1) ((key, value) :: acc)
                (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
            | _ -> List.rev acc, "\003" in
          read 32 [] (try Some (Lmdb.Cursor.seek_range cursor start) with Not_found -> None)) in
      let remove map key = try Lmdb.Map.remove map ~txn key with Not_found -> () in
      let decrease map key =
        let count = Lmdb.Map.get map ~txn key |> decode in
        if count < 1L then invalid_arg "compute history counter empty";
        if count = 1L then remove map key
        else Lmdb.Map.set map ~txn key (encode (Int64.pred count));
        count = 1L in
      let removed = List.fold_left (fun count (key, raw) ->
        let row = read_work raw in
        let caller, id = split_job key in
        let discard = match row.value with
          | Pending (Planned body | Funding body) -> expired ~caller ~session:row.session ~id body
          | Pending (Issued _) | Stored _ -> false in
        if not discard then count
        else begin
          remove t.jobs key;
          remove t.jobs ("\004" ^ job_key caller id);
          ignore (decrease t.jobs "\000");
          ignore (decrease t.jobs ("\001" ^ caller));
          if decrease t.jobs (job_refs caller row.session) then begin
            let session_key = job_key caller row.session in
            match Lmdb.Map.get t.rows ~txn session_key |> read_row with
            | Ready ->
              remove t.rows session_key;
              ignore (decrease t.callers caller);
              remove t.jobs (scope_key caller row.session);
              remove t.jobs (held_key caller row.session);
              remove t.jobs (expiry_key caller row.session);
              let prefix = scope_prefix caller row.session in
              let keys = Lmdb.Cursor.go Lmdb.Ro t.jobs
                ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
                  let rec read acc = function
                    | Some (key, _) when String.starts_with ~prefix key ->
                      read (key :: acc) (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
                    | _ -> acc in
                  read [] (try Some (Lmdb.Cursor.seek_range cursor prefix) with Not_found -> None)) in
              List.iter (remove t.jobs) keys
            | Reserved _ | Finished _ | Cancelled -> ()
          end;
          count + 1
        end) 0 rows in
      Lmdb.Map.set t.identity ~txn "job_cursor" next;
      removed) with
    | Some count -> Ok count
    | None -> Error "compute history prune aborted"
  with error -> Error (Printexc.to_string error)

let prune_cancels t ~epoch =
  try
    if t.closed || epoch < 0L then invalid_arg "compute cancel expiry read";
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      ignore (live_notices t txn epoch);
      index_refs t txn;
      let start = try Lmdb.Map.get t.identity ~txn "cancel_cursor" with Not_found -> "\012" in
      let rows, next = Lmdb.Cursor.go Lmdb.Ro t.jobs
        ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
          let rec read left acc = function
            | Some (key, value) when String.starts_with ~prefix:"\012" key ->
              if left = 0 then List.rev acc, key
              else read (left - 1) ((key, value) :: acc)
                (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
            | _ -> List.rev acc, "\012" in
          read 32 [] (try Some (Lmdb.Cursor.seek_range cursor start) with Not_found -> None)) in
      let remove map key = try Lmdb.Map.remove map ~txn key with Not_found -> () in
      let get key = try Some (Lmdb.Map.get t.jobs ~txn key) with Not_found -> None in
      let removed = List.fold_left (fun count (key, raw) ->
        let caller, session = split_job key in
        if decode raw >= epoch || get (job_refs caller session) <> None then count
        else begin
          let held = match get (held_key caller session) with
            | Some bytes -> decode bytes <> 0L | None -> false in
          if held then invalid_arg "compute paid cancellation expiry";
          let row = job_key caller session in
          if Lmdb.Map.get t.rows ~txn row <> "\001" then
            invalid_arg "compute cancel expiry row differs";
          remove t.rows row;
          let own = Lmdb.Map.get t.callers ~txn caller |> decode in
          if own < 1L then invalid_arg "compute caller count empty";
          if own = 1L then remove t.callers caller
          else Lmdb.Map.set t.callers ~txn caller (encode (Int64.pred own));
          List.iter (remove t.jobs) [key; held_key caller session; expiry_key caller session;
            scope_key caller session];
          let prefix = scope_prefix caller session in
          let keys = Lmdb.Cursor.go Lmdb.Ro t.jobs
            ~txn:(txn :> [`Read] Lmdb.Txn.t) (fun cursor ->
              let rec read acc = function
                | Some (key, _) when String.starts_with ~prefix key ->
                  read (key :: acc) (try Some (Lmdb.Cursor.next cursor) with Not_found -> None)
                | _ -> acc in
              read [] (try Some (Lmdb.Cursor.seek_range cursor prefix) with Not_found -> None)) in
          List.iter (remove t.jobs) keys;
          count + 1
        end) 0 rows in
      Lmdb.Map.set t.identity ~txn "cancel_cursor" next;
      removed) with
    | Some count -> Ok count
    | None -> Error "compute cancel expiry aborted"
  with error -> Error (Printexc.to_string error)

let claim_fetch t ~caller ~session ~id ~limit =
  try
    if t.closed then invalid_arg "compute history closed";
    if String.length session <> 32 || limit < 1 || limit > 256 then
      invalid_arg "compute fetch limit shape";
    let key = job_key caller id in
    let round_key = "\004" ^ key in
    let session_key = encode (Int64.of_int (String.length caller)) ^ caller ^ session in
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let get map key = try Some (Lmdb.Map.get map ~txn key) with Not_found -> None in
      match get t.rows session_key, get t.jobs ("\003" ^ key) with
      | Some row, Some work when row <> "\001" ->
        ignore (read_row row);
        let saved = read_work work in
        begin match saved.value with
        | Pending (Issued _) when saved.session = session ->
          let pages = Option.map (fun _ -> live_bytes t txn) saved.room in
          let used = match get t.jobs round_key with None -> 0L | Some bytes -> decode bytes in
          if saved.room <> None && get t.jobs round_key = None then
            invalid_arg "compute reserved fetch counter absent";
          if used > Int64.of_int limit then invalid_arg "compute fetch counter exceeds limit";
          if used = Int64.of_int limit then None
          else begin
            Lmdb.Map.set t.jobs ~txn round_key (encode (Int64.succ used));
            check_job t txn pages;
            Some (Int64.to_int used)
          end
        | _ -> None
        end
      | _ -> None) with
    | Some value -> Ok value
    | None -> Error "compute fetch write aborted"
  with error -> Error (Printexc.to_string error)

let move_job ?scope ?room ?(check = fun _ -> ()) t ~caller ~session ~id ~before ~after =
  try
    if t.closed then invalid_arg "compute history closed";
    let valid = function body -> body <> "" && String.length body <= max_job_bytes in
    let issued = match after with Issued body | Funding body -> body | Planned _ -> "" in
    let prior = match before with Planned body | Funding body | Issued body -> body in
    if String.length session <> 32 || not (valid prior && valid issued)
       || (match room with None -> false | Some bytes -> bytes < 1 || bytes > max_job_bytes) then
      invalid_arg "compute work transition shape";
    let key = "\003" ^ job_key caller id in
    let session_key = encode (Int64.of_int (String.length caller)) ^ caller ^ session in
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let get map key = try Some (Lmdb.Map.get map ~txn key) with Not_found -> None in
      match get t.rows session_key, get t.jobs key with
      | Some row, Some previous when row <> "\001" ->
        ignore (read_row row);
        check txn;
        let saved = read_work previous in
        if saved.session <> session then false
        else if saved.value = Pending after then begin
          Option.iter (fun bytes ->
            if saved.room <> Some (41 + max bytes (Lmdb.Env.stat t.env).psize) then
              invalid_arg "compute work allocation changed") room;
          let pages = Option.map (fun _ -> live_bytes t txn) saved.room in
          Option.iter (put_scope t txn ~caller ~session) scope;
          check_job t txn pages;
          true
        end
        else if saved.value <> Pending before then false
        else begin
          let pages = Option.map (fun _ -> live_bytes t txn) saved.room in
          let capacity = match saved.room, room, before with
            | Some size, None, _ -> Some size
            | None, Some bytes, Planned _ ->
              Some (41 + max bytes (Lmdb.Env.stat t.env).psize)
            | None, None, _ -> None
            | _ -> invalid_arg "compute work allocation changed" in
          Option.iter (put_scope t txn ~caller ~session) scope;
          (match after, get t.jobs (held_key caller session) with
          | Issued _, Some _ -> Lmdb.Map.set t.jobs ~txn (held_key caller session) (encode 1L)
          | _ -> ());
          Lmdb.Map.set t.jobs ~txn key (job_bytes {saved with value = Pending after; room = capacity});
          if saved.room = None && capacity <> None then begin
            let counter = "\004" ^ job_key caller id in
            if get t.jobs counter <> None then invalid_arg "compute fetch counter already exists";
            Lmdb.Map.set t.jobs ~txn counter (encode 0L)
          end;
          check_job t txn pages;
          true
        end
      | _ -> false) with
    | Some accepted -> Ok accepted
    | None -> Error "compute work write aborted"
  with error -> Error (Printexc.to_string error)

let advance_job ?scope ?room t ~caller ~session ~id ~before ~after =
  match before, after with
  | Funding _, _ | _, Funding _ -> Error "compute funding proof required"
  | _ -> move_job ?scope ?room t ~caller ~session ~id ~before ~after

let wait_job ~scope ~room t ~caller ~session ~id ~before ~body =
  let check txn =
    let saved = Lmdb.Map.get t.jobs ~txn ("\003" ^ job_key caller id) |> read_work in
    match saved.value with
    | Pending (Funding _) ->
      let current = Lmdb.Map.get t.jobs ~txn (scope_key caller session) |> read_scope in
      if current <> scope then invalid_arg "compute funding scope changed"
    | _ -> () in
  move_job ~scope ~room ~check t ~caller ~session ~id
    ~before:(Planned before) ~after:(Funding body)

let fund_job t ~epoch ~state_root ~id ~body (funds : Compute_funding.funded) =
  let request = funds.header.request in
  let caller = request.caller and session = request.session_id in
  if epoch <> funds.epoch || state_root <> funds.state_root then
    Error "compute funding observation changed"
  else if id <> Compute_funding.Protocol.header_id funds.header then
    Error "compute funding job differs"
  else
    let check txn =
      let chain = Lmdb.Map.get t.identity ~txn "chain" in
      if chain <> request.chain_id then invalid_arg "compute funding chain differs";
      let scope = Lmdb.Map.get t.jobs ~txn (scope_key caller session) |> read_scope in
      if scope.sequence <> request.sequence
          || scope.request <> funds.deposit.request_id then
        invalid_arg "compute funding request differs" in
    move_job ~check t ~caller ~session ~id ~before:(Funding body) ~after:(Issued body)

let save_job t ~caller ~session ~id ~body =
  try
    if t.closed then invalid_arg "compute history closed";
    if String.length session <> 32 || body = "" || String.length body > max_job_bytes then
      invalid_arg "compute job body shape";
    let key = "\002" ^ job_key caller id in
    let session_key = encode (Int64.of_int (String.length caller)) ^ caller ^ session in
    let value = session ^ body in
    match Lmdb.Txn.go Lmdb.Rw t.env (fun txn ->
      let get map key = try Some (Lmdb.Map.get map ~txn key) with Not_found -> None in
      match get t.rows session_key with
      | None | Some "\001" -> false
      | Some row ->
        ignore (read_row row);
        let pending = "\003" ^ job_key caller id in
        match get t.jobs key, get t.jobs pending with
        | Some _, Some _ -> invalid_arg "compute job has duplicate rows"
        | Some prior, None -> prior = value
        | None, Some row ->
            let saved = read_work row in
            if saved.session <> session then false
            else begin match saved.value, saved.room with
            | Stored prior, Some _ -> prior = body
            | Pending (Issued _), Some size ->
              let pages = live_bytes t txn in
              let needed = 41 + String.length body in
              if needed > size then invalid_arg "compute job exceeds reserved space";
              let room = max (41 + (Lmdb.Env.stat t.env).psize) needed in
              if room < size then Lmdb.Map.remove t.jobs ~txn pending;
              Lmdb.Map.set t.jobs ~txn pending
                (job_bytes {saved with value = Stored body; room = Some room});
              check_job t txn (Some pages);
              true
            | Pending (Issued _), None ->
              Lmdb.Map.remove t.jobs ~txn pending;
              Lmdb.Map.set t.jobs ~txn key value;
              check_work t txn;
              true
            | _ -> false
            end
        | None, None ->
            count_job t txn caller session;
            Lmdb.Map.set t.jobs ~txn key value;
            check_work t txn;
            true) with
    | Some accepted -> Ok accepted
    | None -> Error "compute job write aborted"
  with error -> Error (Printexc.to_string error)

let load_job t ~caller ~id =
  try
    if t.closed then invalid_arg "compute history closed";
    let key = job_key caller id in
    match Lmdb.Txn.go Lmdb.Ro t.env (fun txn ->
      let get key = try Some (Lmdb.Map.get t.jobs ~txn key) with Not_found -> None in
      match get ("\002" ^ key), get ("\003" ^ key) with
      | Some _, Some _ -> invalid_arg "compute job has duplicate rows"
      | Some value, None when String.length value > 32
          && String.length value <= 32 + max_job_bytes ->
        Some (String.sub value 32 (String.length value - 32))
      | None, Some value ->
        begin match (read_work value).value with Stored body -> Some body | Pending _ -> None end
      | None, None -> None
      | _ -> invalid_arg "compute job row shape") with
    | Some body -> Ok body
    | None -> Error "compute job read aborted"
  with error -> Error (Printexc.to_string error)

let close t =
  if not t.closed then begin
    t.closed <- true;
    Store_scope.close t.resources
  end