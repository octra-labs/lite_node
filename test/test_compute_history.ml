(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module History = Octra_core.Compute_history

let expect name condition =
  if not condition then failwith ("compute_history: " ^ name)

let open_db ?max_sessions ?caller_limit ?(node_id = "node") ?(chain_id = "chain") path =
  History.open_db ?max_sessions ?caller_limit ~chain_id ~node_id path

let reserve ?scope ?(caller = "alice") ?(session = 's') ?(sequence = 1L) db =
  History.reserve ?scope db ~caller ~session:(String.make 32 session)
    ~sequence ~request:(String.make 32 'r')

let test_scope () =
  Test_workspace.with_dir "compute_scope" (fun path ->
    let db = open_db ~max_sessions:1 path in
    let session = String.make 32 's' in
    let read db caller = History.scope db ~caller ~session in
    let find db caller body = History.find_scope db ~caller ~session
      (fun value -> value.body = body) in
    let first = History.{sequence = 1L; request = String.make 32 'r'; body = "keys"} in
    expect "scope absent" (read db "alice" = Ok None);
    expect "scope reserved" (reserve ~scope:first.body db = Ok true);
    expect "scope before completion" (read db "alice" = Ok (Some first));
    expect "scope capacity refused" (Result.is_error (reserve ~scope:"other" ~caller:"bob" db));
    expect "scope capacity atomic" (read db "bob" = Ok None);
    expect "scope repeat refused" (reserve ~scope:"changed" db = Ok false);
    expect "scope repeat unchanged" (read db "alice" = Ok (Some first));
    let id = String.make 32 'j' in
    expect "scope plan" (History.plan_job db ~caller:"alice" ~session ~id ~body:"plan" = Ok true);
    expect "scope mismatch refused" (Result.is_error (History.advance_job db
      ~scope:{first with body = "changed"} ~caller:"alice" ~session ~id
      ~before:(Planned "plan") ~after:(Issued "issued")));
    expect "scope mismatch work atomic" (History.work db ~caller:"alice" ~id = Ok (Some (Planned "plan")));
    expect "scope mismatch unchanged" (read db "alice" = Ok (Some first));
    expect "scope next" (reserve ~scope:"next" ~sequence:2L db = Ok true);
    let next = {first with sequence = 2L; body = "next"} in
    expect "scope replaced" (read db "alice" = Ok (Some next));
    expect "scope older refused" (reserve ~scope:"old" db = Ok false);
    expect "scope older unchanged" (read db "alice" = Ok (Some next));
    expect "scope empty refused" (Result.is_error (reserve ~scope:"" ~sequence:3L db));
    expect "scope excess refused" (Result.is_error (reserve
      ~scope:(String.make (History.max_context_bytes + 1) 'x') ~sequence:3L db));
    expect "scope size refusal atomic" (read db "alice" = Ok (Some next));
    expect "scope first archived" (find db "alice" "keys" = Ok (Some first));
    expect "scope refused absent" (find db "alice" "changed" = Ok None);
    expect "scope caller isolated" (find db "bob" "keys" = Ok None);
    expect "scope session isolated" (History.find_scope db ~caller:"alice"
      ~session:(String.make 32 't') (fun _ -> true) = Ok None);
    expect "scope legacy next" (reserve ~sequence:3L db = Ok true);
    expect "scope legacy cleared" (read db "alice" = Ok None);
    expect "scope legacy keeps archive" (find db "alice" "next" = Ok (Some next));
    expect "scope latest" (reserve ~scope:"latest" ~sequence:4L db = Ok true);
    let latest = {first with sequence = 4L; body = "latest"} in
    expect "scope cancel" (History.cancel db ~caller:"alice" ~session = Ok true);
    expect "scope retained on closure" (read db "alice" = Ok (Some latest));
    History.close db;
    let db = open_db path in
    expect "scope restart" (read db "alice" = Ok (Some latest));
    expect "scope restart keeps first" (find db "alice" "keys" = Ok (Some first));
    expect "scope restart keeps second" (find db "alice" "next" = Ok (Some next));
    expect "scope unknown absent" (find db "alice" "unknown" = Ok None);
    expect "scope closed sequence" (reserve ~scope:"reopen" ~sequence:5L db = Ok false);
    History.close db;
    expect "scope closed read refused" (Result.is_error (read db "alice"));
    expect "scope closed search refused" (Result.is_error (find db "alice" "keys")))

let test_scope_capacity () =
  Test_workspace.with_dir "compute_scope_capacity" (fun path ->
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      let session = String.make 32 's' in
      let body = String.make History.max_context_bytes 'a' in
      let rec fill sequence =
        if sequence > 64L then failwith "compute scope capacity not enforced";
        match reserve ~scope:body ~sequence db with
        | Ok true -> fill (Int64.succ sequence)
        | Error _ -> sequence
        | Ok false -> failwith "compute scope capacity unexpected sequence" in
      let refused = fill 1L in
      expect "scope capacity has committed history" (refused > 1L);
      let prior = History.{sequence = Int64.pred refused; request = String.make 32 'r'; body} in
      expect "scope full keeps latest" (History.scope db ~caller:"alice" ~session = Ok (Some prior));
      expect "scope full excludes refused" (History.find_scope db ~caller:"alice" ~session
        (fun value -> value.sequence = refused) = Ok None);
      expect "scope full keeps oldest" (History.find_scope db ~caller:"alice" ~session
        (fun value -> value.sequence = 1L) = Ok (Some {prior with sequence = 1L}));
      expect "scope full reserve atomic" (reserve ~sequence:refused db = Ok true)))

let test_page_budget () =
  List.iter (fun page ->
    expect "page budget reserves controls" (History.map_limit page > History.work_limit);
    expect "page budget whole pages" (History.map_limit page mod page = 0))
    [512; 1024; 2048; 4096; 8192; 16384; 32768; 65536];
  List.iter (fun page ->
    expect "page budget rejects shape" (try ignore (History.map_limit page); false
      with Invalid_argument _ -> true)) [0; 1; 511; 513; 65537]

let test_control_space () =
  Test_workspace.with_dir "compute_control_space" (fun path ->
    let number value =
      let bytes = Bytes.make 32 '\000' in
      Bytes.set_int64_be bytes 24 (Int64.of_int value);
      Bytes.to_string bytes in
    let caller index = Printf.sprintf "caller-%d" (index / History.caller_notices) in
    let session index = number index in
    let db = open_db path in
    for index = 0 to History.max_notices - 1 do
      expect "control space session" (History.ensure db ~caller:(caller index)
        ~session:(session index) = Ok true)
    done;
    let owner = "worker" in
    let active = number 1001 in
    let request = number 1 in
    let planned = number 2 in
    let issued = number 3 in
    expect "control space active session" (History.reserve db ~caller:owner ~session:active
      ~sequence:1L ~request = Ok true);
    expect "control space earlier session" (History.ensure db ~caller:"prior"
      ~session:active = Ok true);
    List.iter (fun id ->
      expect "control space active plan" (History.plan_job db ~caller:owner ~session:active
        ~id ~body:"plan" = Ok true)) [planned; issued];
    expect "control space active issue" (History.advance_job db ~caller:owner ~session:active
      ~id:issued ~before:(Planned "plan") ~after:(Issued "call") = Ok true);
    expect "control space coordinator" (History.ensure db ~caller:"coord" ~session:active = Ok true);
    expect "control space coordinator plan" (History.plan_job db ~caller:"coord" ~session:active
      ~id:issued ~body:"plan" = Ok true);
    let coord_scope = History.{sequence = 1L; request; body = "original keys"} in
    expect "control space coordinator allocation" (History.advance_job db ~caller:"coord"
      ~session:active ~id:issued ~before:(Planned "plan") ~after:(Issued "call")
      ~scope:coord_scope ~room:131072 = Ok true);
    expect "control space relay session" (History.ensure db ~caller:"relay"
      ~session:active = Ok true);
    let payload = String.make History.max_context_bytes 'p' in
    let rec fill sequence =
      if sequence > 64L then failwith "control reserve consumed by work";
      match History.reserve db ~caller:(caller 0) ~session:(session 0) ~sequence
          ~request:(number 1) ~scope:payload with
      | Ok true -> fill (Int64.succ sequence)
      | Error "Invalid_argument(\"compute history work space full\")" -> sequence
      | Error reason -> failwith ("control space wrong refusal: " ^ reason)
      | Ok false -> failwith "control space sequence" in
    let refused = fill 1L in
    expect "control space work tested" (refused > 1L);
    let body = String.make History.max_notice_bytes 'n' in
    for index = 0 to History.max_notices - 1 do
      expect "control space closure" (History.queue_cancel db ~epoch:10L ~expires:74L
        ~caller:(caller index) ~session:(session index) ~body = Ok true);
      for round = 1 to 2 do
        expect "control space retry" (match History.claim_notice
          ~target:(caller index, session index) db ~epoch:10L with
          | Ok (Some notice) -> notice.round = round
          | _ -> false)
      done
    done;
    History.close db;
    let env = Lmdb.Env.create Lmdb.Rw ~max_maps:4 ~map_size:History.work_limit
      ~flags:Lmdb.Env.Flags.no_tls (Filename.concat path "compute_history") in
    Lmdb.Env.set_map_size env (History.map_limit (Lmdb.Env.stat env).psize);
    let peer index =
      let text = Printf.sprintf "peer-%d" index in
      text ^ String.make (128 - String.length text) 'x' in
    let receipt = String.make History.max_notice_bytes 'r' in
    Fun.protect ~finally:(fun () -> Lmdb.Env.close env) (fun () ->
      let jobs = Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
        ~value:Lmdb.Conv.string ~name:"jobs" env in
      let rows = Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
        ~value:Lmdb.Conv.string ~name:"sessions" env in
      Fun.protect ~finally:(fun () ->
        Octra_core.Lmdb_handle.close rows;
        Octra_core.Lmdb_handle.close jobs) (fun () ->
        expect "control space filled" (Lmdb.Txn.go Lmdb.Rw env (fun txn ->
          let integer value =
            let bytes = Bytes.create 8 in
            Bytes.set_int64_be bytes 0 value;
            Bytes.to_string bytes in
          Lmdb.Map.set rows ~txn (integer 5L ^ "prior" ^ active) (integer 1L ^ request);
          for index = 0 to History.max_notices - 1 do
            let owner = caller index in
            let size = Bytes.create 8 in
            Bytes.set_int64_be size 0 (Int64.of_int (String.length owner));
            let prefix = "\007" ^ Bytes.to_string size ^ owner ^ session index in
            for node = 0 to History.max_cancel_acks - 1 do
              if index <> History.max_notices - 1 || node <> History.max_cancel_acks - 1 then
                Lmdb.Map.set jobs ~txn (prefix ^ peer node) receipt
            done
          done;
          let stat = Lmdb.Map.stat jobs ~txn in
          let bytes = (stat.branch_pages + stat.leaf_pages + stat.overflow_pages) * stat.psize in
          expect "control space exceeds work ceiling" (bytes > History.work_limit);
          Printf.printf "event = compute_space live = %d work_limit = %d map_limit = %d\n%!"
            bytes History.work_limit (Lmdb.Env.info env).map_size) = Some ())));
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      let index = History.max_notices - 1 in
      let refused name result = expect name
        (result = Error "Invalid_argument(\"compute history work space full\")") in
      refused "control space blocks session" (History.ensure db ~caller:"new" ~session:active);
      refused "control space blocks unknown cancel" (History.cancel db ~caller:"new" ~session:active);
      expect "control space unknown cancel atomic" (History.cancelled db
        ~caller:"new" ~session:active = Ok false);
      refused "control space blocks reserve" (History.reserve db ~caller:owner ~session:active
        ~sequence:2L ~request ~scope:"next");
      refused "control space earlier result refused" (History.finish db ~caller:"prior" ~session:active
        ~sequence:1L ~request ~reply:"result");
      expect "control space earlier result atomic" (History.saved db ~caller:"prior" ~session:active
        ~sequence:1L ~request = Ok None);
      expect "control space reserved result" (History.finish db ~caller:owner ~session:active
        ~sequence:1L ~request ~reply:"result" = Ok true);
      refused "control space blocks plan" (History.plan_job db ~caller:owner ~session:active
        ~id:(number 4) ~body:"plan");
      refused "control space blocks issue" (History.advance_job db ~caller:owner ~session:active
        ~id:planned ~before:(Planned "plan") ~after:(Issued "call"));
      refused "control space blocks fetch" (History.claim_fetch db ~caller:owner ~session:active
        ~id:issued ~limit:2);
      refused "control space blocks save" (History.save_job db ~caller:owner ~session:active
        ~id:issued ~body:"done");
      refused "control space blocks direct save" (History.save_job db ~caller:owner ~session:active
        ~id:(number 4) ~body:"done");
      expect "control space reads stay available" (History.work db ~caller:owner ~id:planned =
        Ok (Some (Planned "plan")) && History.work db ~caller:owner ~id:issued =
        Ok (Some (Issued "call")));
      expect "control space saved result" (History.saved db ~caller:owner ~session:active
        ~sequence:1L ~request = Ok (Some History.{reply = "result"; context = None}));
      expect "control space coordinator fetch" (History.claim_fetch db ~caller:"coord"
        ~session:active ~id:issued ~limit:8 = Ok (Some 0));
      let votes = String.make 65536 'v' in
      expect "control space coordinator votes" (History.advance_job db ~caller:"coord"
        ~session:active ~id:issued ~before:(Issued "call") ~after:(Issued votes) = Ok true);
      expect "control space coordinator excess" (Result.is_error (History.advance_job db
        ~caller:"coord" ~session:active ~id:issued ~before:(Issued votes)
        ~after:(Issued (String.make 131073 'x'))));
      expect "control space coordinator atomic" (History.work db ~caller:"coord" ~id:issued =
        Ok (Some (Issued votes)));
      expect "control space coordinator complete" (History.save_job db ~caller:"coord"
        ~session:active ~id:issued ~body:votes = Ok true);
      expect "control space coordinator final" (History.load_job db ~caller:"coord" ~id:issued =
        Ok (Some votes) && History.work db ~caller:"coord" ~id:issued = Ok None);
      expect "control space coordinator keys" (History.scope db ~caller:"coord" ~session:active =
        Ok (Some coord_scope));
      expect "control space failed job atomic" (History.work db ~caller:owner ~id:(number 4) = Ok None
        && History.load_job db ~caller:owner ~id:(number 4) = Ok None);
      expect "control space relay" (History.relay_cancel db ~epoch:10L ~expires:74L
        ~round:2 ~origin:"peer" ~id:request ~caller:"relay" ~session:active = Ok true);
      expect "control space last receipt" (History.confirm_cancel db ~epoch:10L ~expires:74L
        ~round:2 ~caller:(caller index) ~session:(session index) ~cancel:body
        ~node:(peer (History.max_cancel_acks - 1)) ~body:receipt = Ok true);
      expect "control space receipts retained" (match History.cancel_acks db ~epoch:10L
        ~caller:(caller index) ~session:(session index) with
        | Ok (Some (_, receipts)) -> List.length receipts = History.max_cancel_acks
        | _ -> false);
      expect "control space expires" (History.pending_notices db ~epoch:75L = Ok []);
      expect "control space failed fetch atomic" (History.claim_fetch db ~caller:owner
        ~session:active ~id:issued ~limit:2 = Ok (Some 0));
      expect "control space closure permanent" (History.cancelled db
        ~caller:(caller index) ~session:(session index) = Ok true);
      expect "control space reusable" (History.queue_cancel db ~epoch:75L ~expires:139L
        ~caller:(caller index) ~session:(session index) ~body = Ok true)))

let test_scope_upgrade () =
  Test_workspace.with_dir "compute_scope_upgrade" (fun path ->
    let session = String.make 32 's' in
    let number value =
      let bytes = Bytes.create 8 in
      Bytes.set_int64_be bytes 0 value;
      Bytes.to_string bytes in
    let key = "\009" ^ number 5L ^ "alice" ^ session ^ number 1L in
    let db = open_db path in
    expect "scope upgrade setup" (reserve ~scope:"keys" db = Ok true);
    History.close db;
    let env = Lmdb.Env.create Lmdb.Rw ~max_maps:4 ~map_size:(64 * 1024 * 1024)
      ~flags:Lmdb.Env.Flags.no_tls (Filename.concat path "compute_history") in
    Fun.protect ~finally:(fun () -> Lmdb.Env.close env) (fun () ->
      let jobs = Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
        ~value:Lmdb.Conv.string ~name:"jobs" env in
      Fun.protect ~finally:(fun () -> Octra_core.Lmdb_handle.close jobs) (fun () ->
        expect "scope upgrade archive removed" (Lmdb.Txn.go Lmdb.Rw env
          (fun txn -> Lmdb.Map.remove jobs ~txn key) = Some ())));
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      let first = History.{sequence = 1L; request = String.make 32 'r'; body = "keys"} in
      let read () = History.find_scope db ~caller:"alice" ~session
        (fun value -> value.sequence = 1L) in
      expect "scope upgrade latest readable" (read () = Ok (Some first));
      expect "scope upgrade next" (reserve ~scope:"new keys" ~sequence:2L db = Ok true);
      expect "scope upgrade preserves prior" (read () = Ok (Some first))))

let test_issue_scope () =
  Test_workspace.with_dir "compute_issue_scope" (fun path ->
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      let session = String.make 32 's' in
      let id = String.make 32 'j' in
      let scope = History.{sequence = 1L; request = String.make 32 'r'; body = "keys"} in
      let issue scope before = History.advance_job ~scope db ~caller:"alice" ~session ~id
        ~before ~after:(Issued "issued") in
      expect "scope issue session" (History.ensure db ~caller:"alice" ~session = Ok true);
      expect "scope issue plan" (History.plan_job db ~caller:"alice" ~session ~id ~body:"plan" = Ok true);
      expect "scope wrong transition" (issue scope (Planned "other") = Ok false);
      expect "scope wrong transition atomic" (History.scope db ~caller:"alice" ~session = Ok None);
      expect "scope issue commit" (issue scope (Planned "plan") = Ok true);
      expect "scope issue stored" (History.scope db ~caller:"alice" ~session = Ok (Some scope));
      expect "scope issue repeated" (issue scope (Planned "plan") = Ok true);
      expect "scope issue replacement refused" (Result.is_error
        (issue {scope with body = "changed"} (Planned "plan")));
      expect "scope issue unchanged" (History.scope db ~caller:"alice" ~session = Ok (Some scope));
      for _ = 1 to 2 do
        expect "scope refused reserve atomic" (Result.is_error
          (reserve ~scope:scope.body ~sequence:0L db))
      done;
      expect "scope reserve after refusal" (reserve ~scope:scope.body db = Ok true)))

let test_restart () =
  Test_workspace.with_dir "compute_restart" (fun path ->
    let db = open_db path in
    expect "first sequence" (reserve db = Ok true);
    expect "same sequence" (reserve db = Ok false);
    History.close db;
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      expect "restart repeat" (reserve db = Ok false);
      expect "older sequence" (reserve ~sequence:0L db = Ok false);
      expect "next sequence" (reserve ~sequence:2L db = Ok true);
      expect "other caller" (reserve ~caller:"bob" db = Ok true);
      expect "other session" (reserve ~session:'t' db = Ok true);
      expect "maximum sequence" (reserve ~sequence:Int64.max_int db = Ok true);
      expect "no wrap" (Result.is_error (reserve ~sequence:Int64.min_int db))))

let test_capacity () =
  Test_workspace.with_dir "compute_capacity" (fun path ->
    let db = open_db ~max_sessions:2 ~caller_limit:1 path in
    expect "first caller" (reserve db = Ok true);
    expect "caller capacity" (Result.is_error (reserve ~session:'t' db));
    expect "refused count atomic" (reserve ~caller:"bob" ~session:'t' db = Ok true);
    expect "global capacity" (Result.is_error (reserve ~caller:"carol" db));
    expect "known sequence progresses" (reserve ~sequence:2L db = Ok true);
    History.close db;
    let db = open_db ~max_sessions:2 ~caller_limit:1 path in
    expect "capacity restart" (Result.is_error (reserve ~caller:"carol" db));
    expect "capacity keeps history" (reserve db = Ok false);
    History.close db;
    expect "closed write" (Result.is_error (reserve ~sequence:3L db));
    let db = open_db path in
    expect "failed write not consumed" (reserve ~sequence:3L db = Ok true);
    History.close db)

let test_identity () =
  Test_workspace.with_dir "compute_identity" (fun path ->
    let db = open_db path in
    expect "identity write" (reserve db = Ok true);
    History.close db;
    let refused open_store =
      try let db = open_store () in History.close db; false with _ -> true in
    expect "different chain" (refused (fun () -> open_db ~chain_id:"other" path));
    expect "different node" (refused (fun () -> open_db ~node_id:"other" path));
    let db = open_db path in
    expect "identity retained" (reserve db = Ok false);
    History.close db)

let test_cancel () =
  Test_workspace.with_dir "compute_cancel" (fun path ->
    let db = open_db ~max_sessions:2 ~caller_limit:1 path in
    let ensure caller = History.ensure db ~caller ~session:(String.make 32 's') in
    let cancel caller = History.cancel db ~caller ~session:(String.make 32 's') in
    let cancelled caller = History.cancelled db ~caller ~session:(String.make 32 's') in
    expect "unknown session open" (cancelled "alice" = Ok false);
    expect "session reservation" (ensure "alice" = Ok true);
    expect "reserved first sequence" (reserve db = Ok true);
    expect "reserved session open" (cancelled "alice" = Ok false);
    expect "session cancelled" (cancel "alice" = Ok true);
    expect "closed session query" (cancelled "alice" = Ok true);
    expect "cancel repeat" (cancel "alice" = Ok true);
    expect "cancel cannot reopen" (ensure "alice" = Ok false);
    expect "cancel stops later sequence" (reserve ~sequence:2L db = Ok false);
    expect "independent session" (ensure "bob" = Ok true);
    expect "cancel has reserved capacity" (cancel "bob" = Ok true);
    expect "unknown cancel capacity" (Result.is_error (cancel "carol"));
    expect "query at capacity" (cancelled "carol" = Ok false);
    expect "cancel retained at capacity" (reserve db = Ok false);
    History.close db;
    let db = open_db path in
    expect "cancel restart" (reserve ~sequence:Int64.max_int db = Ok false);
    expect "cancel caller isolation" (reserve ~caller:"bob" db = Ok false);
    expect "cancel session isolation" (reserve ~session:'t' db = Ok true);
    expect "query after restart"
      (History.cancelled db ~caller:"alice" ~session:(String.make 32 's') = Ok true);
    History.close db;
    expect "closed query refuses"
      (Result.is_error (History.cancelled db ~caller:"alice" ~session:(String.make 32 's'))))

let test_notices () =
  Test_workspace.with_dir "compute_notices" (fun path ->
    let db = open_db path in
    let session = String.make 32 's' in
    let queue db epoch body = History.queue_cancel db ~epoch ~expires:64L
      ~caller:"alice" ~session ~body in
    expect "notice queued" (queue db 0L "signed" = Ok true);
    expect "notice and cancel atomic" (reserve db = Ok false);
    expect "changed notice refused" (Result.is_error (queue db 0L "other"));
    expect "cancel still present"
      (History.cancelled db ~caller:"alice" ~session = Ok true);
    begin match History.claim_notice db ~epoch:0L with
    | Ok (Some row) -> expect "first notice" (row.round = 1 && row.body = "signed")
    | _ -> failwith "missing first notice"
    end;
    expect "same notice queued" (queue db 1L "signed" = Ok true);
    History.close db;
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      begin match History.claim_notice db ~epoch:1L with
      | Ok (Some row) -> expect "notice round survives restart" (row.round = 2)
      | _ -> failwith "missing saved notice"
      end;
      for round = 3 to History.notice_rounds do
        begin match History.claim_notice db ~epoch:1L with
        | Ok (Some row) -> expect "notice budget" (row.round = round)
        | _ -> failwith "notice budget ended early"
        end
      done;
      expect "notice budget exhausted" (History.claim_notice db ~epoch:1L = Ok None);
      expect "duplicate cannot reset budget" (queue db 2L "signed" = Ok true);
      expect "notice still exhausted" (History.claim_notice db ~epoch:2L = Ok None);
      begin match History.pending_notices db ~epoch:64L with
      | Ok [row] -> expect "notice original expiry" (row.expires = 64L)
      | _ -> failwith "notice exact expiry changed"
      end;
      expect "notice expired" (History.pending_notices db ~epoch:65L = Ok []);
      expect "expiry keeps cancellation" (reserve ~sequence:2L db = Ok false);
      expect "invalid notice refused" (Result.is_error (queue db 65L "signed"));
      expect "invalid notice no send" (History.claim_notice db ~epoch:65L = Ok None)))

let test_notice_capacity () =
  Test_workspace.with_dir "compute_notice_capacity" (fun path ->
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      let session index = Digestif.SHA256.(to_raw_string (digest_string (string_of_int index))) in
      let queue caller index = History.queue_cancel db ~epoch:0L ~expires:64L
        ~caller ~session:(session index) ~body:"signed" in
      for index = 0 to History.max_notices - 1 do
        let caller = "caller-" ^ string_of_int (index / History.caller_notices) in
        expect "notice capacity" (queue caller index = Ok true);
        if index = History.caller_notices - 1 then
          expect "notice isolated caller quota" (Result.is_error (queue caller 256))
      done;
      expect "notice caller quota" (Result.is_error (queue "caller-0" 256));
      expect "notice global quota" (Result.is_error (queue "new" 257));
      expect "queue refusal did not cancel session"
        (History.cancelled db ~caller:"new" ~session:(session 257) = Ok false);
      expect "queue refusal did not reserve session"
        (History.ensure db ~caller:"new" ~session:(session 257) = Ok true);
      for _ = 1 to History.max_notices do
        match History.claim_notice db ~epoch:0L with
        | Ok (Some row) -> expect "notice fair first pass" (row.round = 1)
        | _ -> failwith "notice first pass incomplete"
      done;
      expect "expiry frees notice capacity" (History.pending_notices db ~epoch:65L = Ok []);
      expect "expired notices keep session closed"
        (History.cancelled db ~caller:"caller-0" ~session:(session 0) = Ok true)))

let test_cancel_acks () =
  Test_workspace.with_dir "compute_cancel_acks" (fun path ->
    let db = open_db path in
    let session = String.make 32 's' in
    let read db epoch = History.cancel_acks db ~epoch ~caller:"alice" ~session in
    let save db ?(epoch = 1L) ?(expires = 64L) ?(round = 2) ?(cancel = "signed") node body =
      History.confirm_cancel db ~epoch ~expires ~round ~caller:"alice" ~session
        ~cancel ~node ~body in
    expect "ack cannot invent notice" (save db "peer" "receipt" = Ok false);
    expect "ack notice" (History.queue_cancel db ~epoch:0L ~expires:64L
      ~caller:"alice" ~session ~body:"signed" = Ok true);
    expect "ack requires sent round" (save db "peer" "receipt" = Ok false);
    for _ = 1 to 2 do ignore (History.claim_notice db ~epoch:0L) done;
    expect "ack wrong message" (save db ~cancel:"other" "peer" "receipt" = Ok false);
    expect "ack wrong expiry" (save db ~expires:63L "peer" "receipt" = Ok false);
    expect "ack wrong round" (save db ~round:3 "peer" "receipt" = Ok false);
    expect "ack saved" (save db "peer" "receipt" = Ok true);
    expect "ack immutable" (save db "peer" "changed" = Ok false);
    History.close db;
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      expect "ack restart" (match read db 1L with
        | Ok (Some (_, ["receipt"])) -> true | _ -> false);
      for index = 1 to History.max_cancel_acks - 1 do
        expect "ack capacity" (save db ("peer-" ^ string_of_int index) "receipt" = Ok true)
      done;
      expect "ack capacity refusal" (Result.is_error (save db "excess" "receipt"));
      expect "ack exact deadline" (match read db 64L with
        | Ok (Some (_, rows)) -> List.length rows = History.max_cancel_acks | _ -> false);
      expect "ack expiry" (read db 65L = Ok None);
      expect "ack expiry keeps closure" (reserve ~sequence:2L db = Ok false);
      expect "ack new window" (History.queue_cancel db ~epoch:65L ~expires:129L
        ~caller:"alice" ~session ~body:"signed" = Ok true);
      expect "ack old window cleared" (match read db 65L with
        | Ok (Some (_, [])) -> true | _ -> false)))

let test_cancel_relay () =
  Test_workspace.with_dir "compute_cancel_relay" (fun path ->
    let db = open_db path in
    let session = String.make 32 's' in
    let put db ?(expires = 64L) round = History.relay_cancel db
      ~epoch:1L ~expires ~round ~origin:"relay" ~id:session ~caller:"alice" ~session in
    expect "relay saved" (put db 2 = Ok true);
    expect "relay closes session" (reserve db = Ok false);
    expect "relay duplicate refused" (Result.is_error (put db 2));
    expect "relay older refused" (Result.is_error (put db 1));
    expect "relay deadline fixed" (Result.is_error (put db ~expires:65L 3));
    History.close db;
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      expect "relay restart duplicate refused" (Result.is_error (put db 2));
      expect "relay restart next accepted" (put db 3 = Ok true);
      expect "relay round limit" (Result.is_error (put db (History.notice_rounds + 1)));
      expect "relay final round" (put db History.notice_rounds = Ok true);
      expect "relay final duplicate" (Result.is_error (put db History.notice_rounds));
      expect "relay expired" (Result.is_error (History.relay_cancel db
        ~epoch:65L ~expires:64L ~round:1 ~origin:"relay" ~id:session ~caller:"alice" ~session));
      expect "relay expiry keeps closure" (reserve ~sequence:2L db = Ok false)))

let test_relay_capacity () =
  Test_workspace.with_dir "compute_relay_capacity" (fun path ->
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      let session index = Digestif.SHA256.(to_raw_string (digest_string (string_of_int index))) in
      let put origin index epoch expires = History.relay_cancel db ~epoch ~expires ~round:1
        ~origin ~id:(session index) ~caller:"alice" ~session:(session index) in
      for index = 0 to History.max_notices - 1 do
        let origin = "relay-" ^ string_of_int (index / History.caller_notices) in
        expect "relay capacity" (put origin index 0L 64L = Ok true);
        if index = History.caller_notices - 1 then
          expect "relay isolated origin limit" (Result.is_error (put origin 256 0L 64L))
      done;
      expect "relay full" (Result.is_error (put "other" 257 0L 64L));
      expect "relay capacity refusal atomic"
        (History.cancelled db ~caller:"alice" ~session:(session 257) = Ok false);
      expect "relay expiry releases slots" (put "other" 257 65L 129L = Ok true);
      expect "relay expiry keeps old closure"
        (History.cancelled db ~caller:"alice" ~session:(session 0) = Ok true)))

let test_reply () =
  Test_workspace.with_dir "compute_reply" (fun path ->
    let db = open_db path in
    let read db sequence request = History.reply db ~caller:"alice"
      ~session:(String.make 32 's') ~sequence ~request in
    let finish db sequence request reply = History.finish db ~caller:"alice"
      ~session:(String.make 32 's') ~sequence ~request ~reply in
    let id = String.make 32 'r' in
    let other = String.make 32 'q' in
    expect "reply needs reservation" (finish db 1L id "signed" = Ok false);
    expect "reply not guessed" (read db 1L id = Ok None);
    expect "reply reservation" (reserve db = Ok true);
    expect "reply sequence differs" (finish db 2L id "signed" = Ok false);
    expect "reply request differs" (finish db 1L other "signed" = Ok false);
    expect "reply cap" (Result.is_error
      (finish db 1L id (String.make (History.max_reply_bytes + 1) 'x')));
    expect "reply saved" (finish db 1L id "signed" = Ok true);
    expect "reply idempotent" (finish db 1L id "signed" = Ok true);
    expect "reply conflict" (finish db 1L id "different" = Ok false);
    expect "reply retained" (read db 1L id = Ok (Some "signed"));
    expect "reply not another request" (read db 1L other = Ok None);
    History.close db;
    let db = open_db path in
    expect "reply restart" (read db 1L id = Ok (Some "signed"));
    expect "reply prevents execution" (reserve db = Ok false);
    expect "next signed sequence" (reserve ~sequence:2L db = Ok true);
    expect "old reply replaced" (read db 1L id = Ok None);
    expect "old completion refused" (finish db 1L id "late" = Ok false);
    expect "next reply not guessed" (read db 2L id = Ok None);
    expect "new reply saved" (finish db 2L id "next" = Ok true);
    expect "old completion preserved next" (finish db 1L id "late" = Ok false);
    expect "next reply unchanged" (read db 2L id = Ok (Some "next"));
    expect "cancel reply" (History.cancel db ~caller:"alice"
      ~session:(String.make 32 's') = Ok true);
    expect "cancel removes reply" (read db 2L id = Ok None);
    expect "cancel refuses late reply" (finish db 2L id "late" = Ok false);
    History.close db;
    expect "reply closed store" (Result.is_error (read db 2L id));
    expect "finish closed store" (Result.is_error (finish db 2L id "late")))

let test_reply_space () =
  Test_workspace.with_dir "compute_reply_space" (fun path ->
    let caller = "alice" in
    let session = String.make 32 's' in
    let request = String.make 32 'r' in
    let snapshot ?replace () =
      let env = Lmdb.Env.create Lmdb.Rw ~max_maps:4 ~map_size:History.work_limit
        ~flags:Lmdb.Env.Flags.no_tls (Filename.concat path "compute_history") in
      Fun.protect ~finally:(fun () -> Lmdb.Env.close env) (fun () ->
        let rows = Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
          ~value:Lmdb.Conv.string ~name:"sessions" env in
        Fun.protect ~finally:(fun () -> Octra_core.Lmdb_handle.close rows) (fun () ->
          let size = Bytes.create 8 in
          Bytes.set_int64_be size 0 (Int64.of_int (String.length caller));
          let key = Bytes.to_string size ^ caller ^ session in
          match Lmdb.Txn.go Lmdb.Rw env (fun txn ->
            Option.iter (Lmdb.Map.set rows ~txn key) replace;
            let stat = Lmdb.Map.stat rows ~txn in
            let pages = stat.branch_pages + stat.leaf_pages + stat.overflow_pages in
            Lmdb.Map.get rows ~txn key, pages * stat.psize, stat.psize) with
          | Some value -> value
          | None -> failwith "reply space inspection aborted")) in
    let db = open_db path in
    List.iter (fun room ->
      expect "reply space invalid size" (Result.is_error
        (History.reserve ~room db ~caller ~session ~sequence:1L ~request)))
      [0; -1; History.max_reply_bytes + History.max_context_bytes + 1];
    expect "reply space allocated" (History.reserve ~room:131072 db ~caller ~session
      ~sequence:1L ~request = Ok true);
    expect "reply space is not result" (History.reply db ~caller ~session
      ~sequence:1L ~request = Ok None);
    History.close db;
    let held, before, page = snapshot () in
    expect "reply space stored padding" (held.[0] = '\004' && String.length held = 131129);
    let changed = Bytes.of_string held in
    Bytes.set changed (Bytes.length changed - 1) 'x';
    ignore (snapshot ~replace:(Bytes.to_string changed) ());
    let db = open_db path in
    expect "reply space damaged padding refused" (Result.is_error
      (History.reply db ~caller ~session ~sequence:1L ~request));
    History.close db;
    ignore (snapshot ~replace:held ());
    let db = open_db path in
    expect "reply space oversized result" (Result.is_error (History.finish db ~caller ~session
      ~sequence:1L ~request ~reply:(String.make 131073 'r')));
    expect "reply space refusal atomic" (History.reply db ~caller ~session
      ~sequence:1L ~request = Ok None);
    let payload = String.make 65536 'r' in
    let context = String.make 65536 'k' in
    expect "reply space exact capacity" (History.finish ~context db ~caller ~session
      ~sequence:1L ~request ~reply:payload = Ok true);
    History.close db;
    let stored, after, _ = snapshot () in
    expect "reply space no page growth" (stored.[0] = '\005' && after <= before);
    let db = open_db path in
    expect "reply space restart" (History.saved db ~caller ~session ~sequence:1L ~request =
      Ok (Some History.{reply = payload; context = Some context}));
    expect "reply space repeat" (History.finish ~context db ~caller ~session
      ~sequence:1L ~request ~reply:payload = Ok true);
    expect "reply space conflict" (History.finish ~context db ~caller ~session
      ~sequence:1L ~request ~reply:"other" = Ok false);
    History.close db;
    let old = Bytes.create 8 in
    Bytes.set_int64_be old 0 2L;
    ignore (snapshot ~replace:(Bytes.to_string old ^ request) ());
    let db = open_db path in
    expect "reply space old row readable" (History.finish db ~caller ~session
      ~sequence:2L ~request ~reply:"old" = Ok true);
    History.close db;
    let stored, _, _ = snapshot () in
    expect "reply space old encoding retained" (stored.[0] = '\002');
    let db = open_db path in
    expect "reply space new sequence" (History.reserve ~room:(2 * page) db ~caller ~session
      ~sequence:3L ~request = Ok true);
    History.close db;
    let held, before, _ = snapshot () in
    expect "reply space new allocation" (held.[0] = '\004');
    let db = open_db path in
    expect "reply space compact result" (History.finish db ~caller ~session
      ~sequence:3L ~request ~reply:"small" = Ok true);
    History.close db;
    let stored, after, _ = snapshot () in
    Printf.printf "event = reply_space page = %d before = %d after = %d held = %d stored = %d\n%!"
      page before after (String.length held) (String.length stored);
    expect "reply space releases pages" (stored.[0] = '\005' && after < before);
    List.iteri (fun index (count, keys) ->
      let sequence = Int64.of_int (index + 4) in
      let payload = String.make count 'r' in
      let context = if keys = 0 then None else Some (String.make keys 'k') in
      let db = open_db path in
      expect "reply page allocation" (History.reserve ~room:(count + keys)
        db ~caller ~session ~sequence ~request = Ok true);
      History.close db;
      let _, before, _ = snapshot () in
      let db = open_db path in
      expect "reply page completion" (History.finish ?context db ~caller ~session
        ~sequence ~request ~reply:payload = Ok true);
      expect "reply page value" (History.saved db ~caller ~session ~sequence ~request =
        Ok (Some History.{reply = payload; context}));
      History.close db;
      let _, after, _ = snapshot () in
      expect "reply page count" (after <= before))
      [1, 0; page - 1, 0; page, 1; page + 1, page; 65536, 65536;
       History.max_reply_bytes, History.max_context_bytes])

let test_context () =
  Test_workspace.with_dir "compute_context" (fun path ->
    let db = open_db path in
    let session = String.make 32 's' in
    let request = String.make 32 'r' in
    let read db = History.saved db ~caller:"alice" ~session ~sequence:1L ~request in
    let finish context = History.finish ~context db ~caller:"alice" ~session
      ~sequence:1L ~request ~reply:"signed" in
    expect "context needs reservation" (finish "key" = Ok false);
    expect "context reserve" (reserve db = Ok true);
    expect "context empty" (Result.is_error (finish ""));
    expect "context excessive" (Result.is_error
      (finish (String.make (History.max_context_bytes + 1) 'x')));
    expect "context saved atomically" (finish "key" = Ok true);
    expect "context repeat" (finish "key" = Ok true);
    expect "context cannot change" (finish "other" = Ok false);
    expect "context cannot disappear"
      (History.finish db ~caller:"alice" ~session ~sequence:1L ~request
        ~reply:"signed" = Ok false);
    expect "context and reply" (read db = Ok (Some History.{reply = "signed"; context = Some "key"}));
    expect "context old read" (History.reply db ~caller:"alice" ~session
      ~sequence:1L ~request = Ok (Some "signed"));
    History.close db;
    let db = open_db path in
    expect "context restart" (read db = Ok (Some History.{reply = "signed"; context = Some "key"}));
    expect "context different caller" (History.saved db ~caller:"bob" ~session
      ~sequence:1L ~request = Ok None);
    expect "context new sequence" (reserve ~sequence:2L db = Ok true);
    expect "context old sequence cleared" (read db = Ok None);
    expect "context new result" (History.finish ~context:"next" db ~caller:"alice"
      ~session ~sequence:2L ~request ~reply:"second" = Ok true);
    expect "context cancellation" (History.cancel db ~caller:"alice" ~session = Ok true);
    expect "context cancellation clears" (History.saved db ~caller:"alice" ~session
      ~sequence:2L ~request = Ok None);
    History.close db)

let test_job () =
  Test_workspace.with_dir "compute_job" (fun path ->
    let db = open_db ~max_sessions:2 ~caller_limit:1 path in
    let id = String.make 32 'j' in
    let session = String.make 32 's' in
    let save caller id body = History.save_job db ~caller ~session ~id ~body in
    expect "job needs session" (save "alice" id "result" = Ok false);
    expect "job session" (History.ensure db ~caller:"alice" ~session = Ok true);
    expect "job saved" (save "alice" id "result" = Ok true);
    expect "job repeat" (save "alice" id "result" = Ok true);
    expect "job replacement refused" (save "alice" id "other" = Ok false);
    expect "job size refused" (Result.is_error
      (save "alice" id (String.make (History.max_job_bytes + 1) 'x')));
    expect "job caller isolation" (History.load_job db ~caller:"bob" ~id = Ok None);
    expect "job caller quota" (Result.is_error (save "alice" (String.make 32 'k') "next"));
    expect "job other session" (History.ensure db ~caller:"bob" ~session = Ok true);
    expect "job quota atomic" (save "bob" id "second" = Ok true);
    expect "job cancel" (History.cancel db ~caller:"alice" ~session = Ok true);
    expect "completed job survives cancel" (History.load_job db ~caller:"alice" ~id = Ok (Some "result"));
    expect "cancel prevents completion" (save "alice" (String.make 32 'm') "late" = Ok false);
    History.close db;
    let db = open_db ~max_sessions:2 ~caller_limit:2 path in
    expect "job restart" (History.load_job db ~caller:"alice" ~id = Ok (Some "result"));
    expect "job independent restart" (History.load_job db ~caller:"bob" ~id = Ok (Some "second"));
    expect "job total quota" (Result.is_error (History.save_job db ~caller:"bob" ~session
      ~id:(String.make 32 'n') ~body:"third"));
    History.close db;
    expect "job closed read" (Result.is_error (History.load_job db ~caller:"alice" ~id));
    expect "job closed write" (Result.is_error
      (History.save_job db ~caller:"alice" ~session ~id ~body:"result")))

let test_work () =
  Test_workspace.with_dir "compute_work" (fun path ->
    let id = String.make 32 'j' in
    let next = String.make 32 'k' in
    let session = String.make 32 's' in
    let db = open_db ~max_sessions:2 ~caller_limit:1 path in
    let plan caller id body = History.plan_job db ~caller ~session ~id ~body in
    let advance before after = History.advance_job db ~caller:"alice" ~session ~id
      ~before ~after in
    expect "work creates session" (plan "alice" id "plan" = Ok true);
    expect "work session known" (History.known_job db ~caller:"alice" ~session = Ok true);
    expect "work saved" (plan "alice" id "plan" = Ok true);
    expect "work repeat" (plan "alice" id "plan" = Ok true);
    expect "work conflict" (plan "alice" id "other" = Ok false);
    expect "work private" (History.work db ~caller:"bob" ~id = Ok None);
    expect "work capacity" (Result.is_error (plan "alice" next "plan"));
    expect "work not completed" (History.load_job db ~caller:"alice" ~id = Ok None);
    expect "work cannot skip call" (History.save_job db ~caller:"alice" ~session ~id
      ~body:"done" = Ok false);
    expect "work invalid transition" (advance (History.Planned "other")
      (History.Issued "call") = Ok false);
    expect "work issued" (advance (History.Planned "plan") (History.Issued "call") = Ok true);
    expect "work issue repeat" (advance (History.Planned "plan") (History.Issued "call") = Ok true);
    expect "work cannot replace call" (advance (History.Planned "plan")
      (History.Issued "other") = Ok false);
    expect "work cannot rewind" (Result.is_error
      (advance (History.Issued "call") (History.Planned "plan")));
    expect "work vote" (advance (History.Issued "call") (History.Issued "vote") = Ok true);
    History.close db;
    let db = open_db ~max_sessions:2 ~caller_limit:1 path in
    expect "work restart" (History.work db ~caller:"alice" ~id = Ok (Some (History.Issued "vote")));
    expect "work completion" (History.save_job db ~caller:"alice" ~session ~id ~body:"done" = Ok true);
    expect "work promoted" (History.work db ~caller:"alice" ~id = Ok None);
    expect "work result" (History.load_job db ~caller:"alice" ~id = Ok (Some "done"));
    expect "work does not revive" (History.plan_job db ~caller:"alice" ~session ~id ~body:"plan" = Ok false);
    expect "work second session" (History.ensure db ~caller:"bob" ~session = Ok true);
    expect "work quota unchanged" (History.plan_job db ~caller:"bob" ~session ~id ~body:"plan" = Ok true);
    expect "work cancelled" (History.cancel db ~caller:"bob" ~session = Ok true);
    expect "work cancel stops issue" (History.advance_job db ~caller:"bob" ~session ~id
      ~before:(History.Planned "plan") ~after:(History.Issued "call") = Ok false);
    expect "work cancel stops completion" (History.save_job db ~caller:"bob" ~session ~id
      ~body:"done" = Ok false);
    History.close db;
    expect "work closed" (Result.is_error (History.work db ~caller:"alice" ~id)))

let test_funding_wait () =
  Test_workspace.with_dir "compute_funding_wait" (fun path ->
    let caller = "alice" and session = String.make 32 's' and id = String.make 32 'j' in
    let scope = History.{sequence = 1L; request = String.make 32 'r'; body = "scope"} in
    let db = open_db path in
    expect "funding session" (History.ensure db ~caller ~session = Ok true);
    expect "funding plan" (History.plan_job db ~caller ~session ~id ~body:"plan" = Ok true);
    History.close db;
    let child = Unix.fork () in
    if child = 0 then begin
      let db = open_db path in
      if History.wait_job ~scope ~room:4096 db ~caller ~session ~id
          ~before:"plan" ~body:"call" <> Ok true then Unix._exit 1;
      Unix._exit 73
    end;
    expect "funding child failed" (Test_workspace.wait child = Unix.WEXITED 73);
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      expect "funding restart stage" (History.work db ~caller ~id = Ok (Some (Funding "call")));
      expect "funding restart scope" (History.scope db ~caller ~session = Ok (Some scope));
      expect "funding repeat" (History.wait_job ~scope ~room:4096 db ~caller ~session ~id
        ~before:"plan" ~body:"call" = Ok true);
      expect "funding replaced scope" (Result.is_error (History.wait_job
        ~scope:{scope with sequence = 2L} ~room:4096 db ~caller ~session ~id
        ~before:"plan" ~body:"call"));
      expect "funding early fetch" (History.claim_fetch db ~caller ~session ~id ~limit:8 = Ok None);
      expect "funding early completion" (History.save_job db ~caller ~session ~id ~body:"done" = Ok false);
      expect "funding proof skipped" (Result.is_error (History.advance_job db ~caller ~session ~id
        ~before:(Funding "call") ~after:(Issued "call")));
      expect "funding old path skipped" (History.advance_job db ~caller ~session ~id
        ~before:(Planned "plan") ~after:(Issued "call") = Ok false);
      expect "funding cancel" (History.cancel db ~caller ~session = Ok true);
      expect "funding reopened after cancel" (History.wait_job ~scope ~room:4096 db
        ~caller ~session ~id ~before:"plan" ~body:"call" = Ok false)))

let test_job_space () =
  Test_workspace.with_dir "compute_job_space" (fun path ->
    let caller = "alice" in
    let session = String.make 32 's' in
    let id = String.make 32 'j' in
    let snapshot ?replace ?(id = id) () =
      let env = Lmdb.Env.create Lmdb.Rw ~max_maps:4 ~map_size:History.work_limit
        ~flags:Lmdb.Env.Flags.no_tls (Filename.concat path "compute_history") in
      Fun.protect ~finally:(fun () -> Lmdb.Env.close env) (fun () ->
        let jobs = Lmdb.Map.create Lmdb.Map.Nodup ~key:Lmdb.Conv.string
          ~value:Lmdb.Conv.string ~name:"jobs" env in
        Fun.protect ~finally:(fun () -> Octra_core.Lmdb_handle.close jobs) (fun () ->
          let bytes = Bytes.create 8 in
          Bytes.set_int64_be bytes 0 (Int64.of_int (String.length caller));
          let key = Bytes.to_string bytes ^ caller ^ id in
          match Lmdb.Txn.go Lmdb.Rw env (fun txn ->
            Option.iter (Lmdb.Map.set jobs ~txn ("\003" ^ key)) replace;
            let stat = Lmdb.Map.stat jobs ~txn in
            let pages = stat.branch_pages + stat.leaf_pages + stat.overflow_pages in
            let old = try ignore (Lmdb.Map.get jobs ~txn ("\002" ^ key)); true
              with Not_found -> false in
            Lmdb.Map.get jobs ~txn ("\003" ^ key), pages * stat.psize, stat.psize, old) with
          | Some value -> value
          | None -> failwith "job space inspection aborted")) in
    let db = open_db path in
    expect "job space session" (History.ensure db ~caller ~session = Ok true);
    expect "job space plan" (History.plan_job db ~caller ~session ~id ~body:"plan" = Ok true);
    History.close db;
    let _, _, page, _ = snapshot () in
    let room = 2 * page in
    let db = open_db path in
    let issue ?room () = History.advance_job ?room db ~caller ~session ~id
      ~before:(Planned "plan") ~after:(Issued "call") in
    List.iter (fun room -> expect "job space invalid size" (Result.is_error (issue ~room ())))
      [0; -1; History.max_job_bytes + 1];
    expect "job space allocated" (issue ~room () = Ok true);
    expect "job space not a result" (History.load_job db ~caller ~id = Ok None);
    expect "job space issue repeat" (issue ~room () = Ok true);
    expect "job space cannot resize" (Result.is_error (issue ~room:(room + page) ()));
    expect "job space first fetch" (History.claim_fetch db ~caller ~session ~id ~limit:2 = Ok (Some 0));
    History.close db;
    let held, before, _, old = snapshot () in
    expect "job space real allocation" (held.[32] = '\002' && String.length held = 41 + room && not old);
    let damaged = Bytes.of_string held in
    Bytes.set damaged (Bytes.length damaged - 1) 'x';
    ignore (snapshot ~replace:(Bytes.to_string damaged) ());
    let db = open_db path in
    expect "job space damaged padding" (Result.is_error (History.work db ~caller ~id));
    History.close db;
    ignore (snapshot ~replace:held ());
    let db = open_db path in
    expect "job space next fetch" (History.claim_fetch db ~caller ~session ~id ~limit:2 = Ok (Some 1));
    expect "job space fetch limit" (History.claim_fetch db ~caller ~session ~id ~limit:2 = Ok None);
    let votes = String.make room 'v' in
    expect "job space exact votes" (History.advance_job db ~caller ~session ~id
      ~before:(Issued "call") ~after:(Issued votes) = Ok true);
    expect "job space vote conflict" (History.advance_job db ~caller ~session ~id
      ~before:(Issued "other") ~after:(Issued "changed") = Ok false);
    expect "job space oversized final" (Result.is_error (History.save_job db ~caller ~session ~id
      ~body:(String.make (room + 1) 'x')));
    expect "job space failed final atomic" (History.work db ~caller ~id = Ok (Some (Issued votes)));
    History.close db;
    let row, after, _, _ = snapshot () in
    expect "job space votes retain pages" (String.length row = String.length held && after <= before);
    let db = open_db path in
    expect "job space restart votes" (History.work db ~caller ~id = Ok (Some (Issued votes)));
    expect "job space wrong session" (History.save_job db ~caller ~session:(String.make 32 't')
      ~id ~body:"final" = Ok false);
    expect "job space final" (History.save_job db ~caller ~session ~id ~body:"final" = Ok true);
    expect "job space repeat final" (History.save_job db ~caller ~session ~id ~body:"final" = Ok true);
    expect "job space final conflict" (History.save_job db ~caller ~session ~id ~body:"other" = Ok false);
    expect "job space final not work" (History.work db ~caller ~id = Ok None);
    expect "job space cannot reopen" (History.plan_job db ~caller ~session ~id ~body:"plan" = Ok false);
    expect "job space final cannot fetch" (History.claim_fetch db ~caller ~session ~id ~limit:3 = Ok None);
    History.close db;
    let done_, after, _, old = snapshot () in
    expect "job space same key completed" (done_.[32] = '\003' && not old && after < before);
    Printf.printf "event = job_space page = %d before = %d after = %d held = %d stored = %d\n%!"
      page before after (String.length held) (String.length done_);
    let db = open_db path in
    expect "job space restart final" (History.load_job db ~caller ~id = Ok (Some "final"));
    expect "job space caller isolation" (History.load_job db ~caller:"bob" ~id = Ok None);
    History.close db;
    List.iteri (fun index size ->
      let id = String.make 32 (Char.chr (Char.code 'k' + index)) in
      let db = open_db path in
      expect "job page plan" (History.plan_job db ~caller ~session ~id ~body:"plan" = Ok true);
      expect "job page allocation" (History.advance_job ~room:size db ~caller ~session ~id
        ~before:(Planned "plan") ~after:(Issued "c") = Ok true);
      History.close db;
      let _, before, _, _ = snapshot ~id () in
      let db = open_db path in
      let value = String.make size 'v' in
      expect "job page votes" (History.advance_job db ~caller ~session ~id
        ~before:(Issued "c") ~after:(Issued value) = Ok true);
      expect "job page final" (History.save_job db ~caller ~session ~id ~body:value = Ok true);
      expect "job page read" (History.load_job db ~caller ~id = Ok (Some value));
      History.close db;
      let stored, after, _, old = snapshot ~id () in
      expect "job page footprint" (stored.[32] = '\003' && not old && after <= before))
      [1; page - 1; page; page + 1; History.max_job_bytes];
    let db = open_db path in
    expect "job space cancel" (History.cancel db ~caller ~session = Ok true);
    expect "job space final survives cancel" (History.load_job db ~caller ~id = Ok (Some "final"));
    History.close db)

let test_fetch () =
  Test_workspace.with_dir "compute_fetch" (fun path ->
    let session = String.make 32 's' in
    let id = String.make 32 'j' in
    let next = String.make 32 'k' in
    let claim ?(caller = "alice") ?(session = session) ?(id = id) ?(limit = 8) db =
      History.claim_fetch db ~caller ~session ~id ~limit in
    let issue db id =
      expect "fetch session" (History.ensure db ~caller:"alice" ~session = Ok true);
      expect "fetch plan" (History.plan_job db ~caller:"alice" ~session ~id ~body:"plan" = Ok true);
      expect "fetch issue" (History.advance_job db ~caller:"alice" ~session ~id
        ~before:(Planned "plan") ~after:(Issued "call") = Ok true) in
    let db = open_db path in
    expect "fetch without work" (claim db = Ok None);
    expect "fetch reserved session" (History.ensure db ~caller:"alice" ~session = Ok true);
    expect "fetch saved plan" (History.plan_job db ~caller:"alice" ~session ~id ~body:"plan" = Ok true);
    expect "fetch planned work" (claim db = Ok None);
    issue db id;
    expect "fetch first index" (claim db = Ok (Some 0));
    expect "fetch wrong caller" (claim ~caller:"bob" db = Ok None);
    expect "fetch wrong session" (claim ~session:(String.make 32 't') db = Ok None);
    expect "fetch invalid limit" (Result.is_error (claim ~limit:0 db));
    expect "fetch large limit" (Result.is_error (claim ~limit:257 db));
    expect "fetch next index" (claim db = Ok (Some 1));
    History.close db;
    let db = open_db path in
    for round = 2 to 7 do
      expect "fetch restart index" (claim db = Ok (Some round))
    done;
    expect "fetch exhausted" (claim db = Ok None);
    History.close db;
    let db = open_db path in
    expect "fetch restart exhausted" (claim db = Ok None);
    issue db next;
    expect "fetch other job" (claim ~id:next db = Ok (Some 0));
    expect "fetch completed save" (History.save_job db ~caller:"alice" ~session ~id:next
      ~body:"done" = Ok true);
    expect "fetch completed refusal" (claim ~id:next db = Ok None);
    let last = String.make 32 'm' in
    issue db last;
    expect "fetch cancel" (History.cancel db ~caller:"alice" ~session = Ok true);
    expect "fetch cancelled refusal" (claim ~id:last db = Ok None);
    History.close db;
    expect "fetch closed store" (Result.is_error (claim db)))

let test_fetch_parallel () =
  Test_workspace.with_dir "compute_fetch_parallel" (fun path ->
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      let session = String.make 32 's' in
      let id = String.make 32 'j' in
      expect "parallel fetch session" (History.ensure db ~caller:"alice" ~session = Ok true);
      expect "parallel fetch plan" (History.plan_job db ~caller:"alice" ~session ~id ~body:"plan" = Ok true);
      expect "parallel fetch issue" (History.advance_job db ~caller:"alice" ~session ~id
        ~before:(Planned "plan") ~after:(Issued "call") = Ok true);
      let results = Array.make 16 (Ok None) in
      let workers = Array.init 16 (fun index -> Thread.create (fun () ->
        results.(index) <- History.claim_fetch db ~caller:"alice" ~session ~id ~limit:8) ()) in
      Array.iter Thread.join workers;
      expect "parallel fetch error" (Array.for_all Result.is_ok results);
      let rounds = Array.to_list results |> List.filter_map (function
        | Ok value -> value | Error _ -> None) |> List.sort Int.compare in
      expect "parallel fetch repeated or excess index" (rounds = List.init 8 Fun.id)))

let test_crash ?(cancelled = false) ?(reply = false) ?(context = false) ?(job = false)
    ?(work = false) ?(issued = false) ?(fetch = false) ?(notice = false) ?(relay = false)
    ?(ack = false) ?(scoped = false) ?(archive = false) ?(reserved = false) ?(finished = false) () =
  Test_workspace.with_dir "compute_crash" (fun path ->
    let input, output = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
      Unix.close input;
      let code = try
        let db = open_db path in
        if reserve ?scope:(if scoped then Some "keys" else None) db <> Ok true then 1
        else if archive && reserve ~scope:"new keys" ~sequence:2L db <> Ok true then 15
        else if cancelled && History.cancel db ~caller:"alice"
            ~session:(String.make 32 's') <> Ok true then 4
        else if (notice || ack) && History.queue_cancel db ~epoch:0L ~expires:64L ~caller:"alice"
            ~session:(String.make 32 's') ~body:"signed" <> Ok true then 10
        else if (notice || ack) && (match History.claim_notice db ~epoch:0L with
          | Ok (Some row) when row.round = 1 -> false | _ -> true) then 11
        else if ack && (match History.claim_notice db ~epoch:0L with
          | Ok (Some row) when row.round = 2 -> false | _ -> true) then 13
        else if ack && History.confirm_cancel db ~epoch:0L ~expires:64L ~round:2
            ~caller:"alice" ~session:(String.make 32 's') ~cancel:"signed"
            ~node:"peer" ~body:"receipt" <> Ok true then 14
        else if relay && History.relay_cancel db ~epoch:0L ~expires:64L ~round:2
            ~origin:"relay" ~id:(String.make 32 'r') ~caller:"alice"
            ~session:(String.make 32 's') <> Ok true then 12
        else if (reply || context) && History.finish
            ?context:(if context then Some "key" else None)
            db ~caller:"alice" ~session:(String.make 32 's')
            ~sequence:1L ~request:(String.make 32 'r') ~reply:"signed" <> Ok true then 5
        else if job && History.save_job db ~caller:"alice" ~session:(String.make 32 's')
            ~id:(String.make 32 'j') ~body:"result" <> Ok true then 6
        else if work && History.plan_job db ~caller:"alice" ~session:(String.make 32 's')
            ~id:(String.make 32 'j') ~body:"plan" <> Ok true then 7
        else if issued && History.advance_job db ~caller:"alice" ~session:(String.make 32 's')
            ~id:(String.make 32 'j') ~before:(History.Planned "plan")
            ?room:(if reserved then Some 65536 else None)
            ~after:(History.Issued "votes") <> Ok true then 8
        else if fetch && History.claim_fetch db ~caller:"alice" ~session:(String.make 32 's')
            ~id:(String.make 32 'j') ~limit:8 <> Ok (Some 0) then 9
        else if finished && History.save_job db ~caller:"alice" ~session:(String.make 32 's')
            ~id:(String.make 32 'j') ~body:"result" <> Ok true then 16
        else begin
          ignore (Unix.write_substring output "r" 0 1);
          Unix.kill (Unix.getpid ()) Sys.sigstop;
          2
        end
      with _ -> 3 in
      Unix._exit code
    | pid ->
      Unix.close output;
      let reaped = ref false in
      Fun.protect ~finally:(fun () ->
        Unix.close input;
        if not !reaped then begin
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
          ignore (Unix.waitpid [] pid)
        end) (fun () ->
        let ready, _, _ = Unix.select [input] [] [] 10. in
        expect "crash child timeout" (ready <> []);
        let message = Bytes.create 1 in
        expect "crash child write" (Unix.read input message 0 1 = 1);
        Unix.kill pid Sys.sigkill;
        ignore (Unix.waitpid [] pid);
        reaped := true;
        let db = open_db path in
        if scoped then expect "unclean restart scope"
          (History.scope db ~caller:"alice" ~session:(String.make 32 's') =
            Ok (Some History.{sequence = (if archive then 2L else 1L);
              request = String.make 32 'r'; body = (if archive then "new keys" else "keys")}));
        if archive then expect "unclean restart archive"
          (History.find_scope db ~caller:"alice" ~session:(String.make 32 's')
            (fun value -> value.sequence = 1L) =
              Ok (Some History.{sequence = 1L; request = String.make 32 'r'; body = "keys"}));
        if reply then expect "unclean restart reply"
          (History.reply db ~caller:"alice" ~session:(String.make 32 's')
            ~sequence:1L ~request:(String.make 32 'r') = Ok (Some "signed"));
        if context then expect "unclean restart reply and key"
          (History.saved db ~caller:"alice" ~session:(String.make 32 's')
            ~sequence:1L ~request:(String.make 32 'r') =
              Ok (Some History.{reply = "signed"; context = Some "key"}));
        if job then expect "unclean restart job"
          (History.load_job db ~caller:"alice" ~id:(String.make 32 'j') = Ok (Some "result"));
        if work then expect "unclean restart work"
          (History.work db ~caller:"alice" ~id:(String.make 32 'j') =
            Ok (if finished then None else
              Some (if issued then History.Issued "votes" else History.Planned "plan")));
        if finished then expect "unclean reserved completion"
          (History.load_job db ~caller:"alice" ~id:(String.make 32 'j') = Ok (Some "result"));
        if fetch then expect "unclean restart fetch index"
          (History.claim_fetch db ~caller:"alice" ~session:(String.make 32 's')
            ~id:(String.make 32 'j') ~limit:8 = Ok (Some 1));
        if notice then expect "unclean restart cancel message"
          (match History.claim_notice db ~epoch:0L with
           | Ok (Some row) -> row.round = 2 && row.body = "signed"
           | _ -> false);
        if relay then begin
          let put round = History.relay_cancel db ~epoch:1L ~expires:64L ~round
            ~origin:"relay" ~id:(String.make 32 'r') ~caller:"alice"
            ~session:(String.make 32 's') in
          expect "unclean relay duplicate refused" (Result.is_error (put 2));
          expect "unclean relay next accepted" (put 3 = Ok true)
        end;
        if ack then
          expect "unclean receipt retained"
            (match History.cancel_acks db ~epoch:1L ~caller:"alice"
                ~session:(String.make 32 's') with
             | Ok (Some (_, ["receipt"])) -> true | _ -> false);
        expect "unclean restart repeat" (reserve db = Ok false);
        expect "unclean restart progress"
          (reserve ~sequence:2L db = Ok (not (cancelled || notice || relay || ack)));
        History.close db))

let test_job_expiry () =
  Test_workspace.with_dir "compute_expiry" (fun path ->
    let db = open_db ~max_sessions:3 ~caller_limit:3 path in
    let session = String.make 32 's' in
    let id c = String.make 32 c in
    let plan db caller session name body =
      History.plan_job db ~caller ~session ~id:(id name) ~body in
    let prune db = History.prune_jobs db ~expired:(fun ~caller:_ ~session:_ ~id:_ body -> body = "expired") in
    expect "unknown job" (History.known_job db ~caller:"alice" ~session = Ok false);
    expect "plan creates session" (plan db "alice" session 'a' "expired" = Ok true);
    expect "planned session known" (History.known_job db ~caller:"alice" ~session = Ok true);
    expect "shared session plan" (plan db "alice" session 'b' "live" = Ok true);
    expect "third plan" (plan db "bob" (id 't') 'c' "expired" = Ok true);
    expect "full job table" (Result.is_error (plan db "other" (id 'u') 'd' "live"));
    expect "refused plan leaves no session" (History.known_job db ~caller:"other" ~session:(id 'u') = Ok false);
    expect "expired plans removed" (prune db = Ok 2);
    expect "shared live session retained" (History.known_job db ~caller:"alice" ~session = Ok true);
    expect "empty session removed" (History.known_job db ~caller:"bob" ~session:(id 't') = Ok false);
    expect "released capacity" (plan db "other" (id 'u') 'd' "expired" = Ok true);
    expect "issued state" (History.advance_job db ~caller:"other" ~session:(id 'u') ~id:(id 'd')
      ~before:(Planned "expired") ~after:(Issued "expired") = Ok true);
    History.close db;
    let db = open_db ~max_sessions:3 ~caller_limit:3 path in
    expect "issued survives expiry" (prune db = Ok 0);
    expect "issued survives restart" (History.work db ~caller:"other" ~id:(id 'd') = Ok (Some (Issued "expired")));
    expect "cancel tombstone" (History.cancel db ~caller:"ghost" ~session:(id 'g') = Ok true);
    expect "tombstone is not a job" (History.known_job db ~caller:"ghost" ~session:(id 'g') = Ok false);
    History.close db)

let test_cancel_rate () =
  Test_workspace.with_dir "compute_rate" (fun path ->
    let db = open_db path in
    let session = String.make 32 's' in
    expect "rate notice" (History.queue_cancel db ~epoch:1L ~expires:64L
      ~caller:"alice" ~session ~body:"cancel" = Ok true);
    expect "first send" (Result.map Option.is_some (History.claim_notice ~now:1000L db ~epoch:1L) = Ok true);
    expect "repeat paced" (History.claim_notice ~now:3999L db ~epoch:1L = Ok None);
    History.close db;
    let db = open_db path in
    expect "restart keeps rate" (History.claim_notice ~now:1001L db ~epoch:1L = Ok None);
    expect "clock reversal paced" (History.claim_notice ~now:0L db ~epoch:1L = Ok None);
    expect "prior sends no rounds" (History.claim_notice ~now:4000L ~rounds:false db ~epoch:1L = Ok None);
    expect "next round" (match History.claim_notice ~now:4000L db ~epoch:1L with
      | Ok (Some row) -> row.round = 2 | _ -> false);
    History.close db)

let test_expiry_pages () =
  Test_workspace.with_dir "compute_expiry_pages" (fun path ->
    let db = open_db ~max_sessions:70 ~caller_limit:70 path in
    let id n = Digestif.SHA256.(to_raw_string (digest_string (string_of_int n))) in
    for n = 0 to 64 do
      expect "expiry planned" (History.plan_job db ~caller:"alice" ~session:(id n)
        ~id:(id n) ~body:"expired" = Ok true)
    done;
    let scope = History.{sequence = 1L; request = id 0; body = "keys"} in
    expect "expiry funding" (History.wait_job ~scope ~room:4096 db ~caller:"alice"
      ~session:(id 0) ~id:(id 0) ~before:"expired" ~body:"expired" = Ok true);
    expect "expiry executed" (History.advance_job db ~caller:"alice" ~session:(id 1)
      ~id:(id 1) ~before:(Planned "expired") ~after:(Issued "expired") = Ok true);
    expect "expiry result" (History.save_job db ~caller:"alice" ~session:(id 1)
      ~id:(id 1) ~body:"paid work" = Ok true);
    expect "expiry reserved" (History.reserve db ~caller:"alice" ~session:(id 2)
      ~sequence:1L ~request:(id 2) = Ok true);
    expect "expiry cancelled" (History.cancel db ~caller:"alice" ~session:(id 3) = Ok true);
    History.close db;
    let db = open_db ~max_sessions:70 ~caller_limit:70 path in
    let visited = ref 0 in
    let prune () = History.prune_jobs db ~expired:(fun ~caller:_ ~session:_ ~id:_ _ ->
      incr visited; true) in
    expect "expiry page one" (prune () = Ok 32);
    expect "expiry page size" (!visited = 32);
    expect "expiry page two" (prune () = Ok 32);
    expect "expiry scan complete" (prune () = Ok 0);
    expect "expiry result preserved" (History.load_job db ~caller:"alice" ~id:(id 1)
      = Ok (Some "paid work"));
    expect "expiry reserve preserved" (History.known_job db ~caller:"alice" ~session:(id 2) = Ok true);
    expect "expiry cancellation preserved" (History.cancelled db ~caller:"alice" ~session:(id 3) = Ok true);
    expect "expiry funding removed" (History.work db ~caller:"alice" ~id:(id 0) = Ok None);
    expect "expiry funding scope removed" (History.scope db ~caller:"alice" ~session:(id 0) = Ok None);
    expect "expiry capacity restored" (History.plan_job db ~caller:"alice" ~session:(id 65)
      ~id:(id 65) ~body:"new" = Ok true);
    History.close db)

let test_directory () =
  Test_workspace.with_dir "compute_directory" (fun path ->
    let store = Filename.concat path "compute_history" in
    Unix.symlink path store;
    let refused = try let db = open_db path in History.close db; false with _ -> true in
    expect "symlink refused" refused;
    Unix.unlink store;
    Unix.mkdir store 0o755;
    let refused = try let db = open_db path in History.close db; false with _ -> true in
    expect "shared directory refused" refused)

let test_cancel_expiry () =
  Test_workspace.with_dir "compute_cancel_expiry" (fun path ->
    let open_db () = open_db ~max_sessions:4 ~caller_limit:4 path in
    let db = open_db () in
    let id c = String.make 32 c in
    let plan db c = History.plan_job ~expires:20L db ~caller:"alice"
      ~session:(id c) ~id:(id c) ~body:"plan" in
    List.iter (fun c -> expect "timed plan" (plan db c = Ok true)) ['a'; 'b'; 'c'];
    expect "timed issue" (History.advance_job db ~caller:"alice" ~session:(id 'b') ~id:(id 'b')
      ~before:(Planned "plan") ~after:(Issued "call") = Ok true);
    expect "timed reserve" (History.reserve db ~caller:"alice" ~session:(id 'c')
      ~sequence:1L ~request:(id 'r') = Ok true);
    expect "short cancel refused" (Result.is_error (History.cancel ~expires:19L db
      ~caller:"alice" ~session:(id 'a')));
    expect "short cancel atomic" (History.cancelled db ~caller:"alice" ~session:(id 'a') = Ok false);
    List.iter (fun c -> expect "timed cancellation" (History.cancel ~expires:20L db
      ~caller:"alice" ~session:(id c) = Ok true)) ['a'; 'b'; 'c'; 'd'];
    expect "cancel repeat deadline" (Result.is_error (History.cancel ~expires:21L db
      ~caller:"alice" ~session:(id 'a')));
    expect "timed capacity full" (Result.is_error (plan db 'e'));
    expect "cancel retained on deadline" (History.prune_cancels db ~epoch:20L = Ok 0);
    History.close db;
    let db = open_db () in
    expect "unpaid jobs removed" (History.prune_jobs db
      ~expired:(fun ~caller:_ ~session:_ ~id:_ _ -> true) = Ok 2);
    expect "unpaid cancellation expired" (History.prune_cancels db ~epoch:21L = Ok 2);
    expect "issued cancellation stays" (History.cancelled db ~caller:"alice" ~session:(id 'b') = Ok true);
    expect "reserved cancellation stays" (History.cancelled db ~caller:"alice" ~session:(id 'c') = Ok true);
    expect "unpaid session freed" (History.cancelled db ~caller:"alice" ~session:(id 'a') = Ok false);
    expect "unknown session freed" (History.cancelled db ~caller:"alice" ~session:(id 'd') = Ok false);
    expect "session can be reused" (plan db 'a' = Ok true);
    expect "caller quota freed" (plan db 'e' = Ok true);
    History.close db)

let test_relay_pace () =
  Test_workspace.with_dir "compute_relay_pace" (fun path ->
    let db = open_db path in
    for _ = 1 to History.caller_notices do
      expect "relay quota" (History.claim_relay db ~now:100L = Ok true)
    done;
    expect "relay quota exhausted" (History.claim_relay db ~now:100L = Ok false);
    History.close db;
    let db = open_db path in
    expect "relay restart quota" (History.claim_relay db ~now:60_099L = Ok false);
    expect "relay reversed clock" (History.claim_relay db ~now:1L = Ok false);
    expect "relay quota reset" (History.claim_relay db ~now:60_100L = Ok true);
    History.close db)

let test_send_pace () =
  Test_workspace.with_dir "compute_send_window" (fun path ->
    let db = open_db path in
    expect "local opens window" (History.claim_send db ~now:100L = Ok true);
    for _ = 1 to History.forward_limit do
      expect "late forward" (History.claim_send ~local:false db ~now:50_100L = Ok true)
    done;
    for _ = 1 to History.forward_limit do
      expect "next forward" (History.claim_send ~local:false db ~now:60_100L = Ok true)
    done;
    expect "forward window drift" (History.claim_send ~local:false db ~now:110_100L = Ok false);
    for _ = History.forward_limit + 1 to History.send_limit do
      expect "aligned local reserve" (History.claim_send db ~now:110_100L = Ok true)
    done;
    History.close db);
  Test_workspace.with_dir "compute_send_pace" (fun path ->
    let db = open_db path in
    for _ = 1 to History.forward_limit do
      expect "forward quota" (History.claim_send ~local:false db ~now:100L = Ok true)
    done;
    expect "forward reserve protected"
      (History.claim_send ~local:false db ~now:100L = Ok false);
    History.close db;
    let db = open_db path in
    expect "forward reserve survives restart"
      (History.claim_send ~local:false db ~now:100L = Ok false);
    for _ = History.forward_limit + 1 to History.send_limit do
      expect "local reserve" (History.claim_send db ~now:100L = Ok true)
    done;
    expect "local reserve exhausted" (History.claim_send db ~now:100L = Ok false);
    expect "forward reset" (History.claim_send ~local:false db ~now:60_100L = Ok true);
    for _ = 2 to History.send_limit do
      expect "new window quota" (History.claim_send db ~now:60_100L = Ok true)
    done;
    expect "new window exhausted" (History.claim_send db ~now:60_100L = Ok false);
    History.close db);
  Test_workspace.with_dir "compute_send_total" (fun path ->
    let db = open_db path in
    for _ = 1 to History.send_limit do
      expect "send quota" (History.claim_send db ~now:100L = Ok true)
    done;
    expect "send quota exhausted" (History.claim_send db ~now:100L = Ok false);
    expect "send preserves control" (History.claim_relay db ~now:100L = Ok true);
    for _ = 1 to History.transit_limit do
      expect "transit quota" (History.claim_transit db ~now:100L = Ok true)
    done;
    expect "transit quota exhausted" (History.claim_transit db ~now:100L = Ok false);
    History.close db;
    let db = open_db path in
    expect "send restart quota" (History.claim_send db ~now:60_099L = Ok false);
    expect "send reversed clock" (History.claim_send db ~now:1L = Ok false);
    expect "transit restart quota" (History.claim_transit db ~now:60_099L = Ok false);
    expect "transit reversed clock" (History.claim_transit db ~now:1L = Ok false);
    expect "send quota reset" (History.claim_send db ~now:60_100L = Ok true);
    expect "transit quota reset" (History.claim_transit db ~now:60_100L = Ok true);
    expect "send limits fit peer"
      (2 * (History.send_limit + History.transit_limit + History.caller_notices) + 21 < 512);
    History.close db)

let test_relay_claim () =
  Test_workspace.with_dir "compute_relay_claim" (fun path ->
    let session = String.make 32 'r' in
    let put db now round = History.relay_cancel ~close:false ~now ~transit:true db
      ~epoch:1L ~expires:64L ~round ~origin:"relay" ~id:session ~caller:"alice" ~session in
    let db = open_db path in
    expect "claimed relay saved" (put db 100L 2 = Ok true);
    History.close db;
    let db = open_db path in
    Fun.protect ~finally:(fun () -> History.close db) (fun () ->
      for _ = 1 to History.transit_limit do
        expect "duplicate relay refused" (Result.is_error (put db 100L 2))
      done;
      for _ = 2 to History.transit_limit do
        expect "duplicates preserve quota" (History.claim_transit db ~now:100L = Ok true)
      done;
      expect "quota refuses next round" (Result.is_error (put db 100L 3));
      expect "refused round retry" (put db 60_100L 3 = Ok true);
      expect "transit leaves session open"
        (History.cancelled db ~caller:"alice" ~session = Ok false)))

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--reply-space" then test_reply_space ()
  else if Array.length Sys.argv = 2 && Sys.argv.(1) = "--job-space" then test_job_space ()
  else if Array.length Sys.argv = 2 && Sys.argv.(1) = "--expiry" then begin
    test_job_expiry (); test_expiry_pages (); test_cancel_rate (); test_cancel_expiry ();
    test_relay_pace (); test_send_pace (); test_relay_claim ()
  end
  else begin
  test_job_expiry ();
  test_expiry_pages ();
  test_cancel_rate ();
  test_cancel_expiry ();
  test_relay_pace ();
  test_send_pace ();
  test_relay_claim ();
  test_scope ();
  test_scope_capacity ();
  test_page_budget ();
  test_control_space ();
  test_scope_upgrade ();
  test_issue_scope ();
  test_restart ();
  test_capacity ();
  test_identity ();
  test_cancel ();
  test_notices ();
  test_notice_capacity ();
  test_cancel_acks ();
  test_cancel_relay ();
  test_relay_capacity ();
  test_reply ();
  test_reply_space ();
  test_context ();
  test_job ();
  test_work ();
  test_funding_wait ();
  test_job_space ();
  test_fetch ();
  test_fetch_parallel ();
  test_crash ();
  test_crash ~cancelled:true ();
  test_crash ~notice:true ();
  test_crash ~relay:true ();
  test_crash ~ack:true ();
  test_crash ~reply:true ();
  test_crash ~context:true ();
  test_crash ~job:true ();
  test_crash ~work:true ();
  test_crash ~work:true ~issued:true ();
  test_crash ~work:true ~issued:true ~fetch:true ();
  test_crash ~scoped:true ();
  test_crash ~scoped:true ~cancelled:true ();
  test_crash ~scoped:true ~archive:true ~cancelled:true ();
  test_crash ~scoped:true ~work:true ~issued:true ();
  test_crash ~work:true ~issued:true ~reserved:true ~fetch:true ();
  test_crash ~work:true ~issued:true ~reserved:true ~finished:true ();
  test_directory ()
  end;
  print_endline "status = pass test = compute_history"