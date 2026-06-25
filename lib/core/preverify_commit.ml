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


type t = {
  budgets : Resource_lanes.lane -> Resource_lanes.budget;
  receipts : Preverify_receipt.t list;
}

let create ?(budgets=Resource_lanes.default_budget) receipts = {
  budgets;
  receipts;
}

let same_cost a b =
  a.Resource_lanes.txs = b.Resource_lanes.txs
  && a.bytes = b.bytes
  && Z.equal a.ou b.ou
  && a.proof = b.proof

let tx_hash tx =
  Transaction.hash tx

let heavy tx =
  Preverify_queue.needs_preverify tx

let dup_by f xs =
  let rec go seen = function
    | [] -> None
    | x :: rest ->
      let key = f x in
      if List.exists (String.equal key) seen then Some key
      else go (key :: seen) rest in
  go [] xs

let receipt_hash r =
  r.Preverify_receipt.tx_hash

let find_receipt hash receipts =
  match List.filter (fun r -> String.equal (receipt_hash r) hash) receipts with
  | [r] -> Ok r
  | [] -> Error ("missing_receipt:" ^ hash)
  | _ -> Error ("duplicate_receipt:" ^ hash)

let find_tx hash txs =
  List.find_opt (fun tx -> String.equal (tx_hash tx) hash) txs

let check_receipt tx r =
  match Preverify_receipt.validate r with
  | Error e -> Error ("invalid_receipt:" ^ e)
  | Ok () ->
    let hash = tx_hash tx in
    let lane = Resource_lanes.of_op tx.Transaction.op_type in
    let cost = Resource_lanes.cost tx in
    if not (String.equal r.Preverify_receipt.tx_hash hash) then Error "receipt_tx_mismatch"
    else if r.lane <> lane then Error ("receipt_lane_mismatch:" ^ hash)
    else if not r.ok then Error ("receipt_failed:" ^ hash)
    else if not (same_cost r.cost cost) then Error ("receipt_cost_mismatch:" ^ hash)
    else Ok ()

let add_used used lane cost =
  let current =
    match List.assoc_opt lane used with
    | Some value -> value
    | None -> Resource_lanes.zero in
  let next = Resource_lanes.add current cost in
  List.map (fun (l, u) -> if l = lane then l, next else l, u) used

let check_budget gate used tx =
  let lane = Resource_lanes.of_op tx.Transaction.op_type in
  let cost = Resource_lanes.cost tx in
  let next = Resource_lanes.add (Option.value (List.assoc_opt lane used) ~default:Resource_lanes.zero) cost in
  match Resource_lanes.over (gate.budgets lane) next with
  | Some e -> Error ("lane_budget:" ^ Resource_lanes.to_string lane ^ ":" ^ e)
  | None -> Ok (add_used used lane cost)

let check_orphan txs r =
  match find_tx r.Preverify_receipt.tx_hash txs with
  | None -> Error ("orphan_receipt:" ^ r.tx_hash)
  | Some tx ->
    if heavy tx then Ok ()
    else Error ("receipt_for_light_tx:" ^ r.tx_hash)

let check gate txs =
  match dup_by tx_hash txs with
  | Some hash -> Error ("duplicate_tx:" ^ hash)
  | None ->
    match dup_by receipt_hash gate.receipts with
    | Some hash -> Error ("duplicate_receipt:" ^ hash)
    | None ->
      let rec receipts = function
        | [] -> Ok ()
        | r :: rest ->
          begin
            match Preverify_receipt.validate r with
            | Error e -> Error ("invalid_receipt:" ^ e)
            | Ok () ->
              match check_orphan txs r with
              | Error e -> Error e
              | Ok () -> receipts rest
          end in
      let rec tx_loop used = function
        | [] -> Ok ()
        | tx :: rest ->
          if not (heavy tx) then tx_loop used rest
          else
            let hash = tx_hash tx in
            match find_receipt hash gate.receipts with
            | Error e -> Error e
            | Ok r ->
              match check_receipt tx r with
              | Error e -> Error e
              | Ok () ->
                match check_budget gate used tx with
                | Error e -> Error e
                | Ok used -> tx_loop used rest in
      match receipts gate.receipts with
      | Error e -> Error e
      | Ok () ->
        tx_loop (List.map (fun lane -> lane, Resource_lanes.zero) Resource_lanes.all) txs

let receipts_of_strings raws =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | raw :: rest ->
      match Preverify_receipt.of_string raw with
      | Ok receipt -> go (receipt :: acc) rest
      | Error e -> Error e in
  go [] raws

let check_strings ?budgets raws txs =
  match receipts_of_strings raws with
  | Error e -> Error ("receipt_parse:" ^ e)
  | Ok receipts ->
    let gate =
      match budgets with
      | Some budgets -> create ~budgets receipts
      | None -> create receipts in
    check gate txs