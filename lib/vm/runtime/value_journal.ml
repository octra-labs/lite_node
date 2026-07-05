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


type transfer = {
  from_addr : string;
  to_addr : string;
  amount : Z.t;
}

type snapshot = {
  snap_transfers : transfer list;
  snap_deltas : (string, Z.t) Hashtbl.t;
}

type t = {
  balance : string -> Z.t;
  transfers : transfer list ref;
  deltas : (string, Z.t) Hashtbl.t;
}

let create ~balance =
  {
    balance;
    transfers = ref [];
    deltas = Hashtbl.create 16;
  }

let delta t addr =
  match Hashtbl.find_opt t.deltas addr with
  | Some value -> value
  | None -> Z.zero

let effective t addr =
  Z.add (t.balance addr) (delta t addr)

let add_delta t addr amount =
  Hashtbl.replace t.deltas addr (Z.add (delta t addr) amount)

let add_transfer t ~from_addr ~to_addr ~amount =
  t.transfers := { from_addr; to_addr; amount } :: !(t.transfers)

let transfer t ~from_addr ~to_addr ~amount =
  if Z.leq amount Z.zero then
    false
  else if Z.lt (effective t from_addr) amount then
    false
  else begin
    add_delta t from_addr (Z.neg amount);
    add_delta t to_addr amount;
    add_transfer t ~from_addr ~to_addr ~amount;
    true
  end

let apply t = function
  | Call_plan.Value_noop ->
    true
  | Call_plan.Value_reject ->
    false
  | Call_plan.Value_apply effect ->
    List.iter
      (fun delta -> add_delta t delta.Call_plan.addr delta.delta)
      effect.deltas;
    let entry = effect.journal in
    add_transfer t
      ~from_addr:entry.from_addr
      ~to_addr:entry.target
      ~amount:entry.amount;
    true

let discard t =
  t.transfers := [];
  Hashtbl.reset t.deltas

let commit t ~debit ~credit =
  List.iter
    (fun transfer ->
      debit transfer.from_addr transfer.amount;
      credit transfer.to_addr transfer.amount)
    (List.rev !(t.transfers));
  discard t

let snapshot t =
  {
    snap_transfers = !(t.transfers);
    snap_deltas = Hashtbl.copy t.deltas;
  }

let restore t snapshot =
  t.transfers := snapshot.snap_transfers;
  Hashtbl.reset t.deltas;
  Hashtbl.iter (Hashtbl.replace t.deltas) snapshot.snap_deltas