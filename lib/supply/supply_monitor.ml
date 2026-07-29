(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type buckets = {
  total : Z.t;
  public : Z.t;
  enc : Z.t;
}

type event =
  | Shield of Z.t
  | Unshield of Z.t
  | Stealth_transfer
  | Claim of Z.t

type violation =
  | Negative_enc of { claimed : Z.t; available : Z.t }
  | Negative_public of { public : Z.t }
  | Not_conserved of { sum : Z.t; total : Z.t }
  | Ledger_divergence of { counter : Z.t; ledger : Z.t }
  | Audit_divergence of { counter : Z.t; audit : Z.t }

exception Supply_violation of violation

let make ~total ~public ~enc =
  if Z.sign public < 0 then Error (Negative_public { public })
  else if Z.sign enc < 0 then Error (Negative_enc { claimed = Z.neg enc; available = Z.zero })
  else
    let sum = Z.add public enc in
    if Z.equal sum total then Ok { total; public; enc }
    else Error (Not_conserved { sum; total })

let genesis ~total ~public = make ~total ~public ~enc:(Z.sub total public)

let apply b ev =
  match ev with
  | Stealth_transfer -> Ok b
  | Shield a ->
    let public = Z.sub b.public a in
    if Z.sign public < 0 then Error (Negative_public { public })
    else Ok { b with public; enc = Z.add b.enc a }
  | Unshield a | Claim a ->
    if Z.gt a b.enc then Error (Negative_enc { claimed = a; available = b.enc })
    else Ok { b with public = Z.add b.public a; enc = Z.sub b.enc a }

let replay b events =
  List.fold_left
    (fun acc ev -> match acc with Ok cur -> apply cur ev | Error _ as e -> e)
    (Ok b) events

let mint_of b ev =
  match ev with
  | Unshield a | Claim a -> if Z.gt a b.enc then Z.sub a b.enc else Z.zero
  | Shield _ | Stealth_transfer -> Z.zero

let assert_conserved b =
  if Z.sign b.enc < 0 then Error (Negative_enc { claimed = Z.neg b.enc; available = Z.zero })
  else if Z.sign b.public < 0 then Error (Negative_public { public = b.public })
  else
    let sum = Z.add b.public b.enc in
    if Z.equal sum b.total then Ok () else Error (Not_conserved { sum; total = b.total })

let reconcile_ledger b ~ledger_public =
  if Z.equal b.public ledger_public then Ok ()
  else Error (Ledger_divergence { counter = b.public; ledger = ledger_public })

let check_against_audit b ~audit_enc ~tolerance =
  let drift = Z.abs (Z.sub b.enc audit_enc) in
  if Z.leq drift tolerance then Ok ()
  else Error (Audit_divergence { counter = b.enc; audit = audit_enc })

let violation_to_string v =
  match v with
  | Negative_enc { claimed; available } ->
    Printf.sprintf "kind = negative_enc claimed = %s available = %s mint = %s"
      (Z.to_string claimed) (Z.to_string available) (Z.to_string (Z.sub claimed available))
  | Negative_public { public } ->
    Printf.sprintf "kind = negative_public public = %s" (Z.to_string public)
  | Not_conserved { sum; total } ->
    Printf.sprintf "kind = not_conserved sum = %s total = %s drift = %s"
      (Z.to_string sum) (Z.to_string total) (Z.to_string (Z.sub sum total))
  | Ledger_divergence { counter; ledger } ->
    Printf.sprintf "kind = ledger_divergence counter = %s ledger = %s"
      (Z.to_string counter) (Z.to_string ledger)
  | Audit_divergence { counter; audit } ->
    Printf.sprintf "kind = audit_divergence counter = %s audit = %s mint = %s"
      (Z.to_string counter) (Z.to_string audit) (Z.to_string (Z.sub audit counter))

let to_json b =
  `Assoc [
    "version", `Int 1;
    "total", `String (Z.to_string b.total);
    "public", `String (Z.to_string b.public);
    "enc", `String (Z.to_string b.enc);
  ]

let of_json j =
  match j with
  | `Assoc _ ->
    (try
       let field k = Yojson.Safe.Util.(j |> member k |> to_string) in
       Ok {
         total = Z.of_string (field "total");
         public = Z.of_string (field "public");
         enc = Z.of_string (field "enc");
       }
     with _ -> Error "supply_monitor of_json malformed")
  | _ -> Error "supply_monitor of_json expected object"

type t = { mutable buckets : buckets }

let create buckets = { buckets }

let observe t ev =
  match apply t.buckets ev with
  | Ok b -> t.buckets <- b
  | Error v -> raise (Supply_violation v)

let snapshot t = t.buckets