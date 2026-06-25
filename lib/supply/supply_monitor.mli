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

val make : total:Z.t -> public:Z.t -> enc:Z.t -> (buckets, violation) result
val genesis : total:Z.t -> public:Z.t -> (buckets, violation) result
val apply : buckets -> event -> (buckets, violation) result
val replay : buckets -> event list -> (buckets, violation) result
val mint_of : buckets -> event -> Z.t
val assert_conserved : buckets -> (unit, violation) result
val reconcile_ledger : buckets -> ledger_public:Z.t -> (unit, violation) result
val check_against_audit : buckets -> audit_enc:Z.t -> tolerance:Z.t -> (unit, violation) result
val violation_to_string : violation -> string
val to_json : buckets -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (buckets, string) result

type t

val create : buckets -> t
val observe : t -> event -> unit
val snapshot : t -> buckets