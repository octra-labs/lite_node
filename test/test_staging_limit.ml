(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_core

let check condition reason = if not condition then failwith reason

let transaction ?(fee = 1_000) ?encrypted_data sender nonce = Transaction.{
  from = Printf.sprintf "oct%044d" sender;
  to_ = "oct99999999999999999999999999999999999999999";
  amount = Z.zero;
  nonce;
  ou = Z.of_int fee;
  timestamp = 0.;
  signature = string_of_int sender ^ ":" ^ string_of_int nonce;
  public_key = None;
  message = None;
  op_type = Standard;
  encrypted_data;
}

let lookup _ = Some (Z.of_int 1_000_000_000, 0)

let () =
  if Array.mem "--invalid" Sys.argv then begin
    check (Result.is_error (Tx_staging.check_config ())) "invalid startup config accepted";
    print_endline "status = pass test = staging_invalid";
    exit 0
  end;
  if Array.mem "--defaults" Sys.argv then begin
    Tx_staging.clear ();
    check (Tx_staging.add_smart ~lookup (transaction 1 1) = Ok [])
      "unset byte environment refused ordinary transaction";
    Tx_staging.clear ();
    print_endline "status = pass test = staging_defaults";
    exit 0
  end;
  check (Tx_staging.check_config () = Ok ()) "startup config refused";
  Unix.putenv "OCTRA_STAGING_MAX_BYTES" "invalid";
  check (Tx_staging.check_config () = Ok ()) "startup config changed after initialization";
  Tx_staging.clear ();
  let small = transaction 1 1 in
  check (Tx_staging.add_smart ~lookup small = Ok []) "small payload refused";
  let large = transaction ~encrypted_data:(String.make 65_536 'x') 2 1 in
  check (Result.is_error (Tx_staging.add_smart ~lookup large))
    "oversized payload entered staging";
  check (Tx_staging.find_by_hash (Transaction.hash small) = Some small)
    "byte refusal removed existing transaction";
  check (Tx_staging.staging_size () = 1) "byte refusal changed staging size";
  let replacement = {large with Transaction.from = small.from; ou = Z.of_int 2_000} in
  check (Result.is_error (Tx_staging.add_smart ~lookup replacement))
    "oversized replacement entered staging";
  check (Tx_staging.find_by_hash (Transaction.hash small) = Some small)
    "byte refusal replaced original transaction";
  check (Tx_staging.lookup_dropped (Transaction.hash small) = None)
    "byte refusal recorded replacement";
  check (Tx_staging.staging_total_ou () = Transaction.ou_cost small)
    "byte refusal changed OU";
  Tx_staging.clear ();
  print_endline "status = pass test = staging_limit"