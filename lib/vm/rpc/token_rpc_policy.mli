(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type meta
type page

type add_result =
  | Continue of page
  | Complete of page

val max_scan_programs : int
val max_page_rows : int
val max_page_bytes : int

val meta :
  address:string ->
  owner:string ->
  symbol:string ->
  name:string option ->
  decimals:string option ->
  total_supply:string option ->
  meta option

val row :
  meta ->
  balance:string ->
  Yojson.Safe.t option

val address :
  meta ->
  string

val empty_page :
  address:string ->
  offset:int ->
  limit:int ->
  page

val add :
  page ->
  Yojson.Safe.t ->
  add_result

val finish :
  limited:bool ->
  page ->
  Yojson.Safe.t

val page_bytes :
  Yojson.Safe.t ->
  int