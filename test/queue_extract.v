(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Extraction ExtrOcamlBasic ExtrOcamlNatBigInt.
Require Import fhe_queue.

Extraction Language OCaml.
Set Extraction Output Directory ".".
Extraction "queue_model.ml" delta empty capacity view_limit.