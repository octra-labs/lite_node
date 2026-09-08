(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

val enc_scope : C_sess.scope -> bits option
val dec_scope : bits -> C_sess.scope option
val enc_tok : C_sess.token -> bits option
val dec_tok : bits -> C_sess.token option
val enc_state : C_sess.state -> bits option
val dec_state : bits -> C_sess.state option