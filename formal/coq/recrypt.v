(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

From Stdlib Require Import Ring.

Section Recrypt.

Variable F : Type.
Variables zero one : F.
Variables add mul sub : F -> F -> F.
Variable opp : F -> F.
Variable laws : ring_theory zero one add mul sub opp eq.

Add Ring recrypt_ring : laws.

Local Infix "+" := add.
Local Infix "*" := mul.
Local Infix "-" := sub.

Definition minor a b c d := a * d - b * c.

Theorem mix_cancel :
  forall a b c d e f,
    minor c d e f * a + minor e f a b * c + minor a b c d * e = zero /\
    minor c d e f * b + minor e f a b * d + minor a b c d * f = zero.
Proof.
  intros.
  unfold minor.
  split; ring.
Qed.

Theorem mask_relation :
  forall a b c d e f p q r y z,
    minor c d e f * (p + a * y + b * z)
      + minor e f a b * (q + c * y + d * z)
      + minor a b c d * (r + e * y + f * z)
    = minor c d e f * p + minor e f a b * q + minor a b c d * r.
Proof.
  intros.
  unfold minor.
  ring.
Qed.

Theorem public_relation :
  forall a b c d e f p q r x1 x2 x3 y z,
    x1 = p + a * y + b * z ->
    x2 = q + c * y + d * z ->
    x3 = r + e * y + f * z ->
    minor c d e f * x1 + minor e f a b * x2 + minor a b c d * x3
    = minor c d e f * p + minor e f a b * q + minor a b c d * r.
Proof.
  intros a b c d e f p q r x1 x2 x3 y z first second third.
  rewrite first, second, third.
  apply mask_relation.
Qed.

End Recrypt.

Print Assumptions mix_cancel.
Print Assumptions mask_relation.
Print Assumptions public_relation.