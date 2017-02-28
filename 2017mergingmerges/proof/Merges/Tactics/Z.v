Require Export Omega.
Set Implicit Arguments.

Local Close Scope nat.
Local Open Scope Z.


Lemma Z_mul_neg1_r a:
      a * -1 = - a.
Proof.
 omega.
Qed.

Lemma Z_add_neg_r a b:
      a + -b = a - b.
Proof.
 omega.
Qed.


Ltac Z_simp_all
 := simpl in *;
    repeat rewrite Z.add_0_r in *;
    repeat rewrite Z.mul_1_r in *;
    repeat rewrite Z_mul_neg1_r in *;
    repeat rewrite Z_add_neg_r in *.


