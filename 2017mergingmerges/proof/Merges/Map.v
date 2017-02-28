(* Helper functions for dealing with maps and decidable equality *)
Require Import Merges.Tactics.
Require Import Coq.Program.Equality.
Require Import Coq.Logic.FunctionalExtensionality.


Section Map.
 Variable K : Type.
 Variable V : Type.
 Definition EqDec  := forall (n m : K), { n = m } + { n <> m }.
 Hint Unfold EqDec.
 Hypothesis EqDec_ : EqDec.


 Definition Map := K -> V.
 Hint Unfold Map.

 Definition empty (v : V) : Map
  := fun k => v.
 Hint Unfold empty.

 Definition update (k : K) (v : V) (m : Map) : Map
  := fun k' =>
       if EqDec_ k k'
       then v
       else m k'.
 Hint Unfold update.


 Lemma update_eq_is k v m:
       update k v m k = v.
 Proof.
  unfolds.
  !destruct_t EqDec; tryfalse.
 Qed.

 Lemma update_ne_is k v m k':
       k <> k'
    -> update k v m k' = m k'.
 Proof.
  introv H.
  unfolds.
  !destruct_t EqDec; tryfalse.
 Qed.

 Axiom MapExtensional : forall (m m' : Map),
   (forall k, m k = m' k) ->
   m = m'.


End Map.

Set Default Goal Selector "all".

Theorem eq_is_eq (A : Type) (n m : A) (a b : n = m):
  a = b.
Proof.
  dependent destruction a.
  !dependent destruction b.
Qed.
Hint Resolve eq_is_eq.

Theorem falsy_eq (A : Type) (a b : A -> False):
  a = b.
Proof.
  extensionality x.
  false.
Qed.
Hint Resolve falsy_eq.

Theorem EqDec_Eq (A : Type) (a b : EqDec A):
 a = b.
Proof.
  extensionality n.
  extensionality m.
  !!destruct_t (EqDec A); f_equal; tryfalse.
Qed.

Ltac destroy_eqdecs EQDEC
 := repeat destruct EQDEC; bye_not_eq.
