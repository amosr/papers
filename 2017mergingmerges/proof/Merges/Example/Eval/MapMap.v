Require Import Merges.Tactics.
Require Import Merges.Map.

Require Import Merges.Machine.
Require Import Merges.Fusion.
Require Import Merges.Example.Base.
Require Import Merges.Example.Combinators.


Require Import Coq.Lists.List.
Import ListNotations.
Set Implicit Arguments.
Require Import Coq.Logic.FunctionalExtensionality.

Inductive C := C1 | C2 | C3.
Theorem EqDec_C : EqDec C.
Proof.
  decide_equality_simpl.
Qed.


Module M := Map.

Theorem C1_ne_C2 : C1 <> C2. Proof. intros X. inverts X. Qed.
Theorem C2_ne_C3 : C2 <> C3. Proof. intros X. inverts X. Qed.

Definition P1 := Map.P EqDec_C (fun x => S x) C1_ne_C2.
Definition P2 := Map.P EqDec_C (fun x => S x) C2_ne_C3.

Definition P' := r P1 P2 EqDec_C.


Theorem eval_P'_1':
  exists sh,
 EvalTop P'
  (fun c => match c with
            | C1 => [11; 1]
            | C2 => [12; 2]
            | C3 => [13; 3]
            end)
  sh (P.Init P').
Proof.
  eexists.
  evalsB1 C B.EvalBPullOk.
  evalsB1 C B.EvalBUpdate.
  evalsB1 C B.EvalBPush.
  evalsB1 C B.EvalBRelease.
  evalsB1 C B.EvalBUpdate.
  evalsB1 C B.EvalBPush.
  evalsB1 C B.EvalBJump.

  evalsB1 C B.EvalBPullOk.
  evalsB1 C B.EvalBUpdate.
  evalsB1 C B.EvalBPush.
  evalsB1 C B.EvalBRelease.
  evalsB1 C B.EvalBUpdate.
  evalsB1 C B.EvalBPush.
  evalsB1 C B.EvalBJump.

  applys_eq EvalsB0 0; !fequals.

  repeat (funfolds; simpls).
  !fequals.
  extensionality c.
  repeat destruct_t (EqDec C); !tryfalse.

  extensionality c; !matchmaker_goal.
  extensionality c; funfolds; !matchmaker_goal.
Qed.

