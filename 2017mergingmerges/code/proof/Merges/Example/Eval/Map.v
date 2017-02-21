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


Inductive C := C1 | C2.
Theorem EqDec_C : EqDec C. decides_equality. Qed.

Module M := Map.

Theorem C1_ne_C2 : C1 <> C2. Proof. intros X. inverts X. Qed.

Definition P1 := Map.P EqDec_C (fun x => S x) C1_ne_C2.

Theorem eval_P1_nil:
  exists sh,
 EvalTop P1
  (fun c => match c with
            | C1 => []
            | C2 => []
            end)
  sh Map.L'Pull.
Proof.
  eexists.
  evalsB0 C Map.V.
Qed.


Theorem eval_P1_2:
  exists sh,
 EvalTop P1
  (fun c => match c with
            | C1 => [1; 2]
            | C2 => [2; 3]
            end)
  sh Map.L'Pull.
Proof.
 eexists.
 evalsB1 C B.EvalBPullOk.
 evalsB1 C B.EvalBPush.
 evalsB1 C B.EvalBRelease.
 evalsB1 C B.EvalBPullOk.
 evalsB1 C B.EvalBPush.
 evalsB1 C B.EvalBRelease.
 evalsB0 C Map.V.
Qed.


