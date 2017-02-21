(* Multiple inputs and outputs, blocking reads *)
Require Import Merges.Tactics.
Require Import Merges.Map.

Require Import Merges.Machine.
Require Import Merges.Fusion.Base.


Require Import Coq.Lists.List.
Import ListNotations.
Set Implicit Arguments.
Require Import Coq.Logic.FunctionalExtensionality.


Module Tactics.
  Module B := Machine.Base.
  Module P := Machine.Program.
  Module F := Base.Fuse.

Section Tactics.
  Variable C : Set.
  Variable V1 : Set.
  Variable L1 : Set.
  Variable P1 : P.Program L1 C V1.

  Variable V2 : Set.
  Variable L2 : Set.
  Variable P2 : P.Program L2 C V2.

  Variable EqDec_C : EqDec C.

  Let V := F.V C V1 V2.
  Let V'V1 := @F.V'V1 C V1 V2.
  Let V'V2 := @F.V'V2 C V1 V2.
  Let V'C := @F.V'C C V1 V2.



  Definition isValid (l : F.L' C L1 L2) : Prop :=
    match l with
    | F.LX _ _ _ _ => True
    | F.L'INVALID _ _ _ => False
    end.


  Theorem ScalarVarEqDec_V: EqDec V.
  Proof.
    hint (P.ScalarVarEqDec P1).
    hint (P.ScalarVarEqDec P2).
    hint EqDec_C.
    decides_equality.
  Qed.

  Definition EvalStep b :=
    forall h h' sh sh' l l',
    isValid l -> isValid l' ->
    F.Blocks P1 P2 l = b ->
    F.LabelPre P1 P2 l h sh ->
    B.EvalB EqDec_C ScalarVarEqDec_V
      (F.StreamType P1 P2)
      (F.Blocks P1 P2)
      h sh l h' sh' l' ->
    F.LabelPre P1 P2 l' h' sh'.

  Theorem EvalStep_Ignore :
    forall h sh l i sv,
    F.StreamType P1 P2 sv = B.Ignore ->
    F.LabelPre P1 P2 l h sh ->
    F.LabelPre P1 P2 l (update C _ EqDec_C sv (i :: h sv) h) sh.
  Proof.
      unfolds F.StreamType.
      unfolds F.LabelPre.
      !destruct l.
      introv hStrmT hPre.
      destruct hPre as [hPre1 hPre2].
      destruct hPre1 as [hEv1 hSv1].
      destruct hPre2 as [hEv2 hSv2].
      forwards: hSv1 sv.
      forwards: hSv2 sv.
      matchmaker hStrmT.

      !assert (s1 sv = F.NoValue).
      !assert (s2 sv = F.NoValue).

      jauto_set.

      (* EvalBs P1 *)
      - !eapply B.EvalBs1.
        !applys_eq B.EvalBIgnore 2 3.
        unfolds update.
        extensionality sv'.
        matchmaker_goal'.

      (* Sv P1 *)
      - intros sv'.
        forwards: hSv1 sv'.
        unfolds update.
        matchmaker_goal'.

      (* EvalBs P2 *)
      - !eapply B.EvalBs1.
        !applys_eq B.EvalBIgnore 2 3.
        unfolds update.
        extensionality sv'.
        matchmaker_goal'.

      (* Sv P2 *)
      - intros sv'.
        forwards: hSv2 sv'.
        unfolds update.
        matchmaker_goal'.
  Qed.

  Theorem ScalarHeap_update_sub
      (V' : Set)
      (Eq_V' : EqDec V')
      (mkV' : V' -> V)
      (mkV'inj : forall a b, mkV' a = mkV' b -> a = b) 
      (sh : B.ScalarHeap V)
      (i : B.Value) (v0 : V'):
  (fun v : V' => update V P.B.Value ScalarVarEqDec_V (mkV' v0) i sh (mkV' v)) =
    update V' B.Value Eq_V' v0 i (fun v : V' => sh (mkV' v)).
  Proof.
    extensionality n.
    unfolds.
    destruct (ScalarVarEqDec_V); !destruct (Eq_V').
    !destruct n0.
    !destruct n0. !rewrite e.
  Qed.
  Program Definition ScalarHeap_update_sub_V1
    := ScalarHeap_update_sub (P.ScalarVarEqDec P1) V'V1 _.
  Program Definition ScalarHeap_update_sub_V2
    := ScalarHeap_update_sub (P.ScalarVarEqDec P2) V'V2 _.

  Theorem ScalarHeap_update_V1_as_V2 i sh v0:
    (fun v : V2 => update V P.B.Value ScalarVarEqDec_V (V'V1 v0) i sh (V'V2 v)) =
    (fun v : V2 => sh (V'V2 v)).
  Proof.
    extensionality v.
    unfolds.
    !destruct ScalarVarEqDec_V.
    tryfalse.
  Qed.

  Theorem ScalarHeap_update_V2_as_V1 i sh v0:
    (fun v : V1 => update V P.B.Value ScalarVarEqDec_V (V'V2 v0) i sh (V'V1 v)) =
    (fun v : V1 => sh (V'V1 v)).
  Proof.
    extensionality v.
    unfolds.
    !destruct ScalarVarEqDec_V.
    tryfalse.
  Qed.

  Theorem ScalarHeap_update_VC_as_V1 i sh c0:
    (fun v : V1 => update V P.B.Value ScalarVarEqDec_V (V'C c0) i sh (V'V1 v)) =
    (fun v : V1 => sh (V'V1 v)).
  Proof.
    extensionality v.
    unfolds update.
    !destruct (ScalarVarEqDec_V); tryfalse.
  Qed.

  Theorem ScalarHeap_update_VC_as_V2 i sh c0:
    (fun v : V2 => update V P.B.Value ScalarVarEqDec_V (V'C c0) i sh (V'V2 v)) =
    (fun v : V2 => sh (V'V2 v)).
  Proof.
    extensionality v.
    unfolds update.
    !destruct (ScalarVarEqDec_V); tryfalse.
  Qed.

End Tactics.
End Tactics.


  Ltac doit X := try solve [(!eapply Machine.Base.EvalBs1); (!eapply X)].

  Ltac rewrite_Heaps :=
      try rewrite Tactics.ScalarHeap_update_sub_V1;
      try rewrite Tactics.ScalarHeap_update_sub_V2;
      try rewrite Tactics.ScalarHeap_update_V1_as_V2;
      try rewrite Tactics.ScalarHeap_update_V2_as_V1;
      try rewrite Tactics.ScalarHeap_update_VC_as_V2;
      try rewrite Tactics.ScalarHeap_update_VC_as_V1.




  Ltac EvalStep_unfold_all :=
    unfolds Tactics.EvalStep;
    unfolds Tactics.isValid;
    unfolds Base.Fuse.Blocks;
    unfolds Base.Fuse.LabelPre;
    unfolds update.

  Ltac EvalStep_intros :=
    unfolds Tactics.EvalStep;
    unfolds Tactics.isValid;
    intros h h' sh sh' l l' hValidl hValidl' hBlockEq hLabelPre hEvalB;
    destruct l as [l1 l2 s1 s2 | ];
    destruct l' as [l'1 l'2 s'1 s'2 | ];
    tryfalse;
    inverts hEvalB; tryfalse;
    try solve [!apply Tactics.EvalStep_Ignore];
    destruct hLabelPre as [hLbl1 hLbl2];
    destruct hLbl1 as [hEv1 hSv1];
    destruct hLbl2 as [hEv2 hSv2].


  Ltac EvalStep_Rule' Eval Fwd :=
    let sv := fresh "sv"
         in applys_eq Eval 2 3
          ; eauto
          ; unfolds update
          ; extensionality sv
          ; try forwards: Fwd sv
          ; matchmaker_goal'
          ; simpl
          ; subst
          ; eauto
          .

  Ltac EvalStep_Rule Eval Fwd :=
    solve [EvalStep_Rule' Eval Fwd].

  Ltac EvalStep_Invariant Fwd BlockEq :=
    let sv := fresh "sv"
    in  intros sv
      ; forwards: Fwd sv
      ; matchmaker BlockEq
      ; inject_all
      ; matchmaker_goal'
      ; inject_all
      ; jauto_set
      ; eauto
      ; intros
      ; tryfalse.