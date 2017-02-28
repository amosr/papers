(* Multiple inputs and outputs, blocking reads *)
Require Import Merges.Tactics.
Require Import Merges.Map.

Require Import Merges.Machine.
Require Import Merges.Fusion.Base.
Require Import Merges.Fusion.Tactics.


Require Import Coq.Lists.List.
Import ListNotations.
Set Implicit Arguments.
Require Import Coq.Logic.FunctionalExtensionality.


  Module B := Machine.Base.
  Module P := Machine.Program.
  Module F := Base.Fuse.
  Module FT := Tactics.Tactics.

Section Proof.
  Variable C : Set.
  Variable V1 : Set.
  Variable L1 : Set.
  Variable P1 : P.Program L1 C V1.

  Variable V2 : Set.
  Variable L2 : Set.
  Variable P2 : P.Program L2 C V2.

  Variable EqDec_C : EqDec C.


  Theorem EvalBlockUpdate pV pF pL:
   FT.EvalStep P1 P2 EqDec_C
    (B.BlockUpdate _ pV pF pL).
  Proof.
    EvalStep_intros.
    EvalStep_unfold_all.
    jauto_set.
    Optimize Proof.
      (* EvalBs P1 *)
      - matchmaker hBlockEq; inject_all.

        all: try EvalStep_Rule hEv1 hSv1.
        all: eapply B.EvalBs1; eauto.
        all: try EvalStep_Rule B.EvalBUpdate hSv1.


        all: EvalStep_Rule' B.EvalBPullOk hSv1
           ; destruct (s1 sv); tryfalse
           ; destruct (h' sv); jauto_set; tryfalse
           ; substs; simpls; fequals; rewrite plus_0_r
           ; eauto.

      (* Sv P1 *)
      - EvalStep_Invariant hSv1 hBlockEq.

        all: try solve
          [ fequals
          ; extensionality vv1
          ; matchmaker_goal'
          ].

      (* EvalBs P2 *)
      - matchmaker hBlockEq; inject_all.

        all: try EvalStep_Rule hEv2 hSv2.
        all: !eapply B.EvalBs1.
        all: try EvalStep_Rule B.EvalBUpdate hSv2.

        all: EvalStep_Rule' B.EvalBPullOk hSv2
           ; destruct (s2 sv); tryfalse
           ; destruct (h' sv); jauto_set; tryfalse
           ; substs; simpls; fequals; rewrite plus_0_r
           ; eauto.

      (* Sv P2 *)
      - EvalStep_Invariant hSv2 hBlockEq.

        all: try solve
          [ try rewrite H0
          ; fequals
          ; extensionality vv1
          ; matchmaker_goal'
          ].

    Qed.

End Proof.
