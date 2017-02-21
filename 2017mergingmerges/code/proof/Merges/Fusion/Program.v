(* Multiple inputs and outputs, blocking reads *)
Require Import Merges.Tactics.
Require Import Merges.Map.

Require Import Merges.Machine.
Require Import Merges.Fusion.Base.
Require Import Merges.Fusion.Tactics.
Require Import Merges.Fusion.EvalPull.
Require Import Merges.Fusion.EvalPush.
Require Import Merges.Fusion.EvalRelease.
Require Import Merges.Fusion.EvalIf.
Require Import Merges.Fusion.EvalJump.
Require Import Merges.Fusion.EvalUpdate.


Require Import Coq.Lists.List.
Import ListNotations.
Set Implicit Arguments.
Require Import Coq.Logic.FunctionalExtensionality.


  Module B := Machine.Base.
  Module P := Machine.Program.
  Module F := Base.Fuse.
  Module FT := Tactics.Tactics.

Section Program.
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

  Program Definition r
  := {| P.Blocks := F.Blocks P1 P2
      ; P.LabelPre := F.LabelPre P1 P2
      ; P.Init := F.Init P1 P2
      ; P.StreamType := F.StreamType P1 P2
      ; P.ChanVarEqDec := P.ChanVarEqDec P1
      ; P.ScalarVarEqDec := FT.ScalarVarEqDec_V P1 P2 EqDec_C
      |}.

  Next Obligation.
    unfolds B.BlocksPreT.
    introv hLbl hEvB.
   !destruct l eqn:HL; destruct l'
    ; try solve [!unfolds F.LabelPre]
    ; try solve [inverts hEvB; tryfalse].

  hint (EqDec_Eq _ (P.ChanVarEqDec P1) EqDec_C).

  destruct (F.Blocks P1 P2 l) eqn:HB.

  !eapply EvalBlockPull;
    try solve [
    !subst; !unfolds FT.isValid
    ];
  applys_eq hEvB 0; fequals.

  !eapply EvalBlockRelease;
    !subst; !unfolds FT.isValid;
    applys_eq hEvB 0; fequals.

  !eapply EvalBlockPush;
    !subst; !unfolds FT.isValid;
    applys_eq hEvB 0; fequals.

  !eapply EvalBlockUpdate;
    !subst; !unfolds FT.isValid;
    applys_eq hEvB 0; fequals.

  !eapply EvalBlockIf;
    !subst; !unfolds FT.isValid;
    applys_eq hEvB 0; fequals.

  !eapply EvalBlockJump;
    !subst; !unfolds FT.isValid;
    applys_eq hEvB 0; fequals.
 Qed.
 Next Obligation.
  jauto_set;
  try apply B.EvalBs0;
  !jauto_set;
  !matchmaker_goal.
 Qed.
End Program.
