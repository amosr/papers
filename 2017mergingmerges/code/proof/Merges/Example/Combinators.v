Require Import Merges.Tactics.
Require Import Merges.Map.

Require Import Merges.Machine.
Require Import Merges.Fusion.
Require Import Merges.Example.Base.


Require Import Coq.Lists.List.
Import ListNotations.
Set Implicit Arguments.
Require Import Coq.Logic.FunctionalExtensionality.

Module B := Base.
Module P := Program.

Module Map.
Section Map.
  Variable C : Set.
  Variable Cin : C.
  Variable Cout : C.
  Variable EqDec_C : EqDec C.

  Variable f : B.Value -> B.Value.
  Variable Cin_ne_Cout : Cin <> Cout.

  Inductive V :=
    | V0 : V.

  Inductive L :=
    | L'Pull : L
    | L'Push : L
    | L'Release : L.

  Let Blocks (l : L) : B.Block L C V :=
    match l with
    | L'Pull => B.BlockPull V0 Cin L'Push
    | L'Push => B.BlockPush Cout (fun sh => f (sh V0)) L'Release
    | L'Release => B.BlockRelease V Cin L'Pull
    end.

  Let LabelPre (l : L) : B.Pred C V :=
    match l with
    | L'Pull => fun iss h => iss Cout = map f (iss Cin)
    | L'Push => fun iss h => f (h V0) :: iss Cout = map f (iss Cin)
    | L'Release => fun iss h => iss Cout = map f (iss Cin)
    end.

  Let StreamType (c : C) : B.StreamTypeT :=
    if EqDec_C c Cin
    then B.Input
    else if EqDec_C c Cout
    then B.Output
    else B.Ignore.

  Program Definition P : P.Program L C V :=
    {| P.Init := L'Pull
    ; P.Blocks := Blocks
    ; P.ChanVarEqDec := EqDec_C
    ; P.StreamType := StreamType
    ; P.LabelPre := LabelPre
    |}.
  Next Obligation.
    decide_equality_simpl.
  Qed.
  Next Obligation.
    unfolds B.BlocksPreT. unfolds LabelPre. unfolds Blocks.
    introv hLbl hEvB.
    hint Cin_ne_Cout.

    !inverts hEvB; try destruct l; try destruct l'; inject_all; tryfalse.
    all: unfolds update; unfolds StreamType.

    all: try destruct v.
    all: matchmaker_goal'.
    !rewrite hLbl.
  Qed.
End Map.
End Map.


Module Filter.
Section Filter.
  Variable C : Set.
  Variable Cin : C.
  Variable Cout : C.
  Variable EqDec_C : EqDec C.

  Variable f : B.Value -> bool.
  Variable Cin_ne_Cout : Cin <> Cout.

  Inductive V :=
    | V0 : V.

  Inductive L :=
    | L'Pull
    | L'If
    | L'Push
    | L'Release.

  Let pred (h : B.ScalarHeap V) : B.Value :=
    if   f (h V0)
    then 1
    else 0.

  Let Blocks (l : L) : B.Block L C V :=
    match l with
    | L'Pull =>
        B.BlockPull V0 Cin L'If
    | L'If =>
        B.BlockIf C pred L'Release L'Push
    | L'Push =>
        B.BlockPush Cout (fun sh => sh V0) L'Release
    | L'Release =>
        B.BlockRelease V Cin L'Pull
    end.

  Let LabelPre (l : L) : B.Pred C V :=
    match l with
    | L'Pull => fun iss h =>
        iss Cout = filter f (iss Cin)
    | L'If => fun iss h =>
        filter f [h V0] ++ iss Cout = filter f (iss Cin)
    | L'Push => fun iss h =>
        h V0 :: iss Cout = filter f (iss Cin) /\ f (h V0) = true
    | L'Release => fun iss h => 
        iss Cout = filter f (iss Cin)
    end.

  Let StreamType (c : C) : B.StreamTypeT :=
    if EqDec_C c Cin
    then B.Input
    else if EqDec_C c Cout
    then B.Output
    else B.Ignore.

  Program Definition P : P.Program L C V :=
    {| P.Init := L'Pull
    ; P.Blocks := Blocks
    ; P.ChanVarEqDec := EqDec_C
    ; P.StreamType := StreamType
    ; P.LabelPre := LabelPre
    |}.
  Next Obligation.
    decide_equality_simpl.
  Qed.
  Next Obligation.
    unfolds B.BlocksPreT. unfolds LabelPre. unfolds Blocks.
    introv hLbl hEvB.
    hint Cin_ne_Cout.

    !inverts hEvB; try destruct l; try destruct l'; inject_all; tryfalse.
    all: unfolds update; unfolds StreamType.

    all: try destruct v.
    all: matchmaker_goal'.

    rewrite hLbl. simpl. !destruct (f i).

    destruct hLbl. !rewrite H.

    rewrite <- hLbl. subst. simpls. matchmaker_goal'.

    rewrite <- hLbl. subst. simpls. matchmaker_goal'.
  Qed.
End Filter.
End Filter.


Module Merge.
Section Merge.
  Variable C : Set.
  Variable CinL : C.
  Variable CinR : C.
  Variable Cout : C.
  Variable EqDec_C : EqDec C.

  Variable Cdistinct1 : CinL <> Cout.
  Variable Cdistinct2 : CinR <> Cout.
  Variable Cdistinct3 : CinL <> CinR.

  Inductive V :=
    | VL
    | VR.

  Inductive L :=
    | L'InitL
    | L'InitR

    | L'Case

    | L'PushL
    | L'DropL
    | L'PullL

    | L'PushR
    | L'DropR
    | L'PullR.

  Let Blocks (l : L) : B.Block L C V :=
    match l with
    | L'InitL =>
        B.BlockPull VL CinL L'InitR
    | L'InitR =>
        B.BlockPull VR CinR L'Case

    | L'Case =>
        B.BlockIf C
          (fun sh => if lt_dec (sh VL) (sh VR) then 0 else 1)
          L'PushL L'PushR

    | L'PushL =>
        B.BlockPush Cout (fun sh => sh VL) L'DropL
    | L'DropL =>
        B.BlockRelease V CinL L'PullL
    | L'PullL =>
        B.BlockPull VL CinL L'Case

    | L'PushR =>
        B.BlockPush Cout (fun sh => sh VR) L'DropR
    | L'DropR =>
        B.BlockRelease V CinR L'PullR
    | L'PullR =>
        B.BlockPull VR CinR L'Case
    end.

(*
  Function mergespec (ls : list nat) (rs : list nat) :=
    match ls, rs with
    | l :: ls', r :: rs' =>
        if lt_dec l r
        then l :: mergespec ls' rs
        else r :: mergespec ls  rs'
    | _, _ => []
    end.


  Let LabelPre (l : L) : B.Pred C V := fun iss h =>
      iss Cout = mergespec (iss CinL) (iss CinR).
*)

  Let LabelPre (l : L) : B.Pred C V := fun iss h =>
      True.

  Let StreamType (c : C) : B.StreamTypeT :=
    if EqDec_C c CinL
    then B.Input
    else if EqDec_C c CinR
    then B.Input
    else if EqDec_C c Cout
    then B.Output
    else B.Ignore.

  Program Definition P : P.Program L C V :=
    {| P.Init := L'InitL
    ; P.Blocks := Blocks
    ; P.ChanVarEqDec := EqDec_C
    ; P.StreamType := StreamType
    ; P.LabelPre := LabelPre
    |}.
  Next Obligation.
    decide_equality_simpl.
  Qed.
  Next Obligation.
    unfolds B.BlocksPreT. !unfolds LabelPre.
  Qed.
  Next Obligation.
    !unfolds LabelPre.
  Qed.
End Merge.
End Merge.
