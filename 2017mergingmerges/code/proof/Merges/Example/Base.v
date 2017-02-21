Require Import Merges.Tactics.
Require Import Merges.Map.

Require Import Merges.Machine.
Require Import Merges.Fusion.


Require Import Coq.Lists.List.
Import ListNotations.
Set Implicit Arguments.
Require Import Coq.Logic.FunctionalExtensionality.


Section EvalsB.
  Variable Label : Set.
  Variable ChanV : Set.
  Variable ScalarV : Set.

  Variable P : P.Program Label ChanV ScalarV.

  Inductive EvalsB : B.StreamHeap ChanV -> B.ScalarHeap ScalarV -> Label -> B.StreamHeap ChanV -> B.ScalarHeap ScalarV -> Label -> Prop :=
   | EvalsB0 l h sh
      : EvalsB h sh l   h sh l
   | EvalsB1 l l' l'' h h' h'' sh sh' sh''
      : B.EvalB (P.ChanVarEqDec P) (P.ScalarVarEqDec P) (P.StreamType P) (P.Blocks P) h sh l   h' sh' l'
     -> EvalsB h' sh' l'   h'' sh'' l''
     -> EvalsB h sh l   h'' sh'' l''
   .

  Definition EvalTop :=
   EvalsB (fun _ => []) (fun _ => 0) (P.Init P).
  Hint Unfold EvalTop.
End EvalsB.


Ltac funfolds :=
 repeat match goal with
  | [ |- context [ ?f _ ] ]
  => unfold f in *
  | [ _ : context [ ?f _ ] |- _ ]
  => unfold f in *
  end.

Ltac evalsB1 C X :=
  eapply EvalsB1;
  try
    ( !eapply X
    ; simpls
    ; funfolds
    ; simpls
    ; funfolds
    ; repeat destruct_t (EqDec C)
    ; !tryfalse
    ; !tryfalse
      ).


Ltac evalsB0 C V
  := applys_eq EvalsB0 3
   ; let c := fresh "c"
  in extensionality c
   ; !unfolds update
   ; simpls
   ; destruct c
   ; repeat destruct_t (EqDec C)
   ; tryfalse
   ; repeat destruct_t (EqDec V)
   ; tryfalse
   ; jauto.

Ltac decide_equality_simpl
  := let a := fresh "a"
  in let b := fresh "b"
  in intros a b; destruct a; destruct b;
     try solve [!left | right; discriminate].
