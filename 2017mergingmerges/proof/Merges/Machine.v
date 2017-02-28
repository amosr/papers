(* Definition of Machines (Processes), and their evaluation.  *)
Require Import Merges.Tactics.
Require Import Merges.Map.


Require Import Coq.Lists.List.
Import ListNotations.
Set Implicit Arguments.
Require Import Coq.Logic.FunctionalExtensionality.


Module Base.
Section Machine.
  Variable Label : Set.

  Definition Value := nat.

  Variable ChanV : Set.
  Variable ChanVEqDec : EqDec ChanV.

  Variable ScalarV : Set.
  Variable ScalarVEqDec : EqDec ScalarV.

  Definition StreamHeap := Map ChanV (list Value).
  Definition ScalarHeap := Map ScalarV Value.
  Definition Pred := StreamHeap -> ScalarHeap -> Prop.

  Let streamUpdate := Map.update ChanV (list Value) ChanVEqDec.
  Let scalarUpdate := Map.update ScalarV Value ScalarVEqDec.

  Inductive StreamTypeT :=
   | Input | Output | Ignore.
  Variable StreamType : ChanV -> StreamTypeT.

  Inductive Block : Type :=
   (* Blocking pull, wait forever until we get something *)
   | BlockPull : ScalarV -> ChanV -> Label -> Block
   (* Release the thing we just pulled.
      When two machines are pulling from same thing, this
      signals to other machine that it can now pull if it wants *)
   | BlockRelease : ChanV -> Label -> Block

   (* Push a value *)
   | BlockPush : ChanV -> (ScalarHeap -> Value) -> Label -> Block

   (* Update a variable *)
   | BlockUpdate : ScalarV -> (ScalarHeap -> Value) -> Label -> Block

   (* If non-zero *)
   | BlockIf : (ScalarHeap -> Value) -> Label -> Label -> Block

   (* Jump to another label without doing anything *)
   | BlockJump   : Label -> Block
   .

  Variable Blocks : Label -> Block.
  Variable LabelPre  : Label -> Pred.

  Inductive EvalB : StreamHeap -> ScalarHeap -> Label
                 -> StreamHeap -> ScalarHeap -> Label -> Prop :=
   | EvalBPullOk l v c lok i h sh
      : Blocks l = BlockPull v c lok
     -> StreamType c = Input
     -> EvalB h sh l
              (streamUpdate c (i :: h c) h) (scalarUpdate v i sh) lok

   (* Release does nothing *)
   | EvalBRelease l v l' h sh
      : Blocks l = BlockRelease v l'
     -> StreamType v = Input
     -> EvalB h sh l
              h sh l'

   | EvalBPush l v push l' h sh
      : Blocks l = BlockPush v push l'
     -> StreamType v = Output
     -> EvalB h sh l
              (streamUpdate v (push sh :: h v) h) sh l'

   | EvalBUpdate l v f l' h sh
      : Blocks l = BlockUpdate v f l'
     -> EvalB h sh l
              h (scalarUpdate v (f sh) sh) l'

   | EvalBIfZ l f lz lnz h sh
      : Blocks l = BlockIf f lz lnz
     -> f sh = 0
     -> EvalB h sh l
              h sh lz

   | EvalBIfS l f lz lnz h sh n
      : Blocks l = BlockIf f lz lnz
     -> f sh = S n
     -> EvalB h sh l
              h sh lnz

   | EvalBJump l l' h sh
      : Blocks l = BlockJump l'
     -> EvalB h sh l
              h sh l'

   | EvalBIgnore l v i h sh
      : StreamType v = Ignore
     -> EvalB h sh l
              (streamUpdate v (i :: h v) h) sh l
   .
  Hint Constructors EvalB.

  Variable Init : Label.
  Variable InitPre : LabelPre Init (fun _ => []) (fun _ => 0).

  Inductive EvalBs : StreamHeap -> ScalarHeap -> Label -> Prop :=
   | EvalBs0
      : EvalBs (fun _ => []) (fun _ => 0) Init
   | EvalBs1 l l' h h' sh sh'
      : EvalBs h sh l
     -> EvalB  h sh l h' sh' l'
     -> EvalBs h' sh' l'
   .
  Hint Constructors EvalBs.

  Definition BlocksPreT :=
    forall h h' sh sh' l l',
    LabelPre l h sh ->
    EvalB  h sh l h' sh' l' ->
    LabelPre l' h' sh'.

  Hypothesis BlocksPre: BlocksPreT.

  Theorem EvalBs_Hoare l h sh
   (hEvB : EvalBs h sh l)
         : LabelPre l h sh.
  Proof.
   !induction hEvB.
  Qed.
End Machine.

End Base.


Module Program.
 Module B := Base.

 Record Program (Label : Set) (ChanVar : Set) (ScalarVar : Set) : Type
  := mkProgram
   { Init     : Label
   ; Blocks   : Label -> B.Block Label ChanVar ScalarVar

   ; ChanVarEqDec : EqDec ChanVar
   ; ScalarVarEqDec : EqDec ScalarVar
   ; StreamType : ChanVar -> B.StreamTypeT

   ; LabelPre : Label -> B.Pred ChanVar ScalarVar
   ; BlocksPre: B.BlocksPreT ChanVarEqDec ScalarVarEqDec StreamType Blocks LabelPre
   ; InitPre  : LabelPre Init (fun _ => []) (fun _ => 0)
   }.

  Definition EvalBs (Label : Set) (ChanVar : Set) (ScalarVar : Set) (P : Program Label ChanVar ScalarVar)
   := B.EvalBs (ChanVarEqDec P) (ScalarVarEqDec P) (StreamType P) (Blocks P) (Init P).
End Program.

