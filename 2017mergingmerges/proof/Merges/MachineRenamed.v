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

  Definition Streams := Map ChanV (list Value).
  Definition Heap := Map ScalarV Value.
  Definition Pred := Streams -> Heap -> Prop.

  Let streamUpdate := Map.update ChanV (list Value) ChanVEqDec.
  Let update := Map.update ScalarV Value ScalarVEqDec.
  Definition insert h c i := streamUpdate c (h c ++ [i]) h.

  Inductive StreamTypeT :=
   | Input | Output | Ignore.
  Variable StreamType : ChanV -> StreamTypeT.

  Inductive Instruction : Type :=
   (* Blocking pull, wait forever until we get something *)
   | Pull : ScalarV -> ChanV -> Label -> Instruction
   (* Release the thing we just pulled.
      When two machines are pulling from same thing, this
      signals to other machine that it can now pull if it wants *)
   | Drop : ChanV -> Label -> Instruction

   (* Push a value *)
   | Push : ChanV -> (Heap -> Value) -> Label -> Instruction

   (* Update a variable *)
   | Update : ScalarV -> (Heap -> Value) -> Label -> Instruction

   (* If non-zero *)
   | IfZero : (Heap -> Value) -> Label -> Label -> Instruction

   (* Jump to another label without doing anything *)
   | Jump   : Label -> Instruction
   .

  Variable Instructions : Label -> Instruction.
  Variable LabelPre  : Label -> Pred.


  Inductive Eval1 : Streams -> Heap -> Label -> Instruction
                 -> Streams -> Heap -> Label -> Prop :=
    | EvalPull (ss : Streams) (h : Heap) (l l' : Label)
               (var : ScalarV) (chan : ChanV) (val : Value)
       : StreamType chan = Input
      -> Eval1        ss                           h  l (Pull var chan l')
              (insert ss chan val) (update var val h) l'

    | EvalPush (ss : Streams) (h : Heap) (l l' : Label) (chan : ChanV) (f : Heap -> Value)
       : StreamType chan = Output
      -> Eval1         ss             h l (Push chan f l')
               (insert ss chan (f h)) h l'

    | EvalDrop (ss : Streams) (h : Heap) (l l' : Label) (chan : ChanV)
       : StreamType chan = Input
      -> Eval1 ss h l (Drop chan l')
               ss h l'

    | EvalUpdate (ss : Streams) (h : Heap) (l l' : Label)
                 (var : ScalarV) (f : Heap -> Value)
       : Eval1 ss                   h  l (Update var f l')
               ss (update var (f h) h) l'

    | EvalIfZ (ss : Streams) (h : Heap) (l lz lnz : Label) (f : Heap -> Value)
       : Eval1 ss h l (IfZero f lz lnz)
               ss h (if Nat.eqb (f h) 0 then lz else lnz)

    | EvalJump (ss : Streams) (h : Heap) (l l' : Label)
       : Eval1 ss h l (Jump l')
               ss h l'

    | EvalIgnore (ss : Streams) (h : Heap) (l l' : Label) (instr : Instruction)
                (chan : ChanV) (val : Value)
       : StreamType chan = Ignore
      -> Eval1 ss                   h l instr
               (insert ss chan val) h l.

  Hint Constructors Eval1.


  Variable Init : Label.

  Inductive EvalN : Streams -> Heap -> Label -> Prop :=
   | Eval0
      : EvalN (fun _ => []) (fun _ => 0) Init
   | EvalSuc (l l' : Label) (ss ss' : Streams) (h h' : Heap)
      : EvalN ss h l
     -> Eval1 ss h l (Instructions l) ss' h' l'
     -> EvalN                         ss' h' l'
   .

  Hint Constructors EvalN.

  Definition InstructionsPreT :=
    forall h h' sh sh' l l',
    LabelPre l h sh ->
    Eval1  h sh l (Instructions l) h' sh' l' ->
    LabelPre l' h' sh'.

  Variable InitPre : LabelPre Init (fun _ => []) (fun _ => 0).
  Hypothesis InstructionsPre: InstructionsPreT.

  Theorem EvalBs_Hoare l h sh
   (hEvB : EvalN h sh l)
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
   { Init         : Label
   ; Instructions : Label -> B.Instruction Label ChanVar ScalarVar

   ; ChanVarEqDec   : EqDec ChanVar
   ; ScalarVarEqDec : EqDec ScalarVar
   ; StreamType     : ChanVar -> B.StreamTypeT

   ; LabelInvariants : Label -> B.Pred ChanVar ScalarVar
   ; InstructionsInvariant:
        forall ss ss' h h' l l',
        LabelInvariants l ss h ->
        B.Eval1 ChanVarEqDec ScalarVarEqDec StreamType
                ss h l (Instructions l) ss' h' l' ->
        LabelInvariants l' ss' h'
   ; InitInvariant  : LabelInvariants Init (fun _ => []) (fun _ => 0)
   }.

  Definition EvalBs (Label : Set) (ChanVar : Set) (ScalarVar : Set) (P : Program Label ChanVar ScalarVar)
   := B.EvalN (ChanVarEqDec P) (ScalarVarEqDec P) (StreamType P) (Instructions P) (Init P).
End Program.

