Require Export Merges.Fusion.Base.
Require Export Merges.Fusion.Tactics.
Require Export Merges.Fusion.Program.
Require Import Merges.Map.


Module Soundness.
  Module F := Fuse.
  Module B := Base.
  Module P := Program.

Section Soundness.
  Variable C : Set.
  Variable V1 : Set.
  Variable L1 : Set.
  Variable P1 : P.Program L1 C V1.

  Variable V2 : Set.
  Variable L2 : Set.
  Variable P2 : P.Program L2 C V2.

  Variable EqDec_C : EqDec C.

  Definition Streams := B.StreamHeap C.
  Definition Heap := B.ScalarHeap (F.V C V1 V2).
  Definition InputStates := C -> F.State.

  Definition fused_P1_P2 := r P1 P2 EqDec_C.
  
  Definition Var1 := @F.V'V1 C V1 V2.
  Definition Var2 := @F.V'V2 C V1 V2.

Theorem Soundness
    (streams : Streams) (heap    : Heap)
    (label1  : L1)         (label2  : L2)
    (state1  : InputStates) (state2  : InputStates)
             :
  P.EvalBs fused_P1_P2 
             streams heap 
            (F.LX label1 label2 state1 state2)
  -> F.EvalOriginal Var1 P1 P2 state1 streams heap label1
  /\ F.EvalOriginal Var2 P2 P1 state2 streams heap label2.
Proof.

  intros hEvB.

  eapply B.EvalBs_Hoare with (LabelPre := F.LabelPre P1 P2) in hEvB .
  unfold F.LabelPre in hEvB.
  unfold F.EvalOriginal.
  eauto.

  apply (P.InitPre fused_P1_P2).
  apply (P.BlocksPre fused_P1_P2).
Qed.

End Soundness.
End Soundness.