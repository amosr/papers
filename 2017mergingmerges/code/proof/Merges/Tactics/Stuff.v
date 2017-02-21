Require Import Merges.Tactics.LibTactics.
Require Import Coq.Lists.List.
Require Export Omega.

Import ListNotations.
Set Implicit Arguments.

(* I think using a prefix is a better idea than the suffix in LibTactics:
  not all tactics have "*" and "~" variants defined, and it seems inconsistent.
  At least this way it is consistently defined for all.

  However, "*" does not work as a prefix since it is a bullet.
*)
Tactic Notation "!" tactic1(t) := t; auto_star.

(* This one is "bigger" and binds more. So
      !X; Y; Z
  is parsed as
      (!X); Y; Z
  while
      !!X; Y; Z
  is parsed as
      !!(X; Y; Z)
*)
Tactic Notation "!!" tactic(t) := t; auto_star.

(* *)
Ltac unfold_simp F :=
  repeat match goal with
  | |- context [F _] => unfold F; simpl
  | H : context [F _] |- _ => unfold F in H; simpl in H
  end.

(*
  Definition double n : nat := n * 2.
  Hint Extern 1 => unfold_simp double.
*)


Ltac destruct_t T :=
  repeat match goal with
  | [ |- context [ ?V ] ]
  => match type of V with T => destruct V end
  (* | [ context [ ?V ] |- _ ]
  => match type of V with T => destruct V *)
  end.


Ltac inject_all :=
  repeat match goal with
  | [ H : ?X _ _ _ _ _ _ = ?X _ _ _ _ _ _|- _ ]
  => injects H
  | [ H : ?X _ _ _ _ _ = ?X _ _ _ _ _ |- _ ]
  => injects H
  | [ H : ?X _ _ _ _ = ?X _ _ _ _ |- _ ]
  => injects H
  | [ H : ?X _ _ _ = ?X _ _ _ |- _ ]
  => injects H
  | [ H : ?X _ _ = ?X _ _ |- _ ]
  => injects H
  | [ H : ?X _ = ?X _ |- _ ]
  => injects H
  end.


Ltac crunch_destruct V :=
 repeat (match goal with
  | [ |- context [ V ?X          ] ] => destruct (V X)
  | [ |- context [ V ?X ?Y       ] ] => destruct (V X Y)
  | [ |- context [ V ?X ?Y ?Z    ] ] => destruct (V X Y Z)
  | [ |- context [ V ?X ?Y ?Z ?U ] ] => destruct (V X Y Z U)
  end).

Ltac bye_not_eq :=
 try solve
 [ substs;
   match goal with
    H : ?x <> ?x |- _
    => destruct H; reflexivity
   end].

Ltac bye_in_empty :=
 try solve
 [ substs;
   match goal with
    H : In ?x [] |- _
    => inverts H
   end].

Ltac injects H :=
   injection H; clear H; intros; subst.

(*

Ltac bye_punch_ne :=
 try solve [
 match goal with 
  | H : ?a = ?c |- ?a <> ?b
  => rewrite H; intros not; inversion not
  | H : ?c = ?a |- ?a <> ?b
  => rewrite H; intros not; inversion not
  | H : ?b = ?c |- ?a <> ?b
  => rewrite H; intros not; inversion not
  | H : ?c = ?b |- ?a <> ?b
  => rewrite H; intros not; inversion not
 end].
*)


Ltac best_bet :=
  eauto; try solve [simpl in *; try omega; bye_not_eq; bye_in_empty; congruence].

Tactic Notation "churn_with" tactic(F) :=
  repeat (F; best_bet; simpl in *).



Ltac destruct_apps FUN :=
repeat match goal with
| [ _ : context [ FUN ?a ] |- _ ]
=> let x := fresh "destruct_" FUN in remember (FUN a) as x
   ; destruct x
   ; tryfalse
   ; repeat match goal with
     | [ H : _ = FUN a |- _ ] => gen H
     end
end;
 intros.

Ltac matchmaker Heq :=
 match goal with
| [ Heq : _ = match ?A with | _ => _ end |- _ ]
=> let x    := fresh "scrut_" Heq
in let Heqx := fresh "Heq_" x
in remember A as x eqn:Heqx; destruct x; try rewrite <- Heqx in *; tryfalse
 ; try matchmaker Heqx
| [ Heq : match ?A with | _ => _ end = _ |- _ ]
=> let x    := fresh "scrut_" Heq
in let Heqx := fresh "Heq_" x
in remember A as x eqn:Heqx; destruct x; try rewrite <- Heqx in *; tryfalse
 ; try matchmaker Heqx
end.


Ltac matchmaker_goal := repeat
 match goal with
| [ |- context [match ?A with | _ => _ end] ]
=> let x    := fresh "scrut_"
in let Heqx := fresh "Heq_" x
in remember A as x eqn:Heqx; destruct x; substs; try rewrite <- Heqx in *; tryfalse
 ; try matchmaker Heqx
end.

Ltac matchmaker_goal' := repeat
         match goal with
        | [ |- context [match ?A with | _ => _ end] ]
        => let x    := fresh "scrut_"
        in let Heqx := fresh "Heq_" x
        in remember A as x eqn:Heqx; destruct x
         ; try matchmaker Heqx
         ; try rewrite <- Heqx in *; tryfalse
        end
      ; eauto.


