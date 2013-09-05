Require Import List.

Record Loc : Set := 
  mkLoc { heap: nat ; 
          ref: nat
        }.

Definition Heap := nat.

Inductive Syntax : Set :=
  | Skip : Syntax
  | Seq  : Syntax -> Syntax -> Syntax
  | Log  : nat -> nat -> Syntax
  | Apply : nat -> Syntax
  (* | Query : Heap -> Syntax -> Loc -> Syntax *)
  (* | Write : Loc -> Syntax *)
.

Notation "s1 >> s2" := (Seq s1 s2) (at level 75, right associativity).

Inductive Prog : Set :=
  | Par : Prog -> Prog -> Prog
  | Proc : nat -> Syntax -> Prog
.

Notation "p ::: s" := (Proc p s) (at level 80, right associativity).
Notation "p1 \\ p2" := (Par p1 p2) (at level 85, right associativity).
Inductive Event : Set :=
  | LogEvent : nat -> nat -> nat -> Event
  | InvokeEvent : nat -> Event
  (* | WriteEvent *)
  (* | SyncEvent *)
.

Definition Trace := list Event.

Reserved Notation "( t1 , p1 )  --> ( t2 , p2 )" (at level 0).

Inductive Exec : Trace -> Prog -> Trace -> Prog -> Prop :=
  | SeqE :
    forall t t' p s1 s2 s1' ,
      (t, p ::: s1) --> (t', p ::: s1') -> 
      (t, p ::: s1 >> s2) --> (t', p ::: s1' >> s2)
  | SeqSkipE :
    forall t p s,
      (t, p ::: Skip >> s) --> (t, p ::: s)
  | LogE :
    forall t p q f s ,
      Exec t (p ::: Log q f \\ q ::: s) 
           (LogEvent p q f :: t) (p ::: Skip \\ q ::: s >> Apply f)
  | ApplyE : 
    forall t p f,
      (t, p ::: Apply f) --> (InvokeEvent f :: t, p ::: Skip)
  | ParExec1 :
    forall t t' p s s' rest ,
      (t, p ::: s) --> (t', p ::: s') ->
      (t, p ::: s \\ rest) --> (t' ,p ::: s' \\ rest)
  | ParExec2 :
    forall t t' p s s' rest ,
      (t, p ::: s) --> (t', p ::: s') ->
      (t, rest \\ p ::: s) --> (t' ,rest \\ p ::: s')
  | ParSwizzle :
    forall t p q rest ,
      (t, p \\ q \\ rest) --> (t, q \\ p \\ rest)
           where "( t1 , p1 )  --> ( t2 , p2 )" := (Exec t1 p1 t2 p2)
.

Reserved Notation "( t , p )  -->* ( t' , p' )" (at level 0).

Inductive Execs : Trace -> Prog -> Trace -> Prog -> Prop :=
  | ExecOne :
    forall t t' p p',
      (t, p) --> (t', p') -> 
      (t, p) -->* (t', p')
  | ExecStep :
    forall t t' t'' p p' p'' ,
      (t, p) -->* (t', p') ->
      (t', p') -->* (t'', p'') -> 
      (t, p) -->* (t'', p'')
             where "( t , p )  -->* ( t' , p' )" := (Execs t p t' p')
.

Inductive isSubSyntax : Syntax -> Syntax -> Prop :=
| SubSynEq : forall sub, isSubSyntax sub sub
| SubSynSeq :
  forall sub s1 s2,
    isSubSyntax sub s1 \/ isSubSyntax sub s2 ->
    isSubSyntax sub (s1 >> s2)
.

Inductive syntaxInProg : Syntax -> nat -> Prog -> Prop :=
| SynInPar :
  forall sub p pr1 pr2,
    syntaxInProg sub p pr1 \/ syntaxInProg sub p pr2 ->
    syntaxInProg sub p (pr1 \\ pr2)
| SynInProc :
  forall sub p s,
    isSubSyntax sub s ->
    syntaxInProg sub p (p ::: s)
.

Lemma AddLog: forall p q t t' e e' f,
  (t, e) --> ( t', e') ->
  t' = LogEvent p q f :: t ->
  syntaxInProg (Log q f) p e.
Proof.
intros p q t t' e e' f.
intros Step Trace.
induction Step.

(* Seq *)
apply IHStep in Trace.
inversion Trace.
repeat constructor; auto.

(* Skip *)
induction t.
  (* nil case *)
  discriminate Trace.
  (* cons case *)
  injection Trace.
  intros Eq LogEvent.
  rewrite <- Eq in Trace.
  intuition.

(* Log *)
injection Trace.
intros fEq qEq pEq.
constructor.
left.
rewrite fEq, qEq, pEq.
repeat constructor.

(* Invoke *)
inversion Trace.

(* Par1 *)
apply IHStep in Trace.
constructor.
left.
inversion Trace.
rewrite <- H.
rewrite <- H in Trace.
auto.

(* Par1 *)
apply IHStep in Trace.
constructor.
right.
inversion Trace.
rewrite <- H.
rewrite <- H in Trace.
auto.

(* ParSwizzle *)
induction t.
  (* nil case *)
  discriminate Trace.
  (* cons case *)
  injection Trace.
  intros Eq LogEvent.
  rewrite <- Eq in Trace.
  intuition.
Qed.



Lemma AddLogImpliesInvoke: forall p q t t' e e' f,
  (t, e) --> ( t', e') ->
  t' = LogEvent p q f :: t ->
  syntaxInProg (Apply f) q e'.
Proof.
intros p q t t' e e' f.
intros Step Trace.
induction Step.

(* Seq *)
apply IHStep in Trace.
inversion Trace.
repeat constructor; auto.

(* Skip *)
induction t.
  (* nil case *)
  discriminate Trace.
  (* cons case *)
  injection Trace.
  intros Eq LogEvent.
  rewrite <- Eq in Trace.
  intuition.

(* Log *)
injection Trace.
intros fEq qEq pEq.
constructor.
right.
rewrite fEq, qEq.
apply SynInProc.
apply SubSynSeq.
right.
repeat constructor.

(* Invoke *)
inversion Trace.

(* Par1 *)
apply IHStep in Trace.
constructor.
left.
inversion Trace.
rewrite <- H.
rewrite <- H in Trace.
auto.

(* Par1 *)
apply IHStep in Trace.
constructor.
right.
inversion Trace.
rewrite <- H.
rewrite <- H in Trace.
auto.

(* ParSwizzle *)
induction t.
  (* nil case *)
  discriminate Trace.
  (* cons case *)
  injection Trace.
  intros Eq LogEvent.
  rewrite <- Eq in Trace.
  intuition.
Qed.

Fixpoint OnlyApplies (s : Syntax) :=
  match s with
    | Apply f => True
    | Seq s1 s2 => OnlyApplies s1 /\ OnlyApplies s2
    | Skip => True
    | _ => False
  end.

Lemma NoExecSkip: forall t t' pr pr' p ,
  pr = (p ::: Skip) ->
  ~ (t, pr) -->* (t', pr').
Proof.
intros.
intro.
induction H0.
rewrite H in H0.
inversion H0.
auto.
Qed.

Fixpoint traceOrdAux (t : Trace) (e2: Event) : Prop :=
  match t with
    | h :: tl => h = e2 \/ traceOrdAux tl e2
    | _ => False
  end.

Fixpoint traceOrd (t : Trace) (e1 e2: Event) : Prop :=
  match t with
    | h :: tl => ~ (h = e2) /\ (h = e1 \/ traceOrd tl e1 e2)
    | _ => False
  end.

Fixpoint validSyn (p: nat) (s : Syntax) (t : Trace) : Prop :=
  match s with
    | Skip => True
    | Seq s1 s2 => validSyn p s1 t /\ validSyn p s2 t
    | Log heap call => traceOrd t 
                                (LogEvent p heap call) 
                                (InvokeEvent call)
    | Apply n => True
  (* Missing: query answer received after calculation *)
  end.

Fixpoint validProg (s : Prog) (t : Trace) : Prop :=
  match s with
    | p ::: e => validSyn p e t
    | p1 \\ p2 => validProg p1 t /\ validProg p2 t
  end.

Definition subTrace (t1 t2 : Trace) : Prop :=
  exists t', t1 ++ t' = t2.

Definition prefixTrace (p1 p2 : Prog) (t : Trace) : Prop := 
  exists t' , (t, p1) -->* (t', p2).

Fixpoint finishedProg (pr: Prog) : Prop :=
  match pr with
    | p ::: s => s = Skip
    | pr1 \\ pr2 =>  finishedProg pr1 /\ finishedProg pr2
  end.

Lemma allSatisfyFinished : forall P t,
  finishedProg P ->
  validProg P t.
intros.
induction P.
inversion H.
apply IHP1 in H0.
apply IHP2 in H1.
unfold validProg.
auto.

unfold finishedProg in H.
rewrite H.
simpl; auto.
Qed.

Lemma lem : forall t P t' P',
  (exists oldP, (nil, oldP) -->* (t, P)) ->
  finishedProg P' ->
  (t, P) -->* (t', P') ->
  validProg P t'.
Proof.
intros until P'.
intros H H0 H1.
induction H1.

induction H1.
Focus 8.

(* old proof , with t = nil, no existential or oldP *)
intros.
induction H1.
induction H1.


inversion H.

(* SeqSkip *)
unfold validProg.
unfold validSyn.
destruct s.
auto.
inversion H.
inversion H.
auto.

(* Log *)
inversion H.
inversion H2.

(* Apply *)
simpl.
auto.

(* Par1 *)
inversion H.
apply allSatisfyFinished with (t := t') in H3.
apply IHExec in H2.
simpl.
split.
unfold validProg in H2.
auto.
auto.
auto.

(* Par2 *)
inversion H.
apply allSatisfyFinished with (t := t') in H2.
apply IHExec in H3; try simpl; intuition.

(* Par swizzle *)
inversion H.
inversion H2.
simpl.
split;
  try split;
  apply allSatisfyFinished;
  auto.

apply IHExecs2.

Lemma lem : forall t P t' P',
  (exists P'', (nil, P'') -->* (t, P)) ->
  finishedProg P' ->
  (t, P) -->* (t', P') ->
  validProg P t'.
Proof.
intros.
induction H1.
induction H1.

(* Seq *)
contradict H0.

(* Skip *)



(* Definition isSingle : Prog -> Prop := *)
(*   fun prog => *)
(*     match prog with *)
(*       | (p ::: s) => True *)
(*       | (p1 \\ p2) => False *)
(*     end. *)

(* Lemma SingleProp: forall t t' pr1 pr2, *)
(*   isSingle pr1 -> *)
(*   (t, pr1) -->* (t', pr2) -> *)
(*   isSingle pr2. *)
(* Proof. *)
(* intros. *)
(* induction H0. *)
(* induction H0; *)
(* try (unfold isSingle in H); *)
(* auto. *)
(* auto. *)
(* Qed. *)

(* Definition GetSingle: { p : Prog | isSingle p } -> Syntax := *)
(*   match sig1 with *)
(*     |  *)

(* Lemma SeqLift: forall t t' p s1 s1' s2 pr1 pr2, *)
(*   pr1 = (p ::: s1) -> *)
(*   pr2 = (p ::: s2) -> *)
(*   (* isSingle pr1 -> *) *)
(*   (* isSingle pr2 -> *) *)
(*   (t, pr1) -->* (t', pr2) -> *)
(*   (t, p ::: s1 >> s2) -->* (t', p ::: s1' >> s2). *)
(* intros. *)
(* induction H1. *)
                          
(*                    . *)
(* elim H. *)
(* elim H0. *)
(* intros. *)
(* rewrite H2, H3 in H1. *)
(* exists x0. *)
(* exists x. *)
(* apply ExecOne. *)
(* apply SeqE. *)
(* auto. *)

(* elim H. *)
(* elim H0. *)
(* intros. *)


(* Definition SeqCompose (: (e1 : (t1, forall t1 t2 t3 s1 s1' s2 s3 p  pr1 pr2, *)
(*   pr1 = (p ::: s1) -> pr2 = (p ::: s1') -> *)
(*   (t1, p ::: s1) -->* (t2, p ::: s1') -> *)
(*   (t2, p ::: s1' >> s2) -->* (t3, p ::: s3) -> *)
(*   (t1, p ::: s1 >> s2) -->* (t3, p ::: s3). *)
(* Proof. *)
(* intros until pr2. *)
(* intros H H0 H1. *)
(* generalize s2 s3. *)
(* induction H1. *)
(* rewrite H, H0 in H1. *)
(* intros. *)
(* apply ExecStep with (t' := t') (p' := p ::: s1' >> s0). *)
(* apply ExecOne. *)
(* apply SeqE. *)
(* auto. *)
(* auto. *)
(* intros. *)



(* Lemma SeqCompose: forall t1 t2 t3 s1 s1' s2 s3 p  pr1 pr2, *)
(*   pr1 = (p ::: s1) -> pr2 = (p ::: s1') -> *)
(*   (t1, p ::: s1) -->* (t2, p ::: s1') -> *)
(*   (t2, p ::: s1' >> s2) -->* (t3, p ::: s3) -> *)
(*   (t1, p ::: s1 >> s2) -->* (t3, p ::: s3). *)
(* Proof. *)
(* intros until pr2. *)
(* intros H H0 H1. *)
(* generalize s2 s3. *)
(* induction H1. *)
(* rewrite H, H0 in H1. *)
(* intros. *)
(* apply ExecStep with (t' := t') (p' := p ::: s1' >> s0). *)
(* apply ExecOne. *)
(* apply SeqE. *)
(* auto. *)
(* auto. *)
(* intros. *)

(* lemma WillApply: forall q s f, *)
(*   OnlyApplies s -> *)
(*   forall t,  *)
(*   exists t', (t, q ::: s >> Apply f) -->* (InvokeEvent f :: t', q ::: Skip). *)
(* Proof.                 *)
(* intros q s f Only. *)
(* induction s. *)

(* (* Skip *) *)
(* intro t. *)
(* exists t. *)
(* apply ExecStep with (t' := t) (p' := q ::: Apply f). *)
(* repeat constructor. *)

(* (* Apply *) *)
(* repeat constructor. *)

(* (* Seq *) *)
(* unfold OnlyApplies in Only. *)
(* fold OnlyApplies in Only. *)
(* elim Only. *)
(* intros Only1 Only2 t. *)
(* apply IHs1 with (t := t) in Only1. *)
(* destruct Only1. *)
(* apply IHs2 with (t := x) in Only2. *)
(* destruct Only2. *)
(* exists x0. *)
(* inversion H. *)
(* inversion H1. *)
(* case H. *)
(* elim with (t := t) (p := q) in H. *)
(* Lemma lem1: forall t t' P P' P'' p q f, *)
(*   (t, P) --> ( LogEvent p q f :: t, P') -> *)
(*   (LogEvent p q f :: t, P') -->* (InvokeEvent f :: t', P''). *)
(* Proof. *)
(* intros t t' P P' P'' p q f. *)
(* intros H. *)
(* induction   *)
