Check S O.

(* axiom: 0 is not the successor of any natural number *)
(* use `discriminate` when comparing *)

(* Lemma 2.2.2. For any natural number n, n + 0 = n. *)
Lemma plus_n_0 : forall n : nat, n + 0 = n.
Proof.
    intros. induction n.
    - reflexivity.
    - simpl. rewrite IHn. reflexivity. 
Qed.

(* Lemma 2.2.3. For any natural numbers n and m, n + S m = S (n + m). *)
Lemma plus_n_Sm : forall n m : nat, n + S m = S (n + m).
Proof.
    intros. induction n.
    - reflexivity.
    - simpl. rewrite IHn. reflexivity. 
Qed.

(* Proposition 2.2.4 (Addition is commutative). For any natural numbers n and m, n + m = m + n. *)
Proposition plus_comm : forall n m : nat, n + m = m + n.
Proof.
    intros. induction n.
    - simpl. rewrite plus_n_0. reflexivity.
    - simpl. rewrite plus_n_Sm. rewrite IHn. reflexivity.
Qed.

(* Proposition 2.2.5 (Addition is associative). For any natural numbers a, b, c, we have (a + b) + c = a + (b + c). *)
Proposition plus_asso : forall a b c : nat, (a + b) + c = a + (b + c).
Proof.
    intros. induction a.
    - reflexivity.
    - simpl. rewrite IHa. reflexivity.
Qed.

(* Proposition 2.2.6 (Cancellation law). Let a, b, c be natural numbers such that a + b = a + c. Then we have b = c. *)
Proposition plus_cancellation : forall a b c : nat, a + b = a + c -> b = c.
Proof.
    intros. induction a.
    - simpl in H. assumption.
    (* Axiom 2.4. Different natural numbers must have different successors; *)
    - simpl in H. apply IHa. apply eq_add_S. assumption.
Qed.

(* Definition 2.2.7 (Positive natural numbers). A natural number n is said to be positive iff it is not equal to 0.  *)
Definition is_positive (n : nat) : bool := 
    match n with 
    | O => false
    | _ => true
    end.

(* Proposition 2.2.8. If a is positive and b is a natural number, then a + b is positive. *)
Proposition is_sum_positive : forall a b : nat, is_positive a = true -> is_positive (a + b) = true.
Proof.
    intros. induction a.
    - discriminate.
    - simpl. reflexivity.
Qed.

(* Corollary 2.2.9. If a and b are natural numbers such that a + b = 0, then a = 0 and b = 0. *)
Corollary plus_zero_cancellation : forall a b : nat, a + b = 0 -> a = 0 /\ b = 0.
Proof.
    intros. induction a.
    - auto. 
    - discriminate.
Qed.

(* Lemma 2.2.10. Let a be a positive number. Then there exists exactly one natural number b such that S b = a. *)
(* todo *)

Fixpoint nat_eq (n m : nat) : bool :=
    match n, m with 
    | 0, 0 => true
    | 0, _ => false 
    | S n', 0 => false 
    | S n', S m' => nat_eq n' m'
    end.

Fixpoint nat_le (n m : nat) : bool := 
    match n, m with 
    | O, _ => true 
    | S n', S m' => nat_le n' m'
    | S _, _ => false
    end.

Fixpoint nat_lt (n m : nat) : bool := 
    match n, m with 
    | O, O => false
    | O, _ => true
    | S n', S m' => nat_lt n' m'
    | S n', O => false
    end.

Definition nat_ge (n m : nat) : bool := negb (nat_lt n m).

Definition nat_ := .

 



Notation "x =? y" := (nat_eq x y) (at level 70) : nat_scope.
Notation "x <=? y" := (nat_le x y) (at level 70) : nat_scope.
Notation "x <? y" := (nat_lt x y) (at level 70) : nat_scope.

(* Proposition 2.2.12 
(Order is reflexive) a ≥ a. 
(Order is transitive) If a ≥ b and b ≥ c, then a ≥ c.
(Order is anti-symmetric) If a ≥ b and b ≥ a, then a = b.
(Addition preserves order ) a ≥ b if and only if a + c ≥ b + c.
a<b if and only if a++ ≤ b.
a<b if and only if b = a + d for some positive number d. *)
Proposition order_reflexive : forall a : nat, a >= a.
Proof.
    intros. induction a. 
    - unfold ge. simpl.
Qed.


Proposition order_transitive : forall a b c: nat, a >= b /\ b >= c -> a >= c.
Proof.
    intros. induction a.
    - auto.
    - 
Qed.



Theorem plus_id : forall n m:nat,
  n = m ->
  n + n = m + m.
Proof.
  intros. rewrite H. reflexivity.  
Qed.


(* 0 times n equals 0 *)
Theorem mult_0_l : forall n : nat, 0 * n = 0.
Proof.
  intros n. reflexivity.  
Qed.

