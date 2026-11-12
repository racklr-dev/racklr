(* ========================================================================= *)
(* Effect-algebra kernel, machine-checked.                                   *)
(*                                                                           *)
(* A line-by-line transliteration of the Racket kernel leaf:                 *)
(*   racklr/effects/algebra/{provenance,row,combinators,subtyping,           *)
(*                           certificate}.rkt                                 *)
(*                                                                           *)
(* The Racket kernel stays the executable; this Coq development is the       *)
(* source of truth for its laws (ROADMAP.md kernel-leaf theory, test 2).     *)
(* The property tests in racklr-test/effects-algebra-property-test.rkt are   *)
(* the differential oracle; every theorem here names the Racket law it pins. *)
(*                                                                           *)
(* Carrier:                                                                   *)
(*   effect names, operation names, scope qualifiers : string (Racket symbol *)
(*   provenance evidence                              : set of string         *)
(*     (a rendered site; the kernel only unions/dedups/renders evidence,     *)
(*      so the record's tokens/receiver/chain fields are abstracted away)    *)
(*   rows                                             : finite map name->cell *)
(*     cell = (entry, evidence set); entries canonical, must-not dropped.    *)
(* ========================================================================= *)

From Coq Require Import List String Bool Arith Lia.
From Coq.Structures Require Import OrderedTypeEx.
From Coq.FSets Require Import FSetList FSetDecide FSetFacts FMapList FMapFacts.
Import ListNotations.

Module NameSet := FSetList.Make String_as_OT.
Module NameSetFacts := FSetFacts.WFacts_fun String_as_OT NameSet.
Module NameSetDecide := FSetDecide.WDecide_fun String_as_OT NameSet.
Module RowMap := FMapList.Make String_as_OT.
Module RowMapFacts := WFacts_fun String_as_OT RowMap.
Module RowMapProps := WProperties_fun String_as_OT RowMap.
Ltac fsetdec := NameSetDecide.fsetdec.

(* ------------------------------------------------------------------------- *)
(* modality: must-not <: may <: unknown <: must  (a 4-point lattice)         *)
(* ------------------------------------------------------------------------- *)
Inductive modality : Type := must_not | may | unknown | must.

(* rank-max join, faithful to combinators.rkt's modality-join *) 
Definition modality_join (a b : modality) : modality :=
  match a, b with
  | must, _ => must
  | _, must => must
  | unknown, _ => unknown
  | _, unknown => unknown
  | may, _ => may
  | _, may => may
  | must_not, must_not => must_not
  end.

(* both computations run: prohibition or obligation or deferral survives *) 
Definition modality_sequence (a b : modality) : modality :=
  match a, b with
  | must_not, _ => must_not
  | _, must_not => must_not
  | must, _ => must
  | _, must => must
  | unknown, _ => unknown
  | _, unknown => unknown
  | _, _ => may
  end.

(* only one branch runs: must needs every branch; prohibition needs every *) 
Definition modality_choose (a b : modality) : modality :=
  match a, b with
  | must, must => must
  | must_not, must_not => must_not
  | unknown, _ => unknown
  | _, unknown => unknown
  | _, _ => may
  end.

Lemma modality_join_comm : forall a b, modality_join a b = modality_join b a.
Proof. intros a b; destruct a, b; reflexivity. Qed.

Lemma modality_join_assoc :
  forall a b c, modality_join (modality_join a b) c = modality_join a (modality_join b c).
Proof. intros a b c; destruct a, b, c; reflexivity. Qed.

Lemma modality_join_idem : forall a, modality_join a a = a.
Proof. intros a; destruct a; reflexivity. Qed.

Lemma modality_sequence_assoc :
  forall a b c, modality_sequence (modality_sequence a b) c = modality_sequence a (modality_sequence b c).
Proof. intros a b c; destruct a, b, c; reflexivity. Qed.

Lemma modality_choose_comm : forall a b, modality_choose a b = modality_choose b a.
Proof. intros a b; destruct a, b; reflexivity. Qed.

Lemma modality_choose_assoc :
  forall a b c, modality_choose (modality_choose a b) c = modality_choose a (modality_choose b c).
Proof. intros a b c; destruct a, b, c; reflexivity. Qed.

Lemma modality_choose_idem : forall a, modality_choose a a = a.
Proof. intros a; destruct a; reflexivity. Qed.

(* ------------------------------------------------------------------------- *)
(* operations: a set of operation names, or the distinguished TOP (the empty  *)
(* Racket ops list = all operations).  None = top, which dominates and is     *)
(* never narrowed.                                                           *)
(* ------------------------------------------------------------------------- *)
Definition ops := option NameSet.t.

Definition ops_union (a b : ops) : ops :=
  match a, b with
  | None, _ => None
  | _, None => None
  | Some s, Some t => Some (NameSet.union s t)
  end.

Definition ops_eq (a b : ops) : Prop :=
  match a, b with
  | None, None => True
  | Some s, Some t => NameSet.Equal s t
  | _, _ => False
  end.

Lemma ops_eq_refl : forall a, ops_eq a a.
Proof. intros [a|]; simpl; auto. intro x; tauto. Qed.

Lemma ops_eq_sym : forall a b, ops_eq a b -> ops_eq b a.
Proof.
  intros [a|] [b|] H; simpl in *; auto.
  red. intros x. specialize (H x). tauto.
Qed.

Lemma ops_union_comm : forall a b, ops_eq (ops_union a b) (ops_union b a).
Proof.
  intros [a|] [b|]; simpl; auto; fsetdec.
Qed.

Lemma ops_union_assoc :
  forall a b c, ops_eq (ops_union a (ops_union b c)) (ops_union (ops_union a b) c).
Proof.
  intros [a|] [b|] [c|]; simpl; first [fsetdec | auto].
Qed.

Lemma ops_union_idem : forall a, ops_eq (ops_union a a) a.
Proof.
  intros [a|]; simpl; auto; fsetdec.
Qed.

Lemma ops_union_top : forall a, ops_eq (ops_union a None) None.
Proof.
  intros [a|]; simpl; auto.
Qed.

(* ------------------------------------------------------------------------- *)
(* budget: option nat; None is unbounded (top)                                *)
(* ------------------------------------------------------------------------- *)
Definition budget := option nat.

Definition budget_lub (a b : budget) : budget :=
  match a, b with
  | None, _ => None
  | _, None => None
  | Some n, Some m => Some (Nat.max n m)
  end.

Definition budget_sum (a b : budget) : budget :=
  match a, b with
  | None, _ => None
  | _, None => None
  | Some n, Some m => Some (n + m)
  end.

Lemma budget_lub_comm : forall a b, budget_lub a b = budget_lub b a.
Proof.
  intros [a|] [b|]; simpl; auto. f_equal. lia.
Qed.

Lemma budget_lub_assoc :
  forall a b c, budget_lub (budget_lub a b) c = budget_lub a (budget_lub b c).
Proof.
  intros [a|] [b|] [c|]; simpl; auto; f_equal; lia.
Qed.

Lemma budget_lub_idem : forall a, budget_lub a a = a.
Proof.
  intros [a|]; simpl; auto. f_equal. lia.
Qed.

Lemma budget_sum_comm : forall a b, budget_sum a b = budget_sum b a.
Proof.
  intros [a|] [b|]; simpl; auto. f_equal. lia.
Qed.

Lemma budget_sum_assoc :
  forall a b c, budget_sum (budget_sum a b) c = budget_sum a (budget_sum b c).
Proof.
  intros [a|] [b|] [c|]; simpl; auto; f_equal; lia.
Qed.

(* ------------------------------------------------------------------------- *)
(* scope qualifier: option string; None is unconstrained (top)                *)
(* ------------------------------------------------------------------------- *)
Definition scp := option string.

Definition scope_lub (a b : scp) : scp :=
  match a, b with
  | None, _ => None
  | _, None => None
  | Some s, Some t => if String.eqb s t then Some s else None
  end.

Lemma scope_lub_same : forall s, scope_lub (Some s) (Some s) = Some s.
Proof. intros. simpl. rewrite String.eqb_refl. reflexivity. Qed.

Lemma scope_lub_some : forall a b s, scope_lub a b = Some s <-> a = Some s /\ b = Some s.
Proof.
  intros [a|] [b|] s; simpl; split; intro H.
  - destruct (String.eqb a b) eqn:E.
    + apply String.eqb_eq in E. injection H; intros; subst; subst; split; auto.
    + discriminate.
  - destruct H as [Ha Hb]. injection Ha; injection Hb; intros; subst.
    rewrite String.eqb_refl; reflexivity.
  - discriminate.
  - destruct H as [Ha Hb]; discriminate.
  - discriminate.
  - destruct H as [Ha Hb]; discriminate.
  - discriminate.
  - destruct H as [Ha Hb]; discriminate.
Qed.

Lemma scope_lub_comm : forall a b, scope_lub a b = scope_lub b a.
Proof.
  intros a b.
  destruct (scope_lub a b) eqn:E.
  - apply scope_lub_some in E. destruct E as [Ha Hb]. subst.
    rewrite scope_lub_same. reflexivity.
  - destruct (scope_lub b a) eqn:E'.
    + apply scope_lub_some in E'. destruct E' as [Hb Ha]. subst.
      rewrite scope_lub_same in E. discriminate.
    + reflexivity.
Qed.

Lemma scope_lub_assoc :
  forall a b c, scope_lub (scope_lub a b) c = scope_lub a (scope_lub b c).
Proof.
  intros a b c.
  destruct (scope_lub (scope_lub a b) c) eqn:E.
  - apply scope_lub_some in E. destruct E as [Hab Hc].
    apply scope_lub_some in Hab. destruct Hab as [Ha Hb]. subst.
    rewrite !scope_lub_same. reflexivity.
  - destruct (scope_lub a (scope_lub b c)) eqn:E'.
    + apply scope_lub_some in E'. destruct E' as [Ha Hbc].
      apply scope_lub_some in Hbc. destruct Hbc as [Hb Hc]. subst.
      rewrite !scope_lub_same in E. discriminate.
    + reflexivity.
Qed.

Lemma scope_lub_idem : forall a, scope_lub a a = a.
Proof.
  intros [a|]; simpl. rewrite String.eqb_refl. reflexivity. reflexivity.
Qed.

(* ------------------------------------------------------------------------- *)
(* entry: (modality, ops, budget, scope); the name is the row map key         *)
(* ------------------------------------------------------------------------- *)
Record entry := mk_entry { e_mod : modality; e_ops : ops; e_bud : budget; e_scp : scp }.

Definition entry_eq (a b : entry) : Prop :=
  e_mod a = e_mod b /\ ops_eq (e_ops a) (e_ops b) /\ e_bud a = e_bud b /\ e_scp a = e_scp b.

Lemma entry_eq_refl : forall a, entry_eq a a.
Proof.
  intros a. unfold entry_eq. repeat split; try reflexivity.
  destruct (e_ops a); simpl; auto. intro x; tauto.
Qed.

Definition entry_merge_join (a b : entry) : entry :=
  mk_entry (modality_join (e_mod a) (e_mod b))
           (ops_union (e_ops a) (e_ops b))
           (budget_lub (e_bud a) (e_bud b))
           (scope_lub (e_scp a) (e_scp b)).

Definition entry_merge_sequence (a b : entry) : entry :=
  mk_entry (modality_sequence (e_mod a) (e_mod b))
           (ops_union (e_ops a) (e_ops b))
           (budget_sum (e_bud a) (e_bud b))
           (scope_lub (e_scp a) (e_scp b)).

Definition entry_merge_choose (a b : entry) : entry :=
  mk_entry (modality_choose (e_mod a) (e_mod b))
           (ops_union (e_ops a) (e_ops b))
           (budget_lub (e_bud a) (e_bud b))
           (scope_lub (e_scp a) (e_scp b)).

Lemma entry_merge_join_comm : forall a b, entry_eq (entry_merge_join a b) (entry_merge_join b a).
Proof.
  intros a b. unfold entry_eq, entry_merge_join; simpl. repeat split.
  - apply modality_join_comm.
  - apply ops_union_comm.
  - apply budget_lub_comm.
  - apply scope_lub_comm.
Qed.

Lemma entry_merge_join_assoc :
  forall a b c, entry_eq (entry_merge_join (entry_merge_join a b) c)
                         (entry_merge_join a (entry_merge_join b c)).
Proof.
  intros a b c. unfold entry_eq, entry_merge_join; simpl. repeat split.
  - apply modality_join_assoc.
  - apply ops_eq_sym. apply ops_union_assoc.
  - apply budget_lub_assoc.
  - apply scope_lub_assoc.
Qed.

Lemma entry_merge_join_idem : forall a, entry_eq (entry_merge_join a a) a.
Proof.
  intros a. unfold entry_eq, entry_merge_join; simpl. repeat split.
  - apply modality_join_idem.
  - apply ops_union_idem.
  - apply budget_lub_idem.
  - apply scope_lub_idem.
Qed.

Lemma entry_merge_sequence_assoc :
  forall a b c, entry_eq (entry_merge_sequence (entry_merge_sequence a b) c)
                         (entry_merge_sequence a (entry_merge_sequence b c)).
Proof.
  intros a b c. unfold entry_eq, entry_merge_sequence; simpl. repeat split.
  - apply modality_sequence_assoc.
  - apply ops_eq_sym. apply ops_union_assoc.
  - apply budget_sum_assoc.
  - apply scope_lub_assoc.
Qed.

Lemma entry_merge_choose_comm : forall a b, entry_eq (entry_merge_choose a b) (entry_merge_choose b a).
Proof.
  intros a b. unfold entry_eq, entry_merge_choose; simpl. repeat split.
  - apply modality_choose_comm.
  - apply ops_union_comm.
  - apply budget_lub_comm.
  - apply scope_lub_comm.
Qed.

Lemma entry_merge_choose_assoc :
  forall a b c, entry_eq (entry_merge_choose (entry_merge_choose a b) c)
                         (entry_merge_choose a (entry_merge_choose b c)).
Proof.
  intros a b c. unfold entry_eq, entry_merge_choose; simpl. repeat split.
  - apply modality_choose_assoc.
  - apply ops_eq_sym. apply ops_union_assoc.
  - apply budget_lub_assoc.
  - apply scope_lub_assoc.
Qed.

Lemma entry_merge_choose_idem : forall a, entry_eq (entry_merge_choose a a) a.
Proof.
  intros a. unfold entry_eq, entry_merge_choose; simpl. repeat split.
  - apply modality_choose_idem.
  - apply ops_union_idem.
  - apply budget_lub_idem.
  - apply scope_lub_idem.
Qed.

(* ------------------------------------------------------------------------- *)
(* cell: an entry plus its evidence set (row keeps both views together)       *)
(* ------------------------------------------------------------------------- *)
Definition provset := NameSet.t.
Record cell := mk_cell { c_entry : entry; c_prov : provset }.

Definition cell_eq (a b : cell) : Prop :=
  entry_eq (c_entry a) (c_entry b) /\ NameSet.Equal (c_prov a) (c_prov b).

Lemma cell_eq_refl : forall a, cell_eq a a.
Proof. intros. split; [apply entry_eq_refl | intro x; tauto]. Qed.

Definition merge_cell_join (a b : cell) : cell :=
  mk_cell (entry_merge_join (c_entry a) (c_entry b)) (NameSet.union (c_prov a) (c_prov b)).

Definition merge_cell_sequence (a b : cell) : cell :=
  mk_cell (entry_merge_sequence (c_entry a) (c_entry b)) (NameSet.union (c_prov a) (c_prov b)).

Definition merge_cell_choose (a b : cell) : cell :=
  mk_cell (entry_merge_choose (c_entry a) (c_entry b)) (NameSet.union (c_prov a) (c_prov b)).

Definition demote (c : cell) : cell :=
  mk_cell (mk_entry (modality_choose (e_mod (c_entry c)) must_not)
                    (e_ops (c_entry c)) (e_bud (c_entry c)) (e_scp (c_entry c)))
          (c_prov c).

Lemma merge_cell_join_comm : forall a b, cell_eq (merge_cell_join a b) (merge_cell_join b a).
Proof.
  intros a b. unfold cell_eq, merge_cell_join; simpl. split.
  - apply entry_merge_join_comm.
  - fsetdec.
Qed.

Lemma merge_cell_join_assoc :
  forall a b c, cell_eq (merge_cell_join (merge_cell_join a b) c)
                        (merge_cell_join a (merge_cell_join b c)).
Proof.
  intros a b c. unfold cell_eq, merge_cell_join; simpl. split.
  - apply entry_merge_join_assoc.
  - fsetdec.
Qed.

Lemma merge_cell_join_idem : forall a, cell_eq (merge_cell_join a a) a.
Proof.
  intros a. unfold cell_eq, merge_cell_join; simpl. split.
  - apply entry_merge_join_idem.
  - fsetdec.
Qed.

Lemma merge_cell_sequence_assoc :
  forall a b c, cell_eq (merge_cell_sequence (merge_cell_sequence a b) c)
                        (merge_cell_sequence a (merge_cell_sequence b c)).
Proof.
  intros a b c. unfold cell_eq, merge_cell_sequence; simpl. split.
  - apply entry_merge_sequence_assoc.
  - fsetdec.
Qed.

Lemma merge_cell_choose_comm : forall a b, cell_eq (merge_cell_choose a b) (merge_cell_choose b a).
Proof.
  intros a b. unfold cell_eq, merge_cell_choose; simpl. split.
  - apply entry_merge_choose_comm.
  - fsetdec.
Qed.

Lemma merge_cell_choose_assoc :
  forall a b c, cell_eq (merge_cell_choose (merge_cell_choose a b) c)
                        (merge_cell_choose a (merge_cell_choose b c)).
Proof.
  intros a b c. unfold cell_eq, merge_cell_choose; simpl. split.
  - apply entry_merge_choose_assoc.
  - fsetdec.
Qed.

Lemma merge_cell_choose_idem : forall a, cell_eq (merge_cell_choose a a) a.
Proof.
  intros a. unfold cell_eq, merge_cell_choose; simpl. split.
  - apply entry_merge_choose_idem.
  - fsetdec.
Qed.

(* ------------------------------------------------------------------------- *)
(* rows: finite map name -> cell (entry + evidence), canonical                *)
(* ------------------------------------------------------------------------- *)
Definition row := RowMap.t cell.
Definition empty_row : row := RowMap.empty cell.

Definition opt_cell_eq (a b : option cell) : Prop :=
  match a, b with
  | None, None => True
  | Some x, Some y => cell_eq x y
  | _, _ => False
  end.

Definition row_eq (a b : row) : Prop :=
  forall k, opt_cell_eq (RowMap.find k a) (RowMap.find k b).

Lemma row_eq_refl : forall a, row_eq a a.
Proof.
  intros a k. destruct (RowMap.find k a); simpl; [apply cell_eq_refl | auto].
Qed.

Lemma entry_eq_sym : forall a b, entry_eq a b -> entry_eq b a.
Proof.
  intros a b [Hm [Ho [Hb Hs]]]. unfold entry_eq. repeat split.
  - symmetry. exact Hm.
  - apply ops_eq_sym. exact Ho.
  - symmetry. exact Hb.
  - symmetry. exact Hs.
Qed.

Lemma cell_eq_sym : forall a b, cell_eq a b -> cell_eq b a.
Proof.
  intros a b [He Hp]. split.
  - apply entry_eq_sym. exact He.
  - intro x. specialize (Hp x). tauto.
Qed.

(* fold b into a', adding (h k cb) for every (k,cb) in b *)
Lemma find_fold_add2 : forall a' (h : RowMap.key -> cell -> cell) b k,
  RowMap.find k (RowMap.fold (fun k' cb acc => RowMap.add k' (h k' cb) acc) b a') =
  match RowMap.find k b with
  | None => RowMap.find k a'
  | Some cb => Some (h k cb)
  end.
Proof.
  intros a' h b k.
  set (f := fun k' (cb : cell) acc => RowMap.add k' (h k' cb) acc).
  apply (RowMapProps.fold_rec (elt := cell) (A := row)
           (P := fun (m : row) r => RowMap.find k r =
             match RowMap.find k m with
             | None => RowMap.find k a'
             | Some cb => Some (h k cb)
             end) (f := f) (i := a') (m := b)).
  - intros m Hempty.
    assert (Hf : RowMap.find k m = None).
    { apply RowMapFacts.not_find_in_iff. intro HIn. destruct HIn as [e He].
      apply (Hempty k e). exact He. }
    rewrite Hf. reflexivity.
  - intros k0 e a0 m' m'' Hmaps Hnotin Hadd IH.
    unfold f.
    destruct (string_dec k k0) as [Hkk0 | Hkk0].
    + subst k0.
      rewrite RowMapFacts.add_eq_o by reflexivity.
      rewrite (Hadd k).
      rewrite RowMapFacts.add_eq_o by reflexivity.
      reflexivity.
    + rewrite RowMapFacts.add_neq_o by (intro H; apply Hkk0; symmetry; exact H).
      rewrite (Hadd k).
      rewrite RowMapFacts.add_neq_o by (intro H; apply Hkk0; symmetry; exact H).
      apply IH.
Qed.

Definition id_cell (c : cell) : cell := c.

Definition row_merge3 (both : cell -> cell -> cell) (lft rgt : cell -> cell) (a b : row) : row :=
  let a' := RowMap.map lft a in
  let h := fun k (cb : cell) =>
             match RowMap.find k a with
             | Some ca => both ca cb
             | None => rgt cb
             end in
  RowMap.fold (fun k cb acc => RowMap.add k (h k cb) acc) b a'.

Lemma row_merge3_find : forall both lft rgt a b k,
  RowMap.find k (row_merge3 both lft rgt a b) =
  match RowMap.find k a, RowMap.find k b with
  | Some ca, Some cb => Some (both ca cb)
  | Some ca, None => Some (lft ca)
  | None, Some cb => Some (rgt cb)
  | None, None => None
  end.
Proof.
  intros both lft rgt a b k. unfold row_merge3.
  rewrite find_fold_add2.
  rewrite RowMapFacts.map_o.
  destruct (RowMap.find k a) as [ca|]; destruct (RowMap.find k b) as [cb|]; simpl; reflexivity.
Qed.

Definition join := row_merge3 merge_cell_join id_cell id_cell.
Definition sequence := row_merge3 merge_cell_sequence id_cell id_cell.
Definition choose := row_merge3 merge_cell_choose demote demote.

(* ------------------------------- join laws ------------------------------ *)
Lemma join_comm : forall a b, row_eq (join a b) (join b a).
Proof.
  intros a b k. unfold join. rewrite !row_merge3_find.
  destruct (RowMap.find k a) as [ca|]; destruct (RowMap.find k b) as [cb|]; simpl.
  - apply merge_cell_join_comm.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - auto.
Qed.

Lemma join_assoc : forall a b c, row_eq (join (join a b) c) (join a (join b c)).
Proof.
  intros a b c k. unfold join. rewrite !row_merge3_find.
  destruct (RowMap.find k a) as [ca|]; destruct (RowMap.find k b) as [cb|];
  destruct (RowMap.find k c) as [cc|]; simpl.
  - apply merge_cell_join_assoc.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - auto.
Qed.

Lemma join_idem : forall a, row_eq (join a a) a.
Proof.
  intros a k. unfold join. rewrite !row_merge3_find.
  destruct (RowMap.find k a) as [ca|]; simpl.
  - apply merge_cell_join_idem.
  - auto.
Qed.

Lemma join_identity_l : forall a, row_eq (join a empty_row) a.
Proof.
  intros a k. unfold join. rewrite !row_merge3_find, RowMapFacts.empty_o.
  destruct (RowMap.find k a) as [ca|]; simpl.
  - apply cell_eq_refl.
  - auto.
Qed.

Lemma join_identity_r : forall a, row_eq (join empty_row a) a.
Proof.
  intros a k. unfold join. rewrite !row_merge3_find, RowMapFacts.empty_o.
  destruct (RowMap.find k a) as [ca|]; simpl.
  - apply cell_eq_refl.
  - auto.
Qed.

(* ----------------------------- sequence laws --------------------------- *)
Lemma sequence_assoc : forall a b c, row_eq (sequence (sequence a b) c) (sequence a (sequence b c)).
Proof.
  intros a b c k. unfold sequence. rewrite !row_merge3_find.
  destruct (RowMap.find k a) as [ca|]; destruct (RowMap.find k b) as [cb|];
  destruct (RowMap.find k c) as [cc|]; simpl.
  - apply merge_cell_sequence_assoc.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - auto.
Qed.

Lemma sequence_identity_l : forall a, row_eq (sequence a empty_row) a.
Proof.
  intros a k. unfold sequence. rewrite !row_merge3_find, RowMapFacts.empty_o.
  destruct (RowMap.find k a) as [ca|]; simpl.
  - apply cell_eq_refl.
  - auto.
Qed.

Lemma sequence_identity_r : forall a, row_eq (sequence empty_row a) a.
Proof.
  intros a k. unfold sequence. rewrite !row_merge3_find, RowMapFacts.empty_o.
  destruct (RowMap.find k a) as [ca|]; simpl.
  - apply cell_eq_refl.
  - auto.
Qed.

(* sequence budgets sum (both sides present) *)
Lemma merge_cell_sequence_budget : forall a b,
  e_bud (c_entry (merge_cell_sequence a b)) = budget_sum (e_bud (c_entry a)) (e_bud (c_entry b)).
Proof. intros. reflexivity. Qed.

(* ------------------------------ choose laws ---------------------------- *)
Lemma modality_choose_must_not : forall m,
  modality_choose (modality_choose m must_not) must_not = modality_choose m must_not.
Proof. intros m; destruct m; reflexivity. Qed.

Lemma modality_choose_demote_l : forall a b,
  modality_choose (modality_choose a must_not) b = modality_choose (modality_choose a b) must_not.
Proof. intros a b; destruct a, b; reflexivity. Qed.

Lemma modality_choose_demote_mid : forall a b,
  modality_choose (modality_choose a must_not) b = modality_choose a (modality_choose b must_not).
Proof. intros a b; destruct a, b; reflexivity. Qed.

Lemma demote_idem : forall c, cell_eq (demote (demote c)) (demote c).
Proof.
  intros c. unfold cell_eq, demote. split.
  - unfold entry_eq; repeat split; try reflexivity;
    try apply modality_choose_must_not; try apply ops_eq_refl.
  - intro x; tauto.
Qed.

Lemma demote_merge_choose_r : forall a b,
  cell_eq (demote (merge_cell_choose a b)) (merge_cell_choose a (demote b)).
Proof.
  intros a b. unfold cell_eq, demote, merge_cell_choose, entry_merge_choose. split.
  - unfold entry_eq; simpl; repeat split; try reflexivity;
    try apply modality_choose_assoc; try apply ops_eq_refl.
  - intro x; split; intro H; apply NameSetFacts.union_iff in H;
      apply NameSetFacts.union_iff; tauto.
Qed.

Lemma merge_choose_demote_l : forall a b,
  cell_eq (merge_cell_choose (demote a) b) (merge_cell_choose a (demote b)).
Proof.
  intros a b. unfold cell_eq, demote, merge_cell_choose, entry_merge_choose. split.
  - unfold entry_eq; simpl; repeat split; try reflexivity;
    try apply modality_choose_demote_mid; try apply ops_eq_refl.
  - intro x; split; intro H; apply NameSetFacts.union_iff in H;
      apply NameSetFacts.union_iff; tauto.
Qed.

Lemma demote_merge_choose_l : forall a b,
  cell_eq (merge_cell_choose (demote a) b) (demote (merge_cell_choose a b)).
Proof.
  intros a b. unfold cell_eq, demote, merge_cell_choose, entry_merge_choose. split.
  - unfold entry_eq; simpl; repeat split; try reflexivity;
    try apply modality_choose_demote_l; try apply ops_eq_refl.
  - intro x; split; intro H; apply NameSetFacts.union_iff in H;
      apply NameSetFacts.union_iff; tauto.
Qed.

Lemma choose_comm : forall a b, row_eq (choose a b) (choose b a).
Proof.
  intros a b k. unfold choose. rewrite !row_merge3_find.
  destruct (RowMap.find k a) as [ca|]; destruct (RowMap.find k b) as [cb|]; simpl.
  - apply merge_cell_choose_comm.
  - apply cell_eq_refl.
  - apply cell_eq_refl.
  - auto.
Qed.

Lemma choose_assoc : forall a b c, row_eq (choose (choose a b) c) (choose a (choose b c)).
Proof.
  intros a b c k. unfold choose. rewrite !row_merge3_find.
  destruct (RowMap.find k a) as [ca|]; destruct (RowMap.find k b) as [cb|];
  destruct (RowMap.find k c) as [cc|]; simpl.
  - apply merge_cell_choose_assoc.
  - apply demote_merge_choose_r.
  - apply merge_choose_demote_l.
  - apply demote_idem.
  - apply demote_merge_choose_l.
  - apply cell_eq_refl.
  - apply cell_eq_sym. apply demote_idem.
  - auto.
Qed.

Lemma choose_idem : forall a, row_eq (choose a a) a.
Proof.
  intros a k. unfold choose. rewrite !row_merge3_find.
  destruct (RowMap.find k a) as [ca|]; simpl.
  - apply merge_cell_choose_idem.
  - auto.
Qed.

(* choose budget: both sides present -> max; one side -> that side's budget *)
Lemma merge_cell_choose_budget : forall a b,
  e_bud (c_entry (merge_cell_choose a b)) = budget_lub (e_bud (c_entry a)) (e_bud (c_entry b)).
Proof. intros. reflexivity. Qed.

(* choose: must survives only when every branch must *)
Lemma merge_cell_choose_modality : forall a b,
  e_mod (c_entry (merge_cell_choose a b)) = modality_choose (e_mod (c_entry a)) (e_mod (c_entry b)).
Proof. intros. reflexivity. Qed.

(* ------------------------------------------------------------------------- *)
(* row equality is an equivalence, and join respects it                       *)
(* ------------------------------------------------------------------------- *)
Lemma ops_eq_trans : forall a b c, ops_eq a b -> ops_eq b c -> ops_eq a c.
Proof.
  intros [a|] [b|] [c|] H1 H2; simpl in *; try contradiction; auto.
  intro x. specialize (H1 x). specialize (H2 x). tauto.
Qed.

Lemma entry_eq_trans : forall a b c, entry_eq a b -> entry_eq b c -> entry_eq a c.
Proof.
  intros a b c [Hm1 [Ho1 [Hb1 Hs1]]] [Hm2 [Ho2 [Hb2 Hs2]]].
  unfold entry_eq. repeat split.
  - transitivity (e_mod b); auto.
  - eapply ops_eq_trans; eauto.
  - transitivity (e_bud b); auto.
  - transitivity (e_scp b); auto.
Qed.

Lemma cell_eq_trans : forall a b c, cell_eq a b -> cell_eq b c -> cell_eq a c.
Proof.
  intros a b c [He1 Hp1] [He2 Hp2]. split.
  - eapply entry_eq_trans; eauto.
  - intro x. specialize (Hp1 x). specialize (Hp2 x). tauto.
Qed.

Lemma row_eq_sym : forall a b, row_eq a b -> row_eq b a.
Proof.
  intros a b H k. specialize (H k).
  destruct (RowMap.find k a); destruct (RowMap.find k b); simpl in *; try auto.
  apply cell_eq_sym. exact H.
Qed.

Lemma row_eq_trans : forall a b c, row_eq a b -> row_eq b c -> row_eq a c.
Proof.
  intros a b c H1 H2 k. specialize (H1 k). specialize (H2 k).
  destruct (RowMap.find k a); destruct (RowMap.find k b); destruct (RowMap.find k c);
  simpl in *; try contradiction; try auto.
  eapply cell_eq_trans; eauto.
Qed.

Lemma ops_union_mor : forall a a' b b', ops_eq a a' -> ops_eq b b' -> ops_eq (ops_union a b) (ops_union a' b').
Proof.
  intros [a|] [a'|] [b|] [b'|]; simpl; try contradiction; auto.
  intros Ha Hb x. destruct (Ha x) as [Ha1 Ha2]. destruct (Hb x) as [Hb1 Hb2].
  split; intro H.
  + rewrite NameSetFacts.union_iff in H. rewrite NameSetFacts.union_iff.
    destruct H as [H|H]; [left; apply Ha1; exact H | right; apply Hb1; exact H].
  + rewrite NameSetFacts.union_iff in H. rewrite NameSetFacts.union_iff.
    destruct H as [H|H]; [left; apply Ha2; exact H | right; apply Hb2; exact H].
Qed.

Lemma entry_merge_join_mor : forall a a' b b',
  entry_eq a a' -> entry_eq b b' -> entry_eq (entry_merge_join a b) (entry_merge_join a' b').
Proof.
  intros a a' b b' [Hma [Hoa [Hba Hsa]]] [Hmb [Hob [Hbb Hsb]]].
  unfold entry_eq, entry_merge_join. repeat split.
  - rewrite Hma, Hmb. reflexivity.
  - apply (ops_union_mor _ _ _ _ Hoa Hob).
  - rewrite Hba, Hbb. reflexivity.
  - rewrite Hsa, Hsb. reflexivity.
Qed.

Lemma merge_cell_join_mor : forall a a' b b',
  cell_eq a a' -> cell_eq b b' -> cell_eq (merge_cell_join a b) (merge_cell_join a' b').
Proof.
  intros a a' b b' [Hea Hpa] [Heb Hpb]. unfold cell_eq, merge_cell_join; simpl. split.
  - apply (entry_merge_join_mor _ _ _ _ Hea Heb).
  - intro x. destruct (Hpa x) as [Hpa1 Hpa2]. destruct (Hpb x) as [Hpb1 Hpb2].
    split; intro H.
    + rewrite NameSetFacts.union_iff in H. rewrite NameSetFacts.union_iff.
      destruct H as [H|H]; [left; apply Hpa1; exact H | right; apply Hpb1; exact H].
    + rewrite NameSetFacts.union_iff in H. rewrite NameSetFacts.union_iff.
      destruct H as [H|H]; [left; apply Hpa2; exact H | right; apply Hpb2; exact H].
Qed.

Lemma join_mor : forall a a' b b', row_eq a a' -> row_eq b b' -> row_eq (join a b) (join a' b').
Proof.
  intros a a' b b' Ha Hb k. unfold join. rewrite !row_merge3_find.
  specialize (Ha k). specialize (Hb k).
  destruct (RowMap.find k a) as [ca|]; destruct (RowMap.find k a') as [ca'|];
  destruct (RowMap.find k b) as [cb|]; destruct (RowMap.find k b') as [cb'|];
  simpl in *; try contradiction; try auto.
  - apply (merge_cell_join_mor ca ca' cb cb'); auto.
Qed.

Require Import Coq.Setoids.Setoid Coq.Classes.Morphisms.

Add Parametric Relation : row row_eq
  reflexivity proved by row_eq_refl
  symmetry proved by row_eq_sym
  transitivity proved by row_eq_trans
  as row_eq_rel.

Add Parametric Morphism : join with signature row_eq ==> row_eq ==> row_eq as join_mor_proper.
Proof. intros a a' Ha b b' Hb. apply join_mor; auto. Qed.

(* ------------------------------------------------------------------------- *)
(* join order (row-leq? in the Racket): a <= b iff join a b == b              *)
(* ------------------------------------------------------------------------- *)
Definition row_le (a b : row) : Prop := row_eq (join a b) b.

Lemma row_le_refl : forall a, row_le a a.
Proof. intros a. unfold row_le. apply join_idem. Qed.

Lemma join_absorb_l : forall a c, row_eq (join c (join a c)) (join a c).
Proof.
  intros a c.
  apply row_eq_trans with (join c (join c a)).
  - apply join_mor; [reflexivity | apply join_comm].
  - apply row_eq_trans with (join (join c c) a).
    + apply row_eq_sym. apply join_assoc.
    + apply row_eq_trans with (join c a).
      * apply join_mor; [apply join_idem | reflexivity].
      * apply join_comm.
Qed.

Lemma join_monotone_l : forall a b c, row_le a b -> row_le (join a c) (join b c).
Proof.
  intros a b c H. unfold row_le in *.
  apply row_eq_trans with (join b (join a c)).
  - apply row_eq_trans with (join (join b c) (join a c)).
    + apply join_comm.
    + apply row_eq_trans with (join b (join c (join a c))).
      * apply join_assoc.
      * apply join_mor; [reflexivity | apply join_absorb_l].
  - apply row_eq_trans with (join (join b a) c).
    + apply row_eq_sym. apply join_assoc.
    + apply row_eq_trans with (join (join a b) c).
      * apply join_mor; [apply join_comm | reflexivity].
      * apply row_eq_trans with (join b c).
        -- apply join_mor; [exact H | reflexivity].
        -- reflexivity.
Qed.

Lemma row_le_trans : forall a b c, row_le a b -> row_le b c -> row_le a c.
Proof.
  intros a b c Hab Hbc. unfold row_le in *.
  apply row_eq_trans with (join a (join b c)).
  - apply join_mor; [reflexivity | apply row_eq_sym; exact Hbc].
  - apply row_eq_trans with (join (join a b) c).
    + apply row_eq_sym. apply join_assoc.
    + apply row_eq_trans with (join b c).
      * apply join_mor; [exact Hab | reflexivity].
      * exact Hbc.
Qed.

Lemma join_absorb_r : forall a c, row_eq (join a (join a c)) (join a c).
Proof.
  intros a c. apply row_eq_trans with (join (join a a) c).
  - apply row_eq_sym. apply join_assoc.
  - apply join_mor; [apply join_idem | reflexivity].
Qed.

Lemma join_comm_le : forall a c, row_le (join c a) (join a c).
Proof.
  intros a c. unfold row_le.
  apply row_eq_trans with (join (join a c) (join c a)).
  - apply join_comm.
  - apply row_eq_trans with (join a (join c (join c a))).
    + apply join_assoc.
    + apply row_eq_trans with (join a (join c a)).
      * apply join_mor; [reflexivity | apply join_absorb_r].
      * apply row_eq_trans with (join a (join a c)).
        -- apply join_mor; [reflexivity | apply join_comm].
        -- apply join_absorb_r.
Qed.

Lemma join_monotone_r : forall a b c, row_le a b -> row_le (join c a) (join c b).
Proof.
  intros a b c H.
  apply row_le_trans with (join b c).
  - apply row_le_trans with (join a c).
    + apply join_comm_le.
    + apply join_monotone_l. exact H.
  - apply join_comm_le.
Qed.

Lemma join_monotone_le : forall a b c d, row_le a b -> row_le c d -> row_le (join a c) (join b d).
Proof.
  intros a b c d Hab Hcd.
  apply row_le_trans with (join b c).
  - apply join_monotone_l. exact Hab.
  - apply join_monotone_r. exact Hcd.
Qed.

(* fixpoint-step monotonicity, pointwise in the join order (property-test law) *)
Definition node := RowMap.key.
Definition rowtable := RowMap.t row.

Definition node_row (t : rowtable) (k : node) : row :=
  match RowMap.find k t with Some r => r | None => empty_row end.

Definition rows_le (r1 r2 : rowtable) : Prop :=
  forall k, row_le (node_row r1 k) (node_row r2 k).

Definition step_row (direct : rowtable) (edges : node -> list node) (rows : rowtable) (k : node) : row :=
  fold_left (fun acc c => join acc (node_row rows c)) (edges k) (node_row direct k).

Lemma fold_left_join_monotone : forall (l : list node) (rows1 rows2 : rowtable),
  (forall c, row_le (node_row rows1 c) (node_row rows2 c)) ->
  forall base1 base2, row_le base1 base2 ->
  row_le (fold_left (fun acc c => join acc (node_row rows1 c)) l base1)
         (fold_left (fun acc c => join acc (node_row rows2 c)) l base2).
Proof.
  induction l as [|x l IH]; intros rows1 rows2 H base1 base2 Hb; simpl.
  - exact Hb.
  - apply IH; auto. apply join_monotone_le; auto.
Qed.

Lemma fixpoint_step_monotone : forall direct edges rows1 rows2,
  rows_le rows1 rows2 ->
  forall k, row_le (step_row direct edges rows1 k) (step_row direct edges rows2 k).
Proof.
  intros direct edges rows1 rows2 H k. unfold step_row.
  apply fold_left_join_monotone; auto.
  apply row_le_refl.
Qed.

(* ------------------------------------------------------------------------- *)
(* subtyping: the contract order (effect-subtype? in contract.rkt), over the   *)
(* algebra's effective modalities (may/unknown/must; must-not is absence).    *)
(* ------------------------------------------------------------------------- *)
Definition modality_subtype (a b : modality) : Prop :=
  match a with
  | must => b = must
  | may => b = may \/ b = unknown \/ b = must
  | unknown => b = may \/ b = unknown \/ b = must
  | must_not => True
  end.

Lemma modality_subtype_refl : forall a, modality_subtype a a.
Proof. intros a; destruct a; simpl; auto. Qed.

Lemma modality_join_must : forall b, modality_join must b = must.
Proof. intros b; destruct b; reflexivity. Qed.

Lemma modality_join_ub_l : forall a b, modality_subtype a (modality_join a b).
Proof. intros a b; destruct a, b; simpl; auto. Qed.

Lemma modality_join_ub_r : forall a b, modality_subtype b (modality_join a b).
Proof. intros a b; destruct a, b; simpl; auto. Qed.

Lemma modality_join_lub : forall a b c,
  modality_subtype a c -> modality_subtype b c -> modality_subtype (modality_join a b) c.
Proof. intros a b c H1 H2; destruct a, b, c; simpl in *; auto. Qed.

Definition budget_le (a b : budget) : Prop :=
  match b with
  | None => True
  | Some m => match a with None => False | Some n => n <= m end
  end.

Lemma budget_le_refl : forall a, budget_le a a.
Proof. intros [a|]; simpl; lia. Qed.

Lemma budget_le_trans : forall a b c, budget_le a b -> budget_le b c -> budget_le a c.
Proof.
  intros [a|] [b|] [c|]; simpl; intros H1 H2; try contradiction; try lia; auto.
Qed.

Lemma budget_lub_ub_l : forall a b, budget_le a (budget_lub a b).
Proof. intros [a|] [b|]; simpl; try lia; auto. Qed.

Lemma budget_lub_ub_r : forall a b, budget_le b (budget_lub a b).
Proof. intros [a|] [b|]; simpl; try lia; auto. Qed.

Lemma budget_lub_lub : forall a b c,
  budget_le a c -> budget_le b c -> budget_le (budget_lub a b) c.
Proof.
  intros [a|] [b|] [c|]; simpl; intros H1 H2; try contradiction; try lia; auto.
Qed.

Definition ops_superset (super sub : ops) : Prop :=
  match super, sub with
  | None, _ => True
  | Some s, Some t => NameSet.Subset t s
  | Some _, None => False
  end.

Lemma ops_superset_refl : forall a, ops_superset a a.
Proof. intros [a|]; simpl; auto. intro x; tauto. Qed.

Lemma ops_superset_trans : forall a b c, ops_superset a b -> ops_superset b c -> ops_superset a c.
Proof.
  intros [a|] [b|] [c|]; simpl; intros H1 H2; try contradiction; try auto.
  intros x Hx. apply H1. apply H2. exact Hx.
Qed.

Lemma ops_union_ub_l : forall a b, ops_superset (ops_union a b) a.
Proof.
  intros [a|] [b|]; simpl; auto.
  intros x Hx. apply NameSetFacts.union_iff. left. exact Hx.
Qed.

Lemma ops_union_ub_r : forall a b, ops_superset (ops_union a b) b.
Proof.
  intros [a|] [b|]; simpl; auto.
  intros x Hx. apply NameSetFacts.union_iff. right. exact Hx.
Qed.

Lemma ops_union_lub : forall a b c,
  ops_superset c a -> ops_superset c b -> ops_superset c (ops_union a b).
Proof.
  intros [a|] [b|] [c|]; simpl; intros H1 H2; try contradiction; try auto.
  intros x Hx. apply NameSetFacts.union_iff in Hx. destruct Hx as [Hx|Hx].
  - apply H1. exact Hx.
  - apply H2. exact Hx.
Qed.

(* entry subtyping: faithful to effect-subtype? — `must` compares budget and
   ops exactly like `may`/`unknown`, differing only in the modality target: an
   obligation must be met by an obligation, never a mere permission. *)
Definition entry_subtype (a b : entry) : Prop :=
  match e_mod a with
  | must => e_mod b = must /\ budget_le (e_bud a) (e_bud b) /\ ops_superset (e_ops b) (e_ops a)
  | may => modality_subtype (e_mod a) (e_mod b) /\ budget_le (e_bud a) (e_bud b) /\ ops_superset (e_ops b) (e_ops a)
  | unknown => modality_subtype (e_mod a) (e_mod b) /\ budget_le (e_bud a) (e_bud b) /\ ops_superset (e_ops b) (e_ops a)
  | must_not => True
  end.

Lemma modality_subtype_trans : forall a b c,
  modality_subtype a b -> modality_subtype b c -> modality_subtype a c.
Proof.
  intros a b c H1 H2. destruct a, b, c; simpl in *; try contradiction; firstorder congruence.
Qed.

Lemma entry_subtype_refl : forall a, entry_subtype a a.
Proof.
  intros a. unfold entry_subtype. destruct (e_mod a); simpl.
  - exact I.
  - split; [| split].
    + tauto.
    + apply (budget_le_refl (e_bud a)).
    + apply (ops_superset_refl (e_ops a)).
  - split; [| split].
    + tauto.
    + apply (budget_le_refl (e_bud a)).
    + apply (ops_superset_refl (e_ops a)).
  - split; [| split].
    + reflexivity.
    + apply (budget_le_refl (e_bud a)).
    + apply (ops_superset_refl (e_ops a)).
Qed.

Lemma entry_subtype_join_ub_l : forall a b, entry_subtype a (entry_merge_join a b).
Proof.
  intros a b. unfold entry_subtype, entry_merge_join. destruct (e_mod a); simpl.
  - exact I.
  - split; [| split].
    + destruct (e_mod b); simpl; tauto.
    + apply (budget_lub_ub_l (e_bud a) (e_bud b)).
    + apply (ops_union_ub_l (e_ops a) (e_ops b)).
  - split; [| split].
    + destruct (e_mod b); simpl; tauto.
    + apply (budget_lub_ub_l (e_bud a) (e_bud b)).
    + apply (ops_union_ub_l (e_ops a) (e_ops b)).
  - split; [| split].
    + reflexivity.
    + apply (budget_lub_ub_l (e_bud a) (e_bud b)).
    + apply (ops_union_ub_l (e_ops a) (e_ops b)).
Qed.

Lemma entry_subtype_join_ub_r : forall a b, entry_subtype b (entry_merge_join a b).
Proof.
  intros a b. unfold entry_subtype, entry_merge_join. destruct (e_mod b); simpl.
  - exact I.
  - split; [| split].
    + destruct (e_mod a); simpl; tauto.
    + apply (budget_lub_ub_r (e_bud a) (e_bud b)).
    + apply (ops_union_ub_r (e_ops a) (e_ops b)).
  - split; [| split].
    + destruct (e_mod a); simpl; tauto.
    + apply (budget_lub_ub_r (e_bud a) (e_bud b)).
    + apply (ops_union_ub_r (e_ops a) (e_ops b)).
  - split; [| split].
    + rewrite modality_join_comm; apply (modality_join_must (e_mod a)).
    + apply (budget_lub_ub_r (e_bud a) (e_bud b)).
    + apply (ops_union_ub_r (e_ops a) (e_ops b)).
Qed.

Definition wf_entry (e : entry) : Prop := e_mod e <> must_not.
Definition wf_row (r : row) : Prop :=
  forall k, match RowMap.find k r with Some cl => wf_entry (c_entry cl) | None => True end.

(* For a non-prohibited entry, subtyping is exactly the modality order plus the
   budget and ops orders; this uniform shape is what makes join a least upper
   bound and subtyping transitive. *)
Lemma entry_subtype_wf : forall a b,
  wf_entry a ->
  entry_subtype a b <-> modality_subtype (e_mod a) (e_mod b) /\ budget_le (e_bud a) (e_bud b) /\ ops_superset (e_ops b) (e_ops a).
Proof.
  intros a b WFa. unfold entry_subtype, wf_entry in *. destruct (e_mod a) as [| | |] eqn:Ea; simpl in *.
  - exfalso. apply WFa. reflexivity.
  - tauto.
  - tauto.
  - tauto.
Qed.

Lemma modality_join_wf : forall a b,
  a <> must_not -> b <> must_not -> modality_join a b <> must_not.
Proof.
  intros a b Ha Hb.
  destruct a as [| | |], b as [| | |]; simpl in *;
    try (exfalso; apply Ha; reflexivity);
    try (exfalso; apply Hb; reflexivity);
    intro H; discriminate.
Qed.

Lemma entry_subtype_codom_wf : forall a b,
  wf_entry a -> entry_subtype a b -> wf_entry b.
Proof.
  intros a b WFa Hab. unfold wf_entry in *.
  destruct (e_mod a) as [| | |] eqn:Ea; try (exfalso; apply WFa; reflexivity).
  all: unfold entry_subtype in Hab; rewrite Ea in Hab; simpl in Hab; destruct Hab as [Hm _].
  - destruct (e_mod b) as [| | |] eqn:Eb; simpl in Hm; try (intro H; discriminate); firstorder congruence.
  - destruct (e_mod b) as [| | |] eqn:Eb; simpl in Hm; try (intro H; discriminate); firstorder congruence.
  - rewrite Hm. intro H. discriminate.
Qed.

Lemma entry_subtype_join_lub : forall a b c,
  wf_entry a -> wf_entry b ->
  entry_subtype a c -> entry_subtype b c -> entry_subtype (entry_merge_join a b) c.
Proof.
  intros a b c WFa WFb Ha Hb.
  assert (WFj : wf_entry (entry_merge_join a b)).
  { unfold wf_entry, entry_merge_join. simpl. apply modality_join_wf; auto. }
  apply (proj2 (entry_subtype_wf (entry_merge_join a b) c WFj)).
  destruct (proj1 (entry_subtype_wf a c WFa) Ha) as [Ha_m [Ha_b Ha_o]].
  destruct (proj1 (entry_subtype_wf b c WFb) Hb) as [Hb_m [Hb_b Hb_o]].
  unfold entry_merge_join. simpl. repeat split.
  + apply (modality_join_lub _ _ _ Ha_m Hb_m).
  + apply (budget_lub_lub _ _ _ Ha_b Hb_b).
  + apply (ops_union_lub _ _ _ Ha_o Hb_o).
Qed.

Lemma entry_subtype_trans : forall a b c,
  entry_subtype a b -> entry_subtype b c -> entry_subtype a c.
Proof.
  intros a b c Hab Hbc.
  destruct (e_mod a) as [| | |] eqn:Ea.
  - unfold entry_subtype. rewrite Ea. exact I.
  - assert (WFa : wf_entry a) by (unfold wf_entry; rewrite Ea; intro H; discriminate).
    assert (WFb : wf_entry b) by (apply (entry_subtype_codom_wf a b WFa Hab)).
    destruct (proj1 (entry_subtype_wf a b WFa) Hab) as [Hab_m [Hab_b Hab_o]].
    destruct (proj1 (entry_subtype_wf b c WFb) Hbc) as [Hbc_m [Hbc_b Hbc_o]].
    apply (proj2 (entry_subtype_wf a c WFa)).
    split; [| split].
    + apply (modality_subtype_trans _ _ _ Hab_m Hbc_m).
    + apply (budget_le_trans _ _ _ Hab_b Hbc_b).
    + apply (ops_superset_trans (e_ops c) (e_ops b) (e_ops a) Hbc_o Hab_o).
  - assert (WFa : wf_entry a) by (unfold wf_entry; rewrite Ea; intro H; discriminate).
    assert (WFb : wf_entry b) by (apply (entry_subtype_codom_wf a b WFa Hab)).
    destruct (proj1 (entry_subtype_wf a b WFa) Hab) as [Hab_m [Hab_b Hab_o]].
    destruct (proj1 (entry_subtype_wf b c WFb) Hbc) as [Hbc_m [Hbc_b Hbc_o]].
    apply (proj2 (entry_subtype_wf a c WFa)).
    split; [| split].
    + apply (modality_subtype_trans _ _ _ Hab_m Hbc_m).
    + apply (budget_le_trans _ _ _ Hab_b Hbc_b).
    + apply (ops_superset_trans (e_ops c) (e_ops b) (e_ops a) Hbc_o Hab_o).
  - assert (WFa : wf_entry a) by (unfold wf_entry; rewrite Ea; intro H; discriminate).
    assert (WFb : wf_entry b) by (apply (entry_subtype_codom_wf a b WFa Hab)).
    destruct (proj1 (entry_subtype_wf a b WFa) Hab) as [Hab_m [Hab_b Hab_o]].
    destruct (proj1 (entry_subtype_wf b c WFb) Hbc) as [Hbc_m [Hbc_b Hbc_o]].
    apply (proj2 (entry_subtype_wf a c WFa)).
    split; [| split].
    + apply (modality_subtype_trans _ _ _ Hab_m Hbc_m).
    + apply (budget_le_trans _ _ _ Hab_b Hbc_b).
    + apply (ops_superset_trans (e_ops c) (e_ops b) (e_ops a) Hbc_o Hab_o).
Qed.

Lemma entry_subtype_must_not_codom : forall a b,
  e_mod b = must_not -> entry_subtype a b -> e_mod a = must_not.
Proof.
  intros a b Hb Hab. unfold entry_subtype in Hab. rewrite Hb in Hab.
  destruct (e_mod a) as [| | |] eqn:Ea; simpl in Hab.
  - reflexivity.
  - destruct Hab as [Hm _]. simpl in Hm. firstorder congruence.
  - destruct Hab as [Hm _]. simpl in Hm. firstorder congruence.
  - destruct Hab as [Hm _]. discriminate.
Qed.

(* row subtyping, faithful to row-subtype? (effect-subtype? on algebra rows) *)
Definition row_subtype (a b : row) : Prop :=
  forall k, match RowMap.find k a with
  | None => True
  | Some ea => match RowMap.find k b with
               | None => e_mod (c_entry ea) = must_not
               | Some eb => entry_subtype (c_entry ea) (c_entry eb)
               end
  end.

Lemma row_subtype_refl : forall a, row_subtype a a.
Proof.
  intros a k. destruct (RowMap.find k a) as [ea|]; simpl; auto.
  apply entry_subtype_refl.
Qed.

Lemma empty_row_bottom : forall a, row_subtype empty_row a.
Proof.
  intros a k. rewrite RowMapFacts.empty_o. simpl. auto.
Qed.

Lemma row_subtype_join_ub_l : forall a b, wf_row a -> row_subtype a (join a b).
Proof.
  intros a b WF k. unfold join. rewrite row_merge3_find.
  destruct (RowMap.find k a) as [ea|]; destruct (RowMap.find k b) as [eb|]; simpl; auto.
  - apply entry_subtype_join_ub_l.
  - apply entry_subtype_refl.
Qed.

Lemma row_subtype_join_ub_r : forall a b, wf_row b -> row_subtype b (join a b).
Proof.
  intros a b WF k. unfold join. rewrite row_merge3_find.
  destruct (RowMap.find k a) as [ea|]; destruct (RowMap.find k b) as [eb|]; simpl; auto.
  - apply entry_subtype_join_ub_r.
  - apply entry_subtype_refl.
Qed.

Lemma row_subtype_join_lub : forall a b c,
  wf_row a -> wf_row b ->
  row_subtype a c -> row_subtype b c -> row_subtype (join a b) c.
Proof.
  intros a b c WFa WFb Ha Hb k. unfold join. rewrite row_merge3_find.
  specialize (Ha k). specialize (Hb k).
  destruct (RowMap.find k a) as [ea|] eqn:Ea;
  destruct (RowMap.find k b) as [eb|] eqn:Eb;
  destruct (RowMap.find k c) as [ec|] eqn:Ec; simpl in *; try auto.
  - apply entry_subtype_join_lub; auto.
    + unfold wf_row in WFa. specialize (WFa k). rewrite Ea in WFa. simpl in WFa. exact WFa.
    + unfold wf_row in WFb. specialize (WFb k). rewrite Eb in WFb. simpl in WFb. exact WFb.
  - rewrite Ha, Hb. reflexivity.
Qed.

(* Transitivity of the contract order: with `must` comparing budget and ops,
   the order is genuinely transitive, so row-subtype? is a preorder. *)
Lemma row_subtype_trans : forall a b c,
  row_subtype a b -> row_subtype b c -> row_subtype a c.
Proof.
  intros a b c Hab Hbc k.
  specialize (Hab k). specialize (Hbc k).
  destruct (RowMap.find k a) as [ea|] eqn:Ea; simpl in *; auto.
  destruct (RowMap.find k b) as [eb|] eqn:Eb; simpl in *.
  - destruct (RowMap.find k c) as [ec|] eqn:Ec; simpl in *.
    + apply (entry_subtype_trans _ _ _ Hab Hbc).
    + exact (entry_subtype_must_not_codom _ _ Hbc Hab).
  - destruct (RowMap.find k c) as [ec|] eqn:Ec; simpl in *.
    + unfold entry_subtype. rewrite Hab. simpl. exact I.
    + rewrite Hab. reflexivity.
Qed.

(* ------------------------------------------------------------------------- *)
(* scope: discharge the handled names (row restriction by a name set)         *)
(* ------------------------------------------------------------------------- *)
Definition filter_row (keep : node -> cell -> bool) (r : row) : row :=
  RowMap.fold (fun k v acc => if keep k v then RowMap.add k v acc else acc) r (RowMap.empty cell).

Lemma find_filter_row : forall keep r k,
  RowMap.find k (filter_row keep r) =
  match RowMap.find k r with
  | Some v => if keep k v then Some v else None
  | None => None
  end.
Proof.
  intros keep r k. unfold filter_row.
  set (f := fun k' (v : cell) acc => if keep k' v then RowMap.add k' v acc else acc).
  apply (RowMapProps.fold_rec (elt := cell) (A := row)
    (P := fun (m : row) acc => RowMap.find k acc =
      match RowMap.find k m with
      | Some v => if keep k v then Some v else None
      | None => None
      end) (f := f) (i := RowMap.empty cell) (m := r)).
  - intros m Hempty.
    assert (Hf : RowMap.find k m = None).
    { apply RowMapFacts.not_find_in_iff. intro HIn. destruct HIn as [e He].
      apply (Hempty k e). exact He. }
    rewrite Hf. rewrite RowMapFacts.empty_o. reflexivity.
  - intros k0 e a0 m' m'' Hmaps Hnotin Hadd IH. unfold f.
    destruct (string_dec k k0) as [Hkk0 | Hkk0].
    + subst k0. destruct (keep k e) eqn:Ekeep.
      * rewrite RowMapFacts.add_eq_o by reflexivity.
        rewrite (Hadd k). rewrite RowMapFacts.add_eq_o by reflexivity.
        rewrite Ekeep. reflexivity.
      * rewrite (Hadd k). rewrite RowMapFacts.add_eq_o by reflexivity.
        rewrite Ekeep.
        apply RowMapFacts.not_find_in_iff in Hnotin.
        rewrite Hnotin in IH. simpl in IH. exact IH.
    + rewrite (Hadd k).
      rewrite RowMapFacts.add_neq_o by (intro H; apply Hkk0; symmetry; exact H).
      destruct (keep k0 e) eqn:Ekeep.
      * rewrite RowMapFacts.add_neq_o by (intro H; apply Hkk0; symmetry; exact H).
        apply IH.
      * apply IH.
Qed.

Definition scope (r : row) (handled : NameSet.t) : row * row :=
  (filter_row (fun k _ => negb (NameSet.mem k handled)) r,
   filter_row (fun k _ => NameSet.mem k handled) r).

Lemma scope_split : forall r h, row_eq (join (fst (scope r h)) (snd (scope r h))) r.
Proof.
  intros r h k. unfold scope, join; simpl. rewrite !row_merge3_find.
  rewrite !find_filter_row.
  destruct (RowMap.find k r) as [v|]; simpl.
  - destruct (NameSet.mem k h) eqn:E; simpl; apply cell_eq_refl.
  - auto.
Qed.

Lemma scope_remaining_no_handled : forall r h k,
  NameSet.In k h -> RowMap.find k (fst (scope r h)) = None.
Proof.
  intros r h k Hk. unfold scope; simpl. rewrite find_filter_row.
  destruct (RowMap.find k r) as [v|]; simpl.
  - apply NameSetFacts.mem_iff in Hk. rewrite Hk. reflexivity.
  - reflexivity.
Qed.

Lemma scope_identity : forall r,
  row_eq (fst (scope r NameSet.empty)) r /\
  row_eq (snd (scope r NameSet.empty)) empty_row.
Proof.
  intros r. split.
  - intros k. unfold scope; simpl. rewrite find_filter_row.
    destruct (RowMap.find k r) as [v|]; simpl; auto.
    apply cell_eq_refl.
  - intros k. unfold scope; simpl. rewrite find_filter_row, RowMapFacts.empty_o.
    destruct (RowMap.find k r) as [v|]; simpl; auto.
Qed.

(* ------------------------------------------------------------------------- *)
(* certificate checker: the kernel's decision (certificate.rkt)               *)
(* ------------------------------------------------------------------------- *)
Definition claim := row.

Definition grounded_claim (c : claim) : Prop :=
  forall k cl, RowMap.find k c = Some cl -> exists s, NameSet.In s (c_prov cl).

Definition interprets (interp : node -> list node) (h e : node) : Prop :=
  List.In e (interp h).

Definition interpretable_claim (c : claim) (needs : list node) (interp : node -> list node) : Prop :=
  forall k, RowMap.find k c <> None -> exists h, List.In h needs /\ interprets interp h k.

(* certificate.rkt's check-claim is (and (null? ungrounded) (null? violations));
   the two conjuncts are exactly grounded_claim and interpretable_claim. *)
Definition check_claim_ok (c : claim) (needs : list node) (interp : node -> list node) : Prop :=
  grounded_claim c /\ interpretable_claim c needs interp.

(* Soundness: the kernel never accepts an ungrounded claim. *)
Lemma check_claim_grounding : forall c needs interp,
  check_claim_ok c needs interp -> grounded_claim c.
Proof. intros c n i [Hg Hi]. exact Hg. Qed.

(* Soundness: the kernel never accepts an uncoverable claim. *)
Lemma check_claim_coverage : forall c needs interp,
  check_claim_ok c needs interp -> interpretable_claim c needs interp.
Proof. intros c n i [Hg Hi]. exact Hi. Qed.

(* Mutation test 3 (dropped evidence): a claim whose effect carries no evidence
   is rejected, even when its effect is covered. *)
Lemma dropped_evidence_rejected : forall c needs interp k cl,
  RowMap.find k c = Some cl ->
  NameSet.Empty (c_prov cl) ->
  ~ check_claim_ok c needs interp.
Proof.
  intros c needs interp k cl Hfind Hempty [Hg Hi].
  specialize (Hg k cl Hfind). destruct Hg as [s Hs].
  apply (Hempty s Hs).
Qed.
