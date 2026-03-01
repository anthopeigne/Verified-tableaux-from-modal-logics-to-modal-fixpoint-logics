
type __ = Obj.t

type nat =
| O
| S of nat

val fst : ('a1*'a2) -> 'a1

val snd : ('a1*'a2) -> 'a2



type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| ExistT of 'a * 'p



val min : nat -> nat -> nat

val bool_dec : bool -> bool -> bool

module Nat :
 sig
  val eq_dec : nat -> nat -> bool
 end

val lt_eq_lt_dec : nat -> nat -> bool option

val le_lt_dec : nat -> nat -> bool

val le_lt_eq_dec : nat -> nat -> bool

val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list

val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val in_dec0 : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val incl_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

type 'a set = 'a list

val set_mem : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 set -> bool

val set_inter : ('a1 -> 'a1 -> bool) -> 'a1 set -> 'a1 set -> 'a1 set

val eqset_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

val all_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> bool

type nnf =
| Var of string
| Neg of string
| And of nnf * nnf
| Or of nnf * nnf
| Bef of nnf * nnf
| Unt of nnf * nnf
| Next of nnf

val nnf_true : nnf

val nnf_false : nnf

val nnf_eqdec : nnf -> nnf -> bool

val inter_nnf : nnf set -> nnf set -> nnf set

val remove_nnf : nnf -> nnf list -> nnf list

val neg_nnf : nnf -> nnf

val nnf_isalpha_dec : nnf -> bool

val get_alpha : nnf list -> nnf option

val df_nnf : nnf

val nnf_alpha1 : nnf -> nnf

val nnf_alpha2 : nnf -> nnf

val nnf_isbeta_dec : nnf -> bool

val get_beta : nnf list -> nnf option

val nnf_beta1 : nnf -> nnf

val nnf_beta2 : nnf -> nnf

type doseqt = { s_G : nnf list; s_H : (nat*nnf list) list; s_d : nat;
                s_c : bool; s_R : nnf list }

val seqt_eqdec : doseqt -> doseqt -> bool

type rdoseqt = doseqt

type upseqt = { s_uev : nnf list; s_lp : nat }

val upseqt_eqdec : upseqt -> upseqt -> bool

type full_seqt = doseqt*upseqt

val full_seqt_eqdec : full_seqt -> full_seqt -> bool

val alpha_child : nnf -> doseqt -> doseqt

val get_alpha_seqt : doseqt -> nnf option

val alpha_child_sseqt : nnf -> rdoseqt -> rdoseqt

val beta1_child : nnf -> doseqt -> doseqt

val beta2_child : nnf -> doseqt -> doseqt

val get_beta_seqt : doseqt -> nnf option

val beta1_child_sseqt : nnf -> rdoseqt -> rdoseqt

val beta2_child_sseqt : nnf -> rdoseqt -> rdoseqt

val get_contra : nnf list -> string option

val unnext : nnf list -> nnf list

val jump_child : doseqt -> doseqt

val get_blocker : nnf list -> (nat*nnf list) list -> (nat*nnf list) option

val get_blocker_seqt : doseqt -> (nat*nnf list) option

val jump_child_sseqt : rdoseqt -> rdoseqt

val get_unts : nnf list -> nnf list

type fml =
| Varf of string
| Negf of fml
| Andf of fml * fml
| Orf of fml * fml
| Implf of fml * fml
| Alwf of fml
| Evef of fml
| Beff of fml * fml
| Untf of fml * fml
| Nextf of fml

val to_nnf : fml -> nnf

type ptree =
| Talpha of ptree * full_seqt * nnf
| Tbeta1 of ptree * full_seqt * nnf
| Tbeta2 of ptree * full_seqt * nnf
| Talt of ptree * ptree * full_seqt * nnf
| Tjump of ptree * full_seqt
| Tloop of full_seqt

val t_fseqt : ptree -> full_seqt

val t_dose : ptree -> doseqt

val t_upse : ptree -> upseqt

val t_d : ptree -> nat

val t_uev : ptree -> nnf list

val t_lp : ptree -> nat

val ptree_eqdec : ptree -> ptree -> bool

val desc_dec : ptree -> ptree -> bool

val desc_child_or_child_dec : ptree -> ptree -> ptree option

type filler = ptree -> nat -> __ -> __ -> ptree

type t_info = filler

type open_info = (ptree, t_info) sigT

type status =
| Open of open_info
| Closed

val alpha_rule : nnf -> rdoseqt -> status -> status

val beta1_rule : nnf -> rdoseqt -> ptree -> t_info -> status

val beta_rule : nnf -> rdoseqt -> ptree -> t_info -> status -> status

val beta2_rule : nnf -> rdoseqt -> status -> status

val contra_rule : doseqt -> string -> status

val loop_rule : rdoseqt -> (nat*nnf list) -> status

val unfulfillable_ev : ptree -> bool

val jump_rule : rdoseqt -> status -> status

val tableau : rdoseqt -> status

val mk_root : nnf list -> doseqt

val mk_root_sseqt : nnf list -> rdoseqt

val tableau_open : nnf list -> bool

val fml_tableau_open : fml list -> bool

val is_sat : fml -> bool
