
type __ = Obj.t

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

val fst : ('a1*'a2) -> 'a1

val snd : ('a1*'a2) -> 'a2

val app : 'a1 list -> 'a1 list -> 'a1 list

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

type nnf =
| Var of string
| Neg of string
| And of nnf * nnf
| Or of nnf * nnf
| Box of nnf
| Dia of nnf

val nnf_eqdec : nnf -> nnf -> bool

val neg_nnf : nnf -> nnf

type world = __

val remove_nnf : nnf -> nnf list -> nnf list

val unbox : nnf list -> nnf list

val undia : nnf list -> nnf list

val get_contra : nnf list -> string option

val get_and : nnf list -> (nnf*nnf) option

val get_or : nnf list -> (nnf*nnf) option

val get_dia : nnf list -> nnf option

val get_var : nnf list -> string list

type fml =
| Varf of string
| Negf of fml
| Andf of fml * fml
| Orf of fml * fml
| Implf of fml * fml
| Boxf of fml
| Diaf of fml

val to_nnf : fml -> nnf

type model =
| Mcons of string list * model list

type seqt = { main : nnf list; hdld : nnf list }

type val_constructible =
  string list
  (* singleton inductive, whose constructor was Build_val_constructible *)

val vc_atoms : seqt -> val_constructible -> string list

val and_child : nnf -> nnf -> seqt -> seqt

type and_instance_seqt =
| Ais_cons of nnf * nnf

val or_child_left : nnf -> nnf -> seqt -> seqt

val or_child_right : nnf -> nnf -> seqt -> seqt

type or_instance_seqt =
| Ois_cons of nnf * nnf

val box_child : nnf -> seqt -> seqt

type copy_instance_seqt =
  nnf
  (* singleton inductive, whose constructor was cis_cons *)

val get_contra_seqt : seqt -> string option

val get_and_seqt : seqt -> (nnf*nnf) option

val get_or_seqt : seqt -> (nnf*nnf) option

val get_dia_seqt : seqt -> nnf option

val get_box : nnf list -> nnf option

val get_box_seqt : seqt -> nnf option

val unmodal_seqt : seqt -> seqt list

type node =
| Closed
| Open of world

val contra_rule_seqt : seqt -> string -> node

val and_rule_seqt : seqt -> seqt -> and_instance_seqt -> node -> node

val or_rule_seqt :
  seqt -> seqt -> seqt -> or_instance_seqt -> node -> node -> node

val open_rule_seqt : seqt -> seqt -> seqt -> world -> or_instance_seqt -> node

val copy_rule_seqt : seqt -> seqt -> copy_instance_seqt -> node -> node

type dia_rule_seqt_return = (seqt, model list) sum

val dia_rule_seqt : (seqt -> __ -> node) -> seqt list -> dia_rule_seqt_return

val tableau : seqt -> node

val mk_seqt : nnf list -> seqt

val nnf_is_sat : nnf list -> bool

val fml_is_sat : fml list -> bool

val is_sat : fml -> bool
