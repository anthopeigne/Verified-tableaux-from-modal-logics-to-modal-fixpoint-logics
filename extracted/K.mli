
type __ = Obj.t

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

val fst : ('a1*'a2) -> 'a1

val snd : ('a1*'a2) -> 'a2

val app : 'a1 list -> 'a1 list -> 'a1 list

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



val hd : 'a1 -> 'a1 list -> 'a1

val tl : 'a1 list -> 'a1 list

val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool

val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

type 'a set = 'a list

val set_mem : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 set -> bool

val set_inter : ('a1 -> 'a1 -> bool) -> 'a1 set -> 'a1 set -> 'a1 set

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

val inter_nnf : nnf set -> nnf set -> nnf set

val remove_nnf : nnf -> nnf list -> nnf list

val hd_nnf : nnf list -> nnf

type and_instance =
| Ai_cons of nnf * nnf

type or_instance =
| Oi_cons of nnf * nnf

val left_prcp : nnf list -> nnf list -> nnf list -> or_instance -> nnf

val unbox : nnf list -> nnf list

val rebox : nnf list -> nnf list

val undia : nnf list -> nnf list

val get_contra : nnf list -> string option

val get_and : nnf list -> (nnf*nnf) option

val get_or : nnf list -> (nnf*nnf) option

val get_dia : nnf list -> nnf option

val get_var : nnf list -> string list

val unmodal : nnf list -> nnf list list

type fml =
| Varf of string
| Negf of fml
| Andf of fml * fml
| Orf of fml * fml
| Implf of fml * fml
| Boxf of fml
| Diaf of fml

val to_nnf : fml -> nnf

type val_constructible =
  string list
  (* singleton inductive, whose constructor was Build_val_constructible *)

val vc_atoms : nnf list -> val_constructible -> string list

type model =
| Mcons of string list * model list

val mark_and : nnf -> nnf list -> nnf list

val mark_or : nnf -> nnf list -> nnf list -> nnf list

val mark_modal : nnf list -> nnf list -> nnf list -> nnf list

val pmark_of_closed_and :
  nnf list -> nnf list -> and_instance -> nnf list -> nnf list

val pmark_of_jump :
  nnf list -> nnf list -> nnf list -> or_instance -> nnf list -> nnf list

val pmark_of_closed_or :
  nnf list -> nnf list -> nnf list -> nnf list -> nnf list -> or_instance ->
  nnf list

type node =
| Closed of nnf list
| Open of world

val and_rule : nnf list -> nnf list -> and_instance -> node -> node

val jump_rule :
  nnf list -> nnf list -> nnf list -> or_instance -> nnf list -> node

val or_rule :
  nnf list -> nnf list -> nnf list -> or_instance -> node -> node -> node

val open_rule :
  nnf list -> nnf list -> nnf list -> world -> or_instance -> node

val contra_rule : nnf list -> string -> node

type tmap_return = ((nnf list*nnf list), model list) sum

val tmap : (nnf list -> __ -> node) -> nnf list list -> tmap_return

val tableau : nnf list -> node

val nnf_is_sat : nnf list -> bool

val fml_is_sat : fml list -> bool

val is_sat : fml -> bool
