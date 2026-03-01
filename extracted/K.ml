
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

(** val fst : ('a1*'a2) -> 'a1 **)

let fst = function
| x,_ -> x

(** val snd : ('a1*'a2) -> 'a2 **)

let snd = function
| _,y -> y

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a::l1 -> a::(app l1 m)

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)



(** val hd : 'a1 -> 'a1 list -> 'a1 **)

let hd default = function
| [] -> default
| x::_ -> x

(** val tl : 'a1 list -> 'a1 list **)

let tl = function
| [] -> []
| _::m -> m

(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let rec in_dec h a = function
| [] -> false
| y::l0 -> let s = h y a in if s then true else in_dec h a l0

(** val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list **)

let rec remove eq_dec x = function
| [] -> []
| y::tl0 ->
  if eq_dec x y then remove eq_dec x tl0 else y::(remove eq_dec x tl0)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a::t -> (f a)::(map f t)

type 'a set = 'a list

(** val set_mem : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 set -> bool **)

let rec set_mem aeq_dec a = function
| [] -> false
| a1::x1 -> if aeq_dec a a1 then true else set_mem aeq_dec a x1

(** val set_inter : ('a1 -> 'a1 -> bool) -> 'a1 set -> 'a1 set -> 'a1 set **)

let rec set_inter aeq_dec x y =
  match x with
  | [] -> []
  | a1::x1 ->
    if set_mem aeq_dec a1 y
    then a1::(set_inter aeq_dec x1 y)
    else set_inter aeq_dec x1 y

type nnf =
| Var of string
| Neg of string
| And of nnf * nnf
| Or of nnf * nnf
| Box of nnf
| Dia of nnf

(** val nnf_eqdec : nnf -> nnf -> bool **)

let rec nnf_eqdec n x =
  match n with
  | Var p -> (match x with
              | Var p0 -> (=) p p0
              | _ -> false)
  | Neg p -> (match x with
              | Neg p0 -> (=) p p0
              | _ -> false)
  | And (phi, psi) ->
    (match x with
     | And (phi0, psi0) ->
       if nnf_eqdec phi phi0 then nnf_eqdec psi psi0 else false
     | _ -> false)
  | Or (phi, psi) ->
    (match x with
     | Or (phi0, psi0) ->
       if nnf_eqdec phi phi0 then nnf_eqdec psi psi0 else false
     | _ -> false)
  | Box phi -> (match x with
                | Box phi0 -> nnf_eqdec phi phi0
                | _ -> false)
  | Dia phi -> (match x with
                | Dia phi0 -> nnf_eqdec phi phi0
                | _ -> false)

(** val neg_nnf : nnf -> nnf **)

let rec neg_nnf = function
| Var p -> Neg p
| Neg p -> Var p
| And (phi, psi) -> Or ((neg_nnf phi), (neg_nnf psi))
| Or (phi, psi) -> And ((neg_nnf phi), (neg_nnf psi))
| Box phi -> Dia (neg_nnf phi)
| Dia phi -> Box (neg_nnf phi)

type world = __

(** val inter_nnf : nnf set -> nnf set -> nnf set **)

let inter_nnf =
  set_inter nnf_eqdec

(** val remove_nnf : nnf -> nnf list -> nnf list **)

let remove_nnf =
  remove nnf_eqdec

(** val hd_nnf : nnf list -> nnf **)

let hd_nnf =
  hd (Var "")

type and_instance =
| Ai_cons of nnf * nnf

type or_instance =
| Oi_cons of nnf * nnf

(** val left_prcp : nnf list -> nnf list -> nnf list -> or_instance -> nnf **)

let left_prcp _ _ _ = function
| Oi_cons (phi, _) -> phi

(** val unbox : nnf list -> nnf list **)

let rec unbox = function
| [] -> []
| n::l -> (match n with
           | Box phi -> phi::(unbox l)
           | _ -> unbox l)

(** val rebox : nnf list -> nnf list **)

let rec rebox = function
| [] -> []
| hd0::tl0 -> (Box hd0)::(rebox tl0)

(** val undia : nnf list -> nnf list **)

let rec undia = function
| [] -> []
| n::l -> (match n with
           | Dia phi -> phi::(undia l)
           | _ -> undia l)

(** val get_contra : nnf list -> string option **)

let rec get_contra = function
| [] -> None
| y::l0 ->
  (match get_contra l0 with
   | Some a -> Some a
   | None ->
     (match y with
      | Var p ->
        let s = in_dec nnf_eqdec (Neg p) l0 in if s then Some p else None
      | Neg p ->
        let s = in_dec nnf_eqdec (Var p) l0 in if s then Some p else None
      | _ -> None))

(** val get_and : nnf list -> (nnf*nnf) option **)

let rec get_and = function
| [] -> None
| y::l0 ->
  (match get_and l0 with
   | Some a -> Some a
   | None -> (match y with
              | And (phi, psi) -> Some (phi,psi)
              | _ -> None))

(** val get_or : nnf list -> (nnf*nnf) option **)

let rec get_or = function
| [] -> None
| y::l0 ->
  (match get_or l0 with
   | Some a -> Some a
   | None -> (match y with
              | Or (phi, psi) -> Some (phi,psi)
              | _ -> None))

(** val get_dia : nnf list -> nnf option **)

let rec get_dia = function
| [] -> None
| y::l0 ->
  (match get_dia l0 with
   | Some a -> Some a
   | None -> (match y with
              | Dia phi -> Some phi
              | _ -> None))

(** val get_var : nnf list -> string list **)

let rec get_var = function
| [] -> []
| n0::l -> (match n0 with
            | Var n -> n::(get_var l)
            | _ -> get_var l)

(** val unmodal : nnf list -> nnf list list **)

let unmodal gamma =
  map (fun d -> d::(unbox gamma)) (undia gamma)

type fml =
| Varf of string
| Negf of fml
| Andf of fml * fml
| Orf of fml * fml
| Implf of fml * fml
| Boxf of fml
| Diaf of fml

(** val to_nnf : fml -> nnf **)

let rec to_nnf = function
| Varf p -> Var p
| Negf phi -> neg_nnf (to_nnf phi)
| Andf (phi, psi) -> And ((to_nnf phi), (to_nnf psi))
| Orf (phi, psi) -> Or ((to_nnf phi), (to_nnf psi))
| Implf (phi, psi) -> Or ((neg_nnf (to_nnf phi)), (to_nnf psi))
| Boxf phi -> Box (to_nnf phi)
| Diaf phi -> Dia (to_nnf phi)

type val_constructible =
  string list
  (* singleton inductive, whose constructor was Build_val_constructible *)

(** val vc_atoms : nnf list -> val_constructible -> string list **)

let vc_atoms _ val_constructible0 =
  val_constructible0

type model =
| Mcons of string list * model list

(** val mark_and : nnf -> nnf list -> nnf list **)

let mark_and phi m =
  match phi with
  | And (phi1, phi2) ->
    if in_dec nnf_eqdec phi1 m
    then phi::m
    else if in_dec nnf_eqdec phi2 m then phi::m else m
  | _ -> m

(** val mark_or : nnf -> nnf list -> nnf list -> nnf list **)

let mark_or phi ml mr =
  match phi with
  | Or (phi1, phi2) ->
    if in_dec nnf_eqdec phi1 ml
    then phi::(app ml mr)
    else if in_dec nnf_eqdec phi2 mr then phi::(app ml mr) else app ml mr
  | _ -> app ml mr

(** val mark_modal : nnf list -> nnf list -> nnf list -> nnf list **)

let mark_modal _ i m =
  (Dia (hd_nnf i))::(rebox (inter_nnf (tl i) m))

(** val pmark_of_closed_and :
    nnf list -> nnf list -> and_instance -> nnf list -> nnf list **)

let pmark_of_closed_and _ _ i m =
  let Ai_cons (phi, psi) = i in mark_and (And (phi, psi)) m

(** val pmark_of_jump :
    nnf list -> nnf list -> nnf list -> or_instance -> nnf list -> nnf list **)

let pmark_of_jump _ _ _ _ m =
  m

(** val pmark_of_closed_or :
    nnf list -> nnf list -> nnf list -> nnf list -> nnf list -> or_instance
    -> nnf list **)

let pmark_of_closed_or _ _ _ m1 m2 = function
| Oi_cons (phi, psi) -> mark_or (Or (phi, psi)) m1 m2

type node =
| Closed of nnf list
| Open of world

(** val and_rule : nnf list -> nnf list -> and_instance -> node -> node **)

let and_rule g d i = function
| Closed m -> let hsig = pmark_of_closed_and g d i m in Closed hsig
| Open w -> Open w

(** val jump_rule :
    nnf list -> nnf list -> nnf list -> or_instance -> nnf list -> node **)

let jump_rule g1 g2 d i m =
  let hsig = pmark_of_jump g1 g2 d i m in Closed hsig

(** val or_rule :
    nnf list -> nnf list -> nnf list -> or_instance -> node -> node -> node **)

let or_rule g1 g2 d i mu nu =
  match mu with
  | Closed m1 ->
    (match nu with
     | Closed m2 -> let s = pmark_of_closed_or g1 g2 d m1 m2 i in Closed s
     | Open w -> Open w)
  | Open w -> Open w

(** val open_rule :
    nnf list -> nnf list -> nnf list -> world -> or_instance -> node **)

let open_rule _ _ _ s _ =
  Open s

(** val contra_rule : nnf list -> string -> node **)

let contra_rule _ n =
  Closed ((Var n)::((Neg n)::[]))

type tmap_return = ((nnf list*nnf list), model list) sum

(** val tmap : (nnf list -> __ -> node) -> nnf list list -> tmap_return **)

let rec tmap f = function
| [] -> Inr []
| l::gtl ->
  (match f l __ with
   | Closed m -> Inl (l,m)
   | Open s0 ->
     (match tmap f gtl with
      | Inl s1 -> Inl s1
      | Inr s1 -> Inr ((Obj.magic s0)::s1)))

(** val tableau : nnf list -> node **)

let rec tableau x =
  let filtered_var = get_contra x in
  (match filtered_var with
   | Some s -> contra_rule x s
   | None ->
     let filtered_var0 = get_and x in
     (match filtered_var0 with
      | Some s ->
        let phi,psi = s in
        let d = phi::(psi::(remove_nnf (And (phi, psi)) x)) in
        let inst = Ai_cons ((fst (phi,psi)), (snd (phi,psi))) in
        and_rule x
          ((fst (phi,psi))::((snd (phi,psi))::(remove_nnf (And
                                                ((fst (phi,psi)),
                                                (snd (phi,psi)))) x))) inst
          (tableau d)
      | None ->
        let filtered_var1 = get_or x in
        (match filtered_var1 with
         | Some s ->
           let phi,psi = s in
           let g1 = phi::(remove_nnf (Or (phi, psi)) x) in
           let g2 = psi::(remove_nnf (Or (phi, psi)) x) in
           let inst = Oi_cons ((fst (phi,psi)), (snd (phi,psi))) in
           let filtered_var2 = tableau g1 in
           (match filtered_var2 with
            | Closed m ->
              let filtered_var3 =
                in_dec nnf_eqdec
                  (left_prcp
                    ((fst (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                        (snd (phi,psi)))) x))
                    ((snd (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                        (snd (phi,psi)))) x)) x inst) m
              in
              if filtered_var3
              then or_rule
                     ((fst (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                         (snd (phi,psi)))) x))
                     ((snd (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                         (snd (phi,psi)))) x)) x inst (Closed
                     m) (tableau g2)
              else jump_rule
                     ((fst (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                         (snd (phi,psi)))) x))
                     ((snd (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                         (snd (phi,psi)))) x)) x inst m
            | Open s0 ->
              open_rule
                ((fst (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                    (snd (phi,psi)))) x))
                ((snd (phi,psi))::(remove_nnf (Or ((fst (phi,psi)),
                                    (snd (phi,psi)))) x)) x s0 inst)
         | None ->
           let vc = get_var x in
           let filtered_var2 = get_dia x in
           (match filtered_var2 with
            | Some _ ->
              let l = tmap (fun x0 _ -> tableau x0) (unmodal x) in
              (match l with
               | Inl s -> Closed (mark_modal x (fst s) (snd s))
               | Inr s -> Open (Obj.magic (Mcons ((vc_atoms x vc), s))))
            | None -> Open (Obj.magic (Mcons ((vc_atoms x vc), [])))))))

(** val nnf_is_sat : nnf list -> bool **)

let nnf_is_sat g =
  match tableau g with
  | Closed _ -> false
  | Open _ -> true

(** val fml_is_sat : fml list -> bool **)

let fml_is_sat g =
  nnf_is_sat (map to_nnf g)

(** val is_sat : fml -> bool **)

let is_sat phi =
  fml_is_sat (phi::[])
