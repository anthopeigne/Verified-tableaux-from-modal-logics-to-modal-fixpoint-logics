
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



(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let rec in_dec h a = function
| [] -> false
| y::l0 -> let s = h y a in if s then true else in_dec h a l0

(** val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list **)

let rec remove eq_dec x = function
| [] -> []
| y::tl -> if eq_dec x y then remove eq_dec x tl else y::(remove eq_dec x tl)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a::t -> (f a)::(map f t)

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

(** val remove_nnf : nnf -> nnf list -> nnf list **)

let remove_nnf =
  remove nnf_eqdec

(** val unbox : nnf list -> nnf list **)

let rec unbox = function
| [] -> []
| n::l -> (match n with
           | Box phi -> phi::(unbox l)
           | _ -> unbox l)

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

type model =
| Mcons of string list * model list

type seqt = { main : nnf list; hdld : nnf list }

type val_constructible =
  string list
  (* singleton inductive, whose constructor was Build_val_constructible *)

(** val vc_atoms : seqt -> val_constructible -> string list **)

let vc_atoms _ val_constructible0 =
  val_constructible0

(** val and_child : nnf -> nnf -> seqt -> seqt **)

let and_child phi psi g =
  { main = (phi::(psi::(remove_nnf (And (phi, psi)) g.main))); hdld = g.hdld }

type and_instance_seqt =
| Ais_cons of nnf * nnf

(** val or_child_left : nnf -> nnf -> seqt -> seqt **)

let or_child_left phi psi g =
  { main = (phi::(remove_nnf (Or (phi, psi)) g.main)); hdld = g.hdld }

(** val or_child_right : nnf -> nnf -> seqt -> seqt **)

let or_child_right phi psi g =
  { main = (psi::(remove_nnf (Or (phi, psi)) g.main)); hdld = g.hdld }

type or_instance_seqt =
| Ois_cons of nnf * nnf

(** val box_child : nnf -> seqt -> seqt **)

let box_child phi g =
  { main = (phi::(remove_nnf (Box phi) g.main)); hdld = ((Box phi)::g.hdld) }

type copy_instance_seqt =
  nnf
  (* singleton inductive, whose constructor was cis_cons *)

(** val get_contra_seqt : seqt -> string option **)

let get_contra_seqt g =
  get_contra g.main

(** val get_and_seqt : seqt -> (nnf*nnf) option **)

let get_and_seqt g =
  get_and g.main

(** val get_or_seqt : seqt -> (nnf*nnf) option **)

let get_or_seqt g =
  get_or g.main

(** val get_dia_seqt : seqt -> nnf option **)

let get_dia_seqt g =
  get_dia g.main

(** val get_box : nnf list -> nnf option **)

let rec get_box = function
| [] -> None
| y::l0 ->
  (match get_box l0 with
   | Some a -> Some a
   | None -> (match y with
              | Box phi -> Some phi
              | _ -> None))

(** val get_box_seqt : seqt -> nnf option **)

let get_box_seqt g =
  get_box g.main

(** val unmodal_seqt : seqt -> seqt list **)

let unmodal_seqt g =
  map (fun d -> { main = (d::(unbox (app g.main g.hdld))); hdld = [] })
    (undia (app g.main g.hdld))

type node =
| Closed
| Open of world

(** val contra_rule_seqt : seqt -> string -> node **)

let contra_rule_seqt _ _ =
  Closed

(** val and_rule_seqt : seqt -> seqt -> and_instance_seqt -> node -> node **)

let and_rule_seqt _ _ _ mu =
  mu

(** val or_rule_seqt :
    seqt -> seqt -> seqt -> or_instance_seqt -> node -> node -> node **)

let or_rule_seqt _ _ _ _ mu nu =
  match mu with
  | Closed -> nu
  | Open s -> Open s

(** val open_rule_seqt :
    seqt -> seqt -> seqt -> world -> or_instance_seqt -> node **)

let open_rule_seqt _ _ _ s _ =
  Open s

(** val copy_rule_seqt :
    seqt -> seqt -> copy_instance_seqt -> node -> node **)

let copy_rule_seqt _ _ _ mu =
  mu

type dia_rule_seqt_return = (seqt, model list) sum

(** val dia_rule_seqt :
    (seqt -> __ -> node) -> seqt list -> dia_rule_seqt_return **)

let rec dia_rule_seqt f = function
| [] -> Inr []
| g::ltl ->
  (match f g __ with
   | Closed -> Inl g
   | Open s ->
     (match dia_rule_seqt f ltl with
      | Inl s0 -> Inl s0
      | Inr s0 -> Inr ((Obj.magic s)::s0)))

(** val tableau : seqt -> node **)

let rec tableau x =
  let filtered_var = get_contra_seqt x in
  (match filtered_var with
   | Some s -> contra_rule_seqt x s
   | None ->
     let filtered_var0 = get_and_seqt x in
     (match filtered_var0 with
      | Some s ->
        let phi,psi = s in
        let inst = Ais_cons ((fst (phi,psi)), (snd (phi,psi))) in
        and_rule_seqt x (and_child (fst (phi,psi)) (snd (phi,psi)) x) inst
          (tableau (and_child (fst (phi,psi)) (snd (phi,psi)) x))
      | None ->
        let filtered_var1 = get_or_seqt x in
        (match filtered_var1 with
         | Some s ->
           let phi,psi = s in
           let inst = Ois_cons ((fst (phi,psi)), (snd (phi,psi))) in
           let filtered_var2 =
             tableau (or_child_left (fst (phi,psi)) (snd (phi,psi)) x)
           in
           (match filtered_var2 with
            | Closed ->
              or_rule_seqt (or_child_left (fst (phi,psi)) (snd (phi,psi)) x)
                (or_child_right (fst (phi,psi)) (snd (phi,psi)) x) x inst
                Closed
                (tableau (or_child_right (fst (phi,psi)) (snd (phi,psi)) x))
            | Open s0 ->
              open_rule_seqt
                (or_child_left (fst (phi,psi)) (snd (phi,psi)) x)
                (or_child_right (fst (phi,psi)) (snd (phi,psi)) x) x s0 inst)
         | None ->
           let filtered_var2 = get_box_seqt x in
           (match filtered_var2 with
            | Some s ->
              copy_rule_seqt x (box_child s x) s (tableau (box_child s x))
            | None ->
              let vc = get_var x.main in
              let filtered_var3 = get_dia_seqt x in
              (match filtered_var3 with
               | Some _ ->
                 let filtered_var4 =
                   dia_rule_seqt (fun x0 _ -> tableau x0) (unmodal_seqt x)
                 in
                 (match filtered_var4 with
                  | Inl _ -> Closed
                  | Inr s -> Open (Obj.magic (Mcons ((vc_atoms x vc), s))))
               | None -> Open (Obj.magic (Mcons ((vc_atoms x vc), []))))))))

(** val mk_seqt : nnf list -> seqt **)

let mk_seqt g =
  { main = g; hdld = [] }

(** val nnf_is_sat : nnf list -> bool **)

let nnf_is_sat g =
  match tableau (mk_seqt g) with
  | Closed -> false
  | Open _ -> true

(** val fml_is_sat : fml list -> bool **)

let fml_is_sat g =
  nnf_is_sat (map to_nnf g)

(** val is_sat : fml -> bool **)

let is_sat phi =
  fml_is_sat (phi::[])
