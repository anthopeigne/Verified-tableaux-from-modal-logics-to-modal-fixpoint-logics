
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type nat =
| O
| S of nat

(** val fst : ('a1*'a2) -> 'a1 **)

let fst = function
| x,_ -> x

(** val snd : ('a1*'a2) -> 'a2 **)

let snd = function
| _,y -> y



type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| ExistT of 'a * 'p



(** val min : nat -> nat -> nat **)

let rec min n m =
  match n with
  | O -> O
  | S n' -> (match m with
             | O -> O
             | S m' -> S (min n' m'))

(** val bool_dec : bool -> bool -> bool **)

let bool_dec b1 b2 =
  if b1 then if b2 then true else false else if b2 then false else true

module Nat =
 struct
  (** val eq_dec : nat -> nat -> bool **)

  let rec eq_dec n m =
    match n with
    | O -> (match m with
            | O -> true
            | S _ -> false)
    | S n0 -> (match m with
               | O -> false
               | S n1 -> eq_dec n0 n1)
 end

(** val lt_eq_lt_dec : nat -> nat -> bool option **)

let rec lt_eq_lt_dec n m =
  match n with
  | O -> (match m with
          | O -> Some false
          | S _ -> Some true)
  | S n0 -> (match m with
             | O -> None
             | S n1 -> lt_eq_lt_dec n0 n1)

(** val le_lt_dec : nat -> nat -> bool **)

let rec le_lt_dec n m =
  match n with
  | O -> true
  | S n0 -> (match m with
             | O -> false
             | S n1 -> le_lt_dec n0 n1)

(** val le_lt_eq_dec : nat -> nat -> bool **)

let le_lt_eq_dec n m =
  let s = lt_eq_lt_dec n m in
  (match s with
   | Some a -> a
   | None -> assert false (* absurd case *))

(** val in_dec : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let rec in_dec h a = function
| [] -> false
| y::l0 -> let s = h y a in if s then true else in_dec h a l0

(** val remove : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a1 list **)

let rec remove eq_dec0 x = function
| [] -> []
| y::tl ->
  if eq_dec0 x y then remove eq_dec0 x tl else y::(remove eq_dec0 x tl)

(** val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec list_eq_dec eq_dec0 l l' =
  match l with
  | [] -> (match l' with
           | [] -> true
           | _::_ -> false)
  | y::l0 ->
    (match l' with
     | [] -> false
     | a::l1 -> if eq_dec0 y a then list_eq_dec eq_dec0 l0 l1 else false)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a::t -> (f a)::(map f t)

(** val in_dec0 : ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> bool **)

let in_dec0 =
  in_dec

(** val incl_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec incl_dec dec l l' =
  match l with
  | [] -> true
  | y::l0 ->
    let s = in_dec0 dec y l' in if s then incl_dec dec l0 l' else false

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

(** val eqset_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let eqset_dec a_eqdec l1 l2 =
  let s = incl_dec a_eqdec l1 l2 in
  if s then incl_dec a_eqdec l2 l1 else false

(** val all_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 -> bool **)

let rec all_eq_dec a_eqdec l a =
  match l with
  | [] -> true
  | y::l0 -> if all_eq_dec a_eqdec l0 a then a_eqdec y a else false

type nnf =
| Var of string
| Neg of string
| And of nnf * nnf
| Or of nnf * nnf
| Bef of nnf * nnf
| Unt of nnf * nnf
| Next of nnf

(** val nnf_true : nnf **)

let nnf_true =
  Or ((Var ""), (Neg ""))

(** val nnf_false : nnf **)

let nnf_false =
  And ((Var ""), (Neg ""))

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
  | Bef (phi, psi) ->
    (match x with
     | Bef (phi0, psi0) ->
       if nnf_eqdec phi phi0 then nnf_eqdec psi psi0 else false
     | _ -> false)
  | Unt (phi, psi) ->
    (match x with
     | Unt (phi0, psi0) ->
       if nnf_eqdec phi phi0 then nnf_eqdec psi psi0 else false
     | _ -> false)
  | Next phi -> (match x with
                 | Next phi0 -> nnf_eqdec phi phi0
                 | _ -> false)

(** val inter_nnf : nnf set -> nnf set -> nnf set **)

let inter_nnf =
  set_inter nnf_eqdec

(** val remove_nnf : nnf -> nnf list -> nnf list **)

let remove_nnf =
  remove nnf_eqdec

(** val neg_nnf : nnf -> nnf **)

let rec neg_nnf = function
| Var p -> Neg p
| Neg p -> Var p
| And (phi, psi) -> Or ((neg_nnf phi), (neg_nnf psi))
| Or (phi, psi) -> And ((neg_nnf phi), (neg_nnf psi))
| Bef (phi, psi) -> Unt ((neg_nnf phi), psi)
| Unt (phi, psi) -> Bef ((neg_nnf phi), psi)
| Next phi -> Next (neg_nnf phi)

(** val nnf_isalpha_dec : nnf -> bool **)

let nnf_isalpha_dec = function
| And (_, _) -> true
| Bef (_, _) -> true
| _ -> false

(** val get_alpha : nnf list -> nnf option **)

let rec get_alpha = function
| [] -> None
| y::l0 ->
  (match get_alpha l0 with
   | Some a -> Some a
   | None -> let s = nnf_isalpha_dec y in if s then Some y else None)

(** val df_nnf : nnf **)

let df_nnf =
  Var ""

(** val nnf_alpha1 : nnf -> nnf **)

let nnf_alpha1 = function
| And (phi, _) -> phi
| Bef (_, psi) -> neg_nnf psi
| _ -> df_nnf

(** val nnf_alpha2 : nnf -> nnf **)

let nnf_alpha2 = function
| And (_, psi) -> psi
| Bef (phi, psi) -> Or (phi, (Next (Bef (phi, psi))))
| _ -> df_nnf

(** val nnf_isbeta_dec : nnf -> bool **)

let nnf_isbeta_dec = function
| Or (_, _) -> true
| Unt (_, _) -> true
| _ -> false

(** val get_beta : nnf list -> nnf option **)

let rec get_beta = function
| [] -> None
| y::l0 ->
  (match get_beta l0 with
   | Some a -> Some a
   | None -> let s = nnf_isbeta_dec y in if s then Some y else None)

(** val nnf_beta1 : nnf -> nnf **)

let nnf_beta1 = function
| Or (phi, _) -> phi
| Unt (_, psi) -> psi
| _ -> df_nnf

(** val nnf_beta2 : nnf -> nnf **)

let nnf_beta2 = function
| Or (_, psi) -> psi
| Unt (phi, psi) -> And (phi, (Next (Unt (phi, psi))))
| _ -> df_nnf

type doseqt = { s_G : nnf list; s_H : (nat*nnf list) list; s_d : nat;
                s_c : bool; s_R : nnf list }

(** val seqt_eqdec : doseqt -> doseqt -> bool **)

let seqt_eqdec s t =
  let { s_G = s_G0; s_H = s_H0; s_d = s_d0; s_c = s_c0; s_R = s_R0 } = s in
  let { s_G = s_G1; s_H = s_H1; s_d = s_d1; s_c = s_c1; s_R = s_R1 } = t in
  if list_eq_dec nnf_eqdec s_G0 s_G1
  then if list_eq_dec (fun x y ->
            let a,b = x in
            let a0,b0 = y in
            if Nat.eq_dec a a0 then list_eq_dec nnf_eqdec b b0 else false)
            s_H0 s_H1
       then if Nat.eq_dec s_d0 s_d1
            then if bool_dec s_c0 s_c1
                 then list_eq_dec nnf_eqdec s_R0 s_R1
                 else false
            else false
       else false
  else false

type rdoseqt = doseqt

type upseqt = { s_uev : nnf list; s_lp : nat }

(** val upseqt_eqdec : upseqt -> upseqt -> bool **)

let upseqt_eqdec s t =
  let { s_uev = s_uev0; s_lp = s_lp0 } = s in
  let { s_uev = s_uev1; s_lp = s_lp1 } = t in
  if list_eq_dec nnf_eqdec s_uev0 s_uev1
  then Nat.eq_dec s_lp0 s_lp1
  else false

type full_seqt = doseqt*upseqt

(** val full_seqt_eqdec : full_seqt -> full_seqt -> bool **)

let full_seqt_eqdec fs1 fs2 =
  let a,b = fs1 in
  let a0,b0 = fs2 in if seqt_eqdec a a0 then upseqt_eqdec b b0 else false

(** val alpha_child : nnf -> doseqt -> doseqt **)

let alpha_child phi s =
  { s_G = ((nnf_alpha1 phi)::((nnf_alpha2 phi)::(remove_nnf phi s.s_G)));
    s_H = s.s_H; s_d = s.s_d; s_c = false; s_R = s.s_R }

(** val get_alpha_seqt : doseqt -> nnf option **)

let get_alpha_seqt s =
  get_alpha s.s_G

(** val alpha_child_sseqt : nnf -> rdoseqt -> rdoseqt **)

let alpha_child_sseqt =
  alpha_child

(** val beta1_child : nnf -> doseqt -> doseqt **)

let beta1_child phi s =
  { s_G = ((nnf_beta1 phi)::(remove_nnf phi s.s_G)); s_H = s.s_H; s_d =
    s.s_d; s_c = false; s_R = s.s_R }

(** val beta2_child : nnf -> doseqt -> doseqt **)

let beta2_child phi s =
  { s_G = ((nnf_beta2 phi)::(remove_nnf phi s.s_G)); s_H = s.s_H; s_d =
    s.s_d; s_c = false; s_R = s.s_R }

(** val get_beta_seqt : doseqt -> nnf option **)

let get_beta_seqt s =
  get_beta s.s_G

(** val beta1_child_sseqt : nnf -> rdoseqt -> rdoseqt **)

let beta1_child_sseqt =
  beta1_child

(** val beta2_child_sseqt : nnf -> rdoseqt -> rdoseqt **)

let beta2_child_sseqt =
  beta2_child

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

(** val unnext : nnf list -> nnf list **)

let rec unnext = function
| [] -> []
| n::gtl -> (match n with
             | Next phi -> phi::(unnext gtl)
             | _ -> unnext gtl)

(** val jump_child : doseqt -> doseqt **)

let jump_child s =
  { s_G = (unnext s.s_G); s_H = (((S s.s_d),(unnext s.s_G))::s.s_H); s_d = (S
    s.s_d); s_c = true; s_R = s.s_R }

(** val get_blocker :
    nnf list -> (nat*nnf list) list -> (nat*nnf list) option **)

let rec get_blocker d = function
| [] -> None
| y::l ->
  let iHH' = get_blocker d l in
  let a,b = y in
  (match iHH' with
   | Some a0 -> Some a0
   | None -> let s = eqset_dec nnf_eqdec b d in if s then Some (a,b) else None)

(** val get_blocker_seqt : doseqt -> (nat*nnf list) option **)

let get_blocker_seqt s =
  get_blocker (unnext s.s_G) s.s_H

(** val jump_child_sseqt : rdoseqt -> rdoseqt **)

let jump_child_sseqt =
  jump_child

(** val get_unts : nnf list -> nnf list **)

let rec get_unts = function
| [] -> []
| n::gtl ->
  (match n with
   | Unt (phi, psi) -> (Unt (phi, psi))::(get_unts gtl)
   | _ -> get_unts gtl)

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

(** val to_nnf : fml -> nnf **)

let rec to_nnf = function
| Varf p -> Var p
| Negf phi -> neg_nnf (to_nnf phi)
| Andf (phi, psi) -> And ((to_nnf phi), (to_nnf psi))
| Orf (phi, psi) -> Or ((to_nnf phi), (to_nnf psi))
| Implf (phi, psi) -> Or ((neg_nnf (to_nnf phi)), (to_nnf psi))
| Alwf phi -> Bef (nnf_false, (neg_nnf (to_nnf phi)))
| Evef phi -> Unt (nnf_true, (to_nnf phi))
| Beff (phi, psi) -> Bef ((to_nnf phi), (to_nnf psi))
| Untf (phi, psi) -> Unt ((to_nnf phi), (to_nnf psi))
| Nextf phi -> Next (to_nnf phi)

type ptree =
| Talpha of ptree * full_seqt * nnf
| Tbeta1 of ptree * full_seqt * nnf
| Tbeta2 of ptree * full_seqt * nnf
| Talt of ptree * ptree * full_seqt * nnf
| Tjump of ptree * full_seqt
| Tloop of full_seqt

(** val t_fseqt : ptree -> full_seqt **)

let t_fseqt = function
| Talpha (_, fs, _) -> fs
| Tbeta1 (_, fs, _) -> fs
| Tbeta2 (_, fs, _) -> fs
| Talt (_, _, fs, _) -> fs
| Tjump (_, fs) -> fs
| Tloop fs -> fs

(** val t_dose : ptree -> doseqt **)

let t_dose t =
  fst (t_fseqt t)

(** val t_upse : ptree -> upseqt **)

let t_upse t =
  snd (t_fseqt t)

(** val t_d : ptree -> nat **)

let t_d t =
  (t_dose t).s_d

(** val t_uev : ptree -> nnf list **)

let t_uev t =
  (t_upse t).s_uev

(** val t_lp : ptree -> nat **)

let t_lp t =
  (t_upse t).s_lp

(** val ptree_eqdec : ptree -> ptree -> bool **)

let rec ptree_eqdec p x0 =
  match p with
  | Talpha (p0, f, n) ->
    (match x0 with
     | Talpha (p1, f0, n0) ->
       if ptree_eqdec p0 p1
       then if full_seqt_eqdec f f0 then nnf_eqdec n n0 else false
       else false
     | _ -> false)
  | Tbeta1 (p0, f, n) ->
    (match x0 with
     | Tbeta1 (p1, f0, n0) ->
       if ptree_eqdec p0 p1
       then if full_seqt_eqdec f f0 then nnf_eqdec n n0 else false
       else false
     | _ -> false)
  | Tbeta2 (p0, f, n) ->
    (match x0 with
     | Tbeta2 (p1, f0, n0) ->
       if ptree_eqdec p0 p1
       then if full_seqt_eqdec f f0 then nnf_eqdec n n0 else false
       else false
     | _ -> false)
  | Talt (p0, p1, f, n) ->
    (match x0 with
     | Talt (p2, p3, f0, n0) ->
       if ptree_eqdec p0 p2
       then if ptree_eqdec p1 p3
            then if full_seqt_eqdec f f0 then nnf_eqdec n n0 else false
            else false
       else false
     | _ -> false)
  | Tjump (p0, f) ->
    (match x0 with
     | Tjump (p1, f0) ->
       if ptree_eqdec p0 p1 then full_seqt_eqdec f f0 else false
     | _ -> false)
  | Tloop f -> (match x0 with
                | Tloop f0 -> full_seqt_eqdec f f0
                | _ -> false)

(** val desc_dec : ptree -> ptree -> bool **)

let rec desc_dec z = function
| Talpha (p, _, _) ->
  let s = desc_dec z p in if s then true else ptree_eqdec z p
| Tbeta1 (p, _, _) ->
  let s = desc_dec z p in if s then true else ptree_eqdec z p
| Tbeta2 (p, _, _) ->
  let s = desc_dec z p in if s then true else ptree_eqdec z p
| Talt (p, p0, _, _) ->
  let s = desc_dec z p in
  if s
  then true
  else let s0 = ptree_eqdec z p in
       if s0
       then true
       else let s1 = desc_dec z p0 in if s1 then true else ptree_eqdec z p0
| Tjump (p, _) -> let s = desc_dec z p in if s then true else ptree_eqdec z p
| Tloop _ -> false

(** val desc_child_or_child_dec : ptree -> ptree -> ptree option **)

let desc_child_or_child_dec x z =
  match x with
  | Talpha (p, _, _) -> let s = desc_dec z p in if s then Some p else None
  | Tbeta1 (p, _, _) -> let s = desc_dec z p in if s then Some p else None
  | Tbeta2 (p, _, _) -> let s = desc_dec z p in if s then Some p else None
  | Talt (p, p0, _, _) ->
    let s = desc_dec z p in
    if s
    then Some p
    else let s0 = desc_dec z p0 in if s0 then Some p0 else None
  | Tjump (p, _) -> let s = desc_dec z p in if s then Some p else None
  | Tloop _ -> assert false (* absurd case *)

type filler = ptree -> nat -> __ -> __ -> ptree

type t_info = filler

type open_info = (ptree, t_info) sigT

type status =
| Open of open_info
| Closed

(** val alpha_rule : nnf -> rdoseqt -> status -> status **)

let alpha_rule phi ss = function
| Open i ->
  Open
    (let ExistT (x, p) = i in
     let xfs = ss,(snd (t_fseqt x)) in
     let x0 = Talpha (x, xfs, phi) in
     let fulx = fun z n ->
       let s0 = desc_child_or_child_dec x0 z in
       (match s0 with
        | Some _ -> p z n __ __
        | None -> assert false (* absurd case *))
     in
     ExistT (x0, (fun z n _ _ -> fulx z n)))
| Closed -> Closed

(** val beta1_rule : nnf -> rdoseqt -> ptree -> t_info -> status **)

let beta1_rule phi ss c i =
  let xup = { s_uev = []; s_lp = (t_lp c) } in
  let xfs = ss,xup in
  let x = Tbeta1 (c, xfs, phi) in
  let fulx = fun z n ->
    let s0 = desc_child_or_child_dec x z in
    (match s0 with
     | Some _ -> i z n __ __
     | None -> assert false (* absurd case *))
  in
  Open (ExistT (x, (fun z n _ _ -> fulx z n)))

(** val beta_rule : nnf -> rdoseqt -> ptree -> t_info -> status -> status **)

let beta_rule phi ss c1 i1 nu2 =
  match phi with
  | Or (_, _) ->
    (match nu2 with
     | Open o ->
       let ExistT (c2, i2) = o in
       let xup = { s_uev = (inter_nnf (t_uev c1) (t_uev c2)); s_lp =
         (min (t_lp c1) (t_lp c2)) }
       in
       Open
       (let xfs = ss,xup in
        let x = Talt (c1, c2, xfs, phi) in
        let fulx = fun z n ->
          let s0 = desc_dec z c1 in
          if s0
          then i1 z n __ __
          else let s1 = desc_dec z c2 in
               if s1 then i2 z n __ __ else assert false (* absurd case *)
        in
        ExistT (x, (fun z n _ _ -> fulx z n)))
     | Closed ->
       let xup = { s_uev = (t_uev c1); s_lp = (t_lp c1) } in
       Open
       (let xfs = ss,xup in
        let x = Tbeta1 (c1, xfs, phi) in
        let fulx = fun z n ->
          let s0 = desc_child_or_child_dec x z in
          (match s0 with
           | Some _ -> i1 z n __ __
           | None -> assert false (* absurd case *))
        in
        ExistT (x, (fun z n _ _ -> fulx z n))))
  | Unt (_, _) ->
    let uev1' = remove nnf_eqdec phi (t_uev c1) in
    (match nu2 with
     | Open o ->
       let ExistT (c2, i2) = o in
       let xup = { s_uev = (inter_nnf uev1' (t_uev c2)); s_lp =
         (min (t_lp c1) (t_lp c2)) }
       in
       Open
       (let xfs = ss,xup in
        let x = Talt (c1, c2, xfs, phi) in
        let fulx = fun z n ->
          let s0 = desc_dec z c1 in
          if s0
          then i1 z n __ __
          else let s1 = desc_dec z c2 in
               if s1 then i2 z n __ __ else assert false (* absurd case *)
        in
        ExistT (x, (fun z n _ _ -> fulx z n)))
     | Closed ->
       let xup = { s_uev = uev1'; s_lp = (t_lp c1) } in
       Open
       (let xfs = ss,xup in
        let x = Tbeta1 (c1, xfs, phi) in
        let fulx = fun z n ->
          let s0 = desc_child_or_child_dec x z in
          (match s0 with
           | Some _ -> i1 z n __ __
           | None -> assert false (* absurd case *))
        in
        ExistT (x, (fun z n _ _ -> fulx z n))))
  | _ -> Open (assert false (* absurd case *))

(** val beta2_rule : nnf -> rdoseqt -> status -> status **)

let beta2_rule phi ss = function
| Open i2 ->
  Open
    (let ExistT (x, p) = i2 in
     let xfs = ss,(t_upse x) in
     let x0 = Tbeta2 (x, xfs, phi) in
     let fulx = fun z n ->
       let s0 = desc_child_or_child_dec x0 z in
       (match s0 with
        | Some _ -> p z n __ __
        | None -> assert false (* absurd case *))
     in
     ExistT (x0, (fun z n _ _ -> fulx z n)))
| Closed -> Closed

(** val contra_rule : doseqt -> string -> status **)

let contra_rule _ _ =
  Closed

(** val loop_rule : rdoseqt -> (nat*nnf list) -> status **)

let loop_rule ss h =
  let xup = { s_uev = (get_unts (unnext ss.s_G)); s_lp = (fst h) } in
  let xfs = ss,xup in
  let x = Tloop xfs in
  let fulx = fun _ _ -> assert false (* absurd case *) in
  Open (ExistT (x, (fun z n _ _ -> fulx z n)))

(** val unfulfillable_ev : ptree -> bool **)

let unfulfillable_ev x =
  let l = t_uev x in
  (match l with
   | [] -> false
   | _::_ -> le_lt_dec (t_d x) (t_lp x))

(** val jump_rule : rdoseqt -> status -> status **)

let jump_rule ss = function
| Open o ->
  let ExistT (x0, i) = o in
  if unfulfillable_ev x0
  then Closed
  else Open
         (let xup = { s_uev = (t_uev x0); s_lp = (t_lp x0) } in
          let xfs = ss,xup in
          let x = Tjump (x0, xfs) in
          let fulx = fun z n ->
            let s0 = desc_child_or_child_dec x z in
            (match s0 with
             | Some _ ->
               let s1 = le_lt_eq_dec (S (t_d x)) n in
               if s1 then i z n __ __ else x0
             | None -> x0)
          in
          ExistT (x, (fun z n _ _ -> fulx z n)))
| Closed -> Closed

(** val tableau : rdoseqt -> status **)

let rec tableau x =
  let filtered_var = get_contra x.s_G in
  (match filtered_var with
   | Some s -> contra_rule x s
   | None ->
     let filtered_var0 = get_alpha_seqt x in
     (match filtered_var0 with
      | Some s -> alpha_rule s x (tableau (alpha_child_sseqt s x))
      | None ->
        let filtered_var1 = get_beta_seqt x in
        (match filtered_var1 with
         | Some s ->
           let filtered_var2 = tableau (beta1_child_sseqt s x) in
           (match filtered_var2 with
            | Open o ->
              let ExistT (c1, i1) = o in
              let filtered_var3 = all_eq_dec nnf_eqdec (t_uev c1) s in
              if filtered_var3
              then beta1_rule s x c1 i1
              else beta_rule s x c1 i1 (tableau (beta2_child_sseqt s x))
            | Closed -> beta2_rule s x (tableau (beta2_child_sseqt s x)))
         | None ->
           let filtered_var2 = get_blocker_seqt x in
           (match filtered_var2 with
            | Some s -> loop_rule x s
            | None -> jump_rule x (tableau (jump_child_sseqt x))))))

(** val mk_root : nnf list -> doseqt **)

let mk_root g =
  { s_G = g; s_H = []; s_d = O; s_c = false; s_R = g }

(** val mk_root_sseqt : nnf list -> rdoseqt **)

let mk_root_sseqt =
  mk_root

(** val tableau_open : nnf list -> bool **)

let tableau_open g =
  match tableau (mk_root_sseqt g) with
  | Open _ -> true
  | Closed -> false

(** val fml_tableau_open : fml list -> bool **)

let fml_tableau_open g =
  tableau_open (map to_nnf g)

(** val is_sat : fml -> bool **)

let is_sat phi =
  fml_tableau_open (phi::[])
