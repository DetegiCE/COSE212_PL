type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

exception TypeError

type typ = 
    TyUnit 
  | TyInt 
  | TyBool 
  | TyFun of typ * typ 
  | TyList of typ
  | TyVar of tyvar
and tyvar = string


(* You can invoke "fresh_tyvar()" to generate a fresh type variable *)
let tyvar_num = ref 0
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f l ->
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

let rec for_all : ('a -> bool) -> 'a list -> bool
= fun f l ->
  match l with
  | [] -> true
  | hd::tl -> if f hd then for_all f tl else false

let rec fold_left : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
= fun f accu l ->
  match l with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

module TEnv = struct
  type t = var -> typ

  let empty : t
  = fun _ -> raise TypeError

  let find : t -> var -> typ
  = fun tenv x -> tenv x

  let extend : (var * typ) -> t -> t
  = fun (x, t) tenv -> fun y -> if x = y then t else (tenv y)
end

module Subst = struct 
  type t = (tyvar * typ) list

  let empty = []

  let rec find : t -> tyvar -> typ -> typ
  = fun subst x typ ->
    match subst with
    | [] -> typ
    | (y, ty)::tl -> if x = y then ty else find tl x typ

  let rec apply : typ -> t -> typ
  = fun typ subst ->
    match typ with
    | TyUnit
    | TyInt
    | TyBool -> typ
    | TyFun (t1, t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyList t -> TyList (apply t subst)
    | TyVar x -> find subst x typ

  let extend : (tyvar * typ) -> t -> t
  = fun (tv, ty) subst -> (tv, ty)::(map (fun (x, t) -> (x, apply t [(tv, ty)])) subst)
end

let eq_vars = ref []
let rec gen_equations : TEnv.t -> exp -> typ -> (typ * typ) list
= fun tenv e ty ->
  match e with
  | UNIT
  | PRINT _ -> [(ty, TyUnit)]
  | TRUE
  | FALSE -> [(ty, TyBool)]
  | CONST _ -> [(ty, TyInt)]
  | VAR x -> [(ty, TEnv.find tenv x)]
  | ADD (e1, e2)
  | SUB (e1, e2)
  | MUL (e1, e2)
  | DIV (e1, e2) -> (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | EQUAL (e1, e2) ->
    let tyvar = fresh_tyvar () in
    eq_vars := tyvar::!eq_vars;
    (ty, TyBool) :: (gen_equations tenv e1 tyvar) @ (gen_equations tenv e2 tyvar)
  | LESS (e1, e2) -> (ty, TyBool) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | NOT e -> (ty, TyBool) :: (gen_equations tenv e TyBool)
  | NIL -> let tyvar = fresh_tyvar () in [(ty, TyList tyvar)]
  | CONS (e1, e2) ->
    let tyvar = fresh_tyvar () in
    (ty, TyList tyvar) :: (gen_equations tenv e1 tyvar) @ (gen_equations tenv e2 (TyList tyvar))
  | APPEND (e1, e2) ->
    let tyvar = fresh_tyvar () in
    (ty, TyList tyvar) :: (gen_equations tenv e1 (TyList tyvar)) @ (gen_equations tenv e2 (TyList tyvar))
  | HEAD e ->
    let tyvar = fresh_tyvar () in
    (ty, tyvar) :: (gen_equations tenv e (TyList tyvar))
  | TAIL e ->
    let tyvar = fresh_tyvar () in
    (ty, TyList tyvar) :: (gen_equations tenv e (TyList tyvar))
  | ISNIL e ->
    let tyvar = fresh_tyvar () in
    (ty, TyBool) :: (gen_equations tenv e (TyList tyvar))
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x, e1, e2) ->
    let tyvar = fresh_tyvar () in
    (gen_equations tenv e1 tyvar) @ (gen_equations (TEnv.extend (x, tyvar) tenv) e2 ty)
  | LETREC (f, x, e1, e2) ->
    let arg_ty = fresh_tyvar () in
    let res_ty = fresh_tyvar () in
    let tenv = TEnv.extend (f, TyFun (arg_ty, res_ty)) (TEnv.extend (x, arg_ty) tenv) in
    (gen_equations tenv e1 res_ty) @ (gen_equations tenv e2 ty)
  | LETMREC ((f, x, fbody), (g, y, gbody), e) ->
    let f_arg_ty = fresh_tyvar () in
    let f_res_ty = fresh_tyvar () in
    let g_arg_ty = fresh_tyvar () in
    let g_res_ty = fresh_tyvar () in
    let tenv = TEnv.extend (f, TyFun (f_arg_ty, f_res_ty)) (TEnv.extend (x, f_arg_ty) tenv) in
    let tenv = TEnv.extend (g, TyFun (g_arg_ty, g_res_ty)) (TEnv.extend (y, g_arg_ty) tenv) in
    (gen_equations tenv fbody f_res_ty) @ (gen_equations tenv gbody g_res_ty) @ (gen_equations tenv e ty)
  | PROC (x, e) ->
    let arg_ty = fresh_tyvar () in
    let res_ty = fresh_tyvar () in
    (ty, TyFun (arg_ty, res_ty)) :: (gen_equations (TEnv.extend (x, arg_ty) tenv) e res_ty)
  | CALL (e1, e2) ->
    let arg_ty = fresh_tyvar () in
    (gen_equations tenv e1 (TyFun (arg_ty, ty))) @ (gen_equations tenv e2 arg_ty)
  | SEQ (e1, e2) -> let tyvar = fresh_tyvar () in (gen_equations tenv e1 tyvar) @ (gen_equations tenv e2 ty)

let rec occurrence_check : tyvar -> typ -> bool
= fun x ty ->
  match ty with
  | TyUnit
  | TyInt
  | TyBool -> false
  | TyFun (ty1, ty2) -> occurrence_check x ty1 || occurrence_check x ty2
  | TyList ty -> occurrence_check x ty
  | TyVar y -> x = y

let rec unify : typ -> typ -> Subst.t -> Subst.t
= fun ty1 ty2 subst ->
  match ty1, ty2 with
  | TyVar x, TyVar y -> if x = y then subst else Subst.extend (x, ty2) subst
  | _, TyVar _ -> unify ty2 ty1 subst
  | TyVar x, _ ->
    if occurrence_check x ty2 then
      raise TypeError
    else
      Subst.extend (x, ty2) subst
  | TyInt, TyInt
  | TyBool, TyBool
  | TyUnit, TyUnit -> subst
  | TyList ty1, TyList ty2 -> unify ty1 ty2 subst
  | TyFun (f_arg_ty, f_res_ty), TyFun (g_arg_ty, g_res_ty) ->
    let subst = unify f_arg_ty g_arg_ty subst in
    let f_res_ty = Subst.apply f_res_ty subst in
    let g_res_ty = Subst.apply g_res_ty subst in
    unify f_res_ty g_res_ty subst
  | _ -> raise TypeError
  
let rec unify_all : (typ * typ) list -> Subst.t -> Subst.t
= fun eqns subst ->
  match eqns with
  | [] -> subst
  | (ty1, ty2)::tl ->
    let ty1 = Subst.apply ty1 subst in
    let ty2 = Subst.apply ty2 subst in
    let subst = unify ty1 ty2 subst in
    unify_all tl subst

let solve : (typ * typ) list -> Subst.t
= fun eqns ->
  let subst = unify_all eqns Subst.empty in
  fold_left (fun subst ty ->
    let ty = Subst.apply ty subst in
    match ty with
    | TyInt
    | TyBool -> subst
    | TyVar _ -> unify ty TyInt subst
    | _ -> raise TypeError
  ) subst !eq_vars

let typeof : exp -> typ
= fun e ->
  let ty = fresh_tyvar () in
  let _ = eq_vars := [] in
  let eqns = gen_equations TEnv.empty e ty in
  let subst = solve eqns in
  Subst.apply ty subst
