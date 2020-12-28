type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | SEQ of exp * exp                 (* sequence *)
  | IF of exp * exp * exp            (* if-then-else *)
  | WHILE of exp * exp               (* while loop *)
  | LETV of id * exp * exp           (* variable binding *)
  | LETF of id * id list * exp * exp (* procedure binding *)
  | CALLV of id * exp list           (* call by value *)
  | CALLR of id * id list            (* call by referenece *)
  | RECORD of (id * exp) list        (* record construction *)
  | FIELD of exp * id                (* access record field *)
  | ASSIGN of id * exp               (* assgin to variable *)
  | ASSIGNF of exp * id * exp        (* assign to record field *)
  | WRITE of exp
and id = string

type loc = int
type value =
| Num of int
| Bool of bool
| Unit
| Record of record 
and record = (id * loc) list
type memory = (loc * value) list
type env = binding list
and binding = LocBind of id * loc | ProcBind of id * proc
and proc = id list * exp * env

(********************************)
(*     Handling environment     *)
(********************************)

let rec lookup_loc_env : id -> env -> loc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind (id,l) -> if(x=id) then l else lookup_loc_env x tl
    | ProcBind _ -> lookup_loc_env x tl
    end

let rec lookup_proc_env : id -> env -> proc
= fun x env ->
  match env with
  | [] -> raise(Failure ("Variable "^x^" is not included in environment"))
  | hd::tl ->
    begin match hd with
    | LocBind _ -> lookup_proc_env x tl
    | ProcBind (id,binding) -> if (x=id) then binding else lookup_proc_env x tl
    end

let extend_env : binding -> env -> env
= fun e env -> e::env

let empty_env = []


(***************************)
(*     Handling memory     *)
(***************************)

let rec lookup_mem : loc -> memory -> value
= fun l mem ->
  match mem with
  | [] -> raise(Failure ("location "^(string_of_int l)^" is not included in memory"))
  | (loc,v)::tl -> if(l=loc) then v else lookup_mem l tl

let extend_mem : (loc * value) -> memory -> memory
= fun (l,v) mem -> (l,v)::mem

let empty_mem = []

(***************************)
(*     Handling record     *)
(***************************)

let rec lookup_record : id -> record -> loc
= fun id record -> 
  match record with
    | [] -> raise(Failure ("field "^ id ^" is not included in record"))
    | (x,l)::tl -> if(id=x) then l else lookup_record id tl


let extend_record : (id * loc) -> record -> record
= fun (x,l) record -> (x,l)::record

let empty_record = []

(***************************)

let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics

let rec list_fold2 : ('a -> 'b -> 'c -> 'c)-> 'a list -> 'b list -> 'c -> 'c
= fun func l1 l2 acc ->
  match (l1,l2) with
  | ([],[]) -> acc
  | (hd1::tl1,hd2::tl2) -> list_fold2 func tl1 tl2 (func hd1 hd2 acc)
  | _ -> raise (Failure "two lists have different length")

let rec list_fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun func l acc ->
  match l with
  | [] -> acc
  | hd::tl -> list_fold func tl (func hd acc)

let value2str : value -> string
= fun v ->
  match v with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "unit"
  | Record _ -> "record" 

let getFirst : ('a * 'b) -> 'a
= fun (a, b) -> a

let getSecond : ('a * 'b) -> 'b
= fun (a, b) -> b

let numAdd : int -> int -> int
= fun a b -> a+b

let numSub : int -> int -> int
= fun a b -> a-b

let numMul : int -> int -> int
= fun a b -> a*b

let numDiv : int -> int -> int
= fun a b -> 
  match b with
  | 0 -> raise UndefinedSemantics
  | _ -> a/b

let rec eval_aop : env -> memory -> exp -> exp -> (int -> int -> int) -> (value * memory)
= fun env mem e1 e2 op ->
  let (v1,mem1) = eval env mem e1 in
  let (v2,mem2) = eval env mem1 e2 in
  match (v1,v2) with
  | (Num n1, Num n2) -> (Num (op n1 n2), mem2)
  | _ -> raise (Failure "arithmetic operation type error")

and eval : env -> memory -> exp -> (value * memory) 
=fun env mem e -> 
  match e with
  | WRITE e -> 
    let (v1,mem1) = eval env mem e in
    let _ = print_endline(value2str v1) in
    (v1,mem1)
  | NUM x -> (Num x, mem)
  | TRUE -> (Bool true, mem)
  | FALSE -> (Bool false, mem)
  | UNIT -> (Unit, mem)
  | VAR x -> (lookup_mem (lookup_loc_env x env) mem, mem)
  | ADD (x, y) -> eval_aop env mem x y numAdd
  | SUB (x, y) -> eval_aop env mem x y numSub
  | MUL (x, y) -> eval_aop env mem x y numMul
  | DIV (x, y) -> eval_aop env mem x y numDiv
  | EQUAL (x, y) -> begin
    let (v1,mem1) = eval env mem x in
    let (v2,mem2) = eval env mem1 y in
    match (v1,v2) with
    | (Num n1, Num n2) -> if n1 = n2 then (Bool true, mem2) else (Bool false, mem2)
    | (Bool b1, Bool b2) -> if b1 = b2 then (Bool true, mem2) else (Bool false, mem2)
    | (Unit, Unit) -> (Bool true, mem2)
    | _ -> (Bool false, mem2)
  end
  | LESS (x, y) -> begin
    let (v1,mem1) = eval env mem x in
    let (v2,mem2) = eval env mem1 y in
    match (v1,v2) with
    | (Num n1, Num n2) -> if n1 < n2 then (Bool true, mem2) else (Bool false, mem2)
    | _ -> raise UndefinedSemantics
  end
  | NOT x -> begin
    let (v1,mem1) = eval env mem x in
    match v1 with
    | Bool true -> (Bool false, mem1)
    | Bool false -> (Bool true, mem1)
    | _ -> raise UndefinedSemantics
  end
  | SEQ (x, y) -> begin
    let (v1,mem1) = eval env mem x in
    let (v2,mem2) = eval env mem1 y in
    (v2,mem2)
  end
  | IF (e, e1, e2) -> begin
    let (v1,mem1) = eval env mem e in
    match v1 with
    | Bool true -> eval env mem1 e1
    | Bool false -> eval env mem1 e2
    | _ -> raise UndefinedSemantics
  end
  | WHILE (e1, e2) -> begin
    let (v,memp) = eval env mem e1 in
    match v with
    | Bool true -> (let (v1,mem1) = eval env memp e2 in eval env mem1 (WHILE (e1,e2)))
    | Bool false -> (Unit, memp)
    | _ -> raise UndefinedSemantics
  end
  | LETV (x, e1, e2) -> begin
    let (v,memp) = eval env mem e1 in
    let nloc = new_location() in
    eval (extend_env (LocBind (x,nloc)) env) (extend_mem (nloc,v) memp) e2
  end
  | LETF (f, xl, e1, e2) -> begin
    eval (extend_env (ProcBind (f,(xl,e1,env))) env) mem e2
  end
  | CALLV (f, el) -> begin
    let (xl, ep, envp) = lookup_proc_env f env in
    let (vl, memn) = list_fold (fun nexp (nvl, nmem) ->
      let (nnv, nnmem) = eval env nmem nexp in
        (nvl@[nnv], nnmem)
    ) el ([], mem) in
    let (envp2, memn2) = list_fold2 (fun nx nv (nenv, nmem) ->
      let nl = new_location() in
        let nnenv = extend_env (LocBind (nx,nl)) nenv in
          let nnmem = extend_mem (nl,nv) nmem in
            (nnenv, nnmem)
    ) xl vl (extend_env (ProcBind (f,(xl,ep,envp))) envp, memn) in
    eval envp2 memn2 ep
  end
  | CALLR (f, yl) -> begin
    let (xl, ep, envp) = lookup_proc_env f env in
    let envp2 = list_fold2 (fun x y nenv -> 
      let nyl = lookup_loc_env y env in
        let nnenv = extend_env (LocBind (x,nyl)) nenv in
          nnenv
      ) xl yl (extend_env (ProcBind (f,(xl,ep,envp))) envp) in
    eval envp2 mem ep
  end
  | RECORD (xel) -> begin
    match xel with
    | [] -> (Unit, mem)
    | hd::tl -> begin
      let (vl, memn) = list_fold (fun nexp (nvl, nmem) ->
        let texp = getSecond nexp in
          let (nnv, nnmem) = eval env nmem texp in
            (nvl@[nnv], nnmem)
      ) xel ([], mem) in
      let (nrec2, nmem2) = list_fold2 (fun nx nv (nrec, nmem) ->
        let nl = new_location() in
          let tx = getFirst nx in
            let nnrec = extend_record (tx,nl) nrec in
              let nnmem = extend_mem (nl,nv) nmem in
                (nnrec, nnmem)
      ) xel vl (empty_record, memn) in
      (Record nrec2, nmem2)
    end
  end
  | FIELD (ep, x) -> begin
    let (r, mem1) = eval env mem ep in
    match r with
    | Record rec1 -> (lookup_mem (lookup_record x rec1) mem, mem1)
    | _ -> raise UndefinedSemantics
  end
  | ASSIGN (x, ep) -> begin
    let (v,memp) = eval env mem ep in
    (v, extend_mem ((lookup_loc_env x env), v) memp)
  end
  | ASSIGNF (e1, x, e2) -> begin
    let (r, mem1) = eval env mem e1 in
    let (v, mem2) = eval env mem1 e2 in
    match r with
    | Record rec1 -> (v, extend_mem (lookup_record x rec1, v) mem2)
    | _ -> raise UndefinedSemantics
  end
  | _ -> raise NotImplemented (* TODO *)

let runb : exp -> value 
=fun exp -> let (v, _) = eval empty_env empty_mem exp in v;;

runb(LETV ("ret", NUM 1,
LETV ("n", NUM 5,
SEQ (
WHILE (LESS (NUM 0, VAR "n"),
SEQ (
ASSIGN ("ret", MUL (VAR "ret", VAR "n")),
ASSIGN ("n", SUB (VAR "n", NUM 1))
)
),
VAR "ret"))));;

runb(LETF ("f", ["x1"; "x2"],
SEQ (
ASSIGN ("x1", NUM 3),
ASSIGN ("x2", NUM 3)
),
LETV("x1", NUM 1,
LETV("x2", NUM 1,
SEQ(
CALLR ("f", ["x1"; "x2"]),
ADD(VAR "x1", VAR "x2"))))));;

runb(LETV ("f", RECORD ([("x", NUM 10); ("y", NUM 13)]),
LETF ("swap", ["a"; "b"],
LETV ("temp", VAR "a",
SEQ (
ASSIGN ("a", VAR "b"),
ASSIGN ("b", VAR "temp"))),
SEQ (CALLV("swap", [FIELD (VAR "f", "x"); FIELD (VAR "f", "y")]),
FIELD (VAR "f", "x")
)
)
));;
