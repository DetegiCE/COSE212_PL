type program = exp (* abstract syntax tree *)
and exp =
  | CONST of int      (* n: integers *)
  | VAR of var        (* x: variables *)
  | ADD of exp * exp
  | SUB of exp * exp
  | READ
  | ISZERO of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
and var = string

(* 1 *)
let pgm1 = CONST 1

(* (5+2)+(3-10) *)
let pgm2 = ADD (ADD (CONST 5, CONST 2), SUB (CONST 3, CONST 10))

(* if iszero 1 then 3 else 4 *)
let pgm3 = IF (ISZERO (CONST 1), CONST 3, CONST 4)

(* let x = 5 in x - 3 *)
let pgm4 = LET ("x", CONST 5, SUB(VAR "x", CONST 3))

(* let z = 5
    in let x = 3
      in let y = x - 1 // 2
        in let x = 4
          in z - (x - y) // 3
*)
let pgm5 =
  LET ("z", CONST 5,
    LET ("x", CONST 3,
      LET ("y", SUB (VAR "x", CONST 1),
        LET ("x", CONST 4,
          SUB (VAR "z", (SUB (VAR "x", VAR "y")))))))

(*
let x = 7
in let y = 2
    in let y = (let x = x - 1 in x - y)
        in (x - 8 - y)
*)
let pgm6 =
    LET ("x", CONST 7,
        LET("y", CONST 2,
            LET("y", LET ("x", SUB(VAR "x", CONST 1),
                        SUB (VAR "x", VAR "y")),
                SUB(SUB(VAR("x", CONST 8), VAR "y")))))

(* 
let x = read
in let y = 2
    in x + y
*)

let pgm7 = 
    LET ("x", READ,
        LET ("y", CONST 2,
            ADD (VAR "x", VAR "y")))

(* iszero(iszero 1): semantic error *)
let pgm8 = ISZERO (ISZERO (CONST 1))

(* if 1 then 2 else 3 : syntatically correct but semantically incorrect *)
let pgm9 = IF (CONST 1, CONST 2, CONST 3)

(* let x = 1 in x + y : y is free var so incorrect *)
let pgm10 = LET ("x", CONST 1, ADD (VAR "x", VAR "y"))
