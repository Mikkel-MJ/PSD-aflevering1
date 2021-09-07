(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = []; (* the empty environment *)

let rec lookup env x =
    match env with 
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr = 
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr;;


let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;


(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("==", e1,e2) -> if eval e1 env = eval e2 env then 1 else 0
    | Prim("Max", e1,e2) -> if eval e1 env < eval e2 env then eval e2 env else eval e1 env
    | Prim("Min", e1,e2) -> if eval e1 env > eval e2 env then eval e2 env else eval e1 env
    | Prim _            -> failwith "unknown primitive";;
   

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;

(*Opgave II *)
let e4 = Prim("+", Prim("Max", Var "b", Var "a"), Var "a");;
let e5 = Prim("+", Prim("Min", Var "b", Var "a"), Var "a");;
let e6 = Prim("==", Prim("-", Var "b", Var "a"), CstI 5);;
let e7 = Prim("==", Prim("-", Var "b", Var "a"), CstI 108);;
let e4v = eval e4 env;;
let e5v = eval e5 env;;
let e6v = eval e6 env;;
let e7v = eval e7 env;;

(*Opgave III*)

let rec eval2 e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim(ope, e1, e2) ->
        let i1 = eval e1 env
        let i2 = eval e2 env
        match ope with
            | "+" -> i1 + i2
            | "-" -> i1 - i2
            | "*" -> i1 * i2
            | "==" -> if i1 = i2 then 1 else 0
            | "Max" -> if i1 < i2 then i2 else i1
            | "Min" -> if i1 > i2 then i2 else i1
            | _ -> failwith "unknown primitive";;

let e1v2  = eval2 e1 env;;
let e2v12 = eval2 e2 env;;
let e2v22= eval2 e2 [("a", 314)];;
let e3v2  = eval2 e3 env;;

(*Opgave IV og V*)
let rec eval3 e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x 
    | Prim("+", e1, e2) -> eval e1 env + eval e2 env
    | Prim("*", e1, e2) -> eval e1 env * eval e2 env
    | Prim("-", e1, e2) -> eval e1 env - eval e2 env
    | Prim("==", e1,e2) -> if eval e1 env = eval e2 env then 1 else 0
    | Prim("Max", e1,e2) -> if eval e1 env < eval e2 env then eval e2 env else eval e1 env
    | Prim("Min", e1,e2) -> if eval e1 env > eval e2 env then eval e2 env else eval e1 env
    | If(e1,e2,e3) -> if eval e1 env = 1 then eval e2 env else eval e3 env
    | Prim _            -> failwith "unknown primitive";;

   

(*Exercise 1.2*)

type aexpr = 
  | CstI of int
  | Var of string
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr;;

  (*Opgave II*)
  (* v - (w + z)*)
let ae1 = Sub(Var "v",Add(Var "w",Var "z"));;
let ae2 = Mul(CstI 2,Sub(Var "v",Add(Var "w",Var "z")));;
let ae3 = Add(Var "x",Add(Var "y",Add(Var "z",Var "v")));;

(*Opgave III*)
let rec fmt (e : aexpr) : string =
   match e with
   | CstI e -> e.ToString()
   | Var e -> e
   | Add( e1, e2) -> "(" + fmt e1 + "+" + fmt e2 + ")"
   | Mul( e1, e2) -> "(" + fmt e1 + "*" + fmt e2 + ")"
   | Sub( e1, e2) -> "(" + fmt e1 + "-" + fmt e2 + ")";;


(*Opgave IV*)
let rec simplify (e:aexpr) : aexpr =
  match e with
  | Var _ | CstI _ -> e
  | Add(CstI 0, e2) -> simplify e2
  | Add(e1, CstI 0) -> simplify e1
  | Add(e1,e2) -> Add(simplify e1,simplify e2)
  | Sub(e1,CstI 0) -> simplify e1
  | Sub(e1,e2) -> if e1 = e2 then CstI 0 else Sub(simplify e1,simplify e2)
  | Mul(CstI 1, e2)  -> simplify e2
  | Mul(e1, CstI 1) -> simplify e1
  | Mul(CstI 0, _) ->  CstI 0
  | Mul(_, CstI 0) -> CstI 0
  | Mul(e1,e2) -> Mul(simplify e1,simplify e2);;

 // simplify (Mul(CstI 2,Sub(Var "v",Add(Var "w",Var "z"))));;
  simplify ae2;;