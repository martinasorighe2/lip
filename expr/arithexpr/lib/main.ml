open Ast

(*funzione che valuta se è un numero naturale*)
let rec is_nv = function
  Zero -> true
| Succ n -> is_nv n
| _ -> false

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Zero -> "0"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not e0 -> "Not(" ^ (string_of_expr e0) ^ ")"
  | Pred e0 -> "Pred(" ^ (string_of_expr e0) ^ ")"
  | Succ e0 -> "Succ(" ^ (string_of_expr e0) ^ ")"
  | IsZero e0 -> "IsZero(" ^ (string_of_expr e0) ^ ")"


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If(trace1 e0,e1,e2)
  | And (True,e1) -> e1
  | And (False,_) -> False
  | And (e0,e1) -> And(trace1 e0,e1)
  | Or (True,_) -> True
  | Or (False,e1) -> e1
  | Or (e0,e1) -> Or(trace1 e0,e1)
  | Not True -> False
  | Not False -> True
  | Not e0 -> Not(trace1 e0)
  | Succ e0 -> Succ (trace1 e0)
  | Pred(Succ(e0)) -> e0
  | Pred e0 -> Pred (trace1 e0)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero e0 -> IsZero (trace1 e0)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval = function
    True -> Bool true
  | False -> Bool false
  | If(e0,e1,e2) -> (
    match eval e0 with
    | Nat _ -> failwith "La condizione dell'if deve essere un booleano"
    | Bool e -> if e then eval e1 else eval e2)
  | And (e0,e1) -> (
    match eval e0, eval e1 with
    | Bool e2, Bool e3 -> Bool (e2 && e3)
    | _ -> failwith "gli argomenti devono essere booleani")
  | Or (e0,e1) -> (
    match eval e0, eval e1 with
    | Bool e2, Bool e3 -> Bool (e2 || e3)
    | _ -> failwith "gli argomenti devono essere booleani")
  | Zero -> Nat 0
  | Succ e0 -> (
    match eval e0 with
      | Nat n -> Nat (n+1)
      | _ -> failwith "il successore di un booleano non esiste")
  | Pred e0 -> (
    match eval e0 with
      | Nat n when n>0 -> Nat (n-1)
      | _ -> failwith "il successore di un booleano non esiste")
  | IsZero e0 -> (
    match eval e0 with
    | Nat n -> if n>0 then Bool false else Bool true
    | _ -> failwith "errore: non puoi usare IsZero con un booleano")
  | Not e0 -> (
      match eval e0 with
      | Bool e1 -> if e1 then Bool false else Bool true
      | _ -> failwith "devi usare valori booleani")