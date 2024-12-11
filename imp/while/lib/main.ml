open Types
open Ast

let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | Const n -> "Const(" ^ (string_of_int n) ^ ")"
  | Var x -> "Var(" ^ x ^ ")"
  | And(e1,e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1,e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not e0 -> "Not(" ^ (string_of_expr e0) ^ ")"
  | Add(e1,e2) -> "Add(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Sub(e1,e2) -> "Sub(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Mul(e1,e2) -> "Mul(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Eq(e1,e2) -> "Eq(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Leq(e1,e2) -> "Leq(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
;;

let rec string_of_cmd = function
  Skip -> "Skip"
  | Assign(e1, e2) -> "Assign(" ^ e1 ^ (string_of_expr e2) ^ ")"
  | Seq(c1, c2) -> "Seq(" ^ (string_of_cmd c1) ^ (string_of_cmd c2) ^ ")"
  | If(e1, c1, c2) -> "If(" ^ (string_of_expr e1) ^ "," ^ (string_of_cmd c1) ^ "," ^ (string_of_cmd c2) ^ ")"
  | While (e1, c1) -> "While(" ^ (string_of_expr e1) ^ "," ^ (string_of_cmd c1) ^ ")"



let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec eval_expr state expr = 
  match expr with
    True -> Bool true
  | False -> Bool false
  | Var x -> state x
  | Const n -> Nat n
  | Not e -> 
    (match eval_expr state e with
      Bool b -> Bool (not b)
      | _ -> raise (TypeError "Expected a boolean value"))
  | And(e1, e2) -> 
    (match (eval_expr state e1, eval_expr state e2) with
      (Bool b1, Bool b2) -> Bool (b1 && b2)
      | _ -> raise (TypeError "Expected two boolean values"))
  | Or(e1, e2) -> 
    (match (eval_expr state e1, eval_expr state e2) with
      (Bool b1, Bool b2) -> Bool (b1 || b2)
      | _ -> raise (TypeError "Expected two boolean values"))
  | Add(e1, e2) -> 
    (match (eval_expr state e1, eval_expr state e2) with
      (Nat n1, Nat n2) -> Nat (n1 + n2)
      | _ -> raise (TypeError "Expected two integer values"))
  | Sub(e1, e2) -> 
    (match (eval_expr state e1, eval_expr state e2) with
      (Nat n1, Nat n2) -> Nat (n1 - n2)
      | _ -> raise (TypeError "Expected two integer values"))
  | Mul(e1, e2) -> 
    (match (eval_expr state e1, eval_expr state e2) with
      (Nat n1, Nat n2) -> Nat (n1 * n2)
      | _ -> raise (TypeError "Expected two integer values"))
  | Eq(e1, e2) -> 
    (match (eval_expr state e1, eval_expr state e2) with
      (Nat n1, Nat n2) -> Bool (n1 = n2)
      | _ -> raise (TypeError "Expected two integer values"))
  | Leq(e1, e2) -> 
    (match (eval_expr state e1, eval_expr state e2) with
      (Nat n1, Nat n2) -> Bool (n1 <= n2)
      | _ -> raise (TypeError "Expected two integer values"))

let bind st x v = fun y -> if x = y then v else st y
let rec trace1 = function
  Cmd(c, st) ->
    (match c with
      Skip -> (St st) (* lo stato del programma rimane lo stesso *)
    | Assign(x, e) ->
      (
        let v = eval_expr st e in
          St (bind st x v)
      )
    | Seq(c1, c2) ->
        (
          let conf' = trace1 (Cmd(c1, st)) in (* esecuzione primo comando della sequenza -> cmd1;cmd2 *)
            match conf' with
              Cmd(cmd', st') -> Cmd(Seq(cmd', c2), st') (* qui sta terminando l'esecuzione del primo cmd *)
            | St s -> Cmd(c2, s) (* qui inizia l'esecuzione del secondo comando, che seguirà lo stesso percorso del primo *)
        )
    | If(e, c1, c2) -> 
        (match (eval_expr st e) with
          Bool b -> 
          (if b = true 
            then Cmd(c1, st) (* se b == true  valuta il primo comando  *)
            else Cmd(c2, st) (* se b == false valuta il secondo comando *)
          )
          | _ -> raise (TypeError "Expected a boolean value")
          
        )
    | While(e, c1) -> 
        (match (eval_expr st e) with
          Bool b ->
            (if b = true
              then Cmd(Seq(c1, c1), st) (* se b == true esegue il comando e poi si richiama ricorsivamente *)
              else St st (* se b == false termina l'esecuzione del while aggiornano lo stato del programma *)
            )
          | _ -> raise (TypeError "Expected a boolean value")
        )
    )
  | St _ -> raise NoRuleApplies

let rec create_list n_steps acc f conf = 
  (* va avanti finche non arriva a 0 *)
  if n_steps < 0 
    then List.rev acc (* restituisce la lista di comandi al contrario *)
    else 
      try let conf' = f conf in (* se restituisce una configurazione va avanti nella creazione della lista *)
        create_list (n_steps - 1) (conf' :: acc) f conf'
      with _ -> List.rev acc (* se da' un eccezione allora si ferma perché il cmd non si può ridurre ulteriormente *)

let bottom : state = fun _ -> raise (UnboundVar("Non esistono variabili associate")) 
 
let trace n_steps cmd = 
  (* cmd -> è il comando di partenza 
     bottom -> è lo stato 0; quello di partenza quindi senza valore *)
  let conf' = Cmd(cmd, bottom) in
    create_list n_steps [] trace1 conf'