{
open Parser
}

let white = [' ' '\t']+
let characters = ['a'-'z' 'A'-'Z']+
let numbers = ['0'-'9']+
let newline = '\n'

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":=" { ASSIGN }
  | ";" { SEQ }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "=" { EQ }
  | "<=" { LEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "and" { AND }
  | "or" { OR }
  | "not" { NOT }
  | "while" { WHILE }
  | "do" { DO }
  | newline { read lexbuf }
  | characters { VAR (Lexing.lexeme lexbuf) } (* x *)
  | numbers { CONST (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }