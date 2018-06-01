{
  open Printf
  open Amr_parser
  open Lexing

  exception Bad_char of char
  let line = ref 1
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let quoted = '"'([^ '"' ]+)'"'
let ident = letter (letter | digit | '-')*

rule main = parse
| ' ' | '\t'     { main lexbuf }
| '\n'           { incr line; main lexbuf }
| '-'            { MINUS }
| '+'            { PLUS }
| '('            { LP }
| ')'            { RP }
| '/'            { SLASH }
| ident as s     { IDENT s }
| ":"ident as l  { LABEL l }
| digit+ as i    { INT (int_of_string i) }
| quoted as s    { STRING s }
| _ as c         { raise (Failure (sprintf "Bad char: %c" c)) }
