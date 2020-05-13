{
  open Printf
  open Amr_parser
  open Lexing

  exception Bad_char of char
  let line = ref 1

  let escaped = ref false
  let buff = Buffer.create 32
}

let digit = ['0'-'9']
let numeral = '-'? digit+ (['.' ':'] digit+)?
let letter = ['a'-'z' 'A'-'Z']
let ident = letter (letter | digit | '-' | '.' | '\'')*

rule main = parse
| ' ' | '\t'      { main lexbuf }
| '\n'            { incr line; main lexbuf }
| '-'             { MINUS }
| '+'             { PLUS }
| '('             { LP }
| ')'             { RP }
| '/'             { SLASH }
| ident as s      { IDENT s }
| ":"(ident as l) { LABEL l }
| numeral as i    { DATA i }
| '"'             { Buffer.clear buff; string_lex lexbuf }

| _ as c         { raise (Failure (sprintf "Bad char: %c" c)) }

and string_lex = parse
  | '\\' {
    if !escaped
    then (bprintf buff "\\"; escaped := false; string_lex lexbuf)
    else (escaped := true; string_lex lexbuf)
  }
  | '\n' { incr line; Lexing.new_line lexbuf; bprintf buff "\n"; string_lex lexbuf }
  | '\"' {
    if !escaped
    then (bprintf buff "\""; escaped := false; string_lex lexbuf)
    else (DATA (Buffer.contents buff))
  }
  | _ as c {
    if !escaped then bprintf buff "\\";
    escaped := false;
    bprintf buff "%c" c;
    string_lex lexbuf
  }
