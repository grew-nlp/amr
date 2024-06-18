open Amr_parser

exception LexError of Lexing.position * string

let digit = [%sedlex.regexp? '0'..'9']
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let numeral = [%sedlex.regexp? Opt ('-'), Plus digit, Opt( ('.' | ':'), Plus digit) ] (* ':' is used in time like 16:30 *)

let remove_first = function
  | "" -> ""
  | s -> String.sub s 1 ((String.length s) - 1) 

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t") -> token buf
  | newline -> token buf
  | '"' -> string buf
  | '-' -> MINUS
  | '+' -> PLUS
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '/' -> SLASH
  | numeral -> DATA (Sedlexing.Utf8.lexeme buf)
  | Plus ("-" | tr8876_ident_char | digit) -> IDENT (Sedlexing.Utf8.lexeme buf)
  | ':', Plus (letter | digit | '-') -> LABEL (Sedlexing.Utf8.lexeme buf |> remove_first)
  | eof -> EOF
  | _ ->
    let position = fst @@ Sedlexing.lexing_positions buf in
    let tok = Sedlexing.Utf8.lexeme buf in
    raise @@ LexError (position, Printf.sprintf "unexpected character %S" tok)

(* code taken from https://github.com/ygrek/menhir-workshop *)
and string buf =
  let buffer = Buffer.create 10 in
  let rec read_string buf =
    match%sedlex buf with
    | {|\"|} ->
      Buffer.add_char buffer '"';
      read_string buf
    | '"' -> DATA (Buffer.contents buffer)
    | Star (Compl '"') ->
      Buffer.add_string buffer (Sedlexing.Utf8.lexeme buf);
      read_string buf
    | _ -> assert false
  in
  read_string buf
