open Amr_parser

exception LexError of Lexing.position * string

let digit = [%sedlex.regexp? '0'..'9']
let letter = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let newline = [%sedlex.regexp? '\r' | '\n' | "\r\n"]
let numeral = [%sedlex.regexp? Opt ('-'), Plus digit, Opt( ('.' | ':'), Plus digit) ] (* ':' is used in time like 16:30 *)

let clean_label = function
  | "" -> "" 
  | l -> 
    match String.index_from_opt l 0 '(' with
    | None -> String.sub l 1 ((String.length l) - 1) 
    | Some p -> String.sub l 1 (p-1) 

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
  | Plus ("_" | "-" | "." | tr8876_ident_char | digit) -> IDENT (Sedlexing.Utf8.lexeme buf)
  | ':', Plus (letter | digit | '-'), Opt ('(', Star (letter | digit |'_'), ')') -> LABEL (Sedlexing.Utf8.lexeme buf |> clean_label)
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
