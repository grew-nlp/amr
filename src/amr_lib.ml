open Amr_types

exception Error of string (* TODO json *)

let parse amr_string =
  let lexbuf = Lexing.from_string amr_string in
    try
      let node = Amr_parser.amr Amr_lexer.main lexbuf in
      let amr = { Amr.sent_id = "None"; node; meta=[] } in
    amr
    with Amr_parser.Error ->
      raise (Error (Printf.sprintf "[line %d] Syntax error: %s\n" !Amr_lexer.line (Lexing.lexeme lexbuf)))

let load amr_file =
  let in_ch = open_in amr_file in
    let lexbuf = Lexing.from_channel in_ch in
    try
      let node = Amr_parser.amr Amr_lexer.main lexbuf in
      let amr = { Amr.sent_id = "None"; node; meta=[] } in
      amr
    with Amr_parser.Error ->
      raise (Error (Printf.sprintf "[line %d] Syntax error: %s\n" !Amr_lexer.line (Lexing.lexeme lexbuf)))
