open Printf
open Amr_types

exception Error of string (* TODO json *)

let parse_aux ?(delta=1) amr_string =
  Amr_lexer.line := delta;
  let lexbuf = Lexing.from_string amr_string in
    try
      let node = Amr_parser.amr Amr_lexer.main lexbuf in
      let amr = { Amr.sent_id = "None"; node; meta=[] } in
    amr
    with
      | Amr_parser.Error -> raise (Error (Printf.sprintf "[line %d] Syntax error: %s" !Amr_lexer.line (Lexing.lexeme lexbuf)))
      | Failure msg -> raise (Error (Printf.sprintf "[line %d] Error: %s" !Amr_lexer.line msg))

let parse amr_string = parse_aux amr_string

let load amr_file =
  let in_ch = open_in amr_file in
    let lexbuf = Lexing.from_channel in_ch in
    try
      let node = Amr_parser.amr Amr_lexer.main lexbuf in
      let amr = { Amr.sent_id = "None"; node; meta=[] } in
      amr
    with
      | Amr_parser.Error -> raise (Error (Printf.sprintf "[line %d] Syntax error: %s" !Amr_lexer.line (Lexing.lexeme lexbuf)))
      | Failure msg -> raise (Error (Printf.sprintf "[line %d] Error: %s" !Amr_lexer.line msg))

exception Stop
let load_corpus corpus_file =
  let buff = Buffer.create 32 in
  let current_sentid = ref None in
  let stack = ref [] in
  let delta = ref 0 in

  let push () = match !current_sentid with
    | None -> failwith "problem"
    | Some sentid ->
        stack := (sentid, parse_aux ~delta:!delta (Buffer.contents buff)) :: !stack;
        current_sentid := None in

  let line_num = ref 0 in
  let in_ch = open_in corpus_file in

  let next () = incr line_num; input_line in_ch in
  try
    while true do
      let next_line = next() in
      if next_line <> ""
      then
        if next_line.[0] = '('
        then
          begin
            delta := !line_num;
            Buffer.clear buff;
            bprintf buff "%s\n" next_line;
            try
              while true do
                let next_line = next() in
                if next_line = ""
                then raise Stop
                else bprintf buff "%s\n" next_line
              done
            with Stop -> push()
          end
        else current_sentid := Some next_line
    done;
    assert false
  with
  | End_of_file ->
    push ();
    Array.of_list !stack

