open Printf

module Amr = struct
  type t = Amr_types.Amr.t

  exception Error of string (* TODO json *)

  let parse_aux ?(delta=1) amr_string =
    Amr_lexer.line := delta;
    let lexbuf = Lexing.from_string amr_string in
      try
        let node = Amr_parser.amr Amr_lexer.main lexbuf in
        let amr = { Amr_types.Amr.sent_id = "None"; node; meta=[] } in
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
        let amr = { Amr_types.Amr.sent_id = "None"; node; meta=[] } in
        amr
      with
        | Amr_parser.Error -> raise (Error (Printf.sprintf "[line %d] Syntax error: %s" !Amr_lexer.line (Lexing.lexeme lexbuf)))
        | Failure msg -> raise (Error (Printf.sprintf "[line %d] Error: %s" !Amr_lexer.line msg))


  let to_gr = Amr_types.Amr.to_gr
end

module Amr_corpus = struct
  let rm_peripheral_white s =
    s
    |> (Str.global_replace (Str.regexp "\\( \\|\t\\)*$") "")
    |> (Str.global_replace (Str.regexp "^\\( \\|\t\\)*") "")

  type t = (string * string * Amr.t) array

  exception Stop
  let load corpus_file =
    let buff = Buffer.create 32 in
    let current_meta = ref [] in
    let stack = ref [] in
    let delta = ref 0 in

    let push () = match !current_meta with
      | [] -> ()
      | meta ->
        let sentid = match List.assoc_opt "::id" meta with
        | Some id -> id
        | None -> "No_id" in
        let sent_text = match List.assoc_opt "::snt" meta with
        | Some t -> t
        | None -> "No_text" in
        stack := (sentid, sent_text, Amr.parse_aux ~delta:!delta (Buffer.contents buff)) :: !stack;
        Buffer.clear buff;
        current_meta := [] in

    let rec push_items = function
      | (Str.Delim key) :: (Str.Text value) :: tail ->
        current_meta := (key, rm_peripheral_white value) :: !current_meta;
        push_items tail
      | (Str.Delim key) :: tail -> current_meta := (key, "YES") :: !current_meta; push_items tail
      | (Str.Text _) :: tail -> push_items tail
      | [] -> () in

    let line_num = ref 0 in
    let in_ch = open_in corpus_file in

    let next () = incr line_num; input_line in_ch in

    try
      while true do
        match next () with
        | "" -> push ()
        | s when s.[0] = '#' ->
          let items = Str.full_split (Str.regexp "::[a-z]+") s in
          push_items items
        | s -> bprintf buff "%s\n" s
      done;
      assert false
    with
    | End_of_file ->
      push ();
      Array.of_list !stack
end