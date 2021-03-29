open Printf

(* [read_nlines file] returns a list of couples (line_num, line). *)
let read_nlines file =
  let in_ch = open_in file in
  (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
  (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

  let cpt = ref 0 in
  let rev_lines = ref [] in
  try
    while true do
      let line = input_line in_ch in
      incr cpt;
      rev_lines := (!cpt, line) :: !rev_lines
    done; assert false
  with End_of_file ->
    close_in in_ch;
    List.rev !rev_lines


module Amr = struct
  type t = Amr_types.Amr.t

  exception Error of string (* TODO json *)

  let parse_aux ?(delta=1) sent_id meta amr_string =
    Amr_lexer.line := delta;
    let lexbuf = Lexing.from_string amr_string in
    try
      let node = Amr_parser.amr Amr_lexer.main lexbuf in
      let amr = {
        Amr_types.Amr.sent_id = sent_id;
        node;
        meta;
      } in
      amr
    with
    | Amr_parser.Error ->
      raise (Error (Printf.sprintf "[line %d, sent_id %s] Syntax error: %s" !Amr_lexer.line sent_id (Lexing.lexeme lexbuf)))
    | Failure msg ->
      raise (Error (Printf.sprintf "[line %d, sent_id %s] Error: %s" !Amr_lexer.line sent_id msg))

  let parse amr_string = parse_aux "__no_sent_id__" [] amr_string

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

  let to_json t = Amr_types.Amr.to_json t
end

module Amr_corpus = struct
  let rm_peripheral_white s =
    s
    |> (Str.global_replace (Str.regexp "\\( \\|\t\\)*$") "")
    |> (Str.global_replace (Str.regexp "^\\( \\|\t\\)*") "")

  type t = (string * Amr.t) array

  let of_nlines nlines =
    let buff = Buffer.create 32 in
    let current_meta = ref [] in
    let stack = ref [] in
    let delta = ref None in

    let counter = ref 0 in
    let push () =
      match !current_meta with
      | [] -> () (* get rid of part without any metadata (first lines with info for the whole file) *)
      | _ ->
        incr counter;
        let sent_id = match List.assoc_opt "::id" !current_meta with
          | Some id -> id
          | None -> sprintf "__%05d" !counter in
        let meta =
          !current_meta
          |> (List.remove_assoc "::id" )
          |> (List.map (function ("::snt",t) -> ("text",t) | (k,v) -> (String.sub k 2 ((String.length k) -2),v))) in
        stack := (sent_id, Amr.parse_aux ?delta:!delta sent_id meta (Buffer.contents buff)) :: !stack;
        Buffer.clear buff;
        current_meta := [] in

    let rec push_items = function
      | (Str.Delim key) :: (Str.Text value) :: tail ->
        current_meta := (key, rm_peripheral_white value) :: !current_meta;
        push_items tail
      | (Str.Delim key) :: tail -> current_meta := (key, "YES") :: !current_meta; push_items tail
      | (Str.Text _) :: tail -> push_items tail
      | [] -> () in

    List.iter (
      fun (line_num, line) ->
        match line with
        | "" -> push (); delta := None
        | s when s.[0] = '#' ->
          let items = Str.full_split (Str.regexp "::[-a-z]+") s in
          push_items items
        | s ->
          if !delta=None then delta := Some line_num;
          bprintf buff "%s\n" s
    ) nlines;
    push ();
    Array.of_list (List.rev !stack)

  let load corpus_file = of_nlines (read_nlines corpus_file)

end
