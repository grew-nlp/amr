open Printf

module String_set = Set.Make (String)

module Amr = struct
  type node = {
    id: string;
    concept: string;
    next: (string * value) list
  }

  and value =
    | Node of node
    | String of string
    | Ref of string
    | Int of int
    | Minus
    | Plus

  type t = {
    sent_id: string;
    meta: (string * string) list;
    node: node;
  }

  let to_ids t =
    let rec loop node =
      List.fold_left
        (fun acc (_,value) -> match value with
          | Node n -> String_set.union (loop n) acc
          | _ -> acc
        ) (String_set.singleton node.id) node.next in
    loop t.node

  let to_gr t =
    let buff = Buffer.create 32 in
    let ids = to_ids t in
    bprintf buff "graph {\n";
    let rec loop init already_done node =
      let label = List.fold_left
      (fun acc (lab,value) ->
        match value with
        | Node n ->
          bprintf buff " %s -[%s]-> %s;\n" node.id lab n.id;
          loop ("CONCEPT", n.concept) (node::already_done) n;
          acc
        | String s -> (lab, s) :: acc
        | Int i -> (lab, string_of_int i) :: acc
        | Minus -> (lab, "-") :: acc
        | Plus -> (lab, "+") :: acc
        | Ref r when String_set.mem r ids -> bprintf buff " %s -[%s]-> %s;\n" node.id lab r; acc
        | Ref r -> (lab, r) :: acc
      ) [init] node.next in
      let l = String.concat "," (List.map (fun (x,y) -> sprintf "%s=%s" x y) label) in
      bprintf buff " %s [label=\"%s\"];\n" node.id l in
    loop ("CONCEPT",t.node.concept) [] t.node;
    bprintf buff "}\n";
    Buffer.contents buff


  let rec print_node ?(pref="") t =
    match t.next with
    | [] -> Printf.printf "(%s / %s)" t.id t.concept;
    | l ->
    Printf.printf "(%s / %s" t.id t.concept;
    List.iter (fun (label, value) -> Printf.printf "\n%s%s" (pref^"      ") label; print_value (pref^"      ") value) l;
    Printf.printf ")"

  and print_value pref = function
    | Node t -> print_node ~pref t
    | String s -> Printf.printf "\"%s\"" s
    | Int i -> Printf.printf "%d" i
    | Minus -> Printf.printf " -"
    | Plus -> Printf.printf " +"
    | Ref s -> Printf.printf " %s" s

  let print t = print_node t.node
end

module Amr_corpus = struct
  type t = (string * Amr.t) array
end