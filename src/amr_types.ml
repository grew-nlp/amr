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