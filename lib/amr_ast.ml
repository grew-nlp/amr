open Printf

module String_set = Set.Make (String)

module Ast = struct
  type node = {
    id: string;
    concept: string;
    next: (string * value) list
  }

  and value =
    | Node of node
    | Data of string
    | Ref of string

  type t = {
    sent_id: string;
    meta: (string * string) list;
    graph: node;
    penman: string;
  }

  let to_ids t =
    let rec loop node =
      List.fold_left
        (fun acc (_,value) -> match value with
           | Node n -> String_set.union (loop n) acc
           | _ -> acc
        ) (String_set.singleton node.id) node.next in
    loop t.graph

  let to_json ?(unfold=false) t : Yojson.Basic.t =
    let cpt = ref 0 in
    let fresh_id () = incr cpt; sprintf "v_%d" !cpt in
    let ids = to_ids t in
    let rec loop (acc_nodes, acc_edges) node =
      (* let new_nodes = (node.id, `String node.concept) :: acc_nodes in *)
      let (json_node, new_nodes, new_edges) =
        List.fold_left
          (fun (acc_node, acc2_nodes, acc2_edges) (label,value) ->
             match value with
             | Data s when not unfold -> ((label,`String s)::acc_node, acc2_nodes, acc2_edges)
             | Data s ->
                let new_id = fresh_id () in
                let new_node = (new_id, `Assoc [("value", `String s)]) in
                let new_edge = `Assoc [("src", `String node.id); ("label", `String label); ("tar", `String new_id)] in
                 (acc_node, new_node::acc2_nodes, new_edge :: acc2_edges)
             | Node n ->
                let edge = `Assoc [("src", `String node.id); ("label", `String label); ("tar", `String n.id)] in
                let (new_nodes,new_edges) = loop (acc2_nodes, edge :: acc2_edges) n in
                (acc_node, new_nodes, new_edges)
             | Ref r when String_set.mem r ids ->
                let edge = `Assoc [("src", `String node.id); ("label", `String label); ("tar", `String r)] in
               (acc_node, acc2_nodes, edge :: acc2_edges)
             | Ref r -> ((label,`String r)::acc_node, acc2_nodes, acc2_edges)
          ) ([("concept", `String node.concept)], acc_nodes, acc_edges) node.next in
      ((node.id, `Assoc json_node)::new_nodes, new_edges) in
    let (nodes, edges) = loop ([],[]) t.graph in

    let meta = 
      ("sent_id", `String t.sent_id) ::
      ("code", `String t.penman) ::
      (List.map (fun (k,v) -> (k, `String v)) t.meta) in
    `Assoc [
      ("meta", `Assoc meta);
      ("nodes", `Assoc nodes);
      ("edges", `List edges);
    ]

  let rec print_node ?(pref="") t =
    match t.next with
    | [] -> Printf.printf "(%s / %s)" t.id t.concept;
    | l ->
      Printf.printf "(%s / %s" t.id t.concept;
      List.iter (fun (label, value) -> Printf.printf "\n%s%s" (pref^"      ") label; print_value (pref^"      ") value) l;
      Printf.printf ")"

  and print_value pref = function
    | Node t -> print_node ~pref t
    | Data "-" -> Printf.printf " -"
    | Data "+" -> Printf.printf " +"
    | Data s -> Printf.printf "\"%s\"" s
    | Ref s -> Printf.printf " %s" s

  let print t = print_node t.graph
end

