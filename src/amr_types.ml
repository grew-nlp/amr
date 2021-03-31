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
    | Data of string
    | Ref of string
    | Minus
    | Plus

  type t = {
    sent_id: string;
    meta: (string * string) list;
    node: node;
    code: string;
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
               loop ("label", sprintf "\"%s\"" n.concept) (node::already_done) n;
               acc
             | Data s -> (lab, "\""^s^"\"") :: acc
             | Minus -> (lab, "\"-\"") :: acc
             | Plus -> (lab, "\"+\"") :: acc
             | Ref r when String_set.mem r ids -> bprintf buff " %s -[%s]-> %s;\n" node.id lab r; acc
             | Ref r -> (lab, r) :: acc
          ) [init] node.next in
      let l = String.concat "," (List.map (fun (x,y) -> sprintf "%s=%s" x y) label) in
      bprintf buff " %s [%s];\n" node.id l in
    loop ("label", sprintf "\"%s\"" t.node.concept) [] t.node;
    bprintf buff "}\n";
    Buffer.contents buff

  let to_json t =
    let ids = to_ids t in
    let rec loop (acc_nodes, acc_edges) node =
      (* let new_nodes = (node.id, `String node.concept) :: acc_nodes in *)
      let (json_node, new_nodes, new_edges) =
        List.fold_left
          (fun (acc_node, acc2_nodes, acc2_edges) (label,value) ->
             match value with
             | Data s -> ((label,`String s)::acc_node, acc2_nodes, acc2_edges)
             | Plus -> ((label,`String "+")::acc_node, acc2_nodes, acc2_edges)
             | Minus -> ((label,`String "-")::acc_node, acc2_nodes, acc2_edges)
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
    let (nodes, edges) = loop ([],[]) t.node in

    let meta = ("sent_id", `String t.sent_id) ::
      (List.map (fun (k,v) -> (k, `String v)) t.meta) in
    `Assoc [
      ("meta", `Assoc meta);
      ("nodes", `Assoc nodes);
      ("edges", `List edges);
      ("code", `String t.code);
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
    | Data s -> Printf.printf "\"%s\"" s
    | Minus -> Printf.printf " -"
    | Plus -> Printf.printf " +"
    | Ref s -> Printf.printf " %s" s

  let print t = print_node t.node
end

