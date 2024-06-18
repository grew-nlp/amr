module Ast: sig
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
    sent_id: string option;
    meta: (string * string) list;
    graph: node;
    penman: string option;
  }

  val print: t -> unit

  (* if [unfold] is set to true, all values are represented as a new node; else there are features *)
  val to_json: ?unfold:bool -> t -> Yojson.Basic.t
end
