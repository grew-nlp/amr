module Amr: sig
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
    node: node;
    code: string;
  }

  val print: t -> unit

  val to_gr: t -> string

  (* if [unfold] is set to true, all values are represented as a new node; else there are features *)
  val to_json: ?unfold:bool -> t -> Yojson.Basic.t
end
