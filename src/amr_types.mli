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
    | Minus
    | Plus

  type t = {
    sent_id: string;
    meta: (string * string) list;
    node: node;
  }

  val print: t -> unit

  val to_gr: t -> string
end
