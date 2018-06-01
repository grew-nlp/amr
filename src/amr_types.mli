module Amr: sig
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

  type t = {
    sent_id: string;
    meta: (string * string) list;
    node: node;
  }

  val print: t -> unit
end

module Amr_corpus : sig
  type t = (string * Amr.t) array
end