module Amr : sig
  type t

  exception Error of string

  val parse: string -> t

  val load: string -> t

  val to_gr: t -> string

  (* if [unfold] is set to true, all values are represented as a new node; else there are features *)
  val to_json: ?unfold:bool -> t -> Yojson.Basic.t
end

module Amr_corpus : sig
  type t = (string * Amr.t) array

  val of_nlines: (int*string) list -> t

  val load: string -> t
end