module Amr : sig
  type t

  exception Error of string

  val parse: string -> t

  val load: string -> t

  val to_gr: t -> string

  val to_json: t -> Yojson.Basic.t
end

module Amr_corpus : sig
  type t = (string * Amr.t) array

  val of_nlines: (int*string) list -> t

  val load: string -> t
end