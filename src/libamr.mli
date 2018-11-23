

module Amr : sig
  type t

  val parse: string -> t

  val load: string -> t

  val to_gr: t -> string
end

module Amr_corpus : sig
  type t = (string * string * Amr.t) array

  val load: string -> t
end