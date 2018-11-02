open Amr_types

val parse: string -> Amr.t

val load: string -> Amr.t

val load_corpus: string -> (string * Amr.t) array

val to_gr: Amr.t -> string