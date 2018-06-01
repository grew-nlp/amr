open Amr_types

val parse: string -> Amr.t

val load: string -> Amr.t

val load_corpus: string -> (string * Amr.t) array