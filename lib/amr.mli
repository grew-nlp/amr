
module Amr : sig
(** The [Amr] module implement a parser for the Penman notation used in the AMR project *)

  exception Error of string

  type t
  (** The type of an AMR graph *)

  val parse: string -> t
  (** Parsing of a string into an AMR graph *)

  val load: string -> t
  (** Parsing of a file into an AMR graph *)

  val to_json: ?unfold:bool -> t -> Yojson.Basic.t
  (** Convert the AMR graph into the Grew JSON encoding of graphs.
  @see<https://grew.fr/doc/json/>
  The flag [unfold] controls how constants are presented:
  - [unfold=true] a value node and an edge are created are created 
  - [unfold=false] the value is added as a feaure

  Example: In [(r / remember-01 :polarity - :ARG0 (i / i))], [polarity] is an edge when [unfold] is [true] and a feature in the node [remember-01] when [unfold] is [false].
  The graph use din Grew-match are produced with [unfold=true].
  *)
  
end

module Amr_corpus : sig
(** The [Amr_corpus] module parse files like [https://amr.isi.edu/download/amr-bank-struct-v3.0.txt] into an array of AMR structure *)

  type t = (string * Amr.t) array
  (** A corpus is an array of pair (sent_id, amr_graph) *)

  val of_nlines: (int*string) list -> t
  (** Build an AMR corpus for a list of lines (given with their numlines for better error reporting) *)
 
  val load: string -> t
  (** Parsing of a corpus from a file *)

  val load_list: string list -> t
  (** Parsing of a corpus from a list of files *)

end