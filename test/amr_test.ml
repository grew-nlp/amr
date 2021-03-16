open Printf
open Libamr


let _ =
  printf "======================= Amr_lib.parse =======================\n";
  let amr = Amr.parse "(c / chapter :mod 1)" in
  printf "%s\n%!" (Amr.to_gr amr)

let _ =
  printf "======================= Amr_lib.load =======================\n";
  let amr = Amr.load "one.amr" in
  printf "%s\n%!" (Amr.to_gr amr)

let _ =
  printf "======================= Amr_lib.load =======================\n";
  let amr_corpus = Amr_corpus.load "AMR/lpp_1943.amr" in
  let (_,amr) = amr_corpus.(10) in
  printf "%s\n%!" (Amr.to_gr amr);

  printf "======================= JSON =======================\n";
  printf "%s\n%!" (Yojson.pretty_to_string (Amr.to_json amr));

  Array.iter
    (fun (_,amr) ->
      (* printf "id:%s\n%!" id; *)
      ignore (Amr.to_json amr)
    ) amr_corpus
