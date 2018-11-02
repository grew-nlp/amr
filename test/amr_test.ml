open Printf

(*
let _ =
  printf "======================= Amr_lib.parse =======================\n";
  let amr = Amr_lib.parse "(c / chapter :mod 1)" in
  Amr_types.Amr.print amr;
  printf "\n%!"


let _ =
  printf "======================= Amr_lib.load =======================\n";
  let amr = Amr_lib.load "test/one.amr" in
  Amr_types.Amr.print amr;
  printf "\n%!"
 *)

let _ =
  printf "======================= Amr_lib.load =======================\n";
  let amr_corpus = Amr_lib.load_corpus "test/AMR/amr-bank-v1.6.txt" in
  let amr = snd amr_corpus.(1560) in
  Amr_types.Amr.print amr;
  printf "\n%!";
  printf "======================= Amr_lib.to_gr =======================\n";
  printf "%s\n%!" (Amr_lib.to_gr amr)



