open Printf


let _ =
  printf "======================= Amr_lib.parse =======================\n";
  let amr = Amr_lib.parse "(c / chapter :mod 1)" in
  Amr_types.Amr.print amr;
  printf "\n%!"


let _ =
  printf "======================= Amr_lib.load =======================\n";
  let amr = Amr_lib.load "test/data/amr" in
  Amr_types.Amr.print amr;
  printf "\n%!"


let _ =
  printf "======================= Amr_lib.load =======================\n";
  let amr = Amr_lib.load_corpus "test/data/amr-bank-v1.6.txt" in
  Amr_types.Amr.print (snd amr.(0));
  printf "\n%!"

