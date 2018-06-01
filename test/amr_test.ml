open Printf

let amr = Amr_lib.parse "(c / chapter :mod 1)"

let _ =
  printf "======================= Amr_lib.parse =======================\n";
  Amr_types.Amr.print amr;
  printf "\n%!"

let amr = Amr_lib.load "test/data/amr"

let _ =
  printf "======================= Amr_lib.load =======================\n";
  Amr_types.Amr.print amr;
  printf "\n%!"

