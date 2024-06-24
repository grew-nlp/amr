open Printf
open Amr

let _  =
  printf "---- Amr.parse ----\n%!";

  let amr_string = (* https://amr.isi.edu/language.html *)
"(w / want-01
:ARG0 (b / boy)
:ARG1 (g / go-01
        :ARG0 b))
" in

  amr_string
  |> Amr.parse
  |> Amr.to_json ~unfold:true
  |> Yojson.Basic.pretty_to_string
  |> printf "%s\n%!"

let _  =
  printf "---- Amr.load ----\n%!";

  "penman_ex.amr"
  |> Amr.load
  |> Amr.to_json ~unfold:true
  |> Yojson.Basic.pretty_to_string
  |> printf "%s\n%!"

let _  =
  printf "---- Amr.load non Ascii file ----\n%!";

  "test_amr.4.txt"
  |> Amr.load
  |> Amr.to_json ~unfold:true
  |> Yojson.Basic.pretty_to_string
  |> printf "%s\n%!"

  
let _  =
  printf "---- Amr_corpus.load ----\n%!";

  "amr-bank-struct-v3.0.txt"
  |> Amr_corpus.load
  |> Array.length
  |> printf "%d\n%!"

let _  =
  printf "---- unfold=true ----\n%!";
  "(r / remember-01 :polarity - :ARG0 (i / i))"
  |> Amr.parse
  |> Amr.to_json ~unfold:true
  |> Yojson.Basic.pretty_to_string
  |> printf "%s\n%!"

let _  =
  printf "---- unfold=false ----\n%!";
  "(r / remember-01 :polarity - :ARG0 (i / i))"
  |> Amr.parse
  |> Amr.to_json ~unfold:false
  |> Yojson.Basic.pretty_to_string
  |> printf "%s\n%!"
