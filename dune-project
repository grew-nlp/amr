(lang dune 3.4)

(name amr)
(version 0.8.1)

(using menhir 2.0)

(generate_opam_files true)

(source
  (github grew-nlp/amr)
)

(authors "Bruno Guillaume")

(maintainers "Bruno Guillaume")

(bug_reports <https://github.com/grew-nlp/amr/issues>)

(license CECILL-2.1)

(documentation https://github.com/grew-nlp/amr)

(package
 (name amr)
 (synopsis "Parsing of Abstract Meaning Representation corpora")
 (description "This library was built to be used by Grew to deal with AMR corpora")
 (depends ocaml dune containers yojson menhir sedlex)
 (tags
  ("nlp" "Abstract Meaning Representation")) 
 (conflicts libcaml-amr)
)
