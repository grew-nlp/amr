OCB_FLAGS = -use-ocamlfind -use-menhir -I src
OCB = ocamlbuild $(OCB_FLAGS)

LIB_FILES = libamr.cma libamr.cmxa libamr.a libamr.cmi libamr.cmx libamr.cmxs #amr_types.cmi amr_types.cmx
INSTALL_FILES = $(LIB_FILES:%=_build/src/%)

VERSION = `cat VERSION`

.PHONY: build clean test

build:
	$(OCB) $(LIB_FILES)

install: build uninstall
	ocamlfind install -patch-version $(VERSION) amr META $(INSTALL_FILES)

uninstall:
	ocamlfind remove amr

clean:
	$(OCB) -clean
	rm -f amr_test.native

test: amr_test.native
	./amr_test.native

amr_test.native: test/amr_test.ml
	ocamlbuild -use-ocamlfind -pkg amr -I test amr_test.native
