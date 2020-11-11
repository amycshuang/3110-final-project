MODULES=state player gui pokemon author main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,yojson,ANSITerminal,graphics

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)  

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip -r search.zip *.ml* _tags Makefile engine_test