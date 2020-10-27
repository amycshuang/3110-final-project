MODULES=player pokemon author gui
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,yojson,ANSITerminal,graphics,camlimages,Map.png

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)  

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)
