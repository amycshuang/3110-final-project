MODULES=player pokemon author
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,yojson

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

player:
	utop -init player.ml

pokemon:
	utop -init pokemon.ml

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)  
