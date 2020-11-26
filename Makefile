MODULES=state initial player gui pokemon author main block walking encounter pokemon_json_script map_json_script
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 
PKGS=oUnit,yojson, ANSITerminal, graphics
POKEJSONBUILD=pokemon_json_script.byte
MAPJSONBUILD=map_json_script.byte

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

check:
	bash checkenv.sh && bash checktypes.sh

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf search.zip doc.public doc.private 

loc: 
	cloc .

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

pokemon_json:
	$(OCAMLBUILD) $(POKEJSONBUILD) && ./$(POKEJSONBUILD)

map_json:
	$(OCAMLBUILD) $(MAPJSONBUILD) && ./$(MAPJSONBUILD)

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip -r pokemon.zip *.ml* *.json *.md _tags Makefile 