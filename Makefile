MODULES = json_repr json_schema json_encoding
ML = $(patsubst %, src/%.ml, $(MODULES))
MLI = $(patsubst %, src/%.mli, $(MODULES))
CMX = $(patsubst %, src/%.cmx, $(MODULES))
CMO = $(patsubst %, src/%.cmo, $(MODULES))
CMI = $(patsubst %, src/%.cmi, $(MODULES))
PACKAGES= 'uri'
OPTS = -bin-annot -g -safe-string -I src -package $(PACKAGES)

.PHONY: all clean doc test

all: \
  src/ocplib_json_typed.cmxa \
  src/ocplib_json_typed.cmxs \
  src/ocplib_json_typed.cma

src/ocplib_json_typed.cmxa: $(CMX)
	ocamlfind ocamlopt $(OPTS) $^ -a -o $@

src/ocplib_json_typed.cmxs: $(CMX)
	ocamlfind ocamlopt $(OPTS) $^ -shared -o $@

src/ocplib_json_typed.cma: $(CMO)
	ocamlfind ocamlc $(OPTS) $^ -a -o $@

test/test.asm: src/ocplib_json_typed.cmxa test/test.ml
	ocamlfind ocamlopt $(OPTS) -package "ezjsonm" src/ocplib_json_typed.cmxa $^ -o $@ -linkpkg

%.cmx: %.ml
	ocamlfind ocamlopt $(OPTS) -c $<

%.cmo: %.ml
	ocamlfind ocamlc $(OPTS) -c $<

%.cmi: %.mli
	ocamlfind ocamlopt $(OPTS) -c $<

-include .depend

.depend: $(ML) $(MLI) test/test.ml Makefile
	ocamlfind ocamldep -package $(PACKAGES) -native $(ML) $(MLI) > $@

doc: $(MLIS)
	-mkdir doc
	ocamlfind ocamldoc  -html -d doc -package $(PACKAGES) $(MLIS)

clean:
	-rm -f */*.cm* */*.o */*.a */*.so */*.dylib */*.dll */*~ *~
	-rm -f test/test.asm
	-rm -rf doc

test: test/test.asm

install: all doc
	ocamlfind install ocplib-json-typed src/*
	mkdir -p $(shell ocamlfind printconf destdir)/../doc/ocplib-json-typed
	cp doc/* $(shell ocamlfind printconf destdir)/../doc/ocplib-json-typed

uninstall:
	ocamlfind remove ocplib-json-typed
	rm $(shell ocamlfind printconf destdir)/../doc/ocplib-json-typed/* -rf
