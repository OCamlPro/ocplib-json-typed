MLS = json_repr.ml json_schema.ml json_typed.ml test.ml
MLIS = json_repr.mli json_schema.mli json_typed.mli
CMXS = $(patsubst %.ml, %.cmx, $(MLS))
CMIS = $(patsubst %.mli, %.cmxi, $(MLIS))
PACKAGES= 'ezjsonm,uri'

test: $(CMXS)
	ocamlfind ocamlopt -g -package $(PACKAGES) $^ -o $@ -linkpkg

doc:
	-mkdir doc
	ocamlfind ocamldoc  -html -d doc -package $(PACKAGES) $(MLIS)

%.cmx: %.ml
	ocamlfind ocamlopt -g -package $(PACKAGES) -c $<

%.cmi: %.mli
	ocamlfind ocamlopt -g -package $(PACKAGES) -c $<

-include .depend

.depend: $(MLS) $(MLIS) Makefile
	ocamlfind ocamldep -package $(PACKAGES) -native $(MLS) $(MLIS) > $@
