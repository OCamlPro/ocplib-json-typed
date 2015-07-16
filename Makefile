MLS = json_schema.ml json_typed.ml test.ml
MLIS = json_schema.mli json_typed.mli
CMXS = json_schema.cmx json_typed.cmx test.cmx
CMIS = json_schema.cmi json_typed.cmi

test: $(CMXS)
	ocamlfind ocamlopt -package 'ezjsonm' $^ -o $@ -linkpkg

%.cmx: %.ml
	ocamlfind ocamlopt -package 'ezjsonm' -c $<

%.cmi: %.mli
	ocamlfind ocamlopt -package 'ezjsonm' -c $<

-include .depend

.depend:
	ocamlfind ocamldep -package 'ezjsonm' -native $(MLS) $(MLIS) > $@
