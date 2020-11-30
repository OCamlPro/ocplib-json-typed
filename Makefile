all:
	dune build

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *~ */*~
