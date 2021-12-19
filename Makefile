build:
	dune build src

utop:
	OCAMLRUNPARAM=b dune utop src

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

tests:
	OCAMLRUNPARAM=b dune exec test/main.exe