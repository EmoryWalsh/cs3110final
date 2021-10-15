.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f cs3110final.zip
	zip -r cs3110final.zip . -x@exclude.lst

clean:
	dune clean
	rm -f cs3110final.zip

doc:
	dune build @doc