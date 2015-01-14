all:
	ocamlbuild -tag thread -use-ocamlfind -quiet -pkg batteries -pkg pcre test.native

test: all test.cg
	./test.native test.cg
