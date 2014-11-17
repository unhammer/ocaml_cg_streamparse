all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg batteries -pkg menhirLib -pkg sedlex test.native

test: all test.cg
	./test.native test.cg
