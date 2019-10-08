BUILD_PKGS=
TEST_PKGS=oUnit
BUILD_FLAGS=-Is src -use-menhir
DEBUG_FLAGS=-tag 'debug'
TEST_FLAGS=-use-ocamlfind -pkgs ${TEST_PKGS} -Is src -use-menhir

all: main

main:
	ocamlbuild ${BUILD_FLAGS} src/main.native

debug:
	ocamlbuild ${BUILD_FLAGS} ${DEBUG_FLAGS} src/main.d.byte

test:
	ocamlbuild ${TEST_FLAGS} tests/test.native --

ex:
	ocamlbuild ${TEST_FLAGS} tests/exampleTest.native --

clean:
	ocamlbuild -clean
