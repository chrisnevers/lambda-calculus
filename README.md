# λ-calculus
This project is an implementation of the lambda calculus in OCaml. Hopefully, it will act
as a guide to others who want to learn about:

* Lambda Calculus
* Menhir
* Type Inference
* etc.

## Branches
The `master` branch contains a lexer and a parser for the lambda calculus.

The `type-inference` branch implements a constraint generation and unification algorithm similar to that
described in [Programming Languages and Lambda Calculi](https://www.cs.utah.edu/~mflatt/past-courses/cs7520/public_html/s06/notes.pdf).

## Running The Project

    brew install ocaml opam
    opam install ocamlbuild ounit
    make
    ./main.native

Running the program, will launch a REPL where expressions can be interpretted.
