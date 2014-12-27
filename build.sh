#!/bin/bash

# ocamlfind ocamlopt -package yojson -c test.ml
# ocamlfind ocamlopt -package yojson test.cmx -linkpkg -o test

ocamlfind ocamlc -g -pp camlp4of -I +camlp4 -package camlp4.quotations.o -package camlp4.lib -package yojson -package core -package str -thread -linkpkg test.ml -o test

# ocamlfind ocamlopt -pp camlp4of -I +camlp4 -package camlp4.quotations.o -package camlp4.lib -package yojson -linkpkg -o test