#!/bin/bash

ocamlfind ocamlopt -package yojson -c test.ml
ocamlfind ocamlopt -package yojson test.cmx -linkpkg -o test
