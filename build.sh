#!/bin/bash

# ocamlfind ocamlopt -package yojson -c test.ml
# ocamlfind ocamlopt -package yojson test.cmx -linkpkg -o test
rm generated.ml || true
camlp4of -I/home/anand/.opam/system/lib/xmlm -I/home/anand/.opam/system/lib/cow -I/home/anand/.opam/system/lib/ezjsonm -I/home/anand/.opam/system/lib/hex -I/home/anand/.opam/system/lib/uutf -I/home/anand/.opam/system/lib/jsonm -I/home/anand/.opam/system/lib/ulex -I/home/anand/.opam/system/lib/uri -I/home/anand/.opam/system/lib/re -I/home/anand/.opam/system/lib/sexplib -I/home/anand/.opam/system/lib/dyntype -I/home/anand/.opam/system/lib/omd -I/home/anand/.opam/system/lib/stringext -I/home/anand/.opam/system/lib/type_conv bigarray.cma str.cma stringext.cma omd.cma re.cma re_posix.cma sexplib.cma uutf.cma hex.cma xmlm.cma jsonm.cma ezjsonm.cma ulexing.cma uri.cma pa_type_conv.cma pa_dyntype.cma cow.cma pa_cow.cma generate.ml > desugared.ml

ocamlfind ocamlc -g -package cow -package cow.syntax -package camlp4.quotations.o -package camlp4.lib -package yojson -package core -package str -thread -linkpkg desugared.ml -o generate && echo "success"; ./generate > generated.ml

# ocamlfind ocamlc -g -I +camlp4 -I +cow -pp camlp4of -package cow -package cow.syntax -package camlp4.quotations.o -package camlp4.lib -package yojson -package core -package str -thread -linkpkg generate.ml -o generate && echo "success"; ./generate > generated.ml
