variant: variant.ml
	ocamlc \
	-pp camlp4of -I +camlp4 \
	-o variant dynlink.cma camlp4lib.cma variant.ml \
	-package camlp4.quotations.o -package camlp4.lib \
	-linkpkg -o variant variant.ml
