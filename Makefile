SOURCES = syntax.ml parser.mly lexer.mll value.ml eval.ml main.ml
RESULT = interpreter
OCAMLMAKEFILE = ~/.opam/4.04.0/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
