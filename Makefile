SOURCES = syntax.ml parser.mly lexer.mll eval.ml main.ml value.ml
RESULT = interpreter
OCAMLMAKEFILE = ~/.opam/4.04.0/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
