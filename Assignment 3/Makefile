test: parser.mly lex.mll main.ml
	ocamlyacc -q parser.mly
	ocamllex lex.mll
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamlc -c lex.ml
	ocamlc -c main.ml
	ocamlc -o parser parser.cmo lex.cmo main.cmo
clean:
	rm parser.ml parser.mli parser.cmo parser.cmi lex.ml lex.cmo lex.cmi main.cmo main.cmi