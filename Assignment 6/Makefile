test: alpha_parser.mly alpha_lex.mll alpha_main.ml
	ocamlyacc -qv alpha_parser.mly
	ocamllex alpha_lex.mll
	ocamlc -c alpha_parser.mli
	ocamlc -c alpha_parser.ml
	ocamlc -c alpha_lex.ml
	ocamlc -c alpha_main.ml
	ocamlc -o alpha_parser alpha_parser.cmo alpha_lex.cmo alpha_main.cmo
clean:
	rm alpha_parser.ml alpha_parser.mli alpha_parser.cmo alpha_parser.cmi alpha_lex.ml alpha_lex.cmo alpha_lex.cmi alpha_main.cmo alpha_main.cmi alpha_parser.output