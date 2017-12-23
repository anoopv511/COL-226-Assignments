(*----------------Header----------------*)

{
	open Alpha_parser
	open Printf
	exception Eof
	exception Invalid_input
}

(*-------------Definitions--------------*)

let digit = ['0'-'9']
let alpha = ['a'-'z']
let alpha_caps = ['A'-'Z']
let white_space = (" " | '\t')

(*----------------Rules-----------------*)

rule scan = parse
| (digit+ | digit+'.'digit+) as numeral									{ ATOM (numeral) }
| '\''[^'\'']'\'' as quot_str											{ ATOM (quot_str) }
| alpha+(alpha_caps | alpha | digit | '_')* as str						{ ATOM (str) }
| (alpha_caps | '_')+(alpha_caps | alpha | digit | '_')* as var_name	{ VAR (var_name) }
| '('																	{ OPEN_P }
| ')'																	{ CLOSE_P }
| '.'																	{ DOT }
| ','																	{ COMMA }
| '['																	{ SQ_OPEN_P }
| ']'																	{ SQ_CLOSE_P }
| ":-"																	{ RULE }
| "?-"																	{ QUERY }
| '='																	{ EQ }
| '\\'																	{ NOT }
| '\n'+																	{ EOL }
| white_space+															{ scan lexbuf }
| _																		{ raise Invalid_input }
| eof 																	{ raise Eof }