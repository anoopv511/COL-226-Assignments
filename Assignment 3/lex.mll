(*----------------Header----------------*)

{
	open Parser
	open Printf
	exception Eof
	exception Invalid_input
	exception Quit
}

(*-------------Definitions--------------*)

let digit = ['0'-'9']
let alpha = ['a'-'z']
let alpha_caps = ['A'-'Z']
let white_space = (" " | '\t')

(*----------------Rules-----------------*)

rule scan = parse
| digit+ as integer														{ INT (int_of_string integer) }
| "abs"																	{ ABS }
| '+'																	{ ADD }
| '-'																	{ SUB }
| '*'																	{ MUL }
| "div"																	{ DIV }
| "mod" 																{ MOD }
| '~'																	{ NEG }
| '('																	{ OPEN_P}
| ')'																	{ CLOSE_P }
| 'T'																	{ BOOL (bool_of_string "true")}
| 'F'																	{ BOOL (bool_of_string "false")}
| "not"																	{ NOT }
| "/\\"																	{ AND}
| "\\/"																	{ OR }
| '='																	{ EQ }
| '<'																	{ LT }
| '>'																	{ GT }
| "<="																	{ LE }
| ">="																	{ GE }
| "quit" 																{ raise Quit }
| alpha_caps+(alpha_caps|alpha|digit|'_'|'\'')* as var					{ VAR (var) }
| white_space+															{ scan lexbuf }
| '\n'																	{ EOL }
| _																		{ raise Invalid_input }
| eof 																	{ raise Eof }