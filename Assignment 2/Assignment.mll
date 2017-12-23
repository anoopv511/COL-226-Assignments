(*----------------Header----------------*)

{
	open Printf
}

(*-------------Definitions--------------*)

let nonZ = ['1'-'9']
let digit = ['0'-'9']
let alpha = ['a'-'z']
let alpha_caps = ['A'-'Z']
let white_space = (" " | '\t')

(*----------------Rules-----------------*)

rule scan = parse
| ('+'nonZ+digit* | '-'nonZ+digit* | nonZ+digit*) as integer			{printf "[Int (%s)] " integer; scan lexbuf}
| ('+'('0')+digit* | '-'('0')+digit* | ('0')+digit*) as iinteger		{printf "[Invalid Int (%s)] " iinteger; scan lexbuf}
| "abs"																	{printf "[ABS] "; scan lexbuf}
| '+'																	{printf "[ADD] "; scan lexbuf}
| '-'																	{printf "[SUB] "; scan lexbuf}
| '*'																	{printf "[MUL] "; scan lexbuf}
| ('/' | "div")															{printf "[DIV] "; scan lexbuf}
| ('%' | "mod")															{printf "[MOD] "; scan lexbuf}
| ('^' | "exp")															{printf "[EXP] "; scan lexbuf}
| '('																	{printf "[Open_P] "; scan lexbuf}
| ')'																	{printf "[Close_P] "; scan lexbuf}
| 'T'																	{printf "[True] "; scan lexbuf}
| 'F'																	{printf "[False] "; scan lexbuf}
| "not"																	{printf "[NOT] "; scan lexbuf}
| "/\\"																	{printf "[AND] "; scan lexbuf}
| "\\/"																	{printf "[OR] "; scan lexbuf}
| '='																	{printf "[eq] "; scan lexbuf}
| '<'																	{printf "[lt] "; scan lexbuf}
| '>'																	{printf "[gt] "; scan lexbuf}
| "<="																	{printf "[le] "; scan lexbuf}
| ">="																	{printf "[ge] "; scan lexbuf}
| "if"																	{printf "[If] "; scan lexbuf}
| "then"																{printf "[Then] "; scan lexbuf}
| "else"																{printf "[Else] "; scan lexbuf}
| "def"																	{printf "[DEF] "; scan lexbuf}
| alpha+(alpha_caps|alpha|digit)* as id									{printf "[ID (%s)] " id; scan lexbuf}
| (alpha_caps|digit)+(alpha_caps|alpha|digit)* as iid					{printf "[Invalid ID -(%s)] " iid; scan lexbuf}
| ';'																	{printf "[End] "; scan lexbuf}
| white_space+															{scan lexbuf}
| ('\n')*																{printf "\n"; scan lexbuf}
| _																		{printf "[Unknown] "; scan lexbuf}
| eof																	{ () }

(*---------------Trailer----------------*)

{
printf "-------Begin-------\n\n"
let start () =
	let cin =
		if Array.length Sys.argv > 1 then open_in Sys.argv.(1)
		else stdin in
			let lexbuf = Lexing.from_channel cin in
				scan lexbuf;
printf "\n-------Done-------\n"
let _ = Printexc.print start ()
}