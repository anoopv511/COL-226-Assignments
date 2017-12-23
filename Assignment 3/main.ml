let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
                let result = Parser.main Lex.scan lexbuf in
            		print_string "ANS: ";
            		print_string result;
            		print_newline();
            		flush stdout;
            done
    with 
    	| Lex.Invalid_input -> print_string "INVALID INPUT\n";
    	| Parsing.Parse_error -> print_string "IMPROPER INPUT\n";
    	| Division_by_zero -> print_string "Cannot divide by Zero\n";
    	| Not_found -> print_string "VARIABLE NOT DEFINED\n";
        | Lex.Quit -> exit 0;
    	| Lex.Eof -> exit 0;
