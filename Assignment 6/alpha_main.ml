let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
                let result = Alpha_parser.main Alpha_lex.scan lexbuf in
            		print_string "ANS: ";
            		print_string result;
            		print_newline();
            		flush stdout;
            done
    with 
    	| Alpha_lex.Invalid_input -> print_string "INVALID INPUT\n";
    	| Parsing.Parse_error -> print_string "IMPROPER INPUT\n";
    	| Alpha_lex.Eof -> exit 0;