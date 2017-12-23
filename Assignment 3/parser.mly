/*----------------Header----------------*/

%{
	open Printf
	open Scanf

	type value = 
		| Integer of int
		| Boolean of bool
		| String of string;;
	type parse_tree =
		| Empty
		| Node of parse_tree * value * parse_tree;;

	let variable_table = Hashtbl.create 100;;

	let rec ask_for_variable v = 
		try
			read_int (printf "Enter varaible %s = " v)
		with
			Failure "int_of_string" -> print_string "Enter integer value\n"; ask_for_variable v;;

	let rec bind_variable v =
			try (Hashtbl.find variable_table $1) with Not_found -> raise Not_found

	let rec eval_i tree =
		match tree with
			| Empty -> 0
			| Node (left,node_val,right) ->
				match node_val with
					| Integer i -> i
					| String "ADD" -> eval_i(left) + eval_i(right)
					| String "SUB" -> eval_i(left) - eval_i(right)
					| String "MUL" -> eval_i(left) * eval_i(right)
					| String "DIV" -> eval_i(left) / eval_i(right)
					| String "MOD" -> eval_i(left) mod eval_i(right)
					| String "NEG" -> -1 * eval_i(right)
					| String "ABS" -> abs(eval_i(right))
					| String s -> ask_for_variable s
					| _ -> 0
	let rec eval_b tree =
		match tree with
			| Empty -> false
			| Node (left,node_val,right) ->
				match node_val with
					| Boolean b -> b
					| String "NOT" -> not (eval_b(right))
					| String "AND" -> eval_b(left) && eval_b(right)
					| String "OR" -> eval_b(left) || eval_b(right)
					| String "EQ" -> eval_i(left) == eval_i(right)
					| String "LE" -> eval_i(left) <= eval_i(right)
					| String "GE" -> eval_i(left) >= eval_i(right)
					| String "LT" -> eval_i(left) < eval_i(right)
					| String "GT" -> eval_i(left) > eval_i(right)
					| _ -> false
	let rec print_tree tree =
		match tree with
			| Empty -> print_string ""
			| Node (left,node_val,right) -> 
				print_tree left; 
				print_tree right;
				match node_val with
					| Integer i -> print_int i; print_string " "
					| Boolean b -> print_string (string_of_bool b); print_string " "
					| String s -> print_string s; print_string " "

%}

/*-------------Definitions--------------*/

%token <string> VAR
%token <int> INT
%token <bool> BOOL
%token ADD SUB MUL DIV MOD NEG ABS
%token NOT AND OR
%token LT GT LE GE EQ
%token OPEN_P CLOSE_P
%token EOL

%left ADD SUB
%left MUL DIV MOD
%nonassoc ABS NEG
%left AND OR
%nonassoc NOT

%start main
%type <string> main

/*----------------Rules-----------------*/

%%

main:
	| expr_i EOL								{ print_tree $1 ; print_string "\n" ; string_of_int (eval_i $1) }
	| expr_b EOL 								{ print_tree $1 ; print_string "\n" ; string_of_bool (eval_b $1) }
;
expr_i:
	| INT 										{ Node(Empty,Integer $1,Empty) }
	| VAR 										{ Node(Empty,String $1,Empty) }
	| OPEN_P expr_i CLOSE_P 					{ $2 }
	| expr_i ADD expr_i 						{ Node($1,String "ADD",$3) }
	| expr_i SUB expr_i 						{ Node($1,String "SUB",$3) }
	| SUB expr_i 								{ Node(Empty,String "NEG",$2) }
	| expr_i MUL expr_i 						{ Node($1,String "MUL",$3) }
	| expr_i DIV expr_i 						{ Node($1,String "DIV",$3) }
	| ABS OPEN_P expr_i CLOSE_P 				{ Node(Empty,String "ABS",$3) }
	| NEG expr_i 								{ Node(Empty,String "NEG",$2) }
	| expr_i MOD expr_i 						{ Node($1,String "MOD",$3) }
;
expr_b:
	| BOOL 										{ Node(Empty,Boolean $1,Empty) }
	| OPEN_P expr_b CLOSE_P 					{ $2 }
	| NOT expr_b 								{ Node(Empty,String "NOT",$2) }
	| expr_b AND expr_b 						{ Node($1,String "AND",$3) }
	| expr_b OR expr_b 							{ Node($1,String "OR",$3) }
	| expr_i EQ expr_i 							{ Node($1,String "EQ",$3) }
	| expr_i LE expr_i 							{ Node($1,String "LE",$3) }
	| expr_i GE expr_i 							{ Node($1,String "GE",$3) }
	| expr_i LT expr_i 							{ Node($1,String "LT",$3) }
	| expr_i GT expr_i 							{ Node($1,String "GT",$3) }
;