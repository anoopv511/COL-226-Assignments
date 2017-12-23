/*----------------Header----------------*/

%{
	open Printf
	open Scanf

	(*--------------------------------------------------------------------------------------------------*)

	type symbol =
		| Character of char
		| String of string;;

	type variable =
		| Integer of int
		| Boolean of bool
		| Str of string;;

	type term =
		| V of variable
		| Node of symbol*(term list);;

	type sigma = S of (variable*term);;

	exception Invalid_Signature;;
	exception NOT_UNIFIABLE;;

	let rec match_sym (x:symbol*int) (l:(symbol*int) list) =
		match l with
			| [] -> true
			| s :: l -> (fst(s) <> fst(x) || (fst(s) = fst(x) && snd(s) <> snd(x))) && match_sym x l;; 

	let rec check_sig (l:(symbol*int) list) =
		match l with
			| [] -> true
			| x :: l -> if snd(x) < 0 || not (match_sym x l) then false else check_sig l;;

	let check_sym (x:symbol) (y:symbol*int) = fst(y) = x;;

	let get_sym (x:symbol) (l:(symbol*int) list) = List.find_all (check_sym x) l;;

	let get_arity (x:symbol) (l:(symbol*int) list) = List.map snd (get_sym x l);;

	let rec wfterm (t:term) (l:(symbol*int) list) =
		if check_sig l then 
			match t with 
			| V v -> true
			| Node (n,t_list) -> if not (List.mem (List.length t_list) (get_arity n l)) then false else wfterm_sub t_list l
		else raise Invalid_Signature
	and wfterm_sub (t_sub:term list) (l:(symbol*int) list) = 
		match t_sub with
			| [] -> true
			| x :: t_sub -> wfterm x l && wfterm_sub t_sub l;;

	let rec ht (t:term) =
		match t with
			| V v -> 1
			| Node (n,t_list) -> 1 + ht_sub t_list
	and ht_sub (t_sub:term list) = 
		match t_sub with
			| [] -> 0
			| x :: t_sub -> if (ht x) > (ht_sub t_sub) then (ht x) else (ht_sub t_sub);;

	let rec size (t:term) = 
		match t with
			| V v -> 1
			| Node (n,t_list) -> 1 + size_sub t_list
	and size_sub (t_sub:term list) =
		match t_sub with
			| [] -> 0
			| x :: t_sub -> (size x) + (size_sub t_sub);;

	let rec vars (t:term) =
		match t with
			| V v -> [v]
			| Node (n,t_list) -> vars_sub t_list
	and vars_sub (t_sub:term list) = 
		match t_sub with
			| [] -> []
			| x :: t_sub -> (vars x) @ (vars_sub t_sub);;

	let rec find_term (x:variable) (s:sigma list) =
		match s with
			| [] -> V x
			| S (n,t) :: s -> if n = x then t else find_term x s;;

	let rec check_var (x:variable) (s:sigma list) =
		match s with
			| [] -> true
			| S (n,t) :: s -> if n = x then false else check_var x s;;

	let rec subst (t:term) (s:sigma list) =
		match t with
			| V v -> find_term v s
			| Node (n,t_list) -> Node (n,subst_sub t_list s)
	and subst_sub (t_sub:term list) (s:sigma	 list) =
		match t_sub with
			| [] -> []
			| x :: t_sub -> [subst x s] @ subst_sub t_sub s;;

	let rec substitute (s1:sigma list) (s2:sigma list) =
		match s1 with
			| [] -> []
			| S (n,t) :: s1 -> [S (n,subst t s2)] @ substitute s1 s2;;

	let rec diff (s1:sigma list) (s2:sigma list) =
		match s2 with
			| [] -> []
			| S (n,t) :: s2 -> if check_var n s1 then [S (n,t)] @ diff s1 s2 else diff s1 s2;;

	let rec add (s:sigma) (s_l:sigma list) =
		match s_l with
			| [] -> [s]
			| x :: s_l -> [x] @ add s s_l;;

	let composition (s1:sigma list) (s2:sigma list) = substitute s1 s2 @ diff s1 s2;;

	let rec mgu (t1:term) (t2:term) =
		match t1,t2 with
			| V x,V y -> [S (x,V y)]
			| V x,Node (n,t) -> if not (List.mem x (vars t2)) then [S (x,t2)] else raise NOT_UNIFIABLE
			| Node (n,t),V x -> if not (List.mem x (vars t1)) then [S (x,t1)] else raise NOT_UNIFIABLE
			| Node (m,m_t),Node (n,n_t) -> 
				if m <> n then raise NOT_UNIFIABLE 
				else if List.length m_t <> List.length n_t then raise NOT_UNIFIABLE
				else match m_t,n_t with
						| [],[] -> []
						| a :: m_t,b :: n_t -> composition (mgu_sub m_t n_t (mgu a b)) (mgu a b)
						| ([],_ | _,[]) -> raise NOT_UNIFIABLE
	and mgu_sub (t1_l:term list) (t2_l:term list) (s_l:sigma list) =
		match t1_l,t2_l with
			| [],[] -> s_l
			| ([],_ | _,[]) -> raise NOT_UNIFIABLE
			| p :: t1_l,q :: t2_l -> composition (mgu_sub t1_l t2_l (mgu (subst p s_l) (subst q s_l))) (mgu (subst p s_l) (subst q s_l));;

	(*--------------------------------------------------------------------------------------------------*)

	let signature = ref [];;
	let all_rule_heads = ref [];;
	let all_rules = ref [];;

	let string_of_variable (v:variable) =
		match v with
			| Integer i -> string_of_int i
			| Boolean b -> string_of_bool b
			| Str s -> s;;

	let string_of_symbol (sym:symbol) =
		match sym with
			| Character c -> String.make 1 c
			| String s -> s;;

	let head_node (t:term) =
		match t with
			| V v -> (String (string_of_variable v),0)
			| Node (s,t_list) -> (String (string_of_symbol s),List.length t_list);;

	let rec get_rules_with_head (t:term) (r_list:(term*(term list)) list) =
		match r_list with
			| [] -> []
			| r :: r_list -> if (head_node t) = (head_node (fst(r))) then [r] @ (get_rules_with_head t r_list) else (get_rules_with_head t r_list);;

	let string_of_list string_of_a (l:'a list) =
		let rec string_of_elements = function
			| [] -> ""
			| h :: t -> (string_of_a h) ^ ";" ^ (string_of_elements t) 
		in "[" ^ (string_of_elements l) ^ "]";;

	let string_of_signature (sign:(symbol*int)) = "{" ^ (string_of_symbol (fst(sign))) ^ ", " ^ (string_of_int (snd(sign))) ^ "}";;

	let rec string_of_term (t:term) =
		match t with
			| V v -> "[" ^ (string_of_variable v) ^ "]"
			| Node (sym,t_list) -> "(" ^ (string_of_symbol sym) ^ "," ^ (string_of_list string_of_term t_list) ^ ")";;

	let string_of_sigma (sub:sigma) =
		match sub with
			| S (v,t) -> "(" ^ (string_of_variable v) ^ "," ^ (string_of_term t) ^ ")";;

	let rec all_mgus (t:term) (l:term list) =
		match l with
			| [] -> []
			| x :: l ->
				(try
					[(mgu t x)] @ (all_mgus t l)
				with
					NOT_UNIFIABLE -> (all_mgus t l));;

	let rec string_of_all_mgus (mgu_list:(sigma list) list) =
		match mgu_list with
			| [] -> ""
			| x :: mgu_list -> (string_of_list string_of_sigma x) ^ " ;\n" ^ (string_of_all_mgus mgu_list);;

	let rec append_1_1 (a:'a list) (b:'a list) =
		match b with
			| [] -> []
			| x :: b -> [(a @ [x])] @ append_1_1 a b;;

	let append_1_2 (a:'a list) (b:'a list list) = List.map (append_1_1 a) b;;

	let append_3_4 (a:'a list list list) (b:'a list list list list) = List.map2 (append_1_2) (List.flatten a) (List.map (List.flatten) b);;

	let rec multi_subst (s_list:(sigma list) list) (t_list:term list) =
		match t_list with
			| [] -> []
			| t :: t_list -> (List.map (subst t) s_list) @ multi_subst s_list t_list;;

	let rec solve_rule_body (t_list:term list) (subst_list:(sigma list) list) =
		match t_list with
			| [] -> []
			| x :: t_list -> 
				if List.length t_list > 0 then
					List.flatten (append_3_4 [(List.map fst (List.map solve (List.map (subst x) subst_list)))] (List.map (solve_rule_body (multi_subst subst_list t_list)) (List.map fst (List.map solve (List.map (subst x) subst_list)))))
				else
					[(List.map fst (List.map solve (List.map (subst x) subst_list)))]
	and solve (query:term) =
		if not (List.mem (head_node query) (!signature)) then ([],"Undefined procedure: " ^ (string_of_symbol (fst(head_node query))) ^ "/" ^ (string_of_int (snd(head_node query))) ^ ".\n")
		else let solve_rule (r:term*(term list)) =
			(* print_string ("all_mgus -> " ^ (string_of_all_mgus (all_mgus (fst(r)) [query])) ^ " done."); *)
			if List.length (snd(r)) = 0 then (all_mgus query [(fst(r))])
			else List.flatten (List.flatten (solve_rule_body (snd(r)) (all_mgus (fst(r)) [query]))) in
				let solved_rules = List.map solve_rule (get_rules_with_head query (!all_rules)) in
					(match solved_rules with
						| [] -> ([],"Undefined procedure.")
						| [[]] -> ([],"false.")
						| l -> if List.length (List.flatten l) = 0 then ((List.flatten l),"false.")) else ((List.flatten l),"true."));;

%}

/*-------------Definitions--------------*/

%token <string> VAR
%token <string> ATOM
%token DOT COMMA RULE EQ NOT QUERY
%token OPEN_P CLOSE_P SQ_OPEN_P SQ_CLOSE_P
%token EOL

%start main
%type <string> main

/*----------------Rules-----------------*/

%%

main:
	| statement EOL 							{
													(*(string_of_list string_of_signature (!signature)) ^ "\n" ^ *)(string_of_list string_of_term (!all_rule_heads))
												}
;
statement:
	| expr RULE expr DOT						{
													if check_sig (!signature @ [head_node $1]) then signature := !signature @ [head_node $1];
													all_rule_heads := !all_rule_heads @ [$1];
													all_rules := !all_rules @ [($1, [$3])];
													($1, [$3])
												}
	| expr RULE expr c_expr	DOT					{
													if check_sig (!signature @ [head_node $1]) then signature := !signature @ [head_node $1];
													all_rule_heads := !all_rule_heads @ [$1];
													all_rules := !all_rules @ [($1, [$3] @ $4)];
													($1, [$3] @ $4)
												}
	| expr DOT									{
													if check_sig (!signature @ [head_node $1]) then signature := !signature @ [head_node $1];
													all_rule_heads := !all_rule_heads @ [$1];
													all_rules := !all_rules @ [($1, [])];
													($1, [])
												}
	| QUERY expr DOT 							{
													print_string (string_of_list (string_of_list string_of_sigma) (fst(solve $2)) ^ "\n" ^ (snd(solve $2)) ^ "\n");
													($2, [])
												}
;
expr:
	| ATOM OPEN_P ATOM CLOSE_P 					{
													Node (String $1,[Node (String $3,[])])
												}
	| ATOM OPEN_P VAR CLOSE_P 					{
													Node (String $1,[V (Str $3)])
												}
	| ATOM OPEN_P expr CLOSE_P 					{
													Node (String $1,[$3])
												}
	| ATOM OPEN_P ATOM param CLOSE_P 			{
													Node (String $1,[Node (String $3,[])] @ $4)
												}
	| ATOM OPEN_P VAR param CLOSE_P 			{
													Node (String $1,[V (Str $3)] @ $4)
												}
	| ATOM OPEN_P expr param CLOSE_P 			{
													Node (String $1,[$3] @ $4)
												}
;
param:
	| c_expr									{
													$1
												}
	| COMMA ATOM 								{
													[Node (String $2,[])]
												}
	| COMMA VAR 								{
													[V (Str $2)]
												}
	| param COMMA ATOM							{
													$1 @ [Node (String $3,[])]
												}
	| param COMMA VAR 							{
													$1 @ [V (Str $3)]
												}
;
c_expr:
	| COMMA expr								{
													[$2]
												}
	| c_expr COMMA expr 						{
													$1 @ [$3]
												}
;