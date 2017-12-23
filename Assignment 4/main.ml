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

exception Symbol_Not_Defined;;
exception Invalid_Signature;;
exception NOT_UNIFIABLE;;

let rec match_sym (x:symbol) (l:(symbol*int) list) =
	match l with
		| [] -> true
		| s :: l -> fst(s) <> x && match_sym x l;; 

let rec check_sig (l:(symbol*int) list) =
	match l with
		| [] -> true
		| x :: l -> if snd(x) < 0 || not (match_sym (fst(x)) l) then false else check_sig l;;

let rec get_arity (x:symbol) (l:(symbol*int) list) =
	match l with
		| [] -> raise Symbol_Not_Defined
		| s :: l -> if x = fst(s) then snd(s) else get_arity x l;;

let rec wfterm (t:term) (l:(symbol*int) list) =
	if check_sig l then 
		match t with 
		| V v -> true
		| Node (n,t_list) -> if (get_arity n l) <> (List.length t_list) then false else wfterm_sub t_list l
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

(*-------Signatures--------*)

let sign1 = [(String "ADD",2);(String "SUB",2);(Character '$',1)];;

let sign2 = [(String "ADD",2);(String "SUB",2);(Character '$',-1)];;

let sign3 = [(String "ADD",2);(String "ADD",2);(Character '$',1)];;

(*----------Terms----------*)

let t1 = Node (String "ADD",[Node (String "SUB",[V (Str "x");V (Str "y")]);Node(Character '$',[Node (String "ADD",[V (Str "a");V (Str "B")])])]);;

let t2 = Node (String "ADD",[Node (String "SUB",[V (Integer 5);Node (String "ADD",[V (Integer 6);V (Integer 7)])]);Node (Character '$',[Node (String "ADD",[V (Boolean false);Node (String "SUB",[V (Boolean true);V (Boolean false)])])])]);;

let t3 = Node (String "ADD",[V (Integer 1);V (Str "x");V (Str "y")]);;

(*-------Substitution------*)

let sub1 = [S (Str "x",Node (String "ADD",[V (Integer 1);V (Integer 2)]));S (Integer 1,V (Integer 4))];;
