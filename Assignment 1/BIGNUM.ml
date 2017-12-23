type sign = Neg|NonNeg;;

type bigint = sign * int list;;

(*Required func*)
exception Divided_by_0;;
let rec append0 (x:int list) (l: int)=
	match l with
	| _ when l<=0 -> x
	| k -> append0 (0::x) (l-1);;
let appendb ((s,x):bigint) (n:int) = 
	match (s,x) with
	| (s,x) -> ((s,(append0 x n)):bigint);;

let rec appendback ((s,x):bigint) (n:int) = 
	match n with
	| _ when n<=0 -> ((s,x):bigint)
	| k -> appendback ((s,(x@[0]))) (k-1);;
 	
let define (a:bigint)=
	match a with
		| (Neg,x::xs) -> -x
		| (NonNeg,x::xs) -> x;;
let ms (s1:sign) (s2:sign) = 
	match (s1,s2) with
			| (NonNeg,Neg) -> Neg
			| (Neg,Neg) -> NonNeg			
			| (Neg,NonNeg) -> Neg
			| (NonNeg,NonNeg) -> NonNeg;;
let matchsign (s:sign) ((a,x):bigint) = ((s,x):bigint);;						


let adddigit (a: int)	(l2: bigint)=
	match l2 with
	| (_,[]) -> if a<0 then ((Neg,[-a]):bigint) else ((NonNeg,[a]):bigint)
	| (NonNeg,h::t) -> if a<0 then (Neg,(-a) :: h :: t) else (NonNeg,a+h/10 :: h mod 10 :: t)
	| (Neg,h::t) -> if a <= 0 then (Neg,((-a)+(1)) :: ((-h) + 10) :: t)  else (NonNeg,a-1 :: 10-h :: t);;

let rec multdigit1 (a:bigint) (b:int) =
	match a with
		| (s,[]) -> ((s,[]):bigint)
		| (s,x::xs) -> adddigit ((define a)*b) (multdigit1 (s,xs) b);;
let rec lenstr acc (x:int list) =
	match x with
	 | [] -> acc
	 | x::xs -> lenstr (1+acc) xs;; 

let length (x:int list) = lenstr 0 x ;;

let lenb ((s,x):bigint) = lenstr 0 x;;

let funce (x1: int list) (x2:int list) (a:int) (b: int) = if (a>b) then (x1,append0 x2 (a-b)) else (append0 x1 (b-a),x2);;

let funcbig (x1: bigint) (x2:bigint) (a:int) (b: int) = if (a>b) then (x1,appendb x2 (a-b)) else (appendb x1 (b-a),x2);;

let equalise  (x1: int list) (x2:int list) = funce x1 x2 (length x1) (length x2);;

let equalise_bigint (x1:bigint) (x2:bigint) = funcbig x1 x2 (lenb x1) (lenb x2);;

let remove_last lst = List.rev (List.tl (List.rev lst));;

let bremove_last ((s,x):bigint) = ((s,(remove_last x)):bigint);;
 	  	 

(*Arthmatic Operation *)




let rec compute (f:int -> int -> int) ((x1:bigint),(x2:bigint)) = 
	match (x1,x2) with
	| ((_,[]),(_,[])) -> ((NonNeg,[]):bigint) 
	| ((a,(x::xs)),(b,(y::ys))) -> adddigit (f (define x1) (define x2)) (compute f ((a,xs),(b,ys)));;
let rec check (x1:bigint) = 
	match x1 with
		| (s,[0]) -> ((s,[0]):bigint)
		| (s,0::xs) -> check ((s,xs):bigint)
		| (s,x::xs) -> if x>9 then ((s,(x/10)::x mod 10 :: xs):bigint) else x1;;	

let a x y = x+y;;
let s x y = x-y;;
let multdigit (a:bigint) (b:int) = check (multdigit1 a b);;

	 	
(*Comparision*)

let rec compare_bigint f ((l1:bigint),(l2:bigint)) = 
	match (l1,l2) with
		| ((_,[x]),(_,[y])) -> f (define l1) (define l2)
		| ((a,x::xs),(b,y::ys))-> if x!=y then f (define l1) (define l2) else compare_bigint f ((a,xs),(b,ys));;

 let gt x y = x>y;;
let lt x y = x<y;;
let gte x y = x>=y;;
let lte x y = x<=y;;
let eq x y = x=y;; 

let bgte (x1:bigint) (x2:bigint) = compare_bigint gte (equalise_bigint x1 x2) ;;

let blte (x1:bigint) (x2:bigint) = compare_bigint lte (equalise_bigint x1 x2) ;;

let bgt (x1:bigint) (x2:bigint) = compare_bigint gt (equalise_bigint x1 x2);;

let blt (x1:bigint) (x2:bigint) = compare_bigint lt (equalise_bigint x1 x2);;

let beq  (x1:bigint) (x2:bigint) = compare_bigint eq (equalise_bigint x1 x2);;

let babs (l:bigint) = 
	match l with
	| (_,xs) -> (NonNeg,xs);;


let negation (l:bigint) =
	match l with
		| (Neg,xs) -> (NonNeg,xs)
		| (NonNeg,xs) -> (Neg,xs);;


(*Conversions*)

let add1 (x1:bigint) (x2:bigint) = check (compute a (equalise_bigint (babs x1) (babs x2)));;  

let  sub1 (x1:bigint) (x2:bigint) = 
	match blte (babs x2) (babs x1) with
	| true -> 	check (compute s (equalise_bigint (babs x1) (babs x2))) 
	| false -> negation(check (compute s (equalise_bigint (babs x2) (babs x1)))) ;;


let add (x1:bigint) (x2:bigint) = 
	match (x1,x2) with
	| ((Neg,x),(NonNeg,y)) ->  sub1 x2 x1
	| ((NonNeg,x),(Neg,y))-> sub1 x1 x2
	| ((Neg,x),(Neg,y)) -> negation (add1 x1 x2)
	| ((NonNeg,x),(NonNeg,y)) -> add1 x1 x2;;


let sub (x1:bigint) (x2:bigint) = 
	match (x1,x2) with
	| ((Neg,x),(NonNeg,y)) ->  negation (add1 x1 x2)
	| ((NonNeg,x),(Neg,y))-> add1 x1 x2
	| ((Neg,x),(Neg,y)) -> sub1 x2 x1
	| ((NonNeg,x),(NonNeg,y)) -> sub1 x1 x2;;




let rec mult (x1:bigint) (x2:bigint) (l:int) = 
	match x2 with
	| (a,[x]) -> multdigit x1 (define x2)
	| (a,x::xs) -> add  (mult x1 (a,xs) (l-1)) (appendback (multdigit x1 (define x2)) l);;

let multiply ((s1,x1):bigint) ((s2,x2):bigint) = matchsign (ms s1 s2) (mult (babs (s1,x1)) (babs (s2,x2)) ((lenb (s2,x2))-1));;	
(*Unary negation and babs*)




let rec ltos (x: int list) = 
	match x with
	| [] -> ""
	| (h::t) -> string_of_int(h)^ltos t;;

let rec divh1 (x1:bigint) (x2:bigint) (acc:int) (q:bigint) = 
	match (bgte x1 x2) with
	| false -> (acc,q)
	| true -> divh1 (sub x1 x2) x2 (1+acc) (sub q x2);;	
let div1 (x1:bigint) (x2:bigint) = divh1 (babs x1) (babs x2) 0 (babs x1);;

let r1 (x1:bigint) (x2:bigint) = 
	match div1 x1 x2 with
	| (a,b) -> b;;
let q1 (x1:bigint) (x2:bigint) = 
	match div1 x1 x2 with
	| (a,b) -> a;;

	
let rec div (x1:bigint) (x2:bigint) (l:int) (acc:int list) = 
	match l with
	 	 | _ when l<=0 -> ((acc@[q1 x1 x2]),(r1 x1 x2))
	 	 | k -> div (r1 x1 x2) (bremove_last x2) (k-1) (acc @ [q1 x1 x2]);;

let divide (x1:bigint) (x2:bigint) = div (babs x1) (appendback x2 ((lenb x1)-(lenb x2))) ((lenb x1)-(lenb x2)) [];;

let rec itol (n:int) = if n<10 then [n] else (itol (n/10)) @ [n mod 10];; 


let rec itob (n:int) = if n<0 then ((Neg,itol (-n)):bigint) else ((NonNeg,itol (n)):bigint);; 




let remainder1 ((s1,x1):bigint) ((s2,x2):bigint) = 
	match divide (NonNeg,x1) (NonNeg,x2) with
	| (a,b) -> ((matchsign (ms s1 s2) b):bigint);;

let quotient1 ((s1,x1):bigint) ((s2,x2):bigint) = 
	match divide (NonNeg,x1) (NonNeg,x2) with
	| (a,b) -> (((ms s1 s2),a):bigint);;

let remainder (x:bigint) (y:bigint) = 
	match beq y (itob 0) with
	| true -> raise Divided_by_0
	| false -> remainder1 x y;;

let quotient (x:bigint) (y:bigint) = 
	match beq y (itob 0) with
	| true -> raise Divided_by_0
	| false -> check(quotient1 x y);;

let rec printbigint (x:bigint) = 
	match x with
	| (Neg,l) -> "-"^ltos l
	| (NonNeg,l) -> ltos l;;


let l1 = itob 74057461754031;;
let l2 = itob (-567482169547860);;
let l3 = itob 0;;
let l4 = itob 4436615695956294544;;

(*
multiplication - multiply
addition - add
subraction - sub
remainder - remainder
quotient - quotient
>= - bgte
<= - blte
> - bgt
< - blt
= - beq
int to bigint - itob
bigint to string - printbigint
negation - negation
abs - babs


*)	




