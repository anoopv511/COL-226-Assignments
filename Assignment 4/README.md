Defined types - 
	1. symbol of char or string
	2. variable of int or bool or string
	3. signature of (symbol,int) list
	4. term of variable or (symbol,term list)
	5. sigma of (variable,term)
	6. substitution of sigma list [Substitution is being represented as a list of maps from variables to terms]

Methods in main.ml source file -
	1. check_sig returns true if given signature is valid
	2. wfterm returns true if given term is well-formed with respect to given valid signature
	3. ht, size and vars return height, size and list of variables of given term
	4. subst returns term after substituting given term with given substitution
	5. mgu returns substitution obtained from given two terms

Verification - Some input signatures and terms are provided to verify the implementation (Use utop for easy verification)