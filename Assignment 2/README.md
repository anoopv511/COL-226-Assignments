Use the "make" command to compile "Assignment.mll" and test it on the input file "test.txt".
Output is stored in "out.txt"

Assumptions -
	Integer starting with 0s is identified as an Invalid token
	Strings starting with CAPS or numerals are identified as Invalid tokens
	'+' and '-' are identified as operations only if they are followed by whitespace/symbol/alphabet
	All alphanumeric tokens are identified only if they are preceeded by whitespaces/symbols (like " if" is [IF] but "Tif" is not [True][IF])
	Unmatched tokens are identified with the token - [Unknown]