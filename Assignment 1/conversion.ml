let createBigint a = itob a;;

let bigadd a b = add a b;;
let bigsub a b = sub a b;;
let bigmult a b = multiply a b;;

let bigquot a b = quotient a b;;

let bigrem a b = remainder a b;;

let bigabs a = babs a;;

let bigneg a = negation a;;
let bigequal a b = beq a b;;

let bigless a b = blt a b;;

let biggreater a b = bgt a b;;

let biglesseq a b = blte a b;;

let biggreatereq a b = bgte a b;;
let bigstr a = printbigint a;;