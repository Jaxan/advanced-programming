module shallowWhile

import StdEnv

/*
   Joshua Moerman
   3009408

   Compiled with:
   clm -b -ns shallowWhile -o shallowWhile
   
   I did big step semantics, as composition is the easiest there
   (it doesn't require a distinction between final/intermediate state)
   denotational semantics would be almost the same, except for while.


   OUTPUT:

   Program:
   n := 5;
   m := 1;
   i := 0;
   while(i<n) do {
   i := (i+1);
   m := (i*m)
   }
   
   Results:
   m = 6
*/

// faculty function, pretty neat with infix operators
fac3 =
	"n" := num 3:.  // this is the input
	"m" := num 1:.  // this will be the output
	"i" := num 0:.  // loop variable
	while (var "i" isLessThan var "n") (
		"i" := var "i" + num 1:.
		"m" := var "i" * var "m"
	)

fac3Output = ["m"]  // only interested in output

// Straight forward definitions
:: AExpr     :== State -> (Printable, Num)
:: BExpr     :== State -> (Printable, Bool)
:: Stmt      :== State -> (Printable, State)
:: State     :== Var -> Num
:: Printable :== [String]
:: Num       :== Int
:: Var       :== String

// Arithmetic expressions
num :: Num -> AExpr
num n = const ([toString n], n)

var :: Var -> AExpr
var str = \s . ([str], s str)

instance + AExpr where
	(+) x y = binop x y "+" (+)

instance * AExpr where
	(*) x y = binop x y "*" (*)

instance - AExpr where
	(-) x y = binop x y "-" (-)

// Boolean expressions
true  = const (["true"], True)
false = const (["false"], False)

// To bad == has Bool as return type
(equals) infix 3 :: AExpr AExpr -> BExpr
(equals) x y = binop x y "=" (==)

(isLessThan) infix 3 :: AExpr AExpr -> BExpr
(isLessThan) x y = binop x y "<" (<)

(and) infix 2 :: BExpr BExpr -> BExpr
(and) x y = binop x y "&&" (&&)

instance ~ BExpr where
	(~) x = \s . app2 (cons "~", not) (x s)

// Statements (types commented out to include state as argument)
// (:=) :: Var AExpr -> Stmt
(:=) infix 1
(:=) v e s
# (str, ev) = e s
= ([v:" := ":str], \x . if (x==v) ev (s x))

// skip :: Stmt
skip s = (["skip"], s)

// (:.) :: Stmt Stmt -> Stmt
(:.) infixl 0
(:.) s1 s2 state
# (str1, state) = s1 state
# (str2, state) = s2 state
= (str1 ++ [";\n":str2], state)

// ifte :: BExpr Stmt Stmt -> Stmt
ifte b s1 s2 state
# (strb, b)   = b state
# (str1, st1) = s1 state
# (str2, st2) = s2 state
# txt = ["if":strb] ++ [" then {\n":str1] ++ ["\n} else {\n":str2] ++["\n}"]
= if b (txt, st1) (txt, st2)

// while :: BExpr Stmt -> Stmt
while b s1 state
# (strb, b2)  = b state
# (str1, st1) = s1 state
# txt = ["while":strb] ++ [" do {\n":str1] ++ ["\n}"]
= if b2 (txt, snd $ (while b s1) st1) (txt, state)

// Start with an empty state (really empty!)
Start
# st = \s . abort ("no binding for " +++ s)
# (program, state) = fac3 st
# results = map (toString o state) fac3Output
# printing = map (\(s1, s2) . s1 +++ " = " +++ s2 +++ "\n") $ zip2 fac3Output results
= ("Program:\n", program, "\n\nResults:\n", printing)

// *** Helper functions ***
// as usual: function application
($) infixr 0
($) f x = f x

// hybrid of app2 and zip (i.e. combines two tuples with two functions)
act f g (a1, b1) (a2, b2) = (f a1 a2, g b1 b2)

// put a delimiter in between, also puts it in parens
djoin delim x y = ["(":x] ++ [delim:y] ++ [")"]

// combines act and delim to give a general scheme for binary operators
binop x y delim func s = act (djoin delim) func (x s) (y s)

// (:) is not a function in clean?!?!?
cons x y = [x:y]
