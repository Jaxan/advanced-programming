module shallowWhile

import StdEnv

// Joshua Moerman
// 3009408

:: AExp      :== State -> (Printable, Num)
:: BExpr     :== State -> (Printable, Bool)
:: State     :== Var -> Num
:: Printable :== [String]
:: Num       :== Int
:: Var       :== String

// Arithmetic expressions
num :: Num -> AExp
num n = const ([toString n], n)

var :: Var -> AExp
var str = \s . ([str], s str)

plus :: AExp AExp -> AExp
plus x y = binop x y "+" (+)

mult :: AExp AExp -> AExp
mult x y = binop x y "*" (*)

sub :: AExp AExp -> AExp
sub x y = binop x y "-" (-)

// Boolean expressions
true  = const (["true"], True)
false = const (["false"], False)

equal :: AExp AExp -> BExpr
equal x y = binop x y "=" (==)

less :: AExp AExp -> BExpr
less x y = binop x y "<" (<)

and :: BExpr BExpr -> BExpr
and x y = binop x y "&&" (&&)

not1 :: BExpr -> BExpr
not1 x = \s . app2 (cons "!", not) (x s)

// Program interpreter
evaluator :: AExp -> (Printable, Num)
evaluator expr
# st = \s . abort ("no binding for " +++ s)
= expr st

Start
# (str, n) = evaluator $ plus (num 1337) (num 42)
= (str, "\n", n, "\n")

// *** Helper functions ***
// as usual: function application
($) infixr 0
($) f x = f x

// hybrid of app2 and zip (i.e. combines two tuples with two functions)
act f g (a1, b1) (a2, b2) = (f a1 a2, g b1 b2)

// put a delimiter in between
join delim x y = x ++ [delim:y]

// combines act and delim to give a general scheme for binary operators
binop x y delim func s = act (join delim) func (x s) (y s)

// (:) is not a function in clean?!?!?
cons x y = [x:y]
