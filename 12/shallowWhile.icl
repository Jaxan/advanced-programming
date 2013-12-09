module shallowWhile
/*
   Joshua Moerman
   3009408
*/

import StdEnv

:: State = NoVal | Node Var Num State State
:: Var :== String
:: Num :== Int

read :: Var State -> Int
read v NoVal = 0
read v (Node w m l r)
	| v < w = read v l
	| v > w = read v r
			= m

bind :: Var Num State -> State
bind v n NoVal = Node v n NoVal NoVal
bind v n (Node w m l r)
	| v < w = Node w m (bind v n l) r
	| v > w = Node w m l (bind v n r)
			= Node w n l r

class AExp a where
	num :: Num -> a
	var :: Var -> a
	(+.) infixl 6 :: a a -> a
	(-.) infixl 6 :: a a -> a
	(*.) infixl 7 :: a a -> a

instance AExp [String] where
	num n = [toString n]
	var v = [v]
	(+.) x y = ["(": x] ++ [" + ": y] ++ [")"]
	(-.) x y = ["(": x] ++ [" - ": y] ++ [")"]
	(*.) x y = ["(": x] ++ [" * ": y] ++ [")"]

instance AExp (State->Num) where
	num n = \s. n
	var v = read v 
	(+.) x y = \s. x s + y s
	(-.) x y = \s. x s - y s
	(*.) x y = \s. x s * y s

class BExp a b where						// becomes rather tricky
	true	:: (a->b)						// type system requires an a here
	false	:: (a->b)						// and here
	~.		:: (a->b) -> a->b				// and here
	(&&.) infixr 3:: (a->b) (a->b) -> a->b	// and here
	(<.)  infix 4 :: a a -> a->b
	(==.) infix 4 :: a a -> a->b

instance BExp [String] [String] where
	true  = \c.["true"]
	false = \c.["false"]
	~. b  = \c.["~. ", "(": b c] ++ [")"]
	(&&.) x y = \c.["(": x c] ++ [" && ": y c] ++ [")"]
	(<.)  x y = \c.["(": x] ++ [" < ": y] ++ [")"]
	(==.) x y = \c.["(": x] ++ [" == ": y] ++ [")"]

instance BExp (State->Num) (State->Bool) where
	true  = \c s.True
	false = \c s.False
	~. b  = \c s.not (b c s)
	(&&.) x y = \c s.x c s && y c s
	(<.)  x y = \c s.lessInt (x s) (y s)
	(==.) x y = \c s.eqInt (x s) (y s)

int :: (State -> Int)
int = undef

bool :: (State -> Bool)
bool = undef

print :: [String]
print = undef

lessInt :: (Int Int -> Bool)
lessInt = (<)

eqInt :: (Int Int -> Bool)
eqInt = (==)


// *** Part 1 of the assignment ***
// *** Statements & semantics (printing & evaluating) ***
class Stmt a b c where
	(:=.) infix 1 :: Var a -> (a b -> c)
	(:.) infixl 0 :: (a b -> c) (a b -> c) -> (a b -> c)
	skip   :: (a b -> c)
	If     :: (a->b) (a b -> c) (a b -> c) -> (a b -> c)
	While  :: (a->b) (a b -> c) -> (a b -> c)

instance Stmt [String] [String] [String] where
	(:=.) v a  = \c1 c2. [v:" := ":a]
	(:.) s1 s2 = \c1 c2. s1 c1 c2 ++ [";\n":s2 c1 c2]
	skip       = \c1 c2. ["skip"]
	If b s1 s2 = \c1 c2. ["if ":b c2] ++ ["(\n":s1 c1 c2] ++ ["\n) else (\n":s2 c1 c2] ++ ["\n)"]
	While b st = \c1 c2. ["while ": b c2] ++ ["(\n":st c1 c2] ++ ["\n)"]

instance Stmt (State->Num) (State->Bool) (State->State) where
	(:=.) v a  = \c1 c2 s. bind v (a s) s
	(:.) s1 s2 = \c1 c2 s. s2 c1 c2 (s1 c1 c2 s)
	skip       = \c1 c2 s. s
	If b s1 s2 = \c1 c2 s. if (b c1 s) (s1 c1 c2 s) (s2 c1 c2 s)
	While b st = \c1 c2 s. if (b c1 s) (While b st c1 c2 (st c1 c2 s)) s


// *** Part 2 of the assignment ***
// *** Optimization ***
:: AExpA
	= Int Int
	| Var Var
	| Plus AExpA AExpA
	| Subs AExpA AExpA
	| Mult AExpA AExpA

deepOpt (Int n) = Int n
deepOpt (Var v) = Var v
deepOpt (Plus x y) = case (deepOpt x, deepOpt y) of
	(Int x, Int y) = Int (x + y)
	(x, y)         = Plus x y
deepOpt (Subs x y) = case (deepOpt x, deepOpt y) of
	(Int x, Int y) = Int (x - y)
	(x, y)         = Subs x y
deepOpt (Mult x y) = case (deepOpt x, deepOpt y) of
	(Int 0, _)     = Int 0
	(_, Int 0)     = Int 0
	(Int 1, x)     = x
	(y, Int 1)     = y
	(Int x, Int y) = Int (x * y)
	(x, y)         = Mult x y

// Shallow -> Deep
instance AExp AExpA where
	num n = Int n
	var v = Var v
	(+.) x y = Plus x y
	(-.) x y = Subs x y
	(*.) x y = Mult x y

// Deep -> Shallow
translate :: AExpA -> a | AExp a
translate (Int n) = num n
translate (Var v) = var v
translate (Plus x y) = translate x +. translate y
translate (Subs x y) = translate x -. translate y
translate (Mult x y) = translate x *. translate y

// Lift a transformation in the deep domain to the shallow domain
lift :: (AExpA -> AExpA) (A.a:a | AExp a) -> a | AExp a
lift f e = translate (f e)


// example statement
fac :: (a b -> c) | Stmt a b c & BExp a b & AExp a
fac =
	"x" :=. num 4 :.
	"y" :=. num 1 :.
	While (num 1 <. var "x")(
		"y" :=. var "y" *. var "x" :.
		"x" :=. var "x" -. num 1
	)

// optimizer example (equal to 0; change 14 -> 13 to get x)
expr = (num 2 +. num 3 *. num 4 -. num 14) *. var "x"

// prints & evaluates fac
// prints & optmizes expr
Start =
	( "Program  \n", facStmtPrint, "\n\n"
	, "Output   \n", facStmtEval,  "\n\n"
	, "Original \n", expPrint, "\n\n"
	, "Optimized\n", optPrint, "\n\n"
	) where
	facStmtEval :: State;     facStmtEval = fac int bool NoVal
	facStmtPrint :: [String]; facStmtPrint = fac print print
	expPrint :: [String];     expPrint = expr
	optPrint :: [String];     optPrint = lift deepOpt expr


// *** Output ***
// Program  
// x := 4;
// y := 1;
// while (1 < x)(
// y := (y * x);
// x := (x - 1)
// )

// Output   
// x1y24

// Original 
// (((2 + (3 * 4)) - 14) * x)

// Optimized
// 0
