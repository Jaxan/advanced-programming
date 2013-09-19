module skeleton3b

// Joshua Moerman
// 3009408

/*
	Advanced Programming 2013.
	Skeleton for exercise 3.3 and 3.4.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

import StdEnv, StdGeneric, StdMaybe, GenEq

//------------------ show --------------
generic show_ a :: a [String] -> [String]

show_{|Int|}  i c = [toString i:c]
show_{|Bool|} b c = [toString b:c]

show a = show_{|*|} a []

//------------------ parse --------------

:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

parse{|Bool|} ["True" :r] = Just (True ,r)
parse{|Bool|} ["False":r] = Just (False,r)
parse{|Bool|} _ = Nothing

//------------------ some data types --------------

:: T		= C
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)

//------------------ general useful --------------

instance + String where (+) s t = s+++t
derive bimap Maybe, []

//------------------ to test if parse and show work properly --------------

test :: t -> Bool | gEq{|*|}, show_{|*|}, parse{|*|} t
test x
	= case parse{|*|} (show x) of
		Just (y,[])	= x === y
		_			= False

/**************** End Prelude, add all new code below this line *************************/

($) infixr 0
($) f x = f x

show_{|UNIT|} _ c = c
show_{|PAIR|} showx showy (PAIR x y) c = showx x $ showy y c
show_{|EITHER|} showx _ (LEFT x) c = showx x c
show_{|EITHER|} _ showy (RIGHT y) c = showy y c
show_{|CONS of {gcd_name, gcd_arity}|} showx (CONS x) c
| gcd_arity == 0	= [gcd_name:showx x c]
| otherwise			= ["(":gcd_name:showx x [")":c]]
show_{|OBJECT|} showx (OBJECT x) c = showx x c

derive show_ (,), [], T, Color, Tree

fmap f Nothing = Nothing
fmap f (Just (x, y)) = Just (f x, y)
// Quick and dirty monad for parsing, makes CONS easier
(>>=) infixr 0
(>>=) p f = \xs -> case p xs of
	Just (x, xs) = f x xs
	Nothing = Nothing
(>>>) infixr 0
(>>>) x f = x >>= \_ -> f
unit a = \xs -> Just (a, xs)

strip _ [] = Nothing
strip x [y:xs]
| x == y = Just (x, xs)
| otherwise = Nothing

parse{|Int|} [] = Nothing
parse{|Int|} [x:xs] = Just (toInt x, xs)
parse{|UNIT|} r = Just (UNIT, r)
parse{|PAIR|} parsex parsey r = (parsex >>= \x -> parsey >>= \y -> unit (PAIR x y)) r
parse{|EITHER|} parsex parsey r = case parsex r of
	Nothing = fmap RIGHT $ parsey r
	x = fmap LEFT x
parse{|CONS of {gcd_name, gcd_arity}|} parsex r
| gcd_arity == 0 = (strip gcd_name >>> parsex >>= \z -> unit (CONS z)) r
| gcd_arity  > 0 = (strip "(" >>> strip gcd_name >>> parsex >>= \z -> strip ")" >>> unit (CONS z)) r
parse{|OBJECT|} parsex r = fmap OBJECT $ parsex r

derive parse (,), [], T, Color, Tree

//------------------ tests --------------

perms [] = [[]]
perms [x:xs] = flatten (map (\p -> [insertAt n x p \\ n <- [0..length p]]) (perms xs))
someLists upperbound = flatten o map perms $ [[1..n] \\ n <- [1..upperbound]]

Start = and [test b \\ b <- someLists 5]
// Start = parse{|*|} (show (Bin 5 (Bin 42 Tip Tip) (Bin 37 Tip Tip)))
