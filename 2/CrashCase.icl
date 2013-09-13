module CrashCase

// Joshua Moerman
// 3009408

import StdEnv

/**************** Part 1 *******************************/

:: Tree a = Tip | Bin (Tree a) a (Tree a)

class Container t
where
	Cinsert   :: a (t a) -> t a      | <        a
	Ccontains :: a (t a) -> Bool     | <, Eq    a
	Cshow     ::   (t a) -> [String] | toString a
	Cnew	  :: t a

instance Container [] where
	Cinsert a l = [a:l]
	Ccontains a l = isMember a l
	Cshow l = map toString l
	Cnew = []

instance Container Tree where
	Cinsert a Tip = Bin Tip a Tip
	Cinsert a (Bin l x r)
	| a < x = Bin (Cinsert a l) x r
	| a > x = Bin l x (Cinsert a r)
	| otherwise = Bin l x r
	Ccontains a Tip = False
	Ccontains a (Bin l x r)
	| a < x = Ccontains a l
	| a > x = Ccontains a r
	| otherwise = True
	Cshow Tip = []
	Cshow (Bin l x r) = Cshow l ++ [toString x] ++ Cshow r
	Cnew = Tip

// Possible test:
($) infix 0 :: (a -> b) a -> b
($) f x = f x

Start = (Ccontains 3 c, Cshow c) where
	c :: Tree Int
	c = Cinsert 8 o Cinsert 2 o Cinsert 9 o Cinsert 37 o Cinsert 1 o Cinsert -5 o Cinsert 3 o Cinsert 3 $ Cnew
