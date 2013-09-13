module main

// Joshua Moerman
// 3009408

/*
	Skeleton for Exercise 2 of Advanced Programming.
	Works fine with the environment Everything, but you can also use 
	StdEnv and manually add StdMaybe from the directory {Application}\Libraries\StdLib.
	
	Pieter Koopman, 2013
*/

import StdEnv

/**************** Prelude *************************/

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b
:: CONS   a		= CONS String a

//	Generic type representations
:: ListG a	:== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b	:== CONS (PAIR a b)
:: TG		:== CONS UNIT

// Conversions
fromList :: [a]	-> ListG a
fromList []		= LEFT (CONS "Nil" UNIT)
fromList [a:as]	= RIGHT (CONS "Cons" (PAIR a as))

toList :: (ListG a) -> [a]
toList (LEFT (CONS "Nil" UNIT)) = []
toList (RIGHT (CONS "Cons" (PAIR a as))) = [a:as]

// As shown in the lecture

class eq  a :: a a -> Bool
class eq0 t :: t t -> Bool
class eq1 t :: (a a->Bool) (t a) (t a) -> Bool
class eq2 t :: (a a->Bool) (b b->Bool) (t a b) (t a b) -> Bool

instance eq  Int  where eq x y = x == y
instance eq  Char where eq x y = x == y
instance eq0 UNIT where eq0 _ _ = True
instance eq1 CONS where eq1 eqa (CONS _ x) (CONS _ y) = eqa x y
instance eq2 PAIR where eq2 eqa eqb (PAIR a b) (PAIR x y) = eqa a x && eqb b y
instance eq2 EITHER
where
	eq2 eqa eqb (LEFT x)  (LEFT y)  = eqa x y
	eq2 eqa eqb (RIGHT x) (RIGHT y) = eqb x y
	eq2 _   _   _         _         = False

/**************** End Prelude *************************/

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
	| otherwise = Bin l x r // Don't insert duplicates
	Ccontains a Tip = False
	Ccontains a (Bin l x r)
	| a < x = Ccontains a l
	| a > x = Ccontains a r
	| otherwise = True
	Cshow Tip = []
	Cshow (Bin l x r) = Cshow l ++ [toString x] ++ Cshow r
	Cnew = Tip

// A real life-saver, saves a lot of parens
($) infixr 0 :: (a -> b) a -> b
($) f x = f x

// Possible test:
// Start = (Ccontains 3 c, Cshow c) where
// 	c :: Tree Int // or [Int]
// 	c = Cinsert 8 o Cinsert 2 o Cinsert 9 o Cinsert 37 o Cinsert 1 o Cinsert -5 o Cinsert 3 o Cinsert 3 $ Cnew


/**************** Part 2 *******************************/
// :: IntList = Empty | ConsInt Int IntList					=> IntList :: *
// :: List a = Nil | Cons a (List a)						=> List :: * -> *
// :: Tree a b = Leaf a | Node (Tree a b) b (Tree a b)		=> Tree :: * -> * -> *
// :: T1 a b = C11 (a b) | C12 b							=> T1 :: (* -> *) -> * -> *
// :: T2 a b c = C2 (a (T1 b c))							=> T2 :: (* -> *) -> (* -> *) -> * -> *
// :: T3 a b c = C3 (a b c)									=> T3 :: (* -> * -> *) -> * -> * -> *
// :: T4 a b c = C4 (a (b c))								=> T4 :: (* -> *) -> (* -> *) -> * -> *


/**************** Part 3 *******************************/
//	Example types
show :: a -> [String] | show_ a
show a = show_ a []

class show_ a where show_ :: a [String] -> [String]

instance show_ Int  where show_ i c = ["Int"  : toString i : c]
instance show_ Bool where show_ b c = ["Bool" : toString b : c]

// I removed some redundant things, only for EITHER and CONS we need to do something non-trivial.
instance show_ UNIT where
	show_ _ c = c
instance show_ (PAIR a b) | show_ a & show_ b where
	show_ (PAIR a b) l = show_ a $ show_ b l
instance show_ (EITHER a b) | show_ a & show_ b where
	show_ (LEFT x) l = ["L":show_ x l]
	show_ (RIGHT x) l = ["R":show_ x l]
instance show_ (CONS a) | show_ a where
	show_ (CONS str x) l = [str:show_ x l]

fromT C = CONS "C" UNIT
fromTree Tip = LEFT $ CONS "Tip" UNIT
fromTree (Bin l x r) = RIGHT $ CONS "Bin" $ PAIR x $ PAIR l r
fromTup (x, y) = CONS "," $ PAIR x y

instance show_ T where
	show_ x c = show_ (fromT x) c
instance show_ [a] | show_ a where
	show_ x c = show_ (fromList x) c
instance show_ (a,b) | show_ a & show_ b where
	show_ x c = show_ (fromTup x) c
instance show_ (Tree a) | show_ a where
	show_ x c = show_ (fromTree x) c

/**************** Part 4 *******************************/
:: Result a = Fail | Match a [String]
class parse a :: [String] -> Result a

// Another life-saver, saves a lot of lines of code
fmap :: (a -> b) (Result a) -> Result b
fmap f Fail = Fail
fmap f (Match x l) = Match (f x) l

// Parsing is easy, only PAIR is a bit more work
instance parse Int where
	parse ["Int",i : r]  = Match (toInt i) r
	parse _              = Fail
instance parse Bool where
	parse ["Bool",b : r] = Match (b=="True") r
	parse _              = Fail
instance parse UNIT where
	parse r   = Match UNIT r
instance parse (PAIR a b) | parse a & parse b where
	parse l = case parse l of
		Fail = Fail
		Match x l2 = fmap (PAIR x) $ parse l2
instance parse (EITHER a b) | parse a & parse b where
	parse ["L" : r] = fmap LEFT $ parse r
	parse ["R" : r] = fmap RIGHT $ parse r
instance parse (CONS a) | parse a where
	parse [str : r] = fmap (CONS str) $ parse r

toT :: TG -> T
toT _ = C

toTup :: (TupG a b) -> (a, b)
toTup (CONS _ (PAIR x y)) = (x, y)

toTree :: (TreeG a) -> Tree a
toTree (LEFT _) = Tip
toTree (RIGHT (CONS _ (PAIR x (PAIR l r)))) = Bin l x r

instance parse T where
	parse list = fmap toT $ parse list
instance parse [a] | parse a where
	parse list = fmap toList $ parse list
instance parse (a,b) | parse a & parse b where
	parse list = fmap toTup $ parse list
instance parse (Tree a) | parse a where
	parse list = fmap toTree $ parse list

:: T = C

/**************** Starts *******************************/

// All seems to be OK
Start = (Start1, "\n", Start2, "\n", Start3, "\n", Start4)

// Possible tests:
Start1 :: ([String],Result T)
Start1 = (strings,parse strings) where strings = show C

Start2 :: ([String],Result (Int,Bool))
Start2 = (strings,parse strings) where strings = show (1,False)

Start3 :: ([String],Result [Int])
Start3 = (strings,parse strings) where strings = show l; l :: [Int]; l = [1..4]

instance eq [a] | eq a			where eq x y = eq2 (eq1 eq0) (eq1 (eq2 eq eq)) (fromList x) (fromList y)
instance eq (Tree a) | eq a 	where eq x y = eq2 (eq1 eq0) (eq1 (eq2 eq (eq2 eq eq))) (fromTree x) (fromTree y)
instance eq (Result a) | eq a	where
	eq Fail Fail = True
	eq (Match x l) (Match y k) = (eq x y) && (l == k)
	eq _ _ = False

Start4 :: ([String],Result (Tree Int), Bool)
Start4 = (strings, t2, eq (Match t []) t2)
where
	strings :: [String]
	strings = show t
	
	t :: Tree Int
	t = Bin (Bin Tip 2 (Bin Tip 3 Tip)) 4 (Bin (Bin Tip 5 Tip) 6 Tip)
	
	t2 :: Result (Tree Int)
	t2 = parse strings
