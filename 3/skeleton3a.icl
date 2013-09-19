module skeleton3a

// Joshua Moerman
// 3009408

/*
	Advanced Programming 2013.
	Skeleton for exercise 3.1 and 3.2.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

import StdEnv, StdMaybe

/************* showing *******************/

class show_0 a where show_0 :: a [String] -> [String]

instance show_0 Int  where show_0 i    c = [IntTag :toString i:c]
instance show_0 Bool where show_0 b    c = [BoolTag:toString b:c]
instance show_0 UNIT where show_0 unit c = [UNITTag:c]

IntTag	:== "Int"
BoolTag	:== "Bool"
UNITTag	:== "UNIT"
PAIRTag	:== "PAIR"

show :: a -> [String] | show_0 a
show a = show_0 a []

/**************** parsing *************************/

:: Result a :== Maybe (a,[String])

class parse0 a :: [String] -> Result a

instance parse0 Int
where
	parse0 [IntTag,i:r] = Just (toInt i, r)
	parse0 r = Nothing
instance parse0 Bool
where
	parse0 [BoolTag,b:r] = Just (b=="True", r)
	parse0 r = Nothing
instance parse0 UNIT
where
	parse0 [UNITTag:r] = Just (UNIT, r)
	parse0 r = Nothing

/**************** Example Types and conversions *************************/

:: T		= C
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b
:: CONS   a		= CONS String a

//	Generic type representations
:: TG		:== CONS UNIT
:: ColorG	:== EITHER (EITHER (CONS UNIT) (CONS UNIT)) (CONS UNIT)
:: ListG a	:== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b	:== CONS (PAIR a b)

// Conversions

fromT :: T									-> TG
fromT c										= CONS "C" UNIT

fromColor :: Color 							-> ColorG
fromColor Red								= LEFT (LEFT  (CONS "Red"    UNIT))
fromColor Yellow							= LEFT (RIGHT (CONS "Yellow" UNIT))
fromColor Blue								=       RIGHT (CONS "Blue"   UNIT)

fromList :: [a]								-> ListG a
fromList []									= LEFT  (CONS "Nil"  UNIT)
fromList [a:as]								= RIGHT (CONS "Cons" (PAIR a as))

fromTree :: (Tree a)						-> TreeG a
fromTree Tip								= LEFT  (CONS "Tip" UNIT)
fromTree (Bin a l r)						= RIGHT (CONS "Bin" (PAIR a (PAIR l r)))

fromTup :: (a,b)							-> TupG a b
fromTup (a,b)								= CONS "Tuple2" (PAIR a b)

toT :: TG									-> T
toT (CONS _ UNIT)							= C

toColor :: ColorG							-> Color
toColor (LEFT (LEFT  (CONS _ UNIT)))		= Red
toColor (LEFT (RIGHT (CONS _ UNIT)))		= Yellow
toColor       (RIGHT (CONS _ UNIT))			= Blue

toList :: (ListG a)							-> [a]
toList (LEFT  (CONS s UNIT))        		= []
toList (RIGHT (CONS s (PAIR a as)))		 	= [a:as]

toTree :: (TreeG a)							-> Tree a
toTree (LEFT  (CONS s UNIT))                = Tip
toTree (RIGHT (CONS s (PAIR a (PAIR l r)))) = Bin a l r

toTup :: (TupG a b)							-> (a,b)
toTup (CONS s (PAIR a b))					= (a,b)

/**************** to test if parse and show work properly *************************/

test :: t -> Bool | eq0, show_0, parse0 t
test x
	= case parse0 (show x) of
		Just (y,[])	= eq0 x y
		_			= False

/**************** equality with a class for each kind *************************/

class eq0 t ::                              t       t      -> Bool
class eq1 t :: (a a -> Bool)               (t a)   (t a)   -> Bool
class eq2 t :: (a a -> Bool) (b b -> Bool) (t a b) (t a b) -> Bool

instance eq0 UNIT			where eq0 _ _                       = True
instance eq0 Int			where eq0 n m                       = n == m

instance eq1 CONS			where eq1 f   (CONS s x) (CONS t y) = s == t && f x y

instance eq2 PAIR			where eq2 f g (PAIR a b) (PAIR x y) = f a x && g b y
instance eq2 EITHER			where eq2 f g (LEFT  x)  (LEFT  y)  = f x y
							      eq2 f g (RIGHT x)  (RIGHT y)  = g x y
							      eq2 f g _          _          = False

instance eq0 [a] | eq0 a	where eq0   l m = eq1 eq0 l m
instance eq1 []				where eq1 f l m = eq2 (eq1 eq0) (eq1 (eq2 f (eq1 f))) (fromList l) (fromList m)

/**************** map *************************/

class map0 t ::                    t      -> t
class map1 t :: (a -> b)          (t a)   -> t b
class map2 t :: (a -> b) (c -> d) (t a c) -> t b d 

instance map0 Int			where map0 i              = i
instance map0 UNIT			where map0 UNIT           = UNIT

instance map1 CONS			where map1 f   (CONS n x) = CONS n (f x)

instance map2 PAIR			where map2 f g (PAIR x y) = PAIR  (f x) (g y)
instance map2 EITHER		where map2 f g (LEFT  x)  = LEFT  (f x)
							      map2 f g (RIGHT y)  = RIGHT (g y)

/**************** End Prelude *************************/

/**************** please add all new code below this line *************************/

instance eq0 Color		where eq0  c1 c2 = False		// TO BE IMPROVED
instance ==  Color		where (==) c1 c2 = eq0 c1 c2	// just to use the well-known notation...
instance show_0 Color	where show_0 _ 	   c = c		// TO BE IMPROVED
instance parse0 Color	where parse0 _       = Nothing	// TO BE IMPROVED

instance map1 []	where map1 f l = map f l			// TO BE IMPROVED, use generic version

// some initial tests, please extend
Start
 =	[ and [ test i \\ i <- [-25 .. 25]]
	, and [ c == toColor (fromColor c) \\ c <- [Red, Yellow, Blue]]
	, and [ test c \\ c <- [Red,Yellow,Blue]]
//	, test [1 .. 3]
//	, test [(a,b) \\ a <- [1 .. 2], b <- [5 .. 7]]
//	etc.
	// maps
	, map1 ((+) 1) [0 .. 5] == [1 .. 6]
	]

aTree = Bin 2 Tip (Bin 4 Tip Tip)
