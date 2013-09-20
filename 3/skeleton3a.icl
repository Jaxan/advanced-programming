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

instance show_0 Int  where show_0 i    c = [toString i:c]
instance show_0 Bool where show_0 b    c = [toString b:c]
instance show_0 UNIT where show_0 unit c = c

show :: a -> [String] | show_0 a
show a = show_0 a []

/**************** parsing *************************/

:: Result a :== Maybe (a,[String])

class parse0 a :: [String] -> Result a

instance parse0 Int
where
	parse0 [i:r] = Just (toInt i, r)
	parse0 r = Nothing
instance parse0 Bool
where
	parse0 [b:r] = Just (b=="True", r)
	parse0 r = Nothing
instance parse0 UNIT
where
	parse0 r = Just (UNIT, r)

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
instance eq0 Bool			where eq0 n m                       = n == m
instance eq0 Int			where eq0 n m                       = n == m

instance eq1 CONS			where eq1 f   (CONS s x) (CONS t y) = s == t && f x y

instance eq2 PAIR			where eq2 f g (PAIR a b) (PAIR x y) = f a x && g b y
instance eq2 EITHER			where eq2 f g (LEFT  x)  (LEFT  y)  = f x y
							      eq2 f g (RIGHT x)  (RIGHT y)  = g x y
							      eq2 f g _          _          = False

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

// generic show, show_0 is at the top
class show_1 t :: (a [String] -> [String]) (t a) [String] -> [String]
class show_2 t :: (a [String] -> [String]) (b [String] -> [String]) (t a b) [String] -> [String]

instance show_2 PAIR	where show_2 showx showy (PAIR x y) c	= showx x $ showy y c
instance show_2 EITHER	where show_2 showx _ (LEFT x) c			= ["L":showx x c]
                              show_2 _ showy (RIGHT y) c		= ["R":showy y c]
instance show_1 CONS	where show_1 showx (CONS str x) c		= [str:showx x c]

// generic parse, parse0 is at the top
class parse1 t :: ([String] -> Result a) [String] -> Result (t a)
class parse2 t :: ([String] -> Result a) ([String] -> Result b) [String] -> Result (t a b)

// I decided to keep the tags for EITHER, because we don't know what to match in CONS, so CONS will almost never fail. As a result LEFT will almost always win. We could also pass more information around during parsing, but than we would need to change the signature of parse. Besides I believe that this is more time-efficient. And a full tagless implementation can be found in skeleton3b.icl, as we do have the name of the constructor to match with.
instance parse2 PAIR	where parse2 parsex parsey r	= (parsex >>= \x -> parsey >>= \y -> unit (PAIR x y)) r
instance parse2 EITHER	where parse2 parsex parsey ["L":r]	= fmap LEFT $ parsex r
                              parse2 parsex parsey ["R":r]	= fmap RIGHT $ parsey r
                              parse2 parsex parsey _	= Nothing
instance parse1 CONS	where parse1 parsex r			= (read >>= \str -> parsex >>= \z -> unit (CONS str z)) r

// If we have eq1, show_1, ... we also get eq0, show_0,...
instance eq0 (t a)		| eq0 a & eq1 t			where eq0 x y		= eq1 eq0 x y
instance show_0 (t a)	| show_0 a & show_1 t	where show_0 x c	= show_1 show_0 x c
instance parse0 (t a)	| parse0 a & parse1 t	where parse0 r		= parse1 parse0 r
instance eq1 (t a)		| eq0 a & eq2 t			where eq1 eqy x y		= eq2 eq0 eqy x y
instance show_1 (t a)	| show_0 a & show_2 t	where show_1 show_y x c	= show_2 show_0 show_y x c
instance parse1 (t a)	| parse0 a & parse2 t	where parse1 parsey r	= parse2 parse0 parsey r

// specific instances
instance eq0 Color		where eq0  c1 c2 = eq2 (eq2 (eq1 eq0) (eq1 eq0)) (eq1 eq0) (fromColor c1) (fromColor c2)
instance show_0 Color	where show_0 x c = show_2 (show_2 (show_1 show_0) (show_1 show_0)) (show_1 show_0) (fromColor x) c
instance parse0 Color	where parse0 l   = fmap toColor $ parse2 (parse2 (parse1 parse0) (parse1 parse0)) (parse1 parse0) l

// :: TupG a b	:== CONS (PAIR a b)
instance show_1 []		where show_1 show_x l c	= show_2 (show_1 show_0) (show_1 (show_2 show_x (show_1 show_x))) (fromList l) c
instance parse1 []		where parse1 parsex r	= fmap toList $ parse2 (parse1 parse0) (parse1 (parse2 parsex (parse1 parsex))) r
instance map1 []		where map1 f l			= toList $ map2 (map1 map0) (map1 (map2 f (map1 f))) $ fromList l

instance eq1 Tree		where eq1 eqx t1 t2		= eq2 (eq1 eq0) (eq1 (eq2 eqx (eq2 (eq1 eqx) (eq1 eqx)))) (fromTree t1) (fromTree t2)
instance show_1 Tree	where show_1 show_x l c	= show_2 (show_1 show_0) (show_1 (show_2 show_x (show_2 (show_1 show_x) (show_1 show_x)))) (fromTree l) c
instance parse1 Tree	where parse1 parsex r	= fmap toTree $ parse2 (parse1 parse0) (parse1 (parse2 parsex (parse2 (parse1 parsex) (parse1 parsex)))) r
instance map1 Tree		where map1 f l			= toTree $ map2 (map1 map0) (map1 (map2 f (map2 (map1 f) (map1 f)))) $ fromTree l

instance eq2 (,)		where eq2 eqx eqy x y			= eq1 (eq2 eqx eqy) (fromTup x) (fromTup y)
instance show_2 (,)		where show_2 show_x show_y x c	= show_1 (show_2 show_x show_y) (fromTup x) c
instance parse2 (,)		where parse2 parsex parsey l	= fmap toTup $ parse1 (parse2 parsex parsey) l
instance map2 (,)		where map2 f g x				= toTup $ map1 (map2 f g) $ fromTup x

Start
 =	// parsing & equality
 	[ and [ test i \\ i <- [-25 .. 25]]			// Integers
	, and [ test c \\ c <- [Red,Yellow,Blue]]	// Colors
	, and [ test l \\ l <- someLists 4]			// Lists of Integers
	, and [ test t \\ t <- someTrees]			// Trees of Colors
	, and [ test (a,b) \\ a <- someTrees, b <- someLists 3]	// Tuples of Trees and Lists
	// maps
	, map1 ((+) 1) [0 .. 5] == [1 .. 6]			// basic test
	, eq0 (map1 (const True) [0 .. 20]) (take 21 (repeat True)) // map to different type
	, eq0 (map1 square (Bin 37 Tip $ Bin 4 Tip Tip)) $ Bin 1369 Tip $ Bin 16 Tip Tip	// map over Tree
	, map1 (\n.(n, fac n)) [1..5] == curry zip [1..5] [1, 2, 6, 24, 120]
	]

// Start = map2 (map1 fac) (map1 fac) ([1..10], Bin 2 Tip (Bin 4 Tip Tip))

($) infixr 0
($) f x = f x

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

read [] = Nothing
read [y:xs] = Just (y, xs)

// testing
perms [] = [[]]
perms [x:xs] = flatten (map (\p -> [insertAt n x p \\ n <- [0..length p]]) (perms xs))
someLists upperbound = flatten o map perms $ [[1..n] \\ n <- [1..upperbound]]
someTrees = [Tip, Bin Yellow (Bin Blue Tip Tip) (Bin Red Tip Tip), Bin Yellow (Bin Blue (Bin Yellow Tip Tip) Tip) (Bin Red (Bin Yellow Tip Tip) (Bin Red Tip Tip))]

square x = x * x
fac n = prod [1..n] // fac implemented with prod
