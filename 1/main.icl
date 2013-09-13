module main

// Joshua Moerman
// 3009408

/*
	Course I00032 Advanced Programming 2012
	Skeleton for assignment 1
	Pieter Koopman
	Peter  Achten  (P.Achten@cs.ru.nl)
*/

import StdEnv

/**************** Prelude: *******************************/
//	Example types
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)
:: Rose a	= Rose a [Rose a]

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b

//	Generic type representations
:: RoseG a	:== PAIR a [Rose a]

// Conversions
fromRose :: (Rose a)	-> RoseG a
fromRose (Rose a l)		= PAIR a l

// Oerdering

::	Ordering = Smaller | Equal | Bigger

class (><) infix 4 a :: !a !a -> Ordering

instance >< Int where		// Standard ordering for Int
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Char where		// Standard ordering for Char
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< String where	// Standard lexicographical ordering
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Bool where		// False is smaller than True
	(><) False True  = Smaller
	(><) True  False = Bigger
	(><) _     _     = Equal

/**************** End Prelude *************************/


/********* Part One: Ordering by Overloading **********/
// with invert, we only have to define one half + the diagonal :-)
invert :: Ordering -> Ordering
invert Smaller = Bigger
invert Equal = Equal
invert Bigger = Smaller

instance >< Color where
	(><) Red Red = Equal
	(><) Red Yellow = Smaller
	(><) Red Blue = Smaller
	(><) Yellow Yellow = Equal
	(><) Yellow Blue = Smaller
	(><) Blue Blue = Equal
	(><) x y = invert (y >< x)

instance >< (Tree a) | >< a where
	(><) Tip Tip = Equal
	(><) Tip _ = Smaller
	(><) (Bin a1 l1 r1) (Bin a2 l2 r2) = case a1 >< a2 of
		Equal = case l1 >< l2 of
			Equal = r1 >< r2
			x = x
		x = x
	(><) x y = invert (y >< x)

instance >< (Rose a) | >< a where
	(><) (Rose a1 rs1) (Rose a2 rs2) = case a1 >< a2 of
		Equal = rs1 >< rs2
		x = x

instance >< (a, b) | >< a & >< b where
	(><) (a1, b1) (a2, b2) = case a1 >< a2 of
		Equal = b1 >< b2
		x = x

instance >< [a] | >< a where
	(><) [] [] = Equal
	(><) [] _ = Smaller
	(><) [x1:xs1] [x2:xs2] = case x1 >< x2 of
		Equal = xs1 >< xs2
		x = x
	(><) x y = invert (y >< x)

// some handy things for testing
($) infix 0 :: (a -> b) a -> b
($) f x = f x
perms [] = [[]]
perms [x:xs] = flatten (map (\p -> [insertAt n x p \\ n <- [0..length p]]) (perms xs))
someLists upperbound = flatten o map perms $ [[1..n] \\ n <- [1..upperbound]]
instance == Ordering where
	(==) Smaller Smaller = True
	(==) Bigger Bigger = True
	(==) Equal Equal = True
	(==) _ _ = False
colors = [Red, Yellow, Blue]
someTrees = [Tip, Bin 37 Tip Tip, Bin 21 (Bin 10 Tip Tip) Tip, Bin 42 Tip (Bin 50 Tip Tip)]

// some testcases
testLists = [[1..3] >< [1..2], [1..2] >< [1..5], [1..2] >< [2..3], [1,2] >< [2,1]]
testLists2 = [(l, r, l >< r) \\ l <- someLists 3, r <- someLists 3]
testTuples = [((a, b), (c, d), (a,b) >< (c,d)) \\ a <- [1..3], b <- [1..3], c <- [1..3], d <- [1..3]]
testColors = [(a, b, a >< b) \\ a <- colors, b <- colors]


/********* Part Two: Generic Representation **********/

// 1. Give generic representations for the types Color and [a] (the standard lists of Clean). Name these generic types ColorG and ListG a respectively.
:: ColorG :== EITHER UNIT (EITHER UNIT UNIT)
:: ListG a :== EITHER UNIT (PAIR a [a])
:: TreeG a :== EITHER UNIT (PAIR a (PAIR (Tree a) (Tree a)))

// 2. Define a function listToGen : : [a] ! ListG a that transforms lists to their generic representation.
listToGen :: [a] -> ListG a
listToGen [] = LEFT UNIT
listToGen [x:xs] = RIGHT (PAIR x xs)

fromColor Red = LEFT UNIT
fromColor Yellow = RIGHT (LEFT UNIT)
fromColor Blue = RIGHT (RIGHT UNIT)

fromTree Tip = LEFT UNIT
fromTree (Bin a l r) = RIGHT (PAIR a (PAIR l r))

// 3. What is the generic representation of [1,2,3]?
//		A: RIGHT (PAIR 1 [2,3]), and this is also obtained from the function
// 4. Is it possible to define a general class toGen that transforms ordinary Clean values to their generic representation?
//		A: There is a obvious problem regarding integers and characters, because their generic representation would consist of _many_ EITHERs, as they have many 'constructors' (which are builtins). There is a second difficulty, namely that every type has a different generic representation, so it is not possible to have a class with toGen :: a -> GenericRepr. But it is imaginable to have a class toGen :: a -> b and let b be a builtin or UNIT/PAIR/EITHER, but this is not so useful as Clean doesn't have functional dependencies (i.e. clean doesn't have a way to say that b is uniquely determined by a).


/** Part Three: Ordering via Generic Representation **/

// 1. Define instance of >< for the types UNIT, PAIR, and EITHER.
instance >< UNIT where
	(><) UNIT UNIT = Equal

// PAIR a b is obviously isomorphic to (a, b)
instance >< (PAIR a b) | >< a & >< b where
	(><) (PAIR x1 y1) (PAIR x2 y2) = (x1, y1) >< (x2, y2)

instance >< (EITHER a b) | >< a & >< b where
	(><) (LEFT x1) (LEFT x2) = x1 >< x2
	(><) (LEFT _) (RIGHT _) = Smaller
	(><) (RIGHT y1) (RIGHT y2) = y1 >< y2
	(><) x y = invert (y >< x)

// I commented these out, so I could compare them with the previous definition.
// instance >< Color where
// 	(><) x y = fromColor x >< fromColor y

// instance >< (Tree a) where
// 	(><) x y = fromTree x >< fromTree y

// 2. Are these results equal to the results obtained above?
//		A: Yes, for tuples by definition, other types are demonstrated by the following.

testResults l1 l2 = [x1 >< x2 \\ x1 <- l1, x2 <- l1] == [x1 >< x2 \\ x1 <- l2, x2 <- l2]

someGenLists n = map listToGen $ someLists n
testGenLists = testResults (someGenLists 4) (someLists 4)

genColors = map fromColor colors
testGenColors = testResults genColors colors

someGenTrees = map fromTree someTrees
testGenTrees = testResults someGenTrees someTrees

Start = testGenLists && testGenColors && testGenTrees

// 3. What is the advantage of this generic approach (if any)?
//		A: For many datatypes we only have to specify a function toGen and get (><) for free. For the relation >< we need O(n^2) lines of code with n constructors, this method reduces this to O(n) for the conversion to the generic representation.

// 4. What is the disadvantage of the generic approach (if any)?
//		A: When doing this manually it is error-prone, especially since a datatype with many constructors results in a very nested EITHER.
