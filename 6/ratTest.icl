module ratTest

// Joshua Moerman
// 3009408

import StdEnv, Rat, gast

/*
I fixed one error in Rat.icl, because it really was _too stupid_.
line 24: should be `| n == one = r` instead of `| n == one = one`
So now we only have to fix / and - and <
*/

// Abelian groups
// Basic algebraic properties of + and *
:: Op :== (String, Rat -> Rat -> Rat, Rat)

commutativity :: Op Rat Rat -> Property
commutativity (s,op,_) x y = name n $ op x y == op y x
	where n = "commutativity of " +++ s

associativity :: Op Rat Rat Rat -> Property
associativity (s,op,_) x y z = name n $ op (op x y) z == op x (op y z)
	where n = "associativity of " +++ s

// We only test right unitality, as the operation should be commutative anyway
unitality :: Op Rat -> Property
unitality (s,op,un) x = name n $ op x un == x
	where n = "unitality of " +++ s

basicOps :: [Op]
basicOps = [ ("*", (*), one), ("+", (+), zero) ]

// Ring
// Relation between the two opeartios, also only one kind, since we already tested commutativity
distributivity :: Rat Rat Rat -> Property
distributivity x y z = name "distributivity" $ x * (y + z) == x * y + x * z

// Field
// Relation with inverses (also one-sided, because of commutativity)
inverse :: Rat -> Property
inverse x = name "inverse of +" $ x - x == zero

inverseM :: Rat -> Property
inverseM x = name "inverse of *" $ x <> zero ==> (inv x) * x == one

// We can also test ^, but it's wrong anyways, since it only works for integer, positive powers (I looked at the source)

// Equality
// Testing comparison
reflexivity :: Rat -> Property
reflexivity x = name "reflexivity" $ x == x

symmetry :: Rat Rat -> Property
symmetry x y = name "symmetry" $ (x == y) == (y == x)

transitivity :: Rat Rat Rat -> Property
transitivity x y z = name "transitivity of ==" $ (x == y) /\ (y == z) ==> (x == z)

// Total ordering
// And ordering
totality :: Rat Rat -> Property
totality x y = name "totality" $ x < y \/ x == y \/ x > y

antisymmetry :: Rat Rat -> Property
antisymmetry a b = name "antisymmetry" $ a <= b /\ b <= a ==> a == b

transitivity2 :: Rat Rat Rat -> Property
transitivity2 x y z = name "transitivity of <" $ (x < y) /\ (y < z) ==> (x < z)

// Ordered field
// Relation between the operators and ordering
simpleTest :: Property
simpleTest = name "1 > 0" $ oner > zero
	where oner :: Rat; oner = one;

orderedField1 :: Rat Rat Rat -> Property
orderedField1 a b c = name "ordered field 1" $ a <= b ==> a + c <= b + c

orderedField2 :: Rat Rat -> Property
orderedField2 a b = name "ordered field 2" $ a >= zero /\ b >= zero ==> a * b >= zero

// Even more specific properties, distinquishing Q from other ordered fields
// Bounded to prevent heap overflow :(
characteristicZero :: Int -> Property
characteristicZero n = name "characteristic zero" $ n > 0 /\ n < 100 ==> sum (repeatn n oner) <> zero
	where oner :: Rat; oner = one

// Bounded to prevent heap overflow :(
noNRoots :: Int Rat -> Property
noNRoots n x = name "no n-roots" $ n >= 2 /\ n < 100 ==> prod (repeatn n x) <> (toRat 2)

// Also some conversions, Int \subset Q \subset R
viaRats :: Int -> Property
viaRats n = name "conversion Int -> Rat -> Int" $ n == toInt (toRat n)

// Of course, Reals are represented as floats, so this will probably fail
viaReals :: Rat -> Property
viaReals r = name "conversion Rat -> Reals -> Rat" $ r == toRat (toReal r)

// Doing all the tests
Start =
	( [ Test [] $ commutativity op \\ op <- basicOps ]
	, [ Test [] $ associativity op \\ op <- basicOps ]
	, [ Test [] $ unitality op \\ op <- basicOps ]
	, Test [] inverse
	, Test [] inverseM
	, Test [] distributivity
	, Test [] reflexivity
	, Test [] symmetry
	, Test [] transitivity
	, Test [] totality
	, Test [] transitivity2
	, Test [] simpleTest
	, Test [] orderedField1
	, Test [] orderedField2
	, Test [] characteristicZero
	, Test [] noNRoots
	, Test [] viaRats
	, Test [] viaReals
	)

// helper functions:
($) infixr 0
($) f x = f x

/* OUTPUT:

C:\users\joshua>./ratTest.exe
"commutativity of *"  Passed after 1000 tests
"commutativity of +"  Passed after 1000 tests
"associativity of *"  Passed after 1000 tests
"associativity of +"  Passed after 1000 tests
"unitality of *"  Passed after 1000 tests
"unitality of +"  Passed after 1000 tests
"inverse of +" Counterexample 1 found after 2 tests: 1
"inverse of +" 1 counterexamples found, after 2 tests
"inverse of *" Counterexample 1 found after 7 tests: -2
"inverse of *" 1 counterexamples found, after 7 tests one case rejected
"distributivity"  Passed after 1000 tests
"reflexivity"  Passed after 1000 tests
"symmetry"  Passed after 1000 tests
"transitivity of =="  Passed: maximum number of arguments (2000) generated after 5 tests 1995 cases rejected
"totality"  Passed after 1000 tests
"transitivity of <"  Passed: maximum number of arguments (2000) generated after 253 tests 1747 cases rejected
"1 > 0" Counterexample 1 found after 1 tests:
"1 > 0" 1 counterexamples found, after 1 tests
"ordered field 1"  Passed: maximum number of arguments (2000) generated after 954 tests 1046 cases rejected
"ordered field 2" Counterexample 1 found after 4 tests: -1 -1
"ordered field 2" 1 counterexamples found, after 4 tests 9 cases rejected
"characteristic zero"  Passed: maximum number of arguments (2000) generated after 1 tests 1999 cases rejected
"no n-roots"  No tests performed, maximum number of arguments (2000) generated 2000 cases rejected
"conversion Int -> Rat -> Int"  Passed after 1000 tests
"conversion Rat -> Reals -> Rat" Counterexample 1 found after 7 tests: 1/3
"conversion Rat -> Reals -> Rat" 1 counterexamples found, after 7 tests

*/