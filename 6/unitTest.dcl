definition module unitTest

/*
	A simple unit test framework for AFP
	using gast for generic show
	Pieter Koopman pieter@cs.ru.nl
*/

import StdEnv, gast

:: TestFun

doTest		:: TestFun -> [String]

testEqual	:: String x  x              -> TestFun | genShow{|*|}, gEq  {|*|} x
testUnEqual	:: String x  x              -> TestFun | genShow{|*|}, gEq  {|*|} x
testLess	:: String x  x              -> TestFun | genShow{|*|}, gLess{|*|} x
testLessEQ	:: String x  x              -> TestFun | genShow{|*|}, gLess{|*|} x
testOp		:: String x (x x -> Bool) x -> TestFun | genShow{|*|}             x

testPred1	:: String (x -> Bool) x     -> TestFun | genShow{|*|} x
testPredL	:: String (x -> Bool) [x]   -> TestFun | genShow{|*|} x

class testPred a :: [String] a -> TestFun
instance testPred Bool
instance testPred (a->b) | genShow{|*|}, ggen{|*|} a & testPred b

instance * TestFun
