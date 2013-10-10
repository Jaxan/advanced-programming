module tests

// Joshua Moerman
// 3009408

import StdEnv, unitTest

// Tests
pNot :: Bool -> Bool
pNot b = not (not b) == b

pReverse :: [Bool] -> Bool
pReverse l = reverse (reverse l) == l

pPlus :: Int Int -> Bool
pPlus x y = y + x == x + y

pFaulty :: Int Int -> Bool
pFaulty 0 y = y == 0
pFaulty x y = y + x == x + y

// Do all tests
Start = doTest
	$ testPred ["pNot"] pNot           // generic tests for pNot
	* testPred ["pReverse"] pReverse   // generic tests for pReverse
	* testPred ["pPlus"] pPlus         // generic tests for pPlus
	* testPred ["pFaulty"] pFaulty         // generic tests for pPlus

// helper functions:
($) infixr 0
($) f x = f x

/* OUTPUT:

C:\users\joshua>./tests.exe
Error in (pFaulty 0 -1)
Error in (pFaulty 0 1)
Error in (pFaulty 0 -2147483648)
Error in (pFaulty 0 2147483647)
Error in (pFaulty 0 -144734101)
Error in (pFaulty 0 -779375700)
Error in (pFaulty 0 77682163)
Error in (pFaulty 0 -1938136926)
Error in (pFaulty 0 -1999299896)

Finished testing: 212 tests executed, 9 failures, and 203 successes.

*/