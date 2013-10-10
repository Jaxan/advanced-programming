implementation module unitTest

/*
	A simple unit test framework for AFP
	using gast for generic show
	Pieter Koopman pieter@cs.ru.nl
*/

import StdEnv, gast

:: UnitTest
 =	{ fail :: Int
	, succ :: Int
	}

:: TestFun :== UnitTest -> (UnitTest -> [String]) -> [String]

doTest :: TestFun -> [String]
doTest f = f { fail = 0, succ = 0 } rprt
where
	rprt :: UnitTest -> [String]
	rprt {fail,succ} = ["\nFinished testing: ",toString (fail+succ)," tests executed, ",toString fail," failures, and ",toString succ," successes.\n"]

testEqual :: String x x -> TestFun | genShow{|*|}, gEq{|*|} x
testEqual s x y
	= \a c. if (x === y)
				(c { a & succ = a.succ+1 })
				(["Error in testEqual ",s,": expected ",show1 x," obtained ",show1 y,".\n":c { a & fail = a.fail+1}])

testUnEqual :: String x x -> TestFun | genShow{|*|}, gEq{|*|} x
testUnEqual s x y
 = \a c. if (x =!= y)
			(c { a & succ = a.succ+1 })
			["Error in testUnEqual ",s,": expected ",show1 x," obtained ",show1 y,".\n": c { a & fail = a.fail+1}]

testLess :: String x x -> TestFun | genShow{|*|}, gLess{|*|} x
testLess s x y
 = \a c. if (x -<- y)
			(c { a & succ = a.succ+1 })
			["Error in testLess ",s,": not (",show1 x," less ",show1 y,").\n": c { a & fail = a.fail+1}]

testLessEQ :: String x x -> TestFun | genShow{|*|}, gLess{|*|} x
testLessEQ s x y
 = \a c. if (x -<= y)
			(c { a & succ = a.succ+1 })
			["Error in testLessEQ ",s,": not (",show1 x," lessEq ",show1 y,").\n": c { a & fail = a.fail+1}]

testOp :: String x (x x->Bool) x -> TestFun | genShow{|*|} x
testOp s x op y
 = \a c. if (op x y)
			(c { a & succ = a.succ+1 })
//			["Error in testOp ",s,": not (",show1 x," ",thunk_name_to_string op,"(",thunk_to_module_name_string op,") ",show1 y,").\n": c { a & fail = a.fail+1}]
			["Error in testOp ",s,": Not (",show1 x," ",thunk_name_to_string op," ",show1 y,").\n": c { a & fail = a.fail+1}]

testPred1 :: String (x -> Bool) x -> TestFun | genShow{|*|} x
testPred1 s f x = \a c. if (f x)
	(c { a & succ = a.succ+1 })
	["Error in testPred1: Not (",s," ",show1 x,").\n": c { a & fail = a.fail+1}]

testPredL	:: String (x -> Bool) [x] -> TestFun | genShow{|*|} x
testPredL s f xs = foldr (*) emptyTest testFuns where
	testFuns = map (testPred1 s f) xs
	emptyTest = \y g = g y

instance testPred Bool where
	testPred xs True  = \a c . c { a & succ = a.succ + 1 }
	testPred xs False = \a c . ["Error in (":reverse xs] ++ [")\n":c { a & fail = a.fail + 1 }]

instance testPred (a->b) | genShow{|*|}, ggen{|*|} a & testPred b where
	testPred xs f = foldr (*) emptyTest $ take bound [testPred [show1 x," ":xs] (f x) \\ x <- ggen{|*|} 2 aStream] where
		emptyTest = flip ($)
		bound = 10

// helper functions:
($) infixr 0
($) f x = f x

instance * TestFun
where (*) t1 t2 = \state cont.t1 state (\state2.t2 state2 cont)

/*
	:( :( dirty low level hacking to obtain names of functions :( :(
	Do not touch unless you really know what you are doing!
*/

thunk_name_to_string :: !(a a->Bool) -> {#Char}
thunk_name_to_string a = code {
    pushD_a 0
    pop_a 1
    .d 0 1 i
        jsr DtoAC
    .o 1 0
}

thunk_to_module_name_pointer :: !(a a->Bool) -> Int;
thunk_to_module_name_pointer a = code {
    pushD_a 0
    pop_a 1
    push_b 0
    load_si16 0
    addI
    load_i 6
}

thunk_to_module_name_string :: !(a a->Bool) -> {#Char};
thunk_to_module_name_string a
    = get_module_name (thunk_to_module_name_pointer a);

get_module_name :: !Int -> {#Char};
get_module_name m
    = {get_module_name_char m i\\i<-[0..get_module_name_size m-1]};

get_module_name_size :: !Int -> Int;
get_module_name_size a = code {
    load_i 0
}

get_module_name_char :: !Int !Int -> Char;
get_module_name_char a i = code {
    addI
    load_ui8 4
}
