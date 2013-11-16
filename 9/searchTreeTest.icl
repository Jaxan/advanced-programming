module searchTreeTest

// Joshua Moerman
// 3009408

import StdEnv, StdIO, searchTree, gast

// I just tested searchTree with some basic types. It is very easy to add tests
// for other types, you only have to extend the following two functions, and
// simply provide a value of your favorite type (the actual value is not used).
// We don't have to thest SearchTree for non total orders, as it is a pre-cond.

// *** Extend Here ***
logicTests = 
	[ logicTestsBase "Int" 1
	, logicTestsBase "String" "bla"
	, logicTestsBase "[Real]" [1.0] ]

modelTests =
	[ modelTestBase "Int" 1
	, modelTestBase "String" "bla"
	, modelTestBase "[Real]" [1.0] ]


// *** Logical Tests ***
memberAfterIns    t x = elem x (ins x t)
notMemberAfterDel t x = not (elem x (del x t))
uniqueness        t x = elem x t ==> t === (ins x t)
removeNothing     t x = not (elem x t) ==> t === (del x t)
emptiness         t x = e1 /\ e2 where
	e1 = empty t ==> not (elem x t)
	e2 = elem x t ==> not (empty t)

logicTestsBase n type = Test [] $ name ("SearchTree " +++ n)
	[ label "memberAfterIns"    $ memberAfterIns o fromList
	, label "notMemberAfterDel" $ notMemberAfterDel o fromList
	, label "uniqueness"        $ uniqueness o fromList
	, label "removeNothing"     $ removeNothing o fromList
	, label "emptiness"         $ emptiness o fromList ]
	where fromList = List2ST o asType [type]


// *** Stateful tests ***
:: Input a = Insert a | Delete a | IsMember a | IsEmpty
:: Output  = Member Bool | Emptiness Bool
:: State a = { elements :: [a] }

derive bimap   []
derive gEq     Input, Output, State
derive genShow Input, Output, State
derive ggen    Input, Output, State

searchTreeModel :: (State a) (Input a) -> [Trans Output (State a)] | Eq a
searchTreeModel s (Insert x)   = pt [] { s & elements = [x:s.elements] }
searchTreeModel s (Delete x)   = pt [] { s & elements = filter ((<>) x) s.elements }
searchTreeModel s (IsMember x) = pt [Member $ isMember x s.elements] s
searchTreeModel s IsEmpty      = pt [Emptiness $ isEmpty s.elements] s

searchTreeImpl  :: (SearchTree a) (Input a) -> ([Output], SearchTree a) | < a
searchTreeImpl  t (Insert x)   = ([], ins x t)
searchTreeImpl  t (Delete x)   = ([], del x t)
searchTreeImpl  t (IsMember x) = ([Member $ elem x t], t)
searchTreeImpl  t IsEmpty      = ([Emptiness $ empty t], t)

modelTestBase n type world
# (console,world) = stdio world
# console = console <<< "SearchTree " <<< n <<< " "
# (_, world) = fclose console world
# emptyState = { elements = asType [type] [] }
# emptyTree  = List2ST $ emptyState.elements
# test = testConfSM [] searchTreeModel emptyState searchTreeImpl emptyTree (const emptyTree) world
= snd test


// *** RUN! ***
Start world =
	( flatten logicTests
	, mtests modelTests world )


// *** Helper functions ***
($) infixr 0
($) f x = f x

pt o s = [Pt o s]

// Used to constrain the type (to get rid of polymorphism), discards first arg
asType :: a a -> a
asType x y = y

// monadic sequence
mtests [] world = world
mtests [x:xs] world
# world = x world
= mtests xs world
