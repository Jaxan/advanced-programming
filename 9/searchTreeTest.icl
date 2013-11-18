module searchTreeTest

// Joshua Moerman
// 3009408

import StdEnv, StdIO, searchTree, gast

/*
1. Of course we should test the SearchTree on Ints, as this is really the most
basic total order. I also added Strings and lists as these are commonly used.
So this motivates the tests. However, it is very easy to add tests for other
types, you only have to extend the following two functions by simply providing
a value of your favorite type (the actual value is not used). We don't have to
test SearchTree for non total orders, as this is a pre-condition.

2/3. See code below.

4.
The first three columns are logical properties (L), the second three model
based checking (M). A dot means passed, an X means failure. I, S and LR stand
for Int, String and [Real].
	LI	LS	LLR	MI	MS	MLR
a	.	.	.	.	.	.
b	X	X	X	X	X	X
c	X	X	X	X	X	X
d	X	X	X	X	X	X
e	X	X	X	X	X	X
f	X	X	X	X	X	X
g	X	X	X	X	X	X
h	X	X	X	X	X	X
i	X	X	X	X	X	X
j	.	.	.	X	X	X
k	.	.	.	X	X	X

5. This part I only did for Int (as the traces in the case of String are
generally longer and less readable). The longest traces took 664 (variant j)
and 386 (variant k) transitions. For the k variant I found the following trace
which shows incorrectness in 5 transiftions:
[Insert 0, Insert 1, Insert -1, Delete 0, IsMember -1]

For the k variant I found one in 6 transitions:
[Insert 1, Insert 0, Insert 2, Delete 1, Delete 2, IsMember 2]

6. Incidentally the variants j and k are also the ones for which the logical
propositions were not sufficient. For the j variant we indeed miss one property
(which is now clearly visible with the 5-transition failure): deleting should
not affect other members.
More mathematically: elem x t /\ x <> y ==> elem x (del y t)

*/

// *** Extend Here ***
logicTests =
	[ logicTestsBase "Int" 1
	, logicTestsBase "String" "bla"
	, logicTestsBase "[Real]" [1.0] ]

modelTests =
	[ modelTestBase "Int" 1
	, modelTestBase "String" "bla"
	, modelTestBase "[Real]" [1.0] ]

fixedInputs = FixedInputs
	[ [Insert 0, Insert 1, Insert -1, Delete 0, IsMember -1]
	, [Insert 1, Insert 0, Insert 2, Delete 1, Delete 2, IsMember 2] ]


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

// Specification is in terms of a list. Note that duplicates are allowed, but
// Delete should remove all of them, hence the filter (instead of removeMember)
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
# test = testConfSM [ErrorFile $ "test" +++ n +++ ".txt" /*, fixedInputs */]
         searchTreeModel emptyState searchTreeImpl newST (const newST) world
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
