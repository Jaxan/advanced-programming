module searchTreeTest

// Joshua Moerman
// 3009408

import StdEnv, searchTree, gast

// We cannot test polymorphic functions, so I made the type explicit
intList2ST :: ([Int] -> SearchTree Int)
intList2ST = List2ST

// Some logical properties of trees
memberAfterIns    t x = elem x (ins x t)
notMemberAfterDel t x = not (elem x (del x t))
uniqueness        t x = elem x t ==> t === (ins x t)
removeNothing     t x = not (elem x t) ==> t === (del x t)
emptiness         t x = e1 /\ e2 where
	e1 = empty t ==> not (elem x t)
	e2 = elem x t ==> not (empty t)

Start = Test []
	[ label "memberAfterIns"    $ memberAfterIns o intList2ST
	, label "notMemberAfterDel" $ notMemberAfterDel o intList2ST
	, label "uniqueness"        $ uniqueness o intList2ST
	, label "removeNothing"     $ removeNothing o intList2ST
	, label "emptiness"         $ emptiness o intList2ST ]

// helper functions:
($) infixr 0
($) f x = f x
