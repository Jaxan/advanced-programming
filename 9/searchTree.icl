implementation module searchTree

/*
	Pieter Koopman 2013 pieter@cs.ru.nl
	Search tree implementation for Advanced Programming
*/

import StdEnv, gast

:: SearchTree a = Leaf | Node (SearchTree a) a (SearchTree a)

newST :: SearchTree a
newST = Leaf

empty :: (SearchTree a) -> Bool
empty Leaf = True
empty tree = False

ins :: a (SearchTree a) -> SearchTree a | < a
ins a Leaf = Node Leaf a Leaf
ins a t0=:(Node l b r)
	| a < b = variant t1 t1 t2  t0 t3 t1  t1 t1 t1  t1 t1
	| a > b = variant t2 t2 t1  t1 t1 t4  t2 t2 t2  t2 t2
	        = variant t0 t1 t0  t0 t0 t0  t0 t0 t0  t0 t0
where
	t1 = Node (ins a l) b r
	t2 = Node l b (ins a r)
	t3 = Node l a (ins b r)
	t4 = Node l a (ins a l)

elem :: a (SearchTree a) -> Bool | < a
elem a Leaf = False
elem a (Node l b r)
	| a < b = elem a l
	| a > b = elem a r
	        = True

del :: a (SearchTree a) -> SearchTree a | < a
del a Leaf = Leaf
del a t0=:(Node l b r)
	| a < b = variant t1 t1 t1  t1 t1 t1  t1 t3 t1  t1 t1
	| a > b = variant t2 t2 t2  t2 t2 t2  t0 t2 t4  t2 t2
where
	t1 = Node (del a l) b r
	t2 = Node l b (del a r)
	t3 = Node (del b l) b r
	t4 = Node r b (del a r)
del a (Node Leaf b r) = r
del a (Node l b Leaf) = l
del a (Node l b r) = variant t1 t1 t1  t1 t1 t1  t1 t1 t1  t2 s
where
	t1 = Node l c s
	t2 = Node l c r
	(c, s) = smallest r
	smallest (Node Leaf x r) = (x, r)
	smallest (Node l x r) = (y, Node m x r)
	where (y, m) = smallest l

ST2List :: (SearchTree a) -> [a]
ST2List tree = scan tree []
where
	scan Leaf c = c
	scan (Node l a r) c = scan l [a:scan r c]

List2ST :: ([a] -> SearchTree a) | < a
List2ST = foldr ins newST

derive gEq SearchTree
derive genShow SearchTree
ggen{|SearchTree|} a n r = abort "do not use generic generation for SearchTrees"

