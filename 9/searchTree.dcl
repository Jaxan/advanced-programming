definition module searchTree

/*
	Pieter Koopman 2013 pieter@cs.ru.nl
	Search tree implementation for Advanced Programming
*/

import StdEnv, gast

:: SearchTree a

newST	:: SearchTree a
del     :: a (SearchTree a) -> SearchTree a | < a
ins     :: a (SearchTree a) -> SearchTree a | < a
elem    :: a (SearchTree a) -> Bool | < a
empty   ::   (SearchTree a) -> Bool

ST2List :: (SearchTree a) -> [a]
List2ST :: ([a] -> SearchTree a) | < a

derive gEq		SearchTree
derive genShow	SearchTree
derive ggen 	SearchTree

/*
	for testing purposes
	Change the right hand side to one of the variables to obtain the corresponding variant
*/
variant a b c  d e f  g h i  j k :== a
