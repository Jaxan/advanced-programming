definition module Rat
/*
	Pieter Koopman, pieter@cs.ru.nl
	AFP 2013
*/
import StdEnv, gast

:: Rat
mkRat :: !Int !Int -> Rat

class toRat a :: !a -> Rat
instance toRat Int, Real
instance one Rat
instance zero Rat
instance toString Rat
instance toInt Rat
instance toReal Rat
instance == Rat
instance * Rat 
instance / Rat 
instance + Rat 
instance - Rat 
instance < Rat 
instance ^ Rat 

inv :: Rat -> Rat // inverse

// generic functions needed for testing with Gast
derive gEq Rat
genShow{|Rat|} r = [toString r]
derive ggen Rat
