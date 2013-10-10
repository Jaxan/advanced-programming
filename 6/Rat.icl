implementation module Rat
/*
	Pieter Koopman, pieter@cs.ru.nl
	AFP 2013
	This implementation contains errors on purpose!
	They should be revealed by testing the implementation.
*/
import StdEnv, gast

:: Rat = { n :: !Int, d :: !Int }

// generic functions needed for testing with Gast
derive gEq Rat
genShow{|Rat|} _ _ r c = [toString r:c]
ggen{|Rat|} n r = [{n=n,d=d}\\(n,d)<-diag2 [0:[i\\j<-[1..],i<-[j,~j]]] [1..]|gcd n d==1]	// for an older version of Gast
//ggen{|Rat|} s = [{n=n,d=d}\\(n,d)<-diag2 [0:[i\\j<-[1..],i<-[j,~j]]] [1..]|gcd n d==1]	// for a newer version of Gast

mkRat :: !Int !Int -> Rat
mkRat n d = simplify {n=n,d=d}

simplify :: !Rat -> Rat // normalisation, e.g. (2/-4) -> (-1/2).
simplify r=:{n,d}
	| d == zero	= abort "attempt to divide by 0 in simplifying a Rat"
	| n == one	= r
	| n == zero	= zero
	#! g = gcd n d
	| g==1
		| d<0	= { n= ~n, d= ~d }
				= r
		| d<0	= { n=  n/g, d= ~d/g }
				= { n=  n/g, d=  d/g }

class toRat a :: !a -> Rat
instance toRat Int where toRat i = {n=i, d=1}
instance toRat Real where toRat r = mkRat (toInt (r*10000.0)) 10000

inv :: Rat -> Rat
inv {n, d} = mkRat d n

instance one Rat where one = toRat 1
instance zero Rat where zero = toRat 0
instance toString Rat where toString {n,d} = toString n + if (d == 1) "" ("/" + toString d)
instance toInt Rat where toInt {n,d} = n/d
instance toReal Rat where toReal {n,d} = toReal n/toReal d
instance == Rat where (==) x y = x === y
instance + Rat where (+) x y = mkRat (x.n*y.d+y.n*x.d) (x.d*y.d)
instance * Rat where (*) x y = mkRat (x.n*y.n) (x.d*y.d)
instance / Rat where (/) x y = mkRat (x.n*y.d) (x.d*y.d)
instance - Rat where (-) x y = mkRat (x.n*y.d+y.n*x.d) (x.d*y.d)
instance < Rat where (<) x y = y.n*x.d < y.d*x.n
instance ^ Rat 
where
	(^) x y=:{n,d}
		| n == zero	= one
		| n == one	= x
		| isOdd n	= x * x^(y-one)
					= z*z where z = x^{n=n/2,d=d}
