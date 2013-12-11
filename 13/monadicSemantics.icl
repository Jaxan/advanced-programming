module monadicSemantics

// Joshua Moerman
// 3009408

import StdEnv, StdGeneric, GenMap, GenHylo

:: Op      = Plus | Minus | Times | Rem | Equal | LessThan
:: Var     :== String

:: ExpP a  = Int Int | Var Var | Op Op a a
:: Exp     :== Fix ExpP

:: StmP a  = Assign Var Exp | If Exp a a | While Exp a | Seq a a | Cont
:: Stm     :== Fix StmP

derive gMap ExpP, StmP, Fix

// Environment of integers
:: Env :== Var -> Int
empty = \v . 0

// Semantics type class
:: Sem :== Env -> (Int, Env)
class sem a :: a -> Sem

// return
rtn :: Int -> Sem
rtn i = \e. (i, e)

// the usual bind
(>>=) infixl 1 :: Sem (Int->Sem) -> Sem
(>>=) x y = \e. (\(i,e2).y i e2) (x e)
(>>|) infixl 1 :: Sem Sem -> Sem
(>>|) x y = x >>= \_. y

// read variable from environment
read :: Var -> Sem
read v = \e. (e v, e)

// assign value to give variable in environment
write :: Var Int -> Sem
write v i = \e. (i, \w. if (w==v) i (e w))

// semantics of operators
operator :: Op -> Int -> Int -> Int
operator Plus     = (+)
operator Minus    = (-)
operator Times    = (*)
operator Rem      = rem
operator Equal    = \x y . if (x==y) 1 0
operator LessThan = \x y . if (x< y)  1 0

// semantics of expressions
instance sem Exp where
	sem x = cata phi x where
		phi (Int n)     = rtn n
		phi (Var v)     = read v
		phi (Op op x y) = x >>= \v1. y >>= return o (operator op v1)

// semantics of statments
// NOTE: while will always return 0, as it might not even be executed
instance sem Stm where
	sem x = cata phi x where
		phi (Assign v e)     = sem e >>= write v
		phi (If e s1 s2)     = sem e >>= \b . if (b<>0) s1 s2
		phi stm=:(While e s) = sem e >>= \b . if (b<>0) (s >>| phi stm) (phi Cont)
		phi (Seq s1 s2)      = s1 >>| s2  // Here the cata finally pays off :D
		phi Cont             = rtn 0

// convenience functions
int    = In o Int
var    = In o Var
op o   = In o2 (Op o)
assign = In o2 Assign
ifte e = In o2 (If e)
while  = In o2 While
seq    = In o2 Seq
cont   = In Cont

// test case, also testing the new operator <
pEuclides =
	while (op LessThan (int 0) (var "b"))(
		seq (assign "r" (op Rem (var "a") (var "b")))
		(seq (assign "a" (var "b"))
		( (assign "b" (var "r")))
		)
	)

Start
# p = pEuclides
# args = write "a" 9 >>| write "b" 12
# (_, s) = args empty
# (_, s) = sem p s
= s "a"

(o2) infixr 9
(o2) f g x :== f o (g x)
