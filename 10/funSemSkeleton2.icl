module funSemSkeleton2

import StdEnv, gast

// Joshua Moerman
// 3009408

/********************** data types **********************/

:: Var = VI Ident
:: Fun = FI Ident
unVI (VI i) = i

:: Ident :== String

:: Expr
	= Int   Int
	| Bool  Bool
	| Fun   Fun
	| Var   Var
	| Ap    Expr [Expr]
	| Infix Expr Prim Expr
	| Prim  Prim

:: Prim = IF | +. | *. | -. | <. | NOT

:: Def = Def Ident [Var] Expr

/********************** environments **********************/

:: State :== Env Expr
:: Funs  :== Env Def
:: Env e :== Ident -> e

(|->) infix 9 :: Ident v -> (Env v) -> (Env v)
(|->) v e = \env x.if (x==v) e (env x)

newEnv :: Env e
newEnv = \v.abort ("No binding for " + v)

/********************** Semantic functions **********************/

// some nice diagnosis for bad input
abortIn s e = abort $ " " +++ s +++ " in:\n\t" +++ show1 e +++ "\n"

// put state and functions in front (makes map easier)
E2 :: State Funs Expr -> Expr
E2 vars funs (Int i)          = Int i
E2 vars funs (Bool b)         = Bool b
E2 vars funs (Fun f)          = Fun f
E2 vars funs (Var (VI v))     = E2 vars funs $ vars v
E2 vars funs (Ap e1 es)
	# es = map (E2 vars funs) es   // strict evaluation
	# ap = E2 vars funs e1
	= case ap of
		Fun (FI f)
			# (Def _ vs e) = funs f
			# mapping = curry zip (map unVI vs) es
			# vars = foldr (uncurry (|->)) newEnv mapping
			= E2 vars funs e
		Prim IF  = case es of
			[Bool b, x, y] = if b x y
			_              = abortIn "If was not applicable" es
		Prim NOT = case es of
			[Bool b] = Bool $ not b
			_        = abortIn "Expression does not apply to NOT" es
		Prim op  = case es of
			[e1, e2] = E2 vars funs (Infix e1 op e2)
			_        = abortIn "Wrong number of arguments for operator" es
		_        = abortIn "Expression did not reduce to an applicable" ap
E2 vars funs (Infix e1 op e2)
	# e1 = E2 vars funs e1         // strict evaluation
	# e2 = E2 vars funs e2
	= case (e1, op, e2) of
		(Int x, +., Int y) = Int $ x + y
		(Int x, *., Int y) = Int $ x * y
		(Int x, -., Int y) = Int $ x - y
		(Int x, <., Int y) = Bool $ x < y
		_                  = abortIn "Binary operation does not apply" (e1, op, e2)
E2 vars funs (Prim p)         = Prim p

E :: Expr State Funs -> Expr
E expr vars funs = E2 vars funs expr

Ds :: [Def] -> Expr
Ds defs
	# (funs, starts) = foldl add (newEnv, []) defs
	= case starts of
		[start] = E start newEnv funs
		[]      = abortIn "No Start rule given" defs
		_       = abortIn "Multiple start rules given" defs
	where
		//add :: Def (Funs, [Def]) -> (Funs, [Def])
		add (funs, starts) d=:(Def id vs e)
		| id == "Start"
			| length vs == 0 = ((id |-> d) funs, [e:starts])
			| otherwise      = abortIn "Start rule can not take arguments" d
		| otherwise   = ((id |-> d) funs, starts)


/********************** Properties *********************/

p1 i j = (\p. prfx i j p === infx i j p) For [+. , *. , -. , <.] where
	prfx i j p = E (Ap (Prim p) [Int i, Int j]) newEnv newEnv
	infx i j p = E (Infix (Int i) p (Int j)) newEnv newEnv

/********************** instances of generic functions for gast **********************/

derive gEq		Expr, Prim, Def, Var, Fun
derive genShow	Expr, Prim, Def, Var, Fun
derive ggen		Expr, Prim, Def, Var, Fun	// likely to be improved for real tests

derive bimap []
/********************** some unit tests using gast **********************/

Start
 = ( Test []
	[ ("a" |-> "a") (("b" |-> "b") newEnv) "b" == "b"
	, ("a" |-> "a") (("b" |-> "b") newEnv) "a" == "a"
	, ("a" |-> "a") (("a" |-> "b") newEnv) "a" == "a"
	, E (Ap (Fun (FI "id")) [Int 7]) newEnv (("id" |-> ID) newEnv) === Int 7
	, E (Ap (Prim +.) [Int 3, Int 5]) newEnv newEnv === Int 8
	, E (Infix (Int 3) +. (Int 5)) newEnv newEnv === Int 8
	, E (Ap (Fun (FI "max")) [Int 3, Int 5]) newEnv (("max" |-> MAX) newEnv) === Int 5
	, E (Ap (Fun (FI "max")) [Int 5, Int 3]) newEnv (("max" |-> MAX) newEnv) === Int 5
	, E (Ap (Fun (FI "max")) [Int 5, Int 3]) newEnv (("fac" |-> FAC) (("max" |-> MAX) newEnv)) === Int 5
	, E (Infix (Infix (Int 5) -. (Int 1)) <. (Int 2)) newEnv newEnv === Bool False
	, E (Ap (Fun (FI "dec")) [Int 3]) newEnv (("dec" |-> DEC) newEnv) === Int 2
	, E (Ap (Prim +.) [Ap (Prim +.) [Int 3, Int 5], Int 5]) newEnv newEnv === Int 13
	, E (Ap (Fun (FI "dec")) [Ap (Prim +.) [Int 3, Int 5]]) newEnv (("dec" |-> DEC) newEnv) === Int 7
	, E (Ap (Fun (FI "count")) [Int 0]) newEnv (("count" |-> COUNT) newEnv) === Int 1
	, E (Ap (Fun (FI "count")) [Int 1]) newEnv (("count" |-> COUNT) newEnv) === Int 1
	, E (Ap (Fun (FI "fac")) [Int 1]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 1
	, E (Ap (Fun (FI "fac")) [Int 2]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 2
	, E (Ap (Fun (FI "fac")) [Int 3]) newEnv (("fac" |-> FAC) (("dec" |-> DEC) newEnv)) === Int 6
//	, E (Ap (Fun (FI "twice")) [Fun (FI "inc"),Int 0]) newEnv (("inc" |-> INC) (("twice" |-> TWICE) newEnv)) === Int 3 // higher order
	, Ds [start0  :defs] === Int 42
	, Ds [start1 1:defs] === Int 1
	, Ds [start1 2:defs] === Int 2
	, Ds [start1 3:defs] === Int 6
	, Ds [start1 4:defs] === Int 24
	]
	, Test [] p1
	)

start0   = Def "Start" [] (Int 42)
start1 i = Def "Start" [] (Ap (Fun (FI "fac")) [Int i])

defs = [ID, DEC, INC, FAC, MAX, COUNT]


ID  = Def "id" [(VI "x")] (Var (VI "x"))
MAX = Def "max" [(VI "x"),(VI "y")] (Ap (Prim IF) [Ap (Prim <.) [Var (VI "x"),Var (VI "y")],Var (VI "y"),Var (VI "x")])
DEC = Def "dec" [(VI "x")] (Ap (Prim -.) [Var (VI "x"),Int 1])
INC = Def "inc" [(VI "x")] (Infix (Var (VI "x")) +. (Int 1))
COUNT = Def "count" [(VI "x")] (Ap (Prim IF)
									[Infix (Var (VI "x")) <. (Int 1)
									,Int 1
									,Ap (Fun (FI "count")) [Infix (Var (VI "x")) -. (Int 1)]
									]
						  )
FAC = Def "fac" [(VI "x")] (Ap (Prim IF) [Infix (Var (VI "x")) <. (Int 2)
									,Int 1
									,Ap (Prim *.) [Var (VI "x")
													 ,Ap (Fun (FI "fac")) [Ap (Fun (FI "dec")) [Var (VI "x")]]
													 ]
									]
						  )
TWICE = Def "twice" [(VI "f"),(VI "x")] (Ap (Var (VI "f")) [Ap (Var (VI "f")) [Var (VI "x")]]) // this is higher order !!

// *** Helper functions ***
($) infixr 0
($) f x = f x
