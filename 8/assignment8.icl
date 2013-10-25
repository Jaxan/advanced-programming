module assignment8

// Joshua Moerman
// 3009408

import StdEnv, StdIO, gast

:: State   = { digits :: [Digit], coins :: [Coin] }
:: In      = D Digit | Go | Reset | Return | C Coin
:: Out     = Display [String] | Product Product | Change [Coin]
:: Digit   = Digit Int   // numbers 0..9
:: Coin    = Coin Int    // 5, 10, 20, 50, 100, 200 cents
:: Product = { name :: String, price :: Int }

ggen {|Digit|} n r = randomize (map Digit [0 .. 9]) r 10 (const [])
ggen {|Coin|}  n r = randomize (map Coin [5, 10, 20, 50, 100, 200]) r 6 (const [])

derive gEq     State, In, Out, Digit, Coin, Product
derive genShow State, In, Out, Digit, Coin, Product
derive bimap   []
derive ggen    In

// *** Model ***
// TODO: check correctness product
vendingMachineModel :: State In -> [Trans Out State]
vendingMachineModel s=:{digits=[x:y:rs]} (D d)     = ignore s
vendingMachineModel s                    (D d)     = ignore {s & digits = [d:s.digits]}
vendingMachineModel s=:{digits=[x:y]}    Go        = [Ft fun] where
	fun out // we should get the right product
		# products = getProducts out
		| length products > 1 = []              // more that 1 products is impossible
		| length products < 1 = ignore2 s out   // we shouldn't get change either
		# prod = hd products
		# change = sum o map fromCoin o getCoins $ out
		# inserted = sum o map fromCoin $ s.coins
		| change > inserted - prod.price = []   // shouldn't get too much mony back
		| otherwise           = [emptyState]    // everything went well
vendingMachineModel s                    Go        = ignore s
vendingMachineModel s                    Reset     = ignore {s & digits = []}
vendingMachineModel s                    Return    = [Ft fun] where
	fun out // always return all change (not necessarily same coins)
		# change = sum o map fromCoin o getCoins $ out
		# inserted = sum o map fromCoin $ s.coins
		| change == inserted = [{s & coins = []}]
		| otherwise          = []
vendingMachineModel s                    (C c)     = ignore {s & coins = [c:s.coins]}

// Starting state
emptyState = {digits = [], coins = []}

// Don't allow products or change! Only ignore messages
ignore s = [Ft $ ignore2 s]
ignore2 s []             = [s]
ignore2 s [Display _:xs] = ignore2 s xs
ignore2 s [_:xs]         = []

// Some functions to process [Out]
getCoins []            = []
getCoins [Change l:xs] = l ++ getCoins xs
getCoins [_:xs]        = getCoins xs
fromCoin (Coin n)      = n

getProducts []             = []
getProducts [Product p:xs] = [p:getProducts xs]
getProducts [_:xs]         = getProducts xs

// *** Implementation ***
:: MachineState = Idle
vendingMachine :: MachineState In -> ([Out], MachineState)
vendingMachine s _ = ([], s)

startMachine = Idle;

Start :: *World -> *World
Start world
	# (time, world) = getCurrentTime world
	= snd $ testConfSM
		[Seed time.seconds]
		vendingMachineModel
		emptyState
		vendingMachine
		startMachine
		(const startMachine)
		world

// helper functions:
($) infixr 0
($) f x = f x

