module assignment8

import StdEnv, StdIO, gast

:: MaybeEmpty a = Thing a | Empty | Unknown

// Would be nicer as a function, but equality becomes an issue...
:: Stock :== [[MaybeEmpty Product]]

// stock remembers what (x,y) returned, products should be consistent
// Once an apple means: always an apple _or_ empty
:: State   = { digits :: [Digit], coins :: [Coin], stock :: Stock }
:: In      = D Digit | Go | Reset | Return | C Coin
:: Out     = Display [String] | Product Product | Change [Coin] | NoSuchDigit
:: Digit   = Digit Int   // numbers 0..9
:: Coin    = Coin Int    // 5, 10, 20, 50, 100, 200 cents
:: Product = { description :: String, price :: Int }

ggen {|Digit|} n r = randomize (map Digit [0 .. 9]) r 10 (const [])
ggen {|Coin|}  n r = randomize (map Coin [5, 10, 20, 50, 100, 200]) r 6 (const [])

derive gEq     MaybeEmpty, State, In, Out, Digit, Coin, Product
derive genShow MaybeEmpty, State, In, Out, Digit, Coin, Product
derive bimap   []
derive ggen    In

// At start, the stock is unknown
emptyStock = map (const $ map (const Unknown) l) l; where l = [0 .. 9]
emptyState s = {s & digits = [], coins = []}


// *************
// *** Model ***
vendingMachineModel :: State In -> [Trans Out State]
// User presses digits, maximum of 2 digits
vendingMachineModel s=:{digits=[c:r:rs]} (D d)     = ignore s
// User presses digit, we only should keep track of it, if it was valid
vendingMachineModel s                    (D d)     = [Ft fun] where
	fun out
		| any ((===) NoSuchDigit) out = ignore2 s out
		| otherwise = ignore2 {s & digits = [d:s.digits]} out
// User wants his coffe (only possible with 2 digits)
vendingMachineModel s=:{digits=[c,r]}    Go        = [Ft fun] where
	fun out // we should get the right product
		# products = getProducts out
		| length products > 1 = []              // more that 1 products is impossible
		| length products < 1 = ignore2 s out   // So it's empty, we shouldn't get change
		# prod = hd products
		# isExpected = case s.stock !? r !? c of
			Thing p -> prod === p           // (r,c) should always give the same product
			Empty   -> False                // we assume the machine doesn't get refilled
			Unknown -> True                 // this product we havent seen yet
		| not isExpected      = []              // not the product we're looking for
		# change = sum o map fromCoin o getCoins $ out
		# inserted = sum o map fromCoin $ s.coins
		| change > inserted - prod.price = []   // shouldn't get too much mony back
		| otherwise           = [setProduct (emptyState s) r c (Thing prod)] // everything went well
vendingMachineModel s                    Go        = ignore s
// User wants a different product
vendingMachineModel s                    Reset     = ignore {s & digits = []}
// User doesn't want something at all (give back his money!)
vendingMachineModel s                    Return    = [Ft fun] where
	fun out // always return all change (not necessarily same coins)
		# change = sum o map fromCoin o getCoins $ out
		# inserted = sum o map fromCoin $ s.coins
		| change == inserted = [{s & coins = []}]
		| otherwise          = []
// User lacks financial skills
vendingMachineModel s                    (C c)     = ignore {s & coins = [c:s.coins]}

// some helper functions: update stock information
(!?) infixl 9
(!?) l (Digit d) = l !! d
setProduct s (Digit r) (Digit c) p = {s & stock = updateAt r (updateAt c p (s.stock !! r)) s.stock}

// Don't allow products or change! Only ignore messages!
ignore s = [Ft $ ignore2 s]
ignore2 s []               = [s]
ignore2 s [Display _:xs]   = ignore2 s xs
ignore2 s [NoSuchDigit:xs] = ignore2 s xs
ignore2 s [_:xs]           = []

// Some functions to process [Out]
getCoins []            = []
getCoins [Change l:xs] = l ++ getCoins xs
getCoins [_:xs]        = getCoins xs
fromCoin (Coin n)      = n

getProducts []             = []
getProducts [Product p:xs] = [p:getProducts xs]
getProducts [_:xs]         = getProducts xs


// **********************
// *** Implementation ***

// :: In      = D Digit | Go | Reset | Return | C Coin
// :: Out     = Display [String] | Product Product | Change [Coin]

:: MachineState   = { drigits :: [Digit], croins :: [Coin], strock :: Digit -> Maybe (Digit -> [Product]) }

derive genShow Maybe, MachineState

// Super simple vendingmachine, which keeps all the change
vendingMachine :: MachineState In -> ([Out], MachineState)
vendingMachine s (D d)
| validDigit d s = ([], {s & drigits = [d:s.drigits]})
| otherwise      = ([Display ["Please press a different button"], NoSuchDigit], s)
vendingMachine s=:{drigits=[c,r]} Go
# prod = hd $ (fromJust $ s.strock r) c
# money = sum o map fromCoin $ s.croins
| prod.price > money = ([Display ["Please insert more money"]], s)
| otherwise          = ([Display ["Enjoy your:", prod.description], Product prod], resetDigits $ keepMoney $ popProduct r c s)
vendingMachine s Go = ([Display ["Please select a product first"]], s)
vendingMachine s Reset = ([Display ["Ok, start again"]], resetDigits s)
vendingMachine s Return
| isEmpty s.croins = ([Display ["No money inserted"]], s)
| otherwise      = ([Change s.croins], {s & croins = []})
vendingMachine s (C c) = ([Display ["You inserted money :)"]], {s & croins = [c:s.croins]})

// some helper functions
validDigit d s=:{drigits = []} = case s.strock d of
	Nothing -> False
	Just f  -> True
validDigit d s=:{drigits = [x]} = case s.strock x of
	Nothing -> False
	Just f  -> case f d of
		[] -> False
		_  -> True
validDigit _ _ = False

resetDigits s    = {s & drigits = []}
keepMoney s      = {s & croins = []}
popProduct r c s = {s & strock = fun} where
	fun r2 = if_ (r =!= r2) (s.strock r2) $ case s.strock r of
		Nothing = Nothing
		Just f	# tail = tl (f c)
			| and [isEmpty tail:[isEmpty (f y) \\ y <- map Digit [0..9] | y =!= c]] = Nothing
			| otherwise = Just $ \c2 . if_ (c =!= c2) (f c2) tail

// some random stock to start with
// someProducts = map (\(x,y) -> {description = x, price = y})
// 	[ ("Mars", 100)
// 	, ("Twix", 120)
// 	, ("Stroopwafel", 60)
// 	, ("Gevulde koek", 40)
// 	, ("Slagroomtaart", 400)
// 	, ("Kaki fruit", 90)
// 	, ("Condoom", 10)
// 	, ("Banaan", 80)
// 	, ("Appel", 70)
// 	, ("Mandarijn", 70) ]
// getSomeProduct n = someProducts !! (n rem (length someProducts))
// startStrock (Digit n)
// | n > 7     = Nothing // some empty rows
// | otherwise = Just fun where
// 	fun (Digit m)
// 	| m rem 3 <> 0 = []
// 	| otherwise    = repeatn (m*n+1) (getSomeProduct (m + n))

startStrock (Digit 0) = Just fun where
	fun (Digit 0) = repeatn 10 {description = "Appel", price = 50}
	fun (Digit 1) = repeatn 3 {description = "Banaan", price = 70}
	fun _ = []
startStrock _ = Nothing

startMachine = { drigits = [], croins = [], strock = startStrock }


// ***************
// *** Testing ***
Start :: *World -> *World
Start world
	# (time, world) = getCurrentTime world
	= snd $ testConfSM
		[Seed time.seconds]
		vendingMachineModel
		{digits = [], coins = [], stock = emptyStock}
		vendingMachine
		startMachine
		(const startMachine)
		world

// helper functions:
($) infixr 0
($) f x = f x
if_ b x y = if b x y // more useful as function
