module skeleton5

// Joshua Moerman
// 3009408

import iTasks

:: BasicIdea =
	{ title :: String
	, description :: Maybe Note }

:: Idea	=
	{ idea :: BasicIdea
	, owner :: Name
	, number :: Int
	, likes :: Int }

// We compare by unique number
instance == Idea where
	(==) i j = i.number == j.number

:: Name	:== String

derive class iTask BasicIdea, Idea

ideas :: Shared [Idea]
ideas = sharedStore "Ideas" []

// Step 1. enter name
identify :: Task Name
identify = enterInformation "Login with your name" []
	>>* [OnAction (Action "Login" []) (hasValue return)]

// We keep the list reversed-sorted, so last element gives highest number
addIdea :: (Int -> Idea) -> Task [Idea]
addIdea i = get ideas >>= \l -> set [i $ n l:l] ideas
	where n [] = 0 ; n [x:_] = x.number + 1

// Construct an idea
enterIdea :: Name -> Task (Int -> Idea)
enterIdea name = enterInformation "Give us your idea!" [EnterWith \i -> \n -> { idea = i, owner = name, number = n, likes = 0}]
	>>* [OnAction (Action "Add idea" []) (hasValue return)]

// Upper view
ideaCreator name = forever_ $ enterIdea name >>= addIdea

// Idea selector, with a detailed view, and some actions
selectIdea :: Name -> Task [Idea]
selectIdea name = enterChoiceWithShared "All ideas" [] ideas
	>&^ updateSharedInformation "Edit selected idea" []
	>>*	[ OnAction (Action "Ok" []) (hasValue ignore)
		, OnAction (Action "Delete all" []) (always deleteAll)
		, OnAction (Action "Delete" []) (ifValue (\i -> i.owner == name) deleteIdea)
		, OnAction (Action "Like" []) (ifValue (\i -> i.owner <> name) likeIdea) ]

// Actions
ignore :: a -> Task [Idea]
ignore _ = return []

deleteAll :: Task [Idea]
deleteAll = set [] ideas

deleteIdea :: Idea -> Task [Idea]
deleteIdea i = update (filter ((<>) i)) ideas

likeIdea :: Idea -> Task [Idea]
likeIdea i = update (mapIf ((==) i) like) ideas
	where like j = {j & likes = j.likes + 1}

// Lower view
ideaSelector name = forever_ $ selectIdea name

// Combine this all
ideaEditor name = ideaCreator name -||- ideaSelector name

// And go:
Start :: *World -> *World
Start world = startEngine (identify >>= ideaEditor) world

// helper functions:
($) infixr 0
($) f x = f x

// ignore output, so that we can easily combine unrelated tasks
forever_ task = forever task >>| return Void

mapIf p f [] = []
mapIf p f [x:xs] = [if (p x) (f x) x:mapIf p f xs]
