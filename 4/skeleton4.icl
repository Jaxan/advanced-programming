module skeleton4

// Joshua Moerman
// 3009408

import iTasks

:: BasicIdea =
	{ title :: String
	, description :: Maybe Note }

:: Idea	=
	{ idea :: BasicIdea
	, owner :: Name
	, ideaNumber :: Int }

:: Name	:== String

derive class iTask BasicIdea, Idea

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   enterInformation "Enter your name" []
	>>= task

// It's not simply a map, since we want to count the ideas
addInformation :: Name -> [BasicIdea] -> [Idea]
addInformation name = addInformation` 0
	where
		addInformation` n [] = []
		addInformation` n [x:xs] = [{ idea = x, owner = name, ideaNumber = n} : addInformation` (n+1) xs]

editIdeas :: Name -> Task [Idea]
editIdeas name = enterInformation "Add your ideas" [EnterWith \x -> addInformation name x]
// OR:         = enterInformation "Add your ideas" [] @ addInformation name
// I'm not sure what the pragmatic way is...
	
Start :: *World -> *World
Start world = startEngine (doIdentified editIdeas >>! viewInformation "Your ideas" []) world
