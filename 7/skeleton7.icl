module skeleton7

// Joshua Moerman
// 3009408

import iTasks

:: Question :== String
:: Answer = StronglyAgree | Agree | Neutral | Disagree | StronglyDisagree
:: MyProgress = { current :: Int, total :: Int }
derive class iTask Answer, MyProgress

// Almost the same as sequence, from CommonCombinators. I was not in the mood of using a fold.
seqWithProgress :: ![Task a]  -> Task [a] | iTask a
seqWithProgress tasks = seqTasks tasks {current = 1, total = length tasks} where
	seqTasks [] _     = return []
	seqTasks [t:ts] p
		  = viewInformation "Progress in Questionaire:" [] p ||- t 
		>>= \a -> seqTasks ts {p & current = p.current + 1} 
		>>= \as -> return [a:as]

// Is now almost a oneliner
answerQuestionaire :: [Question] -> Task [Answer]
answerQuestionaire l = seqWithProgress (map q l)
	where q x = enterInformation x []

// To simulate a real-world questionaire, the questions should be in Dutch, while the framework is in English :).
Start :: *World -> *World
Start world = startEngine task world where
	task = answerQuestionaire test >>= viewInformation "Your answers:" []
	test =
		[ "Ik ben tevreden met het product."
		, "Ik zou het product aanbevelen bij mijn collega's en/of zakenrelaties."
		, "Ik denk het product in de toekomst te gebruiken?" ]
