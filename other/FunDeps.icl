module FunDeps

import StdEnv

class toGen a b where
	toGen :: a -> b

instance toGen Int Int where
	toGen n = n

class something a where
	something :: a -> String

instance something Int where
	something _ = "Int"

instance something String where
	something _ = "String"

test = something (toGen 5)

Start = test
