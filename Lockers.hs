module Lockers
( lockerLookup
, lockers 
) where

import qualified Data.Map as Map

-- data declarations
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
data LockerState = Taken | Free deriving (Show, Eq)

-- type synonyms
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

-- the main func
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
	case Map.lookup lockerNumber map of
		Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
		Just (state, code) -> if state /= Taken
								then Right code
								else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
	[(100,(Taken,"WRAA"))
	,(101,(Free,"@#L!"))
	,(102,(Free,"3#U!"))
	]
